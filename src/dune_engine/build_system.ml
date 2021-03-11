open! Stdune
open Import
open Memo.Build.O

let () = Hooks.End_of_build.always Memo.reset

module Fs : sig
  val mkdir_p : Path.Build.t -> unit Memo.Build.t

  (** Creates directory if inside build path, otherwise asserts that directory
      exists. *)
  val mkdir_p_or_check_exists : loc:Loc.t -> Path.t -> unit Memo.Build.t

  val assert_exists : loc:Loc.t -> Path.t -> unit Memo.Build.t
end = struct
  let mkdir_p_def =
    Memo.create "mkdir_p" ~doc:"mkdir_p"
      ~input:(module Path.Build)
      ~output:(Simple (module Unit))
      ~visibility:Hidden Async
      (fun p ->
        Path.mkdir_p (Path.build p);
        Memo.Build.return ())

  let mkdir_p = Memo.exec mkdir_p_def

  let assert_exists_def =
    Memo.create "assert_path_exists" ~doc:"Path.exists"
      ~input:(module Path)
      ~output:(Simple (module Bool))
      ~visibility:Hidden Async
      (fun p -> Memo.Build.return (Path.exists p))

  let assert_exists ~loc path =
    Memo.exec assert_exists_def path >>| function
    | false ->
      User_error.raise ~loc
        [ Pp.textf "%S does not exist" (Path.to_string_maybe_quoted path) ]
    | true -> ()

  let mkdir_p_or_check_exists ~loc path =
    match Path.as_in_build_dir path with
    | None -> assert_exists ~loc path
    | Some path -> mkdir_p path
end

(* [Promoted_to_delete] is used mostly to implement [dune clean]. It is an
   imperfect heuristic, in particular it can go wrong if:

   - the user deletes .to-delete-in-source-tree file

   - the user edits a previously promoted file with the intention of keeping it
   in the source tree, or creates a new file with the same name *)
module Promoted_to_delete : sig
  val add : Path.t -> unit

  val remove : Path.t -> unit

  val mem : Path.t -> bool

  val get_db : unit -> Path.Set.t
end = struct
  module P = Dune_util.Persistent.Make (struct
    type t = Path.Set.t

    let name = "PROMOTED-TO-DELETE"

    let version = 1
  end)

  let fn = Path.relative Path.build_dir ".to-delete-in-source-tree"

  (* [db] is used to accumulate promoted files from rules. *)
  let db = lazy (ref (Option.value ~default:Path.Set.empty (P.load fn)))

  let get_db () = !(Lazy.force db)

  let set_db new_db = Lazy.force db := new_db

  let needs_dumping = ref false

  let modify_db f =
    match f (get_db ()) with
    | None -> ()
    | Some new_db ->
      set_db new_db;
      needs_dumping := true

  let add p =
    modify_db (fun db ->
        if Path.Set.mem db p then
          None
        else
          Some (Path.Set.add db p))

  let remove p =
    modify_db (fun db ->
        if Path.Set.mem db p then
          Some (Path.Set.remove db p)
        else
          None)

  let dump () =
    if !needs_dumping && Path.build_dir_exists () then (
      needs_dumping := false;
      get_db () |> P.dump fn
    )

  let mem p = Path.Set.mem !(Lazy.force db) p

  let () = Hooks.End_of_build.always dump
end

let files_in_source_tree_to_delete () = Promoted_to_delete.get_db ()

let alias_exists_fdecl = Fdecl.create (fun _ -> Dyn.Opaque)

module Alias0 = struct
  include Alias

  let dep t = Action_builder.dep (Dep.alias t)

  let dep_multi_contexts ~dir ~name ~contexts =
    ignore (File_tree.find_dir_specified_on_command_line ~dir);
    let context_to_alias_expansion ctx =
      let ctx_dir = Context_name.build_dir ctx in
      let dir = Path.Build.(append_source ctx_dir dir) in
      dep (make ~dir name)
    in
    Action_builder.all_unit (List.map contexts ~f:context_to_alias_expansion)

  open Action_builder.O

  let dep_rec_internal ~name ~dir ~ctx_dir =
    let f dir =
      let path = Path.Build.append_source ctx_dir (File_tree.Dir.path dir) in
      Action_builder.dep_on_alias_if_exists (make ~dir:path name)
    in
    let module A = struct
      type t = bool Action_builder.t

      let empty = Action_builder.return false

      let combine = Action_builder.map2 ~f:( || )
    end in
    (* jeremiedimino: this wrapping is non-sense, we should be able to do the
       map_reduce directly in the Action_builder monad. TBC. *)
    let* x =
      Action_builder.memo_build
        (File_tree.Dir.map_reduce
           (module A)
           dir ~traverse:Sub_dirs.Status.Set.normal_only
           ~f:(fun x -> Memo.Build.return (f x)))
    in
    x

  let dep_rec t ~loc =
    let ctx_dir, src_dir =
      Path.Build.extract_build_context_dir_exn (Alias.dir t)
    in
    match File_tree.find_dir src_dir with
    | None ->
      Action_builder.fail
        { fail =
            (fun () ->
              User_error.raise ~loc
                [ Pp.textf "Don't know about directory %s!"
                    (Path.Source.to_string_maybe_quoted src_dir)
                ])
        }
    | Some dir ->
      let name = Alias.name t in
      let+ is_nonempty = dep_rec_internal ~name ~dir ~ctx_dir in
      if (not is_nonempty) && not (is_standard name) then
        User_error.raise ~loc
          [ Pp.text "This alias is empty."
          ; Pp.textf "Alias %S is not defined in %s or any of its descendants."
              (Alias.Name.to_string name)
              (Path.Source.to_string_maybe_quoted src_dir)
          ]

  let dep_rec_multi_contexts ~dir:src_dir ~name ~contexts =
    let open Action_builder.O in
    let dir = File_tree.find_dir_specified_on_command_line ~dir:src_dir in
    let+ is_nonempty_list =
      Action_builder.all
        (List.map contexts ~f:(fun ctx ->
             let ctx_dir = Context_name.build_dir ctx in
             dep_rec_internal ~name ~dir ~ctx_dir))
    in
    let is_nonempty = List.exists is_nonempty_list ~f:Fun.id in
    if (not is_nonempty) && not (is_standard name) then
      User_error.raise
        [ Pp.textf "Alias %S specified on the command line is empty."
            (Alias.Name.to_string name)
        ; Pp.textf "It is not defined in %s or any of its descendants."
            (Path.Source.to_string_maybe_quoted src_dir)
        ]

  let package_install ~(context : Build_context.t) ~(pkg : Package.t) =
    let dir =
      let dir = Package.dir pkg in
      Path.Build.append_source context.build_dir dir
    in
    let name = Package.name pkg in
    sprintf ".%s-files" (Package.Name.to_string name)
    |> Alias.Name.of_string |> make ~dir
end

module Loaded = struct
  type build =
    { allowed_subdirs : Path.Unspecified.w Dir_set.t
    ; rules_produced : Rules.t
    ; rules_here : Rule.t Path.Build.Map.t
    ; rules_of_alias_dir : Rule.t Path.Build.Map.t
    ; aliases : (Loc.t * unit Action_builder.t) list Alias.Name.Map.t
    }

  type t =
    | Non_build of Path.Set.t
    | Build of build

  let no_rules ~allowed_subdirs =
    Build
      { allowed_subdirs
      ; rules_produced = Rules.empty
      ; rules_here = Path.Build.Map.empty
      ; rules_of_alias_dir = Path.Build.Map.empty
      ; aliases = Alias.Name.Map.empty
      }
end

module Dir_triage = struct
  type t =
    | Known of Loaded.t
    | Alias_dir_of of Path.Build.t
    | Need_step2
end

(* Stores information needed to determine if rule need to be reexecuted. *)
module Trace_db : sig
  module Entry : sig
    type t =
      { rule_digest : Digest.t
      ; dynamic_deps_stages : (Action_exec.Dynamic_dep.Set.t * Digest.t) list
      ; targets_digest : Digest.t
      }
  end

  val get : Path.t -> Entry.t option

  val set : Path.t -> Entry.t -> unit
end = struct
  module Entry = struct
    type t =
      { rule_digest : Digest.t
      ; dynamic_deps_stages : (Action_exec.Dynamic_dep.Set.t * Digest.t) list
      ; targets_digest : Digest.t
      }
  end

  (* Keyed by the first target of the rule. *)
  type t = Entry.t Path.Table.t

  let file = Path.relative Path.build_dir ".db"

  module P = Dune_util.Persistent.Make (struct
    type nonrec t = t

    let name = "INCREMENTAL-DB"

    let version = 4
  end)

  let needs_dumping = ref false

  let t =
    (* This [lazy] is safe: it does not call any memoized functions. *)
    lazy
      (match P.load file with
      | Some t -> t
      (* This mutable table is safe: it's only used by [execute_rule_impl] to
         decide whether to rebuild a rule or not; [execute_rule_impl] ensures
         that the targets are produced deterministically. *)
      | None -> Path.Table.create 1024)

  let dump () =
    if !needs_dumping && Path.build_dir_exists () then (
      needs_dumping := false;
      P.dump file (Lazy.force t)
    )

  let () = Hooks.End_of_build.always dump

  let get path =
    let t = Lazy.force t in
    Path.Table.find t path

  let set path e =
    let t = Lazy.force t in
    needs_dumping := true;
    Path.Table.set t path e
end

module Subdir_set = struct
  type t =
    | All
    | These of String.Set.t

  let to_dir_set = function
    | All -> Dir_set.universal
    | These s ->
      String.Set.fold s ~init:Dir_set.empty ~f:(fun path acc ->
          let path = Path.Local.of_string path in
          Dir_set.union acc (Dir_set.singleton path))

  let of_dir_set d =
    match Dir_set.toplevel_subdirs d with
    | Infinite -> All
    | Finite s -> These s

  let of_list l = These (String.Set.of_list l)

  let empty = These String.Set.empty

  let mem t dir =
    match t with
    | All -> true
    | These t -> String.Set.mem t dir

  let union a b =
    match (a, b) with
    | All, _
    | _, All ->
      All
    | These a, These b -> These (String.Set.union a b)

  let union_all = List.fold_left ~init:empty ~f:union
end

type extra_sub_directories_to_keep = Subdir_set.t

module Rule_fn = struct
  let loc_decl = Fdecl.create Dyn.Encoder.opaque

  let loc () = Fdecl.get loc_decl ()
end

module Context_or_install = struct
  type t =
    | Install of Context_name.t
    | Context of Context_name.t

  let to_dyn = function
    | Install ctx -> Dyn.List [ Dyn.String "install"; Context_name.to_dyn ctx ]
    | Context s -> Context_name.to_dyn s
end

type caching =
  { cache : (module Cache.Caching)
  ; check_probability : float
  }

type t =
  { contexts : Build_context.t Context_name.Map.t
  ; init_rules : Rules.t Fdecl.t
  ; gen_rules :
      (   Context_or_install.t
       -> (   dir:Path.Build.t
           -> string list
           -> extra_sub_directories_to_keep Memo.Build.t)
          option)
      Fdecl.t
  ; (* Package files are part of *)
    packages : (Path.Build.t -> Package.Id.Set.t Memo.Build.t) Fdecl.t
  ; mutable caching : caching option
  ; sandboxing_preference : Sandbox_mode.t list
  ; mutable rule_done : int
  ; mutable rule_total : int
  ; mutable errors : Exn_with_backtrace.t list
  ; vcs : Vcs.t list Fdecl.t
  ; promote_source :
         ?chmod:(int -> int)
      -> src:Path.Build.t
      -> dst:Path.Source.t
      -> Build_context.t option
      -> unit Fiber.t
  ; locks : (Path.t, Fiber.Mutex.t) Table.t
  ; build_mutex : Fiber.Mutex.t option
  }

let t = ref None

let set x =
  match !t with
  | None -> t := Some x
  | Some _ -> Code_error.raise "build system already initialized" []

let get_build_system () =
  match !t with
  | Some t -> t
  | None -> Code_error.raise "build system not yet initialized" []

let reset () = t := None

let t = get_build_system

let contexts () = (t ()).contexts

let pp_paths set =
  Pp.enumerate (Path.Set.to_list set) ~f:(fun p ->
      Path.drop_optional_build_context p
      |> Path.to_string_maybe_quoted |> Pp.verbatim)

let set_rule_generators ~init ~gen_rules =
  let t = t () in
  let (), init_rules = Rules.collect init in
  Fdecl.set t.init_rules init_rules;
  Fdecl.set t.gen_rules gen_rules

let set_vcs vcs =
  let open Fiber.O in
  let t = t () in
  let () = Fdecl.set t.vcs vcs in
  match t.caching with
  | None -> Fiber.return ()
  | Some ({ cache = (module Caching); _ } as caching) ->
    let+ caching =
      let+ with_repositories =
        let f ({ Vcs.root; _ } as vcs) =
          let+ commit = Memo.Build.run (Vcs.commit_id vcs) in
          { Cache.directory = Path.to_absolute_filename root
          ; remote = "" (* FIXME: fill or drop from the protocol *)
          ; commit
          }
        in
        let+ repositories = Fiber.parallel_map ~f (Fdecl.get t.vcs) in
        Caching.Cache.with_repositories Caching.cache repositories
      in
      match with_repositories with
      | Result.Ok cache ->
        let cache =
          (module struct
            let cache = cache

            module Cache = Caching.Cache
          end : Cache.Caching)
        in
        Some { caching with cache }
      | Result.Error e ->
        User_warning.emit
          [ Pp.textf "Unable to set cache repositiories, disabling cache: %s" e
          ];
        None
    in
    t.caching <- caching

let get_vcs () =
  let t = t () in
  Fdecl.get t.vcs

let get_cache () =
  let t = t () in
  t.caching

let get_dir_triage t ~dir =
  match Dpath.analyse_dir dir with
  | Source dir ->
    Dir_triage.Known
      (Non_build (Path.set_of_source_paths (File_tree.files_of dir)))
  | External _ ->
    Dir_triage.Known
      (Non_build
         (match Path.readdir_unsorted dir with
         | Error Unix.ENOENT -> Path.Set.empty
         | Error m ->
           User_warning.emit
             [ Pp.textf "Unable to read %s" (Path.to_string_maybe_quoted dir)
             ; Pp.textf "Reason: %s" (Unix.error_message m)
             ];
           Path.Set.empty
         | Ok filenames -> Path.Set.of_listing ~dir ~filenames))
  | Build (Regular Root) ->
    let allowed_subdirs =
      Subdir_set.to_dir_set
        (Subdir_set.of_list
           (([ Dpath.Build.alias_dir; Dpath.Build.install_dir ]
            |> List.map ~f:Path.Build.basename)
           @ (Context_name.Map.keys t.contexts
             |> List.map ~f:Context_name.to_string)))
    in
    Dir_triage.Known (Loaded.no_rules ~allowed_subdirs)
  | Build (Install Root) ->
    let allowed_subdirs =
      Context_name.Map.keys t.contexts
      |> List.map ~f:Context_name.to_string
      |> Subdir_set.of_list |> Subdir_set.to_dir_set
    in
    Dir_triage.Known (Loaded.no_rules ~allowed_subdirs)
  | Build (Alias p) ->
    let build_dir = Dpath.Target_dir.build_dir p in
    Alias_dir_of build_dir
  | Build (Invalid _) ->
    Dir_triage.Known (Loaded.no_rules ~allowed_subdirs:Dir_set.empty)
  | Build (Install (With_context _))
  | Build (Regular (With_context _)) ->
    Need_step2

let describe_rule (rule : Rule.t) =
  match rule.info with
  | From_dune_file { start; _ } ->
    start.pos_fname ^ ":" ^ string_of_int start.pos_lnum
  | Internal -> "<internal location>"
  | Source_file_copy -> "file present in source tree"

let report_rule_src_dir_conflict dir fn (rule : Rule.t) =
  let loc =
    match rule.info with
    | From_dune_file loc -> loc
    | Internal
    | Source_file_copy ->
      let dir =
        match Path.Build.drop_build_context dir with
        | None -> Path.build dir
        | Some s -> Path.source s
      in
      Loc.in_dir dir
  in
  User_error.raise ~loc
    [ Pp.textf "Rule has a target %s" (Path.Build.to_string_maybe_quoted fn)
    ; Pp.textf "This conflicts with a source directory in the same directory"
    ]

let report_rule_conflict fn (rule' : Rule.t) (rule : Rule.t) =
  let fn = Path.build fn in
  User_error.raise
    [ Pp.textf "Multiple rules generated for %s:"
        (Path.to_string_maybe_quoted fn)
    ; Pp.textf "- %s" (describe_rule rule')
    ; Pp.textf "- %s" (describe_rule rule)
    ]
    ~hints:
      (match (rule.info, rule'.info) with
      | Source_file_copy, _
      | _, Source_file_copy ->
        [ Pp.textf "rm -f %s"
            (Path.to_string_maybe_quoted (Path.drop_optional_build_context fn))
        ]
      | _ -> [])

(* This contains the targets of the actions that are being executed. On exit, we
   need to delete them as they might contain garbage *)
let pending_targets = ref Path.Build.Set.empty

let () =
  Hooks.End_of_build.always (fun () ->
      let fns = !pending_targets in
      pending_targets := Path.Build.Set.empty;
      Path.Build.Set.iter fns ~f:(fun p -> Path.unlink_no_err (Path.build p)))

let compute_targets_digests targets =
  match
    Path.Build.Set.to_list_map targets ~f:(fun target ->
        (target, Cached_digest.file (Path.build target)))
  with
  | l -> Some l
  | exception (Unix.Unix_error _ | Sys_error _) -> None

let compute_targets_digests_or_raise_error ~loc targets =
  let remove_write_permissions =
    (* Remove write permissions on targets. A first theoretical reason is that
       the build process should be a computational graph and targets should not
       change state once built. A very practical reason is that enabling the
       cache will remove write permission because of hardlink sharing anyway, so
       always removing them enables to catch mistakes earlier. *)
    (* FIXME: searching the dune version for each single target seems way
       suboptimal. This information could probably be stored in rules directly. *)
    if Path.Build.Set.is_empty targets then
      false
    else
      let _, src_dir =
        Path.Build.extract_build_context_dir_exn
          (Path.Build.Set.choose_exn targets)
      in
      let dir = File_tree.nearest_dir src_dir in
      let version = File_tree.Dir.project dir |> Dune_project.dune_version in
      version >= (2, 4)
  in
  let refresh =
    if remove_write_permissions then
      Cached_digest.refresh_and_chmod
    else
      Cached_digest.refresh
  in
  let good, bad =
    Path.Build.Set.fold targets ~init:([], []) ~f:(fun target (good, bad) ->
        let fn = Path.build target in
        match refresh fn with
        | digest -> ((target, digest) :: good, bad)
        | exception (Unix.Unix_error _ | Sys_error _) -> (good, fn :: bad))
  in
  match bad with
  | [] -> List.rev good
  | missing ->
    User_error.raise ~loc
      [ Pp.textf "Rule failed to generate the following targets:"
      ; pp_paths (Path.Set.of_list missing)
      ]

let sandbox_dir = Path.Build.relative Path.Build.root ".sandbox"

let rec with_locks t mutexes ~f =
  match mutexes with
  | [] -> f ()
  | m :: mutexes ->
    Fiber.Mutex.with_lock
      (Table.find_or_add t.locks m ~f:(fun _ -> Fiber.Mutex.create ()))
      (fun () -> with_locks t mutexes ~f)

let remove_old_artifacts ~dir ~rules_here ~(subdirs_to_keep : Subdir_set.t) =
  match Path.readdir_unsorted_with_kinds (Path.build dir) with
  | exception _ -> ()
  | Error _ -> ()
  | Ok files ->
    List.iter files ~f:(fun (fn, kind) ->
        let path = Path.Build.relative dir fn in
        let path_is_a_target = Path.Build.Map.mem rules_here path in
        if not path_is_a_target then
          match kind with
          | Unix.S_DIR -> (
            match subdirs_to_keep with
            | All -> ()
            | These set ->
              if not (String.Set.mem set fn) then Path.rm_rf (Path.build path))
          | _ -> Path.unlink (Path.build path))

let no_rule_found t ~loc fn =
  let fail fn ~loc =
    User_error.raise ?loc
      [ Pp.textf "No rule found for %s" (Dpath.describe_target fn) ]
  in
  let hints ctx =
    let candidates =
      Context_name.Map.keys t.contexts |> List.map ~f:Context_name.to_string
    in
    User_message.did_you_mean (Context_name.to_string ctx) ~candidates
  in
  match Dpath.analyse_target fn with
  | Other _ -> fail fn ~loc
  | Regular (ctx, _) ->
    if Context_name.Map.mem t.contexts ctx then
      fail fn ~loc
    else
      User_error.raise
        [ Pp.textf "Trying to build %s but build context %s doesn't exist."
            (Path.Build.to_string_maybe_quoted fn)
            (Context_name.to_string ctx)
        ]
        ~hints:(hints ctx)
  | Install (ctx, _) ->
    if Context_name.Map.mem t.contexts ctx then
      fail fn ~loc
    else
      User_error.raise
        [ Pp.textf
            "Trying to build %s for install but build context %s doesn't exist."
            (Path.Build.to_string_maybe_quoted fn)
            (Context_name.to_string ctx)
        ]
        ~hints:(hints ctx)
  | Alias (ctx, fn') ->
    if Context_name.Map.mem t.contexts ctx then
      fail fn ~loc
    else
      let fn =
        Path.append_source (Path.build (Context_name.build_dir ctx)) fn'
      in
      User_error.raise
        [ Pp.textf
            "Trying to build alias %s but build context %s doesn't exist."
            (Path.to_string_maybe_quoted fn)
            (Context_name.to_string ctx)
        ]
        ~hints:(hints ctx)

(* +-------------------- Adding rules to the system --------------------+ *)

let build_deps_fdecl = Fdecl.create (fun _ -> Dyn.Opaque)

module rec Load_rules : sig
  val load_dir : dir:Path.t -> Loaded.t Memo.Build.t

  val file_exists : Path.t -> bool Memo.Build.t

  val targets_of : dir:Path.t -> Path.Set.t Memo.Build.t

  val lookup_alias :
    Alias.t -> (Loc.t * unit Action_builder.t) list option Memo.Build.t
end = struct
  open Load_rules

  let create_copy_rules ~ctx_dir ~non_target_source_files =
    Path.Source.Set.to_list_map non_target_source_files ~f:(fun path ->
        let ctx_path = Path.Build.append_source ctx_dir path in
        let build = Action_builder.copy ~src:(Path.source path) ~dst:ctx_path in
        Rule.make
        (* There's an [assert false] in [prepare_managed_paths] that blows up if
           we try to sandbox this. *)
          ~sandbox:Sandbox_config.no_sandboxing build ~context:None ~env:None
          ~info:Source_file_copy)

  let compile_rules ~dir ~source_dirs rules =
    List.concat_map rules ~f:(fun rule ->
        assert (Path.Build.( = ) dir rule.Rule.dir);
        Path.Build.Set.to_list_map rule.action.targets ~f:(fun target ->
            if String.Set.mem source_dirs (Path.Build.basename target) then
              report_rule_src_dir_conflict dir target rule
            else
              (target, rule)))
    |> Path.Build.Map.of_list_reducei ~f:report_rule_conflict

  (* Here we are doing a O(log |S|) lookup in a set S of files in the build
     directory [dir]. We could memoize these lookups, but it doesn't seem to be
     worth it, since we're unlikely to perform exactly the same lookup many
     times. As far as I can tell, each lookup will be done twice: when computing
     static dependencies of a [Action_builder.t] with
     [Action_builder.static_deps] and when executing the very same
     [Action_builder.t] with [Action_builder.exec] -- the results of both
     [Action_builder.static_deps] and [Action_builder.exec] are cached. *)
  let file_exists fn =
    load_dir ~dir:(Path.parent_exn fn) >>| function
    | Non_build targets -> Path.Set.mem targets fn
    | Build { rules_here; _ } -> (
      match Path.as_in_build_dir fn with
      | None -> false
      | Some fn -> Path.Build.Map.mem rules_here fn)

  let targets_of ~dir =
    load_dir ~dir >>| function
    | Non_build targets -> targets
    | Build { rules_here; _ } ->
      Path.Build.Map.keys rules_here |> Path.Set.of_list_map ~f:Path.build

  let lookup_alias alias =
    load_dir ~dir:(Path.build (Alias.dir alias)) >>| function
    | Non_build _ ->
      Code_error.raise "Alias in a non-build dir"
        [ ("alias", Alias.to_dyn alias) ]
    | Build { aliases; _ } -> Alias.Name.Map.find aliases (Alias.name alias)

  let () =
    Fdecl.set alias_exists_fdecl (fun alias ->
        lookup_alias alias >>| function
        | None -> false
        | Some _ -> true)

  let compute_alias_rules ~context_name ~(collected : Rules.Dir_rules.ready)
      ~dir ~sub_dir =
    let alias_dir =
      let context_name = Context_name.to_string context_name in
      Path.Build.append_source
        (Path.Build.relative Dpath.Build.alias_dir context_name)
        sub_dir
    in
    let alias_action_rules, alias_expansions =
      let open Action_builder.O in
      let aliases = collected.aliases in
      let aliases =
        if Alias.Name.Map.mem aliases Alias.Name.default then
          aliases
        else
          match Path.Build.extract_build_context_dir dir with
          | None -> aliases
          | Some (ctx_dir, src_dir) -> (
            match File_tree.find_dir src_dir with
            | None -> aliases
            | Some dir ->
              let default_alias =
                let dune_version =
                  File_tree.Dir.project dir |> Dune_project.dune_version
                in
                if dune_version >= (2, 0) then
                  Alias.Name.all
                else
                  Alias.Name.install
              in
              Alias.Name.Map.set aliases Alias.Name.default
                { expansions =
                    Appendable_list.singleton
                      ( Loc.none
                      , let+ _ =
                          Alias0.dep_rec_internal ~name:default_alias ~dir
                            ~ctx_dir
                        in
                        () )
                ; actions = Appendable_list.empty
                })
      in
      Alias.Name.Map.fold_mapi aliases ~init:[]
        ~f:(fun name rules { Rules.Dir_rules.Alias_spec.expansions; actions } ->
          let base_path =
            Path.Build.relative alias_dir (Alias.Name.to_string name)
          in
          let rules, action_stamp_files =
            List.fold_left (Appendable_list.to_list actions)
              ~init:(rules, Path.Set.empty)
              ~f:(fun
                   (rules, action_stamp_files)
                   { Rules.Dir_rules.stamp; action; locks; context; loc; env }
                 ->
                let path =
                  Path.Build.extend_basename base_path
                    ~suffix:("-" ^ Digest.to_string stamp)
                in
                let rule =
                  Rule.make ~locks ~context:(Some context) ~env
                    ~info:(Rule.Info.of_loc_opt loc)
                    (Action_builder.progn
                       [ action; Action_builder.create_file path ])
                in
                ( rule :: rules
                , Path.Set.add action_stamp_files (Path.build path) ))
          in
          let expansions =
            Appendable_list.to_list expansions
            @ [ ( Loc.none
                , Action_builder.memo_build
                    (Memo.Build.map
                       (Fdecl.get build_deps_fdecl
                          (Dep.Set.of_files_set action_stamp_files))
                       ~f:(fun _facts -> ())) )
              ]
          in
          (rules, expansions))
    in
    fun ~subdirs_to_keep ->
      let rules_here =
        compile_rules ~dir:alias_dir ~source_dirs:String.Set.empty
          alias_action_rules
      in
      remove_old_artifacts ~rules_here ~dir:alias_dir ~subdirs_to_keep;
      (rules_here, alias_expansions)

  let filter_out_fallback_rules ~to_copy rules =
    List.filter rules ~f:(fun (rule : Rule.t) ->
        match rule.mode with
        | Standard
        | Promote _
        | Ignore_source_files ->
          true
        | Fallback ->
          let source_files_for_targtes =
            (* All targets are in [dir] and we know it correspond to a directory
               of a build context since there are source files to copy, so this
               call can't fail. *)
            Path.Build.Set.to_list rule.action.targets
            |> Path.Source.Set.of_list_map ~f:Path.Build.drop_build_context_exn
          in
          if Path.Source.Set.is_subset source_files_for_targtes ~of_:to_copy
          then
            (* All targets are present *)
            false
          else if
            Path.Source.Set.is_empty
              (Path.Source.Set.inter source_files_for_targtes to_copy)
          then
            (* No target is present *)
            true
          else
            let absent_targets =
              Path.Source.Set.diff source_files_for_targtes to_copy
            in
            let present_targets =
              Path.Source.Set.diff source_files_for_targtes absent_targets
            in
            User_error.raise ~loc:(Rule.loc rule)
              [ Pp.text
                  "Some of the targets of this fallback rule are present in \
                   the source tree, and some are not. This is not allowed. \
                   Either none of the targets must be present in the source \
                   tree, either they must all be."
              ; Pp.nop
              ; Pp.text "The following targets are present:"
              ; pp_paths (Path.set_of_source_paths present_targets)
              ; Pp.nop
              ; Pp.text "The following targets are not:"
              ; pp_paths (Path.set_of_source_paths absent_targets)
              ])

  (** A directory is only allowed to be generated if its parent knows about it.
      This restriction is necessary to prevent stale artifact deletion from
      removing that directory.

      This module encodes that restriction. *)
  module Generated_directory_restrictions : sig
    type restriction =
      | Unrestricted
      | Restricted of Path.Unspecified.w Dir_set.t Memo.Lazy.Async.t

    (** Used by the child to ask about the restrictions placed by the parent. *)
    val allowed_by_parent : dir:Path.Build.t -> restriction
  end = struct
    type restriction =
      | Unrestricted
      | Restricted of Path.Unspecified.w Dir_set.t Memo.Lazy.Async.t

    let corresponding_source_dir ~dir =
      match Dpath.analyse_target dir with
      | Install _
      | Alias _
      | Other _ ->
        None
      | Regular (_ctx, sub_dir) -> File_tree.find_dir sub_dir

    let source_subdirs_of_build_dir ~dir =
      match corresponding_source_dir ~dir with
      | None -> String.Set.empty
      | Some dir -> File_tree.Dir.sub_dir_names dir

    let allowed_dirs ~dir ~subdir : restriction =
      if String.Set.mem (source_subdirs_of_build_dir ~dir) subdir then
        Unrestricted
      else
        Restricted
          (Memo.Lazy.Async.create (fun () ->
               load_dir ~dir:(Path.build dir) >>| function
               | Non_build _ -> Dir_set.just_the_root
               | Build { allowed_subdirs; _ } ->
                 Dir_set.descend allowed_subdirs subdir))

    let allowed_by_parent ~dir =
      allowed_dirs
        ~dir:(Path.Build.parent_exn dir)
        ~subdir:(Path.Build.basename dir)
  end

  (* TODO: Delete this step after users of dune <2.8 are sufficiently rare. This
     step is sketchy because it's using the [Promoted_to_delete] database and
     that can get out of date (see a comment on [Promoted_to_delete]), so we
     should not widen the scope of it too much. *)
  let delete_stale_dot_merlin_file ~dir ~source_files_to_ignore =
    (* If a [.merlin] file is present in the [Promoted_to_delete] set but not in
       the [Source_files_to_ignore] that means the rule that ordered its
       promotion is no more valid. This would happen when upgrading to Dune 2.8
       from ealier version without and building uncleaned projects. We delete
       these leftover files here. *)
    let merlin_file = ".merlin" in
    let source_dir = Path.Build.drop_build_context_exn dir in
    let merlin_in_src = Path.Source.(relative source_dir merlin_file) in
    let source_files_to_ignore =
      if
        Promoted_to_delete.mem (Path.source merlin_in_src)
        && not (Path.Source.Set.mem source_files_to_ignore merlin_in_src)
      then (
        let path = Path.source merlin_in_src in
        Log.info
          [ Pp.textf "Deleting left-over Merlin file %s.\n"
              (Path.to_string path)
          ];
        (* We remove the file from the promoted database *)
        Promoted_to_delete.remove path;
        Path.unlink_no_err path;
        (* We need to keep ignoring the .merlin file for that build or Dune will
           attempt to copy it and fail because it has been deleted *)
        Path.Source.Set.add source_files_to_ignore merlin_in_src
      ) else
        source_files_to_ignore
    in
    source_files_to_ignore

  let load_dir_step2_exn t ~dir =
    let context_name, sub_dir =
      match Dpath.analyse_path dir with
      | Build (Install (ctx, path)) -> (Context_or_install.Install ctx, path)
      | Build (Regular (ctx, path)) -> (Context_or_install.Context ctx, path)
      | Build (Alias _)
      | Build (Other _)
      | Source _
      | External _ ->
        Code_error.raise "[load_dir_step2_exn] was called on a strange path"
          [ ("path", Path.to_dyn dir) ]
    in
    (* the above check makes this safe *)
    let dir = Path.as_in_build_dir_exn dir in
    (* Load all the rules *)
    let* extra_subdirs_to_keep, rules_produced =
      let gen_rules =
        match (Fdecl.get t.gen_rules) context_name with
        | None ->
          Code_error.raise "[gen_rules] did not specify rules for the context"
            [ ("context_name", Context_or_install.to_dyn context_name) ]
        | Some f -> f
      in
      Rules.collect_async (fun () ->
          gen_rules ~dir (Path.Source.explode sub_dir))
    in
    let rules =
      let dir = Path.build dir in
      Rules.Dir_rules.union
        (Rules.find rules_produced dir)
        (Rules.find (Fdecl.get t.init_rules) dir)
    in
    let collected = Rules.Dir_rules.consume rules in
    let rules = collected.rules in
    let alias_rules =
      match context_name with
      | Context context_name ->
        Some (compute_alias_rules ~context_name ~collected ~dir ~sub_dir)
      | Install _ -> None
    in
    let file_tree_dir =
      match context_name with
      | Install _ -> None
      | Context _ -> File_tree.find_dir sub_dir
    in
    (* Compute the set of targets and the set of source files that must not be
       copied *)
    let source_files_to_ignore =
      List.fold_left rules ~init:Path.Build.Set.empty
        ~f:(fun acc_ignored { Rule.action; mode; _ } ->
          let targets = action.targets in
          match mode with
          | Promote { only = None; _ }
          | Ignore_source_files ->
            Path.Build.Set.union targets acc_ignored
          | Promote { only = Some pred; _ } ->
            let to_ignore =
              Path.Build.Set.filter targets ~f:(fun target ->
                  Predicate_lang.Glob.exec pred
                    (Path.reach (Path.build target) ~from:(Path.build dir))
                    ~standard:Predicate_lang.any)
            in
            Path.Build.Set.union to_ignore acc_ignored
          | _ -> acc_ignored)
    in
    let source_files_to_ignore =
      Path.Build.Set.to_list source_files_to_ignore
      |> Path.Source.Set.of_list_map ~f:Path.Build.drop_build_context_exn
    in
    let source_files_to_ignore =
      delete_stale_dot_merlin_file ~dir ~source_files_to_ignore
    in
    (* Take into account the source files *)
    let to_copy, source_dirs =
      match context_name with
      | Install _ -> (None, String.Set.empty)
      | Context context_name ->
        (* This condition is [true] because of [get_dir_status] *)
        assert (Context_name.Map.mem t.contexts context_name);
        let files, subdirs =
          match file_tree_dir with
          | None -> (Path.Source.Set.empty, String.Set.empty)
          | Some dir ->
            (File_tree.Dir.file_paths dir, File_tree.Dir.sub_dir_names dir)
        in
        let files = Path.Source.Set.diff files source_files_to_ignore in
        if Path.Source.Set.is_empty files then
          (None, subdirs)
        else
          let ctx_path = Context_name.build_dir context_name in
          (Some (ctx_path, files), subdirs)
    in
    let subdirs_to_keep =
      match extra_subdirs_to_keep with
      | All -> Subdir_set.All
      | These set -> These (String.Set.union source_dirs set)
    in
    (* Filter out fallback rules *)
    let rules =
      match to_copy with
      | None ->
        (* If there are no source files to copy, fallback rules are
           automatically kept *)
        rules
      | Some (_, to_copy) -> filter_out_fallback_rules ~to_copy rules
    in
    (* Compile the rules and cleanup stale artifacts *)
    let rules =
      (match to_copy with
      | None -> []
      | Some (ctx_dir, source_files) ->
        create_copy_rules ~ctx_dir ~non_target_source_files:source_files)
      @ rules
    in
    let rules_here = compile_rules ~dir ~source_dirs rules in
    let allowed_by_parent =
      Generated_directory_restrictions.allowed_by_parent ~dir
    in
    let* () =
      match allowed_by_parent with
      | Unrestricted -> Memo.Build.return ()
      | Restricted restriction -> (
        match Path.Build.Map.find (Rules.to_map rules_produced) dir with
        | None -> Memo.Build.return ()
        | Some rules ->
          let+ restriction = Memo.Lazy.Async.force restriction in
          if not (Dir_set.here restriction) then
            Code_error.raise
              "Generated rules in a directory not allowed by the parent"
              [ ("dir", Path.Build.to_dyn dir)
              ; ("rules", Rules.Dir_rules.to_dyn rules)
              ])
    in
    let rules_generated_in =
      Rules.to_map rules_produced
      |> Path.Build.Map.foldi ~init:Dir_set.empty ~f:(fun p _ acc ->
             match Path.Local_gen.descendant ~of_:dir p with
             | None -> acc
             | Some p -> Dir_set.union acc (Dir_set.singleton p))
    in
    let* allowed_granddescendants_of_parent =
      match allowed_by_parent with
      | Unrestricted ->
        (* In this case the parent isn't going to be able to create any
           generated granddescendant directories. (rules that attempt to do so
           may run into the [allowed_by_parent] check or will be simply ignored) *)
        Memo.Build.return Dir_set.empty
      | Restricted restriction -> Memo.Lazy.Async.force restriction
    in
    let descendants_to_keep =
      Dir_set.union_all
        [ rules_generated_in
        ; Subdir_set.to_dir_set subdirs_to_keep
        ; allowed_granddescendants_of_parent
        ]
    in
    let subdirs_to_keep = Subdir_set.of_dir_set descendants_to_keep in
    remove_old_artifacts ~dir ~rules_here ~subdirs_to_keep;
    let alias_rules = Option.map ~f:(fun f -> f ~subdirs_to_keep) alias_rules in
    let rules_of_alias_dir, aliases =
      Option.value
        ~default:(Path.Build.Map.empty, Alias.Name.Map.empty)
        alias_rules
    in
    Memo.Build.return
      (Loaded.Build
         { allowed_subdirs = descendants_to_keep
         ; rules_produced
         ; rules_here
         ; rules_of_alias_dir
         ; aliases
         })

  let load_dir_impl t ~dir : Loaded.t Memo.Build.t =
    match get_dir_triage t ~dir with
    | Known l -> Memo.Build.return l
    | Need_step2 -> load_dir_step2_exn t ~dir
    | Alias_dir_of dir' -> (
      load_dir ~dir:(Path.build dir') >>| function
      | Non_build _ -> Code_error.raise "Can only forward to a build dir" []
      | Build
          ({ rules_here = _
           ; rules_of_alias_dir
           ; rules_produced = _
           ; allowed_subdirs = _
           ; aliases = _
           } as load) ->
        Loaded.Build
          { load with
            rules_here = rules_of_alias_dir
          ; rules_of_alias_dir = Path.Build.Map.empty
          })

  let load_dir =
    let load_dir_impl dir = load_dir_impl (t ()) ~dir in
    let memo =
      Memo.create_hidden "load-dir" ~doc:"load dir"
        ~input:(module Path)
        Async load_dir_impl
    in
    fun ~dir -> Memo.exec memo dir
end

open Load_rules

let load_dir_and_get_buildable_targets ~dir =
  load_dir ~dir >>| function
  | Non_build _ -> Path.Build.Map.empty
  | Build { rules_here; _ } -> rules_here

let get_rule fn =
  match Path.as_in_build_dir fn with
  | None -> Memo.Build.return None
  | Some fn -> (
    let dir = Path.Build.parent_exn fn in
    load_dir ~dir:(Path.build dir) >>| function
    | Non_build _ -> assert false
    | Build { rules_here; _ } -> Path.Build.Map.find rules_here fn)

type rule_or_source =
  | Source of Digest.t
  | Rule of Path.Build.t * Rule.t

let get_rule_or_source t path =
  let dir = Path.parent_exn path in
  if Path.is_strict_descendant_of_build_dir dir then
    let+ rules = load_dir_and_get_buildable_targets ~dir in
    let path = Path.as_in_build_dir_exn path in
    match Path.Build.Map.find rules path with
    | Some rule -> Rule (path, rule)
    | None ->
      let loc = Rule_fn.loc () in
      no_rule_found t ~loc path
  else if Path.exists path then
    Memo.Build.return (Source (Cached_digest.file path))
  else
    let loc = Rule_fn.loc () in
    User_error.raise ?loc
      [ Pp.textf "File unavailable: %s" (Path.to_string_maybe_quoted path) ]

let all_targets t =
  let root = File_tree.root () in
  Memo.Build.parallel_map (Context_name.Map.values t.contexts) ~f:(fun ctx ->
      File_tree.Dir.map_reduce
        (module Path.Build.Set)
        root ~traverse:Sub_dirs.Status.Set.all
        ~f:(fun dir ->
          load_dir
            ~dir:
              (Path.build
                 (Path.Build.append_source ctx.Build_context.build_dir
                    (File_tree.Dir.path dir)))
          >>| function
          | Non_build _ -> Path.Build.Set.empty
          | Build { rules_here; rules_of_alias_dir; _ } ->
            Path.Build.Set.of_list
              (Path.Build.Map.keys rules_of_alias_dir
              @ Path.Build.Map.keys rules_here)))
  >>| Path.Build.Set.union_all

let expand_alias_gen alias ~eval_build_request ~paths_of_facts ~paths_union_all
    =
  lookup_alias alias >>= function
  | None ->
    let alias_descr = sprintf "alias %s" (Alias.describe alias) in
    User_error.raise ?loc:(Rule_fn.loc ())
      [ Pp.textf "No rule found for %s" alias_descr ]
  | Some alias_definitions ->
    Memo.Build.map
      (Memo.Build.parallel_map alias_definitions ~f:(fun (loc, definition) ->
           let on_error exn = Dep_path.reraise exn (Alias (loc, alias)) in
           Memo.Build.with_error_handler ~on_error (fun () ->
               let* (), facts = eval_build_request definition in
               paths_of_facts facts)))
      ~f:paths_union_all

type rule_execution_result =
  { deps : Dep.Fact.t Dep.Map.t
  ; targets : Digest.t Path.Build.Map.t
  }

module type Rec = sig
  (** Build all the transitive dependencies of the alias and return the alias
      expansion. *)
  val build_alias : Alias.t -> Digest.t Path.Map.t Memo.Build.t

  val build_file : Path.t -> Digest.t Memo.Build.t

  val execute_rule : Rule.t -> rule_execution_result Memo.Build.t

  module Pred : sig
    val eval : File_selector.t -> Path.Set.t Memo.Build.t

    val build : File_selector.t -> Digest.t Path.Map.t Memo.Build.t
  end
end

(* Separation between [Used_recursively] and [Exported] is necessary because at
   least one module in the recursive module group must be pure (i.e. only expose
   functions). *)
module rec Used_recursively : Rec = Exported

and Exported : sig
  include Rec

  val exec_build_request :
    'a Action_builder.t -> ('a * Dep.Fact.t Dep.Map.t) Memo.Build.t

  val execute_rule : Rule.t -> rule_execution_result Memo.Build.t

  (** Exported to inspect memoization cycles. *)
  val build_file_memo :
    (Path.t, Digest.t, Path.t -> Digest.t Memo.Build.t) Memo.t

  val build_alias_memo :
    ( Alias.t
    , Digest.t Path.Map.t
    , Alias.t -> Digest.t Path.Map.t Memo.Build.t )
    Memo.t
end = struct
  open Used_recursively

  (* [build_dep] turns a [Dep.t] which is a description of a dependency into a
     fact about the world. To do that, it needs to do some building. *)
  let build_dep : Dep.t -> Dep.Fact.t Memo.Build.t = function
    | Alias a ->
      let+ digests = build_alias a in
      (* Fact: alias [a] expand to the set of files with their digest [digests] *)
      Dep.Fact.alias a digests
    | File f ->
      let+ digest = build_file f in
      (* Fact: file [f] has digest [digest] *)
      Dep.Fact.file f digest
    | File_selector g ->
      let+ digests = Pred.build g in
      (* Fact: file selector [g] expands to the set of files with their digest
         [digests] *)
      Dep.Fact.file_selector g digests
    | Universe
    | Env _
    | Sandbox_config _ ->
      (* Facts about these dependencies are constructed in [Dep.Facts.digest]. *)
      Memo.Build.return Dep.Fact.nothing

  let build_deps deps =
    Dep.Map.parallel_map deps ~f:(fun dep () -> build_dep dep)

  let () = Fdecl.set build_deps_fdecl build_deps

  module Build_exec = Action_builder.Make_exec (struct
    type fact = Dep.Fact.t

    let merge_facts = Dep.Facts.union

    let read_file p ~f =
      let+ _digest = build_file p in
      f p

    let register_action_deps = build_deps

    let register_action_dep_pred g =
      let+ digests = Pred.build g in
      ( Path.Map.keys digests |> Path.Set.of_list
      , Dep.Fact.file_selector g digests )

    let file_exists = file_exists

    let alias_exists alias = Fdecl.get alias_exists_fdecl alias
  end)
  [@@inlined]

  let exec_build_request = Build_exec.exec

  let select_sandbox_mode (config : Sandbox_config.t) ~loc
      ~sandboxing_preference =
    let evaluate_sandboxing_preference preference =
      match Sandbox_mode.Set.mem config preference with
      | false -> None
      | true -> (
        match preference with
        | Some Symlink ->
          if Sandbox_mode.Set.mem config Sandbox_mode.copy then
            Some
              (if Sys.win32 then
                Sandbox_mode.copy
              else
                Sandbox_mode.symlink)
          else
            User_error.raise ~loc
              [ Pp.text
                  "This rule requires sandboxing with symlinks, but that won't \
                   work on Windows."
              ]
        | _ -> Some preference)
    in
    match
      List.find_map sandboxing_preference ~f:evaluate_sandboxing_preference
    with
    | Some choice -> choice
    | None ->
      (* This is not trivial to reach because the user rules are checked at
         parse time and [sandboxing_preference] always includes all possible
         modes. However, it can still be reached if multiple sandbox config
         specs are combined into an unsatisfiable one. *)
      User_error.raise ~loc
        [ Pp.text
            "This rule forbids all sandboxing modes (but it also requires \
             sandboxing)"
        ]

  let start_rule t _rule = t.rule_total <- t.rule_total + 1

  (* Same as [rename] except that if the source doesn't exist we delete the
     destination *)
  let rename_optional_file ~src ~dst =
    let src = Path.Build.to_string src in
    let dst = Path.Build.to_string dst in
    match Unix.rename src dst with
    | () -> ()
    | exception Unix.Unix_error ((ENOENT | ENOTDIR), _, _) -> (
      match Unix.unlink dst with
      | exception Unix.Unix_error (ENOENT, _, _) -> ()
      | () -> ())

  (* The current version of the rule digest scheme. We should increment it when
     making any changes to the scheme, to avoid collisions. *)
  let rule_digest_version = 2

  let compute_rule_digest (rule : Rule.t) ~deps ~action ~sandbox_mode =
    let env = Rule.effective_env rule in
    let trace =
      ( rule_digest_version (* Update when changing the rule digest scheme. *)
      , Dep.Facts.digest deps ~sandbox_mode ~env
      , Path.Build.Set.to_list_map rule.action.targets ~f:Path.Build.to_string
      , Option.map rule.context ~f:(fun c -> c.name)
      , Action.for_shell action )
    in
    Digest.generic trace

  let execute_rule_impl rule =
    let t = t () in
    let { Rule.id = _; dir; env = _; context; mode; locks; action; info = _ } =
      rule
    in
    start_rule t rule;
    let targets = action.targets in
    let head_target = Path.Build.Set.choose_exn targets in
    let* action, deps = exec_build_request rule.action.build in
    Memo.Build.of_reproducible_fiber
      (let open Fiber.O in
      let build_deps deps = Memo.Build.run (build_deps deps) in
      Stats.new_evaluated_rule ();
      let* () = Memo.Build.run (Fs.mkdir_p dir) in
      let env = Rule.effective_env rule in
      let loc = Rule.loc rule in
      let is_action_dynamic = Action.is_dynamic action in
      let sandbox_mode =
        match Action.is_useful_to_sandbox action with
        | Clearly_not ->
          let config = Dep.Map.sandbox_config deps in
          if Sandbox_config.mem config Sandbox_mode.none then
            Sandbox_mode.none
          else
            User_error.raise ~loc
              [ Pp.text
                  "Rule dependencies are configured to require sandboxing, but \
                   the rule has no actions that could potentially require \
                   sandboxing."
              ]
        | Maybe ->
          select_sandbox_mode ~loc
            (Dep.Map.sandbox_config deps)
            ~sandboxing_preference:t.sandboxing_preference
      in
      let always_rerun =
        let force_rerun =
          !Clflags.force
          && Path.Build.Set.exists targets ~f:Dpath.Build.is_alias_stamp_file
        and depends_on_universe = Dep.Map.has_universe deps in
        force_rerun || depends_on_universe
      in
      let rule_digest = compute_rule_digest rule ~deps ~action ~sandbox_mode in
      let () =
        (* FIXME: Rule hinting provide no relevant speed increase for now.
           Disable the overhead until we make a decision. *)
        if false then
          if Action.is_useful_to_distribute action = Maybe then
            let f { cache = (module Caching); _ } =
              match Caching.Cache.hint Caching.cache [ rule_digest ] with
              | Result.Ok _ -> ()
              | Result.Error e ->
                User_warning.emit [ Pp.textf "unable to hint the cache: %s" e ]
            in
            Option.iter ~f t.caching
      in
      let do_not_memoize =
        always_rerun || is_action_dynamic
        || Action.is_useful_to_memoize action = Clearly_not
      in
      (* We don't need to digest names here, as these are already part of the
         rule digest. *)
      let digest_of_targets_digests l = Digest.generic (List.map l ~f:snd) in
      (* Here we determine if we need to rerun the action based on information
         stored in Trace_db. If it does, [targets_digests] is [None], otherwise
         it is [Some l] where [l] is the list of targets with their digests. *)
      let* (targets_digests : (Path.Build.t * Digest.t) list option) =
        if always_rerun then
          Fiber.return None
        else
          (* [prev_trace] will be [None] if rule is run for the first time. *)
          let prev_trace = Trace_db.get (Path.build head_target) in
          let prev_trace_and_targets_digests =
            match prev_trace with
            | None -> None
            | Some prev_trace -> (
              if prev_trace.rule_digest <> rule_digest then
                None
              else
                (* [targets_digest] will be [None] if not all targets were
                   build. *)
                match compute_targets_digests targets with
                | None -> None
                | Some targets_digests ->
                  if
                    Digest.equal prev_trace.targets_digest
                      (digest_of_targets_digests targets_digests)
                  then
                    Some (prev_trace, targets_digests)
                  else
                    None)
          in
          match prev_trace_and_targets_digests with
          | None -> Fiber.return None
          | Some (prev_trace, targets_digests) ->
            (* CR-someday aalekseyev: If there's a change at one of the last
               stages, we still re-run all the previous stages, which is a bit
               of a waste. We could remember what stage needs re-running and
               only re-run that (and later stages). *)
            let rec loop stages =
              match stages with
              | [] -> Fiber.return (Some targets_digests)
              | (deps, old_digest) :: rest ->
                let deps = Action_exec.Dynamic_dep.Set.to_dep_set deps in
                let* deps = build_deps deps in
                let new_digest = Dep.Facts.digest deps ~sandbox_mode ~env in
                if old_digest = new_digest then
                  loop rest
                else
                  Fiber.return None
            in
            loop prev_trace.dynamic_deps_stages
      in
      let sandbox =
        Option.map sandbox_mode ~f:(fun mode ->
            let sandbox_suffix = rule_digest |> Digest.to_string in
            (Path.Build.relative sandbox_dir sandbox_suffix, mode))
      in
      let* targets_digests =
        match targets_digests with
        | Some x -> Fiber.return x
        | None -> (
          let from_cache =
            match (do_not_memoize, t.caching) with
            | true, _
            | _, None ->
              None
            | false, Some { cache = (module Caching) as cache; _ } -> (
              match Caching.Cache.search Caching.cache rule_digest with
              | Ok (_, files) ->
                Log.info
                  [ Pp.textf "cache hit for %s" (Digest.to_string rule_digest) ];
                Some (files, cache)
              | Error msg ->
                Log.info
                  [ Pp.textf "cache miss for %s: %s"
                      (Digest.to_string rule_digest)
                      msg
                  ];
                None)
          and cache_checking =
            match t.caching with
            | Some { check_probability; _ } ->
              Random.float 1. < check_probability
            | _ -> false
          in
          let remove_targets () =
            Path.Build.Set.iter targets ~f:(fun target ->
                Cached_digest.remove (Path.build target);
                Path.unlink_no_err (Path.build target))
          in
          let pulled_from_cache =
            match from_cache with
            | Some (files, (module Caching)) when not cache_checking -> (
              let () = remove_targets () in
              let retrieve (file : Cache.File.t) =
                let retrieved = Caching.Cache.retrieve Caching.cache file in
                Cached_digest.set retrieved file.digest;
                (file.path, file.digest)
              in
              match List.map files ~f:retrieve with
              | exception Unix.(Unix_error (ENOENT, _, f)) ->
                Log.info
                  [ Pp.textf "missing data file for cached rule %s: %s"
                      (Digest.to_string rule_digest)
                      f
                  ];
                None
              | exception Sys_error m ->
                Log.info [ Pp.textf "error retrieving data file: %s" m ];
                None
              | targets_digests ->
                Trace_db.set (Path.build head_target)
                  (* We do not cache dynamic actions so [dynamic_deps_stages] is
                     always an empty list here. *)
                  { rule_digest
                  ; targets_digest = digest_of_targets_digests targets_digests
                  ; dynamic_deps_stages = []
                  };
                Some targets_digests)
            | _ -> None
          in
          match pulled_from_cache with
          | Some x -> Fiber.return x
          | None ->
            let () = remove_targets () in
            pending_targets := Path.Build.Set.union targets !pending_targets;
            let* sandboxed, action =
              match sandbox with
              | None -> Fiber.return (None, action)
              | Some (sandbox_dir, sandbox_mode) ->
                Path.rm_rf (Path.build sandbox_dir);
                let sandboxed path : Path.Build.t =
                  Path.Build.append_local sandbox_dir (Path.Build.local path)
                in
                let* () =
                  Fiber.parallel_iter_set
                    (module Path.Set)
                    (Dep.Facts.dirs deps)
                    ~f:(fun path ->
                      Memo.Build.run
                        (match Path.as_in_build_dir path with
                        | None -> Fs.assert_exists ~loc path
                        | Some path -> Fs.mkdir_p (sandboxed path)))
                in
                let+ () = Memo.Build.run (Fs.mkdir_p (sandboxed dir)) in
                ( Some sandboxed
                , Action.sandbox action ~sandboxed ~mode:sandbox_mode ~deps )
            and* () =
              let chdirs = Action.chdirs action in
              Fiber.parallel_iter_set
                (module Path.Set)
                chdirs
                ~f:(fun p -> Memo.Build.run (Fs.mkdir_p_or_check_exists ~loc p))
            in
            let+ exec_result =
              with_locks t locks ~f:(fun () ->
                  let copy_files_from_sandbox sandboxed =
                    Path.Build.Set.iter targets ~f:(fun target ->
                        rename_optional_file ~src:(sandboxed target) ~dst:target)
                  in
                  let+ exec_result =
                    Action_exec.exec ~context ~env ~targets ~rule_loc:loc
                      ~build_deps action
                  in
                  Option.iter sandboxed ~f:copy_files_from_sandbox;
                  exec_result)
            in
            Option.iter sandbox ~f:(fun (p, _mode) -> Path.rm_rf (Path.build p));
            (* All went well, these targets are no longer pending *)
            pending_targets := Path.Build.Set.diff !pending_targets targets;
            let targets_digests =
              compute_targets_digests_or_raise_error ~loc targets
            in
            let targets_digest = digest_of_targets_digests targets_digests in
            let () =
              (* Check cache. We don't check for missing file in the cache,
                 since the file list is part of the rule hash this really never
                 should happen. *)
              match from_cache with
              | Some (cached, _) when cache_checking ->
                (* This being [false] is unexpected and means we have a hash
                   collision *)
                let data_are_ok =
                  match
                    List.for_all2 targets_digests cached
                      ~f:(fun (target, _) (c : Cache.File.t) ->
                        Path.Build.equal target c.path)
                  with
                  | Ok b -> b
                  | Error `Length_mismatch -> false
                in
                if not data_are_ok then
                  let open Pp.O in
                  let pp x l ~f =
                    Pp.box ~indent:2
                      (Pp.verbatim x
                      ++ Dyn.pp
                           (Dyn.Encoder.list Path.Build.to_dyn (List.map l ~f))
                      )
                  in
                  User_warning.emit
                    [ Pp.text "unexpected list of targets in the cache"
                    ; pp "expected: " targets_digests ~f:fst
                    ; pp "got:      " cached ~f:(fun (c : Cache.File.t) ->
                          c.path)
                    ]
                else
                  List.iter2 targets_digests cached
                    ~f:(fun (_, digest) (c : Cache.File.t) ->
                      if not (Digest.equal digest c.digest) then
                        User_warning.emit
                          [ Pp.textf "cache mismatch on %s: hash differ with %s"
                              (Path.Build.to_string_maybe_quoted c.path)
                              (Path.Build.to_string_maybe_quoted c.path)
                          ])
              | _ -> ()
            in
            let () =
              (* Promote *)
              match t.caching with
              | Some { cache = (module Caching : Cache.Caching); _ }
                when not do_not_memoize ->
                let report msg =
                  let targets =
                    Path.Build.Set.to_list_map rule.action.targets
                      ~f:Path.Build.to_string
                    |> String.concat ~sep:", "
                  in
                  Log.info
                    [ Pp.textf "promotion failed for %s: %s" targets msg ]
                in
                let repository =
                  let dir = Rule.find_source_dir rule in
                  let open Option.O in
                  let* vcs = File_tree.Dir.vcs dir in
                  let f found = Path.equal found.Vcs.root vcs.Vcs.root in
                  let+ _, i = get_vcs () |> List.findi ~f in
                  i
                in
                Caching.Cache.promote Caching.cache targets_digests rule_digest
                  [] ~repository ~duplication:None
                |> Result.map_error ~f:report |> ignore
              | _ -> ()
            in
            let dynamic_deps_stages =
              List.map exec_result.dynamic_deps_stages
                ~f:(fun (deps, fact_map) ->
                  (deps, Dep.Facts.digest fact_map ~sandbox_mode ~env))
            in
            Trace_db.set (Path.build head_target)
              { rule_digest; dynamic_deps_stages; targets_digest };
            targets_digests)
      in
      let+ () =
        match (mode, !Clflags.promote) with
        | (Standard | Fallback | Ignore_source_files), _
        | Promote _, Some Never ->
          Fiber.return ()
        | Promote { lifetime; into; only }, (Some Automatically | None) ->
          Fiber.parallel_iter_set
            (module Path.Build.Set)
            targets
            ~f:(fun path ->
              let consider_for_promotion =
                match only with
                | None -> true
                | Some pred ->
                  Predicate_lang.Glob.exec pred
                    (Path.reach (Path.build path) ~from:(Path.build dir))
                    ~standard:Predicate_lang.any
              in
              match consider_for_promotion with
              | false -> Fiber.return ()
              | true -> (
                let in_source_tree = Path.Build.drop_build_context_exn path in
                let in_source_tree =
                  match into with
                  | None -> in_source_tree
                  | Some { loc; dir } ->
                    Path.Source.relative
                      (Path.Source.relative
                         (Path.Source.parent_exn in_source_tree)
                         dir ~error_loc:loc)
                      (Path.Source.basename in_source_tree)
                in
                let () =
                  let dir = Path.Source.parent_exn in_source_tree in
                  match File_tree.find_dir dir with
                  | Some _ -> ()
                  | None ->
                    let loc =
                      match into with
                      | Some into -> into.loc
                      | None ->
                        Code_error.raise
                          "promoting into directory that does not exist"
                          [ ("in_source_tree", Path.Source.to_dyn in_source_tree)
                          ]
                    in
                    User_error.raise ~loc
                      [ Pp.textf "directory %S does not exist"
                          (Path.Source.to_string_maybe_quoted dir)
                      ]
                in
                let dst = in_source_tree in
                let in_source_tree = Path.source in_source_tree in
                match
                  Path.exists in_source_tree
                  && Cached_digest.file (Path.build path)
                     = Cached_digest.file in_source_tree
                with
                | true -> Fiber.return ()
                | false ->
                  if lifetime = Until_clean then
                    Promoted_to_delete.add in_source_tree;
                  Scheduler.ignore_for_watch in_source_tree;
                  (* The file in the build directory might be read-only if it
                     comes from the shared cache. However, we want the file in
                     the source tree to be writable by the user, so we
                     explicitly set the user writable bit. *)
                  let chmod n = n lor 0o200 in
                  t.promote_source ~src:path ~dst ~chmod context))
      in
      t.rule_done <- t.rule_done + 1;
      targets_digests)
    (* jeremidimino: we need to include the dependencies discovered while
       running the action here. Otherwise, package dependencies are broken in
       the presence of dynamic actions *)
    >>=
    fun targets_digests ->
    Memo.Build.return
      { deps; targets = Path.Build.Map.of_list_exn targets_digests }

  (* a rule can have multiple files, but rule.run_rule may only be called once.

     [build_file_impl] returns both the set of dependencies of the file as well
     as its digest. *)
  let build_file_impl path =
    let t = t () in
    let on_error exn = Dep_path.reraise exn (Path path) in
    Memo.Build.with_error_handler ~on_error (fun () ->
        get_rule_or_source t path >>= function
        | Source digest -> Memo.Build.return digest
        | Rule (path, rule) ->
          let+ { deps = _; targets } = execute_rule rule in
          Path.Build.Map.find_exn targets path)

  let build_alias_impl alias =
    expand_alias_gen alias ~eval_build_request:exec_build_request
      ~paths_of_facts:(fun facts -> Memo.Build.return (Dep.Facts.paths facts))
      ~paths_union_all:(fun l ->
        List.fold_left l ~init:Path.Map.empty
          ~f:(Path.Map.union ~f:(fun _ x _ -> Some x)))

  module Digest_path_map = struct
    type t = Digest.t Path.Map.t

    let to_dyn = Path.Map.to_dyn Digest.to_dyn

    let equal = Path.Map.equal ~equal:Digest.equal
  end

  module Pred = struct
    let build_impl g =
      let* paths = Pred.eval g in
      Memo.Build.parallel_map (Path.Set.to_list paths) ~f:(fun p ->
          let+ d = build_file p in
          (p, d))
      >>| Path.Map.of_list_exn

    let eval_impl g =
      let dir = File_selector.dir g in
      load_dir ~dir >>| function
      | Non_build targets -> Path.Set.filter targets ~f:(File_selector.test g)
      | Build { rules_here; _ } ->
        Path.Build.Map.foldi ~init:[] rules_here ~f:(fun s _ acc ->
            let s = Path.build s in
            if File_selector.test g s then
              s :: acc
            else
              acc)
        |> Path.Set.of_list

    let eval_memo =
      Memo.create "eval-pred" ~doc:"Evaluate a predicate in a directory"
        ~input:(module File_selector)
        ~output:(Allow_cutoff (module Path.Set))
        ~visibility:Hidden Async eval_impl

    let eval = Memo.exec eval_memo

    let build =
      Memo.exec
        (Memo.create "build-pred" ~doc:"build a predicate"
           ~input:(module File_selector)
           ~output:(Allow_cutoff (module Digest_path_map))
           ~visibility:Hidden Async build_impl)
  end

  let build_file_memo =
    Memo.create "build-file"
      ~output:(Allow_cutoff (module Digest))
      ~doc:"Build a file."
      ~input:(module Path)
      ~visibility:Hidden Async build_file_impl

  let build_file = Memo.exec build_file_memo

  let build_alias_memo =
    Memo.create "build-alias"
      ~output:(Allow_cutoff (module Digest_path_map))
      ~doc:"Build an alias."
      ~input:(module Alias)
      ~visibility:Hidden Async build_alias_impl

  let build_alias = Memo.exec build_alias_memo

  let execute_rule_memo =
    Memo.create_hidden "execute-rule"
      ~input:(module Rule)
      Async execute_rule_impl

  let execute_rule = Memo.exec execute_rule_memo

  let () =
    Fdecl.set Rule_fn.loc_decl (fun () ->
        let stack = Memo.get_call_stack () in
        List.find_map stack ~f:(fun frame ->
            Memo.Stack_frame.as_instance_of frame ~of_:execute_rule_memo)
        |> Option.map ~f:Rule.loc)
end

open Exported

let eval_pred = Pred.eval

let get_human_readable_info stack_frame =
  match Memo.Stack_frame.as_instance_of ~of_:build_file_memo stack_frame with
  | Some p -> Some (Pp.verbatim (Path.to_string_maybe_quoted p))
  | None -> (
    match Memo.Stack_frame.as_instance_of ~of_:build_alias_memo stack_frame with
    | Some alias -> Some (Pp.verbatim ("alias " ^ Alias.describe alias))
    | None -> None)

let process_memcycle (cycle_error : Memo.Cycle_error.t) =
  let cycle =
    Memo.Cycle_error.get cycle_error
    |> List.filter_map ~f:get_human_readable_info
  in
  match List.last cycle with
  | None ->
    let frames = Memo.Cycle_error.get cycle_error in
    Code_error.raise "dependency cycle that does not involve any files"
      [ ("frames", Dyn.Encoder.(list Memo.Stack_frame.to_dyn) frames) ]
  | Some last ->
    let first = List.hd cycle in
    let cycle =
      if last = first then
        cycle
      else
        last :: cycle
    in
    User_error.raise
      [ Pp.text "Dependency cycle between the following files:"
      ; Pp.chain cycle ~f:(fun p -> p)
      ]

let set_packages f =
  let t = t () in
  Fdecl.set t.packages f

let package_deps (pkg : Package.t) files =
  let t = t () in
  let rules_seen = ref Rule.Set.empty in
  let rec loop fn =
    match Path.as_in_build_dir fn with
    | None ->
      (* if this file isn't in the build dir, it doesn't belong to any package
         and it doesn't have dependencies that do *)
      Memo.Build.return Package.Id.Set.empty
    | Some fn ->
      let* pkgs = Fdecl.get t.packages fn in
      if Package.Id.Set.is_empty pkgs || Package.Id.Set.mem pkgs pkg.id then
        loop_deps fn
      else
        Memo.Build.return pkgs
  and loop_deps fn =
    get_rule (Path.build fn) >>= function
    | None -> Memo.Build.return Package.Id.Set.empty
    | Some rule ->
      if Rule.Set.mem !rules_seen rule then
        Memo.Build.return Package.Id.Set.empty
      else (
        rules_seen := Rule.Set.add !rules_seen rule;
        let* res = execute_rule rule in
        loop_files (Dep.Facts.paths res.deps |> Path.Map.keys)
      )
  and loop_files files =
    let+ sets = Memo.Build.parallel_map files ~f:loop in
    List.fold_left sets ~init:Package.Id.Set.empty ~f:Package.Id.Set.union
  in
  loop_files (Path.Set.to_list files)

let prefix_rules (prefix : unit Action_builder.t) ~f =
  let+ res, rules = Rules.collect_async f in
  Rules.produce (Rules.map_rules rules ~f:(Rule.with_prefix ~build:prefix));
  res

module Alias = Alias0

let process_exn_and_reraise exn =
  let exn =
    Exn_with_backtrace.map exn
      ~f:
        (Dep_path.map ~f:(function
          | Memo.Cycle_error.E cycle_error -> process_memcycle cycle_error
          | _ as exn -> exn))
  in
  let build = get_build_system () in
  build.errors <- exn :: build.errors;
  Exn_with_backtrace.reraise exn

let run f =
  Hooks.End_of_build.once Promotion.finalize;
  let t = get_build_system () in
  t.errors <- [];
  let f () =
    Memo.Build.run
      (Memo.Build.with_error_handler f ~on_error:process_exn_and_reraise)
  in
  match t.build_mutex with
  | None -> f ()
  | Some m -> Fiber.Mutex.with_lock m f

let build request =
  let+ result, _deps = exec_build_request request in
  result

let is_target file =
  let+ targets = targets_of ~dir:(Path.parent_exn file) in
  Path.Set.mem targets file

module For_command_line : sig
  module Rule : sig
    type t = private
      { id : Rule.Id.t
      ; dir : Path.Build.t
      ; deps : Dep.Set.t
      ; expanded_deps : Path.Set.t
      ; targets : Path.Build.Set.t
      ; context : Build_context.t option
      ; action : Action.t
      }
  end

  val evaluate_rules :
    recursive:bool -> request:unit Action_builder.t -> Rule.t list Memo.Build.t

  val eval_build_request : 'a Action_builder.t -> ('a * Dep.Set.t) Memo.Build.t
end = struct
  module Non_evaluated_rule = Rule

  module Rule = struct
    type t =
      { id : Rule.Id.t
      ; dir : Path.Build.t
      ; deps : Dep.Set.t
      ; expanded_deps : Path.Set.t
      ; targets : Path.Build.Set.t
      ; context : Build_context.t option
      ; action : Action.t
      }
  end

  module Rule_top_closure =
    Top_closure.Make
      (Non_evaluated_rule.Id.Set)
      (struct
        include Memo.Build

        let ( >>= ) = O.( >>= )
      end)

  (* Evaluate a rule without building the action dependencies *)
  module Eval_action_builder = Action_builder.Make_exec (struct
    type fact = unit

    let merge_facts = Dep.Set.union

    let read_file p ~f =
      let+ _digest = build_file p in
      f p

    let register_action_deps deps = Memo.Build.return deps

    let register_action_dep_pred g =
      let+ ps = Pred.eval g in
      (ps, ())

    let file_exists = file_exists

    let alias_exists a = Fdecl.get alias_exists_fdecl a
  end)

  let eval_build_request = Eval_action_builder.exec

  module rec Expand : sig
    val alias : Alias.t -> Path.Set.t Memo.Build.t

    val deps : Dep.Set.t -> Path.Set.t Memo.Build.t
  end = struct
    let alias =
      let memo =
        Memo.create_hidden "expand-alias"
          ~input:(module Alias)
          Async
          (fun alias ->
            expand_alias_gen alias ~eval_build_request
              ~paths_of_facts:Expand.deps ~paths_union_all:Path.Set.union_all)
      in
      Memo.exec memo

    let deps deps =
      Memo.Build.parallel_map (Dep.Set.to_list deps) ~f:(fun (dep : Dep.t) ->
          match dep with
          | File p -> Memo.Build.return (Path.Set.singleton p)
          | File_selector g -> eval_pred g
          | Alias a -> Expand.alias a
          | Env _
          | Universe
          | Sandbox_config _ ->
            Memo.Build.return Path.Set.empty)
      >>| Path.Set.union_all
  end

  let evaluate_rule =
    let memo =
      Memo.create_hidden "evaluate-rule"
        ~input:(module Non_evaluated_rule)
        Async
        (fun rule ->
          let* action, deps = eval_build_request rule.action.build in
          let* expanded_deps = Expand.deps deps in
          Memo.Build.return
            { Rule.id = rule.id
            ; dir = rule.dir
            ; deps
            ; expanded_deps
            ; targets = rule.action.targets
            ; context = rule.context
            ; action
            })
    in
    Memo.exec memo

  let evaluate_rules ~recursive ~request =
    let rules_of_deps deps =
      Expand.deps deps >>| Path.Set.to_list
      >>= Memo.Build.parallel_map ~f:(fun p ->
              get_rule p >>= function
              | None -> Memo.Build.return None
              | Some rule -> evaluate_rule rule >>| Option.some)
      >>| List.filter_map ~f:Fun.id
    in
    let* (), deps = Eval_action_builder.exec request in
    let* root_rules = rules_of_deps deps in
    Rule_top_closure.top_closure root_rules
      ~key:(fun rule -> rule.Rule.id)
      ~deps:(fun rule ->
        if recursive then
          rules_of_deps rule.deps
        else
          Memo.Build.return [])
    >>| function
    | Ok l -> l
    | Error cycle ->
      User_error.raise
        [ Pp.text "Dependency cycle detected:"
        ; Pp.chain cycle ~f:(fun rule ->
              Pp.verbatim
                (Path.to_string_maybe_quoted
                   (Path.build (Path.Build.Set.choose_exn rule.targets))))
        ]
end

let load_dir_and_produce_its_rules ~dir =
  load_dir ~dir >>| function
  | Non_build _ -> ()
  | Build loaded -> Rules.produce loaded.rules_produced

let load_dir ~dir = load_dir_and_produce_its_rules ~dir

let init ~contexts ?build_mutex ~promote_source ?caching ~sandboxing_preference
    () =
  let contexts =
    Context_name.Map.of_list_map_exn contexts ~f:(fun c ->
        (c.Build_context.name, c))
  in
  let caching =
    let open Option.O in
    let* ({ cache = (module Caching : Cache.Caching); _ } as v) = caching in
    let open Result.O in
    let res =
      let+ cache = Caching.Cache.set_build_dir Caching.cache Path.build_dir in
      (module struct
        module Cache = Caching.Cache

        let cache = cache
      end : Cache.Caching)
    in
    match res with
    | Result.Ok cache -> Some { v with cache }
    | Result.Error e ->
      User_warning.emit
        [ Pp.text "Unable to set cache build directory"
        ; Pp.textf "Reason: %s" e
        ];
      None
  in
  let t =
    { contexts
    ; packages = Fdecl.create Dyn.Encoder.opaque
    ; gen_rules = Fdecl.create Dyn.Encoder.opaque
    ; init_rules = Fdecl.create Dyn.Encoder.opaque
    ; vcs = Fdecl.create Dyn.Encoder.opaque
    ; caching
    ; sandboxing_preference = sandboxing_preference @ Sandbox_mode.all
    ; rule_done = 0
    ; rule_total = 0
    ; errors = []
    ; (* This mutable table is safe: it merely maps paths to lazily created
         mutexes. *)
      locks = Table.create (module Path) 32
    ; promote_source
    ; build_mutex
    }
  in
  Console.Status_line.set (fun () ->
      Some
        (Pp.verbatim
           (sprintf "Done: %u/%u (jobs: %u)" t.rule_done t.rule_total
              (Scheduler.running_jobs_count ()))));
  set t

let cache_teardown () =
  match get_cache () with
  | Some { cache = (module Caching : Cache.Caching); _ } ->
    (* Synchronously wait for the end of the connection with the cache daemon,
       ensuring all dedup messages have been queued. *)
    Caching.Cache.teardown Caching.cache;
    (* Hande all remaining dedup messages. *)
    Scheduler.wait_for_dune_cache ()
  | None -> ()

let targets_of = targets_of

let all_targets () = all_targets (t ())
