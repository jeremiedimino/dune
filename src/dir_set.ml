open! Stdune

type t =
  | Empty
  | Universal
  | Nontrivial of nontrivial
and
  nontrivial = {
  here : bool;
  children : children;
}
and
  children = {
  default : bool;
  exceptions : t String.Map.t;
}

let trivial = function
  | false -> Empty
  | true -> Universal

let create ~here ~children =
  if String.Map.is_empty children.exceptions && here = children.default then
    trivial here
  else
    Nontrivial { here; children }

let is_empty = function
  | Empty -> true
  | _ -> false

let is_universal = function
  | Universal -> true
  | _ -> false

let is_trivial ~value t =
  match value, t with
  | false, Empty | true, Universal -> true
  | _ -> false

let empty = Empty
let universal = Universal

let empty_children =
  { default = false
  ; exceptions = String.Map.empty
  }
let universal_children =
  { default = true
  ; exceptions = String.Map.empty
  }

module Children = struct
  type nonrec t = children = private {
    default : bool;
    exceptions : t String.Map.t;
  }

  let exceptions t = t.exceptions
  let default t = t.default
end

let merge_children a b ~f =
  let default = a.default || b.default in
  { default
  ; exceptions =
      String.Map.merge a.exceptions b.exceptions ~f:(fun _ x y ->
        let x = Option.value x ~default:(trivial a.default) in
        let y = Option.value y ~default:(trivial b.default) in
        let res = f x y in
        Option.some_if (not (is_trivial ~value:default res)) res)
  }

let rec union x y =
  match x, y with
  | Empty, _ -> y
  | _, Empty -> x
  | Universal, _ | _, Universal -> universal
  | Nontrivial x, Nontrivial y ->
    create
      ~here:(x.here || y.here)
      ~children:(merge_children x.children y.children ~f:union)

let rec intersect x y =
  match x, y with
  | Universal, _ -> y
  | _, Universal -> x
  | Empty, _ | _, Empty -> empty
  | Nontrivial x, Nontrivial y ->
    create
      ~here:(x.here && y.here)
      ~children:(merge_children x.children y.children ~f:intersect)

let rec negate x =
  match x with
  | Universal -> empty
  | Empty -> universal
  | Nontrivial { here; children = { default; exceptions } } ->
    Nontrivial { here = not here
               ; children =
                   { default = not default
                   ; exceptions = String.Map.map exceptions ~f:negate
                   }
               }

let here = function
  | Empty -> false
  | Universal -> true
  | Nontrivial t -> t.here

let children = function
  | Empty -> empty_children
  | Universal -> universal_children
  | Nontrivial t -> t.children

let rec mem t dir = match dir with
  | [] -> here t
  | child :: rest ->
    let children = children t in
    match String.Map.find children.exceptions child with
    | None -> children.default
    | Some t ->
      mem t rest

let mem t dir = mem t (Path.Build.explode dir)

let descend t child =
  let children = children t in
  match String.Map.find children.exceptions child with
  | None -> trivial children.default
  | Some t -> t

let union_all = List.fold_left ~init:empty ~f:union

let of_subtree_gen subtree =
  let rec loop = function
    | [] -> subtree
    | component :: rest ->
      Nontrivial
        { here = false
        ; children =
            { default = false
            ; exceptions = String.Map.singleton component (loop rest)
            }
        }
  in
  fun path -> loop (Path.Build.explode path)

let just_the_root =
  Nontrivial
    { here = true
    ; children = empty_children
    }

let of_subtrees paths =
  List.map paths ~f:(of_subtree_gen universal)
  |> union_all

let of_individual_dirs paths =
  List.map paths ~f:(of_subtree_gen just_the_root)
  |> union_all

type element =
  | One_dir of Path.Build.t
  | Subtree of Path.Build.t

let of_list list =
  List.map list ~f:(function
    | One_dir dir -> of_subtree_gen just_the_root dir
    | Subtree dir -> of_subtree_gen universal dir)
  |> union_all

let is_subset x ~of_ =
  is_empty (intersect x (negate of_))

let rec to_sexp t = match t with
  | Empty -> Sexp.Atom "Empty"
  | Universal -> Sexp.Atom "Universal"
  | Nontrivial t ->
    Sexp.List (
      (
        (match t.here with | true -> [ ".", Sexp.Atom "true" ] | false -> []) @
        (String.Map.to_list t.children.exceptions
         |> List.map ~f:(fun (s, t) ->
           s, to_sexp t)) @
        (match t.children.default with
         | false -> []
         | true -> [("*", Sexp.Atom "Universal")]))
      |> List.map ~f:(fun (k, v) -> Sexp.List [Sexp.Atom k; v]))
