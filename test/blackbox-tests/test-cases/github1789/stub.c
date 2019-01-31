#include <caml/mlvalues.h>
#include <caml/alloc.h>

#include "message.h"

CAMLprim value test_message()
{
  return caml_copy_string(MSG);
}
