#include <jane_common.h>
#include <caml/mlvalues.h>

CAMLprim value caml_running_byte_code_nc(
  value __unused v1, value __unused v2,
  value __unused v3, value __unused v4,
  value __unused v5, value __unused v6)
{
  return Val_bool(0);
}

CAMLprim value caml_running_byte_code_bc(value __unused *argv, int __unused argn)
{
  return Val_bool(1);
}
