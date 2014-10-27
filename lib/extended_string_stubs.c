#define _GNU_SOURCE

#include <stdio.h>
#include <string.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>

value core_extended_is_substring(value v_haystack, value v_needle)
{
  return Val_bool(memmem((const void *) String_val(v_haystack),
                         caml_string_length(v_haystack),
                         (const void *) String_val(v_needle),
                         caml_string_length(v_needle)) != NULL);
}
