let () =
  let fmt file = Format.formatter_of_out_channel (open_out file) in
  let fmt_c = fmt "sodium_stubs.c" in
  Format.fprintf fmt_c {|#include "sodium.h"@.|};
  Cstubs.write_c fmt_c ~prefix:"caml_" (module Sodium_bindings.C);
  let fmt_ml = fmt "sodium_generated.ml" in
  Cstubs.write_ml fmt_ml ~prefix:"caml_" (module Sodium_bindings.C);
  flush_all ()
;;
