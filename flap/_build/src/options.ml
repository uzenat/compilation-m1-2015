(** Options *)

let make_string_option what kind =
  let language = ref "" in
  let get () =
    if !language = "" then
      Error.global_error
        "during analysis of options"
        (Printf.sprintf "You should specify the %s %s using '--%s'."
           kind what kind);
    !language
  in
  let set = ( := ) language in
  let is_set () = !language <> "" in
  get, set, is_set

let (get_source_language, set_source_language, is_source_language_set) =
  make_string_option "language" "source"

let (get_target_language, set_target_language, is_target_language_set) =
  make_string_option "language" "target"

type mode = Interactive | Batch

let mode = ref Batch

let set_mode = ( := ) mode

let get_mode () = !mode

let (get_input_filename, set_input_filename, is_input_filename_set) =
  make_string_option "filename" "input"

let set_interactive_mode = function
  | true -> set_mode Interactive
  | false -> set_mode Batch

let running_mode = ref false

let get_running_mode () = !running_mode

let set_running_mode = ( := ) running_mode

let verbose_mode = ref false

let get_verbose_mode () = !verbose_mode

let set_verbose_mode = ( := ) verbose_mode

let dry_mode = ref false

let get_dry_mode () = !dry_mode

let set_dry_mode = ( := ) dry_mode

let benchmark = ref true

let set_benchmark = ( := ) benchmark

let get_benchmark () = !benchmark

let unsafe = ref false

let set_unsafe = ( := ) unsafe

let get_unsafe () = !unsafe

let using : string list ref = ref []

let insert_using x = using := x :: !using

let get_using () = !using

let gcc : bool ref = ref false

let set_gcc v = gcc := v

let get_gcc () = !gcc

let show_types : bool ref = ref false

let set_show_types = (:=) show_types

let get_show_types () = !show_types

let infer_types : bool ref = ref false

let set_infer_types = (:=) infer_types

let get_infer_types () = !infer_types

let check_types : bool ref = ref true

let set_check_types = (:=) check_types

let get_check_types () = !check_types
