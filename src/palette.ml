
type t = (int * int * int * int) array

let generate_mono_palette (size : int) : t =
  if size <= 0 then raise (Invalid_argument "Palette size must not be zero or negative");
  Array.init size (fun (index : int) ->
    let fi = float_of_int index and fsize = float_of_int size in
    let ch = ((fi /. fsize) *. 255.0) in
    (int_of_float ch, int_of_float ch, int_of_float ch, 255)
  )

let generate_plasma_palette (size : int) : t =
  if size <= 0 then raise (Invalid_argument "Palette size must not be zero or negative");
  Array.init size (fun (index : int) ->
    let fi = float_of_int index and fsize = float_of_int size in
    let fred = (cos (fi *. ((2.0 *. Float.pi) /. fsize)) *. 127.0) +. 128.0 in
    let fgreen = (cos ((fi +. (fsize /. 3.0)) *. ((2.0 *. Float.pi) /. fsize)) *. 127.0) +. 128.0 in
    let fblue = (cos ((fi +. ((fsize *. 2.0) /. 3.0)) *. ((2.0 *. Float.pi) /. fsize)) *. 127.0) +. 128.0 in
    (int_of_float fblue, int_of_float fgreen, int_of_float fred, 255)
    (* Int32.of_int ((0xFF lsl 24) lor ((int_of_float fred) lsl 16) lor ((int_of_float fgreen) lsl 8) lor (int_of_float fblue))*)
  )

let string_to_chunks (x : string) (size : int) : string list =
  let rec loop sofar remainder =
    let length_left = String.length remainder in
    if length_left >= size then
      loop ((String.sub remainder 0 size) :: sofar) (String.sub remainder size (length_left - size))
    else if length_left == 0 then
      sofar
    else
      raise (Invalid_argument "String size not a multiple of 6 chars per colour")
  in
  List.rev (loop [] x)

let chunks_to_colors (raw : string list) : t =
  Array.of_list raw |>
  Array.map (fun s ->
    let v = Int64.of_string s in
    (Int64.to_int (Int64.logand v 255L),
      Int64.to_int (Int64.logand (Int64.shift_right v 8) 255L),
      Int64.to_int (Int64.logand (Int64.shift_right v 16) 255L),
      Int64.to_int (Int64.logand (Int64.shift_right v 24) 255L)
    )
  )

let load_tic80_palette (raw : string) : t =
  let parts = String.split_on_char ':' raw in
  let strchunks = string_to_chunks (List.nth parts 1) 6 in
  if List.length strchunks > 0 then
    chunks_to_colors strchunks
  else
    raise (Invalid_argument "Palette size must not be zero or negative")

let size (palette : t) : int =
    Array.length palette

let index_to_rgb (palette : t) (index : int) =
  let palsize = Array.length palette in
  let index = index mod palsize in
  palette.(if index >= 0 then index else index + palsize)

let to_list (palette : t) : (int * int * int * int) list =
    (Array.to_list palette)

let of_list (rgb_list : (int * int * int * int) list) : t =
  if List.length rgb_list > 0 then
    Array.of_list rgb_list
  else
    raise (Invalid_argument "Palette size must not be zero or negative")
