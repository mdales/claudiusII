
type t = {
  width           : int ;
  height          : int ;
  scale           : int ;
  palette         : Palette.t ;
}


let create (width : int) (height : int) (scale : int) (palette : Palette.t) : t =
  if scale <= 0 then raise (Invalid_argument "Invalid scale");
  if width <= 0 then raise (Invalid_argument "Invalid width");
  if height <= 0 then raise (Invalid_argument "Invalid height");
  { width ; height ; scale ; palette }


let dimensions (screen : t) : int * int =
  screen.width, screen.height

let palette (screen : t) : Palette.t =
  screen.palette

let scale (screen : t) : int =
  screen.scale
