(* open Graphics *)
open Tsdl

module KeyCodeSet = Set.Make(Int)

type boot_func = Screen.t -> Primitives.t list
type tick_func = int -> Screen.t -> unit -> KeyCodeSet.t -> Primitives.t list


(* ----- *)

let (>>=) = Result.bind
let (>|=) v f = Result.map f v

let sdl_init (width : int) (height : int) (title : string) (make_fullscreen : bool) =
  Sdl.init Sdl.Init.(video + events) >>= fun () ->
  Sdl.create_window ~w:width ~h:height title Sdl.Window.(if make_fullscreen then fullscreen else windowed) >>= fun w ->
  Sdl.create_renderer ~flags:Sdl.Renderer.(accelerated + presentvsync) w >>= fun r ->
  Sdl.show_cursor (not make_fullscreen) >|= fun _ -> (w, r)
  
let renderer_set_color (rend : Sdl.renderer) (palette : Palette.t) (c : int)  = 
  let col = Int32.to_int (Palette.index_to_rgb palette c) in
  let r = col land 0xFF
  and g = (col lsl 8) land 0xFF
  and b = (col lsl 16) land 0xFF
  and a = (col lsl 24) land 0xFF in
  match (Sdl.set_render_draw_color rend r g b a) with
  | Ok () -> ()
  | Error (`Msg e) -> failwith (Printf.sprintf "failed to set color: %s" e) 

let render_primitive (rend : Sdl.renderer) (palette : Palette.t) (p : Primitives.t) =
  match p with
  | Point (a, c) -> (
    renderer_set_color rend palette c;
    Sdl.render_draw_point rend a.x a.y
  )
  | Line (a, b, c) -> (
    renderer_set_color rend palette c;
    Sdl.render_draw_line rend a.x a.y b.x b.y
  )
  | _ -> Ok ()
  

let render_primitives (rend : Sdl.renderer) (palette : Palette.t) (pl : Primitives.t list) =
  List.fold_left (fun acc prim -> 
    match acc with
    | Ok () -> render_primitive rend palette prim
    | _ -> acc
   ) (Ok ()) pl

(* ----- *)

let run (title : string) (boot : boot_func option) (tick : tick_func) (s : Screen.t) =
  let make_full = Array.to_list Sys.argv |> List.exists (fun a -> (String.compare a "-f") == 0) in

  let s = match make_full with
  | false -> s
  | true -> (
    let w, h = Screen.dimensions s
    and p = Screen.palette s in
    Screen.create w h 1 p
  )
  in

  let width, height = Screen.dimensions s
  and scale = Screen.scale s in

  match sdl_init (width * scale) (height * scale) title make_full with
  | Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
  | Ok (w, r) ->

      let initial_scene = match boot with
      | None -> []
      | Some bfunc -> bfunc s
      in
      
      ignore(render_primitives r (Screen.palette s) initial_scene);

      let e = Sdl.Event.create () in

      let rec loop (t : int) (keys : KeyCodeSet.t) last_t = (

        let now = Sdl.get_ticks () in
        let diff = Int32.(sub (of_int(1000 / 60)) (sub now last_t)) in
        if Int32.(compare diff zero) > 0 then (
          Sdl.delay diff
        );

        let primitives = tick t s () keys in

        

        match render_primitives r (Screen.palette s) primitives with
        | Error (`Msg e) -> Sdl.log "Boot error: %s" e
        | Ok () -> (
          let exit, keys =
          match Sdl.poll_event (Some e) with
          | true -> (
            match Sdl.Event.(enum (get e typ)) with
            | `Quit -> (true, keys)
            | `Key_down -> (false, KeyCodeSet.add Sdl.Event.(get e keyboard_keycode) keys)
            | `Key_up -> (false, KeyCodeSet.remove Sdl.Event.(get e keyboard_keycode) keys)
            | _ -> (false, keys)
          )
          | false -> (false, keys) in
          match exit with
          | true -> ()
          | false -> loop (t + 1) keys now
        )
      ) in loop 0 KeyCodeSet.empty Int32.zero;


      Sdl.destroy_renderer r;
      Sdl.destroy_window w;
      Sdl.quit ()
