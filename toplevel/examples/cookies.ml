let document = Dom_html.document
let window = Dom_html.window

let _s s = Js.string s
let _f f = Js.wrap_callback f

let getElementById id =
  Js.Opt.get (document##getElementById (_s id)) (fun () -> assert false)

let get_cookies () =
  let reg1 = Regexp.regexp "; " in
  let list = Regexp.split reg1 (Js.to_string document##cookie) in
  let reg2 = Regexp.regexp "=" in
  List.map (fun s ->
    match Regexp.split reg2 s with
        x :: y -> (x, String.concat "=" y)
      | [] -> ("", "")
  ) list

let jsnew0 (constr : 'a Js.t Js.constr) () =
  (Js.Unsafe.new_obj constr [| |] : 'a Js.t)

let jsnew1 (constr : ('a -> 'z Js.t) Js.constr) (a) =
  (Js.Unsafe.new_obj constr [|
    Js.Unsafe.inject (a : 'a)
                            |] : 'z Js.t)

let jsnew2 (constr : ('a -> 'b -> 'z Js.t) Js.constr) (a,b) =
  (Js.Unsafe.new_obj constr [|
    Js.Unsafe.inject (a : 'a);
    Js.Unsafe.inject (b : 'b);
                            |] : 'z Js.t)

let jsnew3 (constr : ('a -> 'b -> 'c -> 'z Js.t) Js.constr) (a,b,c) =
  (Js.Unsafe.new_obj constr [|
    Js.Unsafe.inject (a : 'a);
    Js.Unsafe.inject (b : 'b);
    Js.Unsafe.inject (c : 'c);
                            |] : 'z Js.t)

let set_cookie key value =
  let today = jsnew0 Js.date_now () in
  let expire_time = today##setTime
    ((Js.to_float today##getTime()) *. 60. *. 60. *. 24. *. 365.) in
  document##cookie <- Js.string (Printf.sprintf "%s=%s;expires=%f" key value
                              (Js.to_float expire_time))

let _ =
  let title = getElementById "main-title" in
  let v = try
            int_of_string (List.assoc "counter" (get_cookies() ))
    with Not_found ->
      0
  in
  let v = v + 1 in
  set_cookie "counter" (string_of_int v);
  title##innerHTML <- _s (
    Printf.sprintf "This is your %s time in TryOCaml"
      (match v with
          1 -> "first"
        | 2 -> "second"
        | 3 -> "third"
        | _ -> string_of_int v ^ "th"))
