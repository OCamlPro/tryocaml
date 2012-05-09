let get_cookie () =
  let reg1 = Regexp.regexp "; " in
  let list = Regexp.split reg1 (Js.to_string Dom_html.document##cookie) in
  let reg2 = Regexp.regexp "=" in
  List.map (fun s ->
    match Regexp.split reg2 s with
        x :: y -> (x, String.concat "=" y)
      | [] -> ("", "")
  ) list

let set_cookie key value =
  let today = jsnew Js.date_now () in
  let expire_date = jsnew Js.date_ms 
      (today##getFullYear () + 1, today##getMonth (), today##getDay (), 
       today##getHours (), today##getMinutes (), today##getSeconds (), 
       today##getMilliseconds ()) in
  let expire_time = Js.to_string expire_date##toGMTString () in
  Dom_html.document##cookie <- 
    Js.string (Printf.sprintf "%s=%s;expires=%s" key value expire_time)

let set_cookie_with_timeout key value date =
  let expire_time = Js.to_string date##toGMTString () in
  Dom_html.document##cookie <- 
    Js.string (Printf.sprintf "%s=%s;expires=%s" key value expire_time)
