
open Graphics

type couleur = Rouge | Bleu | Jaune | Rien

let case = 60
let rayon = 30

let random_couleur () =
  match Random.int 3 with
    | 0 -> Rouge
    | 1 -> Bleu
    | 2 -> Jaune
    | _ -> assert false

let grille =
  Array.init 15
    (fun _ ->
       Array.init 10 (fun _ -> random_couleur ()))

let color_of v =
  match v with
    | Rouge -> red
    | Bleu -> blue
    | Jaune -> yellow
    | Rien -> black

let dessine () =
  for i = 0 to 14 do
    for j = 0 to 9 do
      let c = color_of (grille.(i).(j)) in
      set_color c;
      fill_circle (i * case + rayon) (j * case + rayon) rayon
    done
  done

let couleur_case i j =
  if i>=0 && i<=14 && j>=0 && j<=9 then grille.(i).(j) else Rien

let efface_region i j c =
  let rec efface i j =
    if couleur_case i j = c then
      begin
        grille.(i).(j) <- Rien;
        efface (i+1) j;
        efface (i-1) j;
        efface i (j+1);
        efface i (j-1)
      end
  in
  efface i j

let tasse_tableau garde t =
  let d = ref 0 in
  for i = 0 to Array.length t - 1 do
    if garde t.(i) then
      let v = t.(!d) in
      t.(!d) <- t.(i);
      t.(i) <- v;
      incr d
  done

let tasse () =
  for i = 0 to 14 do
    tasse_tableau (fun c -> c<>Rien) grille.(i)
  done;
  tasse_tableau (fun c -> c.(0)<>Rien) grille

let click i j =
  let c = grille.(i).(j) in
  if c<>Rien &&
    (couleur_case (i+1) j = c ||
     couleur_case (i-1) j = c ||
     couleur_case i (j+1) = c ||
     couleur_case i (j-1) = c)
  then
    begin
      efface_region i j c;
      tasse ();
      dessine ()
    end

let main () =
  open_graph " 900x600";
  set_color black;
  fill_rect 0 0 900 600;
  dessine ();
  loop_at_exit [Button_down] (fun st ->
  let dx = st.mouse_x mod case - rayon in
  let dy = st.mouse_y mod case - rayon in
  if dx * dx + dy * dy <= rayon * rayon then
    begin
      let i = st.mouse_x / case in
      let j = st.mouse_y / case in
      click i j
    end;
  )
