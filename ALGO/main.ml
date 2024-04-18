(*main.ml*)
open Util
open AnalyseMoindresCarres
open Polynome



let d = 2;;  (* Par exemple, pour une courbe quadratique *)

let generer_points_cercle h k r n =
  let points = ref [] in
  for i = 0 to n - 1 do
    let t = 2. *. Float.pi *. (float_of_int i) /. (float_of_int n) in
    let x = h +. r *. cos(t) in
    let y = k +. r *. sin(t) in
    points := (x, y) :: !points
  done;
  !points

(* Fonction pour écrire les points dans un fichier *)
let ecrire_points_dans_fichier fichier points =
  let oc = open_out fichier in
  List.iter (fun (x, y) ->
    Printf.fprintf oc "%.4f,%.4f\n" x y
  ) points;
  close_out oc

let () =
  let points_cercle = generer_points_cercle 0. 0. 5. 7 in
  ecrire_points_dans_fichier "./points.csv" points_cercle;
  let points = lire_points "./points.csv" in  (* Assurez-vous que lire_points est accessible *)
  let analyse = new analyseMoindresCarres points d in
  let coefficients_triplets = analyse#resoudreSysteme in
  let polynome = new polynome d coefficients_triplets in
  analyse#calculer_cercle;
  print_endline polynome#to_string


  
  