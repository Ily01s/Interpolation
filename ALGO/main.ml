(*main.ml*)
open Util
open AnalyseMoindresCarres
open Polynome



let d = 3;;  (* Par exemple, pour une courbe quadratique *)

let () =
  let points = lire_points "./points.csv" in  (* Assurez-vous que lire_points est accessible *)
  let analyse = new analyseMoindresCarres points d in
  let coefficients_triplets = analyse#resoudreSysteme in
  let polynome = new polynome d coefficients_triplets in
  print_endline polynome#to_string


  (*let generer_points_cercle h k r n =
    let points = ref [] in
    for i = 0 to n - 1 do
      let theta = 2.0 *. Float.pi *. (float_of_int i) /. (float_of_int n) in
      let x = h +. r *. cos(theta) in
      let y = k +. r *. sin(theta) in
      points := new point x y :: !points  (* Création d'objets point ici *)
    done;
    List.rev !points
  
  let () =
    let h, k, r = 0., 0., 5. in  (* Centre (h, k) et rayon r du cercle *)
    let n = 100 in  (* Nombre de points à générer *)*)
  