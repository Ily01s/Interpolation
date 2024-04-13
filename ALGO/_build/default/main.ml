(*main.ml*)
open Util
open AnalyseMoindresCarres
open Polynome



let d = 1;;  (* Par exemple, pour une courbe quadratique *)

let () =
  let points = lire_points "./points.csv" in  (* Assurez-vous que lire_points est accessible *)
  let analyse = new analyseMoindresCarres points d in
  let coefficients_triplets = analyse#resoudreSysteme in
  let polynome = new polynome d coefficients_triplets in
  print_endline polynome#to_string


