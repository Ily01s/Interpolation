(*main.ml*)
open Util
open AnalyseMoindresCarres
open Polynome
open AnalyseRBF


let d = 2;;  

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
let points_validation = generer_points_cercle 2. 1. 5. 8 in
  let points_cercle = generer_points_cercle 1. 1. 5. 7 in
  ecrire_points_dans_fichier "./points.csv" points_cercle;
  let points = lire_points "./points.csv" in  (* Assurez-vous que lire_points est accessible *)
  let analyse = new analyseMoindresCarres points d in
  let coefficients_triplets = analyse#resoudreSysteme in
  let polynome = new polynome d coefficients_triplets in
  analyse#calculer_cercle;
  let points_tuples = List.map (fun p -> (p#get_x, p#get_y)) points in
  let mon_analyse_rbf = new analyseRBF points_tuples d in
  let poids = mon_analyse_rbf#trouver_poids in
  let resultats_validation = mon_analyse_rbf#valider points_validation in
  (* Étape 5: Afficher ou analyser les résultats de validation *)
  List.iter (fun (x, y, valeur) ->
    Printf.printf "Validation: f(%f, %f) = %f\n" x y valeur
  ) resultats_validation;
  Owl.Mat.print poids;
  print_endline polynome#to_string


  
  