(* Définition du type pour un point dans le plan 2D *)
type point = { x: float; y: float }

(* Fonction pour lire les points depuis un fichier *)
let lire_points fichier =
  let ic = open_in fichier in
  let rec aux points =
    try
      let line = input_line ic in
      let coords = String.split_on_char ',' line in
      let x = float_of_string (List.nth coords 0) in
      let y = float_of_string (List.nth coords 1) in
      aux ({x; y} :: points)
    with End_of_file -> points
  in
  let points = aux [] in
  close_in ic;
  List.rev points

(* Fonction principale *)
let () =
  let points = lire_points "chemin/vers/votre/fichier.csv" in
  (* Ici vous allez construire la matrice A et résoudre le système *)
