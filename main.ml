(*main.ml*)

let d = 2  (* Par exemple, pour une courbe quadratique *)

let lire_points fichier =
  let ic = open_in fichier in
  let rec aux acc =
    try
      let line = input_line ic in
      let coords = String.split_on_char ',' line in
      let x = float_of_string (List.nth coords 0) in
      let y = float_of_string (List.nth coords 1) in
      aux (new point x y :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  aux []

let () =
  let points = lire_points "chemin/vers/votre/fichier.csv" in
  let analyse = new analyseMoindresCarres points d in
  let coefficients = analyse#resoudreSysteme in
  (* Utilisez `coefficients` ici, par exemple pour les imprimer *)
