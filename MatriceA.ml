open Owl

let construire_matrice_a points d =
  let n = List.length points in
  let m = (* Calculez le nombre de colonnes basé sur d *) in
  let a = Mat.zeros n m in
  List.iteri (fun i {x; y} ->
    (* Remplissez la i-ème ligne de la matrice A ici *)
  ) points;
  
