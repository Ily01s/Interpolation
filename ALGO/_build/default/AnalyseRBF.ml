open Owl;;

class analyseRBF points poids = object (self)
  val mutable points = points
  val mutable poids = poids

(* Construire la matrice RBF (fonction de base radiale)*)
  method private construire_matrice_rbf =
    let n = List.length points in
    let mat = Mat.zeros n n in
    let epsilon = 0.0001 in  
    List.iteri (fun i (xi, yi) ->
      List.iteri (fun j (xj, yj) ->
        let r2 = ((xi -. xj) ** 2.) +. ((yi -. yj) ** 2.) in
        let exp_val = exp (-. epsilon *. r2) in
        Mat.set mat i j exp_val
      ) points
    ) points;
    mat

    (* Définir le vecteur cible (y) pour la régression linéaire *)
  method private definir_vecteur_cible =
    let n = List.length points in
    Mat.zeros n 1

  method trouver_poids =
    let a = self#construire_matrice_rbf in
    (* Effectuer SVD sur A *)
    let _, _, vt = Linalg.D.svd a in

      (* La solution w est le vecteur propre correspondant à la plus petite valeur singulière
   qui est la dernière colonne de V (puisque Vt est déjà transposée) *)
    let w = Mat.col vt (Mat.col_num vt - 1) in
    w

    (* Mettre à jour les poids *)
  method reconstruire_fonction x y =
    let epsilon = 0.1 in
    let valeurs_rbf = List.mapi (fun _ (xi, yi) ->
      let r2 = ((x -. xi) ** 2.) +. ((y -. yi) ** 2.) in
      exp (-. epsilon *. r2)
    ) points in
    List.fold_left2 (fun acc rbf_val w_val ->
      acc +. (rbf_val *. w_val)
    ) 0. valeurs_rbf (Array.to_list (Mat.to_array self#trouver_poids))
  
    (* Valider la fonction reconstruite en comparant les valeurs prédites avec les valeurs réelles *)
  method valider points_validation =
    List.map (fun (x, y) ->
      let valeur = self#reconstruire_fonction x y in
      (x, y, valeur)
    ) points_validation
end;;
