open Owl;;

class analyseRBF points d  = object (self)
  val mutable points = points
  val d = d
  

  method private construire_matrice_rbf =
    let n = List.length points in
    let mat = Mat.zeros n n in
    let epsilon = 0.1 in  (* Récupère ou calcule epsilon *)
    List.iteri (fun i (xi, yi) ->
      List.iteri (fun j (xj, yj) ->
        let r2 = ((xi -. xj) ** 2.) +. ((yi -. yj) ** 2.) in
        let exp_val = exp (-. epsilon *. r2) in
        Printf.printf "Distance^2 entre (%f, %f) et (%f, %f) : %f, exp_val: %f\n" xi yi xj yj r2 exp_val;
        Mat.set mat i j exp_val
      ) points
    ) points;
    mat

  method private definir_vecteur_cible =
    let n = List.length points in
    Mat.zeros n 1

    method trouver_poids =
      (* Obtenir la matrice A *)
      let a = self#construire_matrice_rbf in
      Printf.printf "Matrice A :\n";
      Owl.Mat.print a;
    
      (* Définir le vecteur cible b, qui est un vecteur de zéros dans ce contexte *)
      let b = self#definir_vecteur_cible in
      Printf.printf "Vecteur b :\n";
      Owl.Mat.print b;
    
      (* Calculer A^T A *)
      let ata = Mat.(transpose a *@ a) in
      Printf.printf "Matrice A^T A :\n";
      Owl.Mat.print ata;
    
      (* Paramètre de régularisation λ, à ajuster selon les besoins *)
      let lambda = 0.1 in
    
      (* Ajouter le terme de régularisation λI à A^T A *)
      let n = Mat.row_num ata in
      let lambda_i = Mat.(scalar_mul lambda (eye n)) in
      let ata_reg = Mat.(add ata lambda_i) in
      Printf.printf "Matrice A^T A + λI (régularisée) :\n";
      Owl.Mat.print ata_reg;
    
      (* Résoudre le système régularisé (A^T A + λI)w = A^T b *)
      (* Note : Comme b est un vecteur de zéros, A^T b sera également un vecteur de zéros. *)
      let atb = Mat.(transpose a *@ b) in
      let w = Linalg.D.linsolve ata_reg atb in
    
      (* Retourner les poids calculés *)
      w
    
    
      
end;;
