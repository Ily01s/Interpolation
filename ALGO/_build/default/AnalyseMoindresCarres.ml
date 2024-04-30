open Owl;;

class analyseMoindresCarres points d = object (self)
  val mutable points = points
  val d = d
  val mutable coeffs_list = []  
  
  (* Calcul de la ligne de la matrice A pour un point donné, avec ordre correct des termes *)
  method private calculer_ligne_matrice (p : Point.point) =
    let x = p#get_x in
    let y = p#get_y in
    let terms = ref [] in
    for i = d downto 0 do
      for j = d - i downto 0 do
        terms := (x ** float_of_int i) *. (y ** float_of_int j) :: !terms (* x^i * y^j * z^k * ... * a^b * c^d * e^f * ... * *)
      done;
    done;
    Array.of_list (List.rev !terms)

  (* Construction de la matrice A avec l'ordre correct des termes *)
  method private construireMatriceA =
    let terms_count = (d + 1) * (d + 2) / 2 in
    let matrix = Array.make_matrix (List.length points) terms_count 0. in
    List.iteri (fun i point ->
      let line = self#calculer_ligne_matrice point in     (* Calculer la ligne de la matrice A pour le point donné *)
      Array.iteri (fun j v -> matrix.(i).(j) <- v) line   (* Copier les valeurs de la ligne dans la matrice A *)
    ) points;
    matrix
  
    method update_coeffs_list coeffs =
      coeffs_list <- Array.to_list coeffs     (* Mettre à jour la liste des coefficients *)
  
      (* Méthode pour calculer le cercle à partir des coefficients trouvés *)
      method calculer_cercle =
        match coeffs_list with
        | a :: d :: e :: f :: _ ->
          let h = -.d /. (2. *. a) in
          let k = -.e /. (2. *. a) in
          let r = sqrt ((d ** 2. +. e ** 2.) /. (4. *. a ** 2.) -. f /. a) in
          Printf.printf "Centre: (%.2f, %.2f)\nRayon: %.2f\n" h k r
        | _ -> 
          let taille = List.length coeffs_list in
          Printf.printf "La taille de la liste des coefficients est : %d\n" taille


  (* Méthode pour resoudre systeme *)
  method resoudreSysteme =
    let a = Mat.of_arrays (self#construireMatriceA) in
    Printf.printf "Matrice a :\n";
      Owl.Mat.print a;
        (* Calculer A^T A *)
      let ata = Mat.(transpose a *@ a) in
      Printf.printf "Matrice ata :\n";
          Owl.Mat.print ata;
      (* Utiliser SVD pour résoudre A^TA x = 0 *)
      let _, s, vt = Linalg.D.svd ata in

    (* Imprimez les dimensions de 'a' et 'b' pour le débogage *)
    Printf.printf "Dimensions de 'a': %d x %d\n" (Mat.row_num a) (Mat.col_num a);

     let _, smallest_singular_value_index = Mat.min_i s in  (* Trouver l'index de la plus petite valeur singulière *)
     let solution = Mat.col vt smallest_singular_value_index.(1) in
     let coeffs = Mat.to_array solution in
     self#update_coeffs_list coeffs;  (* Mettre à jour la liste des coefficients *)
     let coeffs_triplets =
      let rec aux acc i j =
        if i > d then acc         (* Si i dépasse d, on a fini *)
        else if i + j > d then aux acc (i + 1) 0    (* Si i + j dépasse d, on passe à la ligne suivante *)
        else
          let index = (i * (d + 1)) - (i * (i - 1) / 2) + j in      (* Calculer l'index de l'élément dans la liste des coefficients *)
          if index >= List.length coeffs_list then acc     (* Si l'index dépasse la taille de la liste des coefficients, on a fini *)
          else
            let coeff = List.nth coeffs_list index in
            aux ((i, j, coeff) :: acc) i (j + 1)
      in
      List.rev (aux [] 0 0)
    
     in
     coeffs_triplets

     

end