open Owl;;

class analyseMoindresCarres points d = object (self)
  val mutable points = points
  val d = d
  
  (* Calcul de la ligne de la matrice A pour un point donné, avec ordre correct des termes *)
  method private calculer_ligne_matrice (p : Point.point) =
    let x = p#get_x in
    let y = p#get_y in
    let terms = ref [] in
    for i = d downto 0 do
      for j = d - i downto 0 do
        terms := (x ** float_of_int i) *. (y ** float_of_int j) :: !terms
      done;
    done;
    Array.of_list (List.rev !terms)

  (* Construction de la matrice A avec l'ordre correct des termes *)
  method private construireMatriceA =
    let terms_count = (d + 1) * (d + 2) / 2 in
    let matrix = Array.make_matrix (List.length points) terms_count 0. in
    List.iteri (fun i point ->
      let line = self#calculer_ligne_matrice point in
      Array.iteri (fun j v -> matrix.(i).(j) <- v) line
    ) points;
    matrix
  
  
  method resoudreSysteme =
    let a = Mat.of_arrays (self#construireMatriceA) in
    Printf.printf "Matrice a :\n";
      Owl.Mat.print a;
    (* Fill b with values *)
    (* Calculer A^T A *)
  let ata = Mat.(transpose a *@ a) in
  Printf.printf "Matrice ata :\n";
      Owl.Mat.print ata;
  (* Utiliser SVD pour résoudre A^TA x = 0 *)
  let _, s, vt = Linalg.D.svd ata in

(* Imprimez les dimensions de 'a' et 'b' pour le débogage *)
Printf.printf "Dimensions de 'a': %d x %d\n" (Mat.row_num a) (Mat.col_num a);

     let _, smallest_singular_value_index = Mat.min_i s in
     let solution = Mat.col vt smallest_singular_value_index.(1) in
     let coeffs = Mat.to_array solution in
     let coeffs_list = Array.to_list coeffs in (* Separate step for clarity *)
     let coeffs_triplets =
      let rec aux acc i j =
        if i > d then acc
        else if i + j > d then aux acc (i + 1) 0
        else
          let index = (i * (d + 1)) - (i * (i - 1) / 2) + j in
          if index >= List.length coeffs_list then acc
          else
            let coeff = List.nth coeffs_list index in
            aux ((i, j, coeff) :: acc) i (j + 1)
      in
      List.rev (aux [] 0 0)
    
     in
     coeffs_triplets


end
