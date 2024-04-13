open Owl;;

class analyseMoindresCarres points d = object (self)
  val mutable points = points
  val d = d
  
  (* Calcul de la ligne de la matrice A pour un point donné *)
  method private calculer_ligne_matrice (p : Point.point) =
    let x = p#get_x in
    let y = p#get_y in
    let rec aux i j acc =
      if i > d then acc
      else if i + j > d then aux (i + 1) 0 acc
      else aux i (j + 1) ((x ** float_of_int i) *. (y ** float_of_int j) :: acc)
    in
    Array.of_list (List.rev (aux 0 0 []))
  
  (* Construction de la matrice A *)
  method private construireMatriceA =
    let terms_count = (d + 1) * (d + 2) / 2 in
    let matrix = Array.make_matrix (List.length points) terms_count 0. in
    List.iteri (fun i point ->
      let line = self#calculer_ligne_matrice point in
      Array.iteri (fun j v -> matrix.(i).(j) <- v) line
    ) points;
    matrix
  
  (* Construction du vecteur b (ici zéros car non spécifié autrement) *)
  method private construireVecteurB =
    (* Création d'un tableau pour stocker les valeurs de 'y' pour chaque point *)
    let b_values = List.map (fun pt -> pt#get_y) points |> Array.of_list in
    (* Transformation du tableau en une matrice colonne (8x1 dans votre cas) *)
    Mat.of_array b_values 1 (Array.length b_values) |> Mat.transpose
  
   
  
  (* Résolution du système A^TAx = A^Tb *)
  method resoudreSysteme =
    let a = Mat.of_arrays (self#construireMatriceA) in
    let b = self#construireVecteurB in
    Printf.printf "Matrice a :\n";
      Owl.Mat.print a;
      Printf.printf "Vecteur b :\n";
      Owl.Mat.print b;
      
    (* Fill b with values *)
    
    let a = Mat.of_arrays (self#construireMatriceA) in
let b = self#construireVecteurB in

(* Imprimez les dimensions de 'a' et 'b' pour le débogage *)
Printf.printf "Dimensions de 'a': %d x %d\n" (Mat.row_num a) (Mat.col_num a);
Printf.printf "Dimensions de 'b': %d x %d\n" (Mat.row_num b) (Mat.col_num b);

let at = Mat.transpose a in
(* Imprimez les dimensions de 'at' pour le débogage *)
Printf.printf "Dimensions de 'at': %d x %d\n" (Mat.row_num at) (Mat.col_num at);

let ata = Mat.dot at a in
(* Imprimez les dimensions de 'ata' pour le débogage *)
Printf.printf "Dimensions de 'ata': %d x %d\n" (Mat.row_num ata) (Mat.col_num ata);

let atb = Mat.dot at b in
(* Imprimez les dimensions de 'atb' pour le débogage *)
Printf.printf "Dimensions de 'atb': %d x %d\n" (Mat.row_num atb) (Mat.col_num atb);



    let x = Linalg.D.linsolve ata atb in
    let coeffs = Mat.to_array x in
    let coeffs_list = Array.to_list coeffs in (* Separate step for clarity *)
    let coeffs_triplets = List.mapi (fun index coeff ->
      let i = index / (d + 1)in 
      let j = index mod (d + 1)in
      (i, j, coeff))
     coeffs_list 
    in
    coeffs_triplets
end
