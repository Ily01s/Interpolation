open Owl;;

class analyseMoindresCarres points d = object (self)
  val mutable points = points  (* Liste d'instances de la classe `point` *)
  val d = d  (* Degré de la courbe algébrique *)
  val mutable term_index_map = []  (* Ajout pour stocker la correspondance entre les termes et leur index *)

  method private calculer_ligne_matrice (p : Point.point) =
    let x = p#get_x in
    let y = p#get_y in
    term_index_map <- [];  (* Réinitialiser pour chaque nouvelle ligne/matricielle *)
    let terms = ref [] in
    for i = 0 to d do
      for j = 0 to d - i do
        let term = (x ** float_of_int i) *. (y ** float_of_int j) in
        (* Enregistrement de la correspondance entre les indices et les puissances i, j *)
        term_index_map <- (i, j) :: term_index_map;
        terms := term :: !terms;
      done;
    done;
    Array.of_list (List.rev !terms)

  method private construireMatriceA =
    let n = List.length points in
    let m = ((d+1)*(d+2))/2 in
    let a = Mat.zeros n m in
    List.iteri (fun i point ->
      let ligne = self#calculer_ligne_matrice point in
      Array.iteri (fun j valeur -> Mat.set a i j valeur) ligne
    ) points;
    a

  method private construireVecteurB =
    let n = List.length points in
    Mat.zeros n 1  (* Crée un vecteur de zéros de dimension n x 1 *)

  method resoudreSysteme =
    try
      let a = self#construireMatriceA in
      let b = self#construireVecteurB in
      Printf.printf "Matrice a :\n";
      Owl.Mat.print a;
      Printf.printf "Vecteur b :\n";
      Owl.Mat.print b;
      
      let x = Linalg.D.linsolve a b in
      let coeffs_array = Owl.Mat.to_array x in
      let coeffs = Array.to_list coeffs_array in
      (* Utilisation de term_index_map pour générer les triplets corrects *)
      let coeffs_triplets = List.mapi (fun index coeff ->
        let (i, j) = List.nth term_index_map index in
        (i, j, coeff)
      ) coeffs in
      coeffs_triplets
    with
    | exn -> failwith ("Erreur lors de la resolution du systeme : " ^ Printexc.to_string exn)
end
