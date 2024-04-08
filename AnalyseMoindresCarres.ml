(*analyseMoindresCarres.ml*)
open Owl;;


class analyseMoindresCarres points d = object (self)
  val mutable points = points  (* Liste d'instances de la classe `point` *)
  val d = d  (* Degré de la courbe algébrique *)

  method private calculer_ligne_matrice (point : point) =
    let x = point#get_x in
    let y = point#get_y in
    let rec aux i j acc =
      if i > d then acc  (* Si i dépasse le degré, retournez l'accumulateur *)
      else if i + j > d then aux (i + 1) 0 acc  (* Si i + j dépasse le degré, passez au prochain i *)
      else aux i (j + 1) ((x ** float_of_int i) *. (y ** float_of_int j) :: acc)  (* Calculez le terme et ajoutez-le à l'accumulateur *)
    in
    Array.of_list (List.rev (aux 0 0 []))  (* Convertissez la liste finale en un tableau et inversez-la pour obtenir l'ordre correct *)

  method private construireMatriceA =
    let n = List.length points in
    let m =((d+1)*(d+1))/2 in
    let a = Mat.zeros n m in
    List.iteri (fun i point ->
      let ligne = calculer_ligne_matrice point d in
      Mat.set_row a i ligne  (* Remplissez la i-ème ligne de la matrice A avec les valeurs calculées *)
    ) points;
    a

  method private construireVecteurB =
    let n = List.length points in
    Mat.zeros n 1

  method resoudreSysteme =
    let a = self#construireMatriceA in
    let b = self#construireVecteurB in
    Linalg.D.linsolve a b
end
