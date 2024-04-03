class analyseMoindresCarres points d = object (self)
  val mutable points = points  (* Liste d'instances de la classe `point` *)
  val d = d  (* Degré de la courbe algébrique *)

  method private construireMatriceA =
    let n = List.length points in
    let m = (* Calculez le nombre de colonnes basé sur d *) in
    let a = Mat.zeros n m in
    List.iteri (fun i p ->
      (* Remplissez la i-ème ligne de la matrice A ici en utilisant p#get_x et p#get_y *)
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
