class polynome d coefficients_init =object
  val degree : int = d
  val coefficients : (int * int * float) list = coefficients_init
  
  
  method get_degree = degree
  method get_coefficients = coefficients
  
  
    (*afficher le polynome*)
  method to_string =
    let string_of_coeff (i, j, coeff) =
      let coeff_str = if coeff <> 1. then Printf.sprintf "%.2f" coeff else "" in
      match (i, j) with
      | (0, 0) -> Printf.sprintf "%.2f" coeff
      | (_, _) when i = 0 -> coeff_str ^ "y^" ^ string_of_int j
      | (_, _) when j = 0 -> coeff_str ^ "x^" ^ string_of_int i
      | _ -> coeff_str ^ "x^" ^ string_of_int i ^ "y^" ^ string_of_int j
    in
    let terms = List.map string_of_coeff coefficients in
    String.concat " + " terms

    method to_string_a =                (*test pour voir des resultat pendant la conception*)
      let coeffs_only = List.map (fun (_, _, coeff) -> Printf.sprintf "%.2f" coeff) coefficients in
      String.concat ", " coeffs_only
    
end
