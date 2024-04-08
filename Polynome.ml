class polynome d =
object (self)
  val degree : int = d
  val mutable coefficients : (int * int * float) list = []
  method get_degree = degree
  method get_coefficients = coefficients
  method add_coefficient i j coeff = coefficients <- (i, j, coeff) :: coefficients
  method evaluate (p : point) =  
    List.fold_left (fun acc (i, j, coeff) ->
    acc +. coeff *. (p#get_x ** float_of_int i) *. (p#get_y ** float_of_int j)
  ) 0.0 coefficients

  
  method to_string = 
    let string_of_coeff (i, j, coeff) =
      Printf.sprintf "%.2fx^%dy^%d" coeff i j
    in
    let terms = List.map string_of_coeff coefficients in
    String.concat " + " terms
end
