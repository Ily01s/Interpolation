(*main.ml*)


let d = 2;;  (* Par exemple, pour une courbe quadratique *)

let () =
  let points = Util.lire_points "data.txt" in
  let interpol = new Moindres_carres.interpoler points in
  let polynome = interpol#interpoler points in
  print_endline polynome#to_string
