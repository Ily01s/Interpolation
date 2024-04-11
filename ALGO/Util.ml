let lire_points fichier =
  let ic = open_in fichier in
  let current_line = ref "" in (* Référence mutable pour stocker la ligne actuelle *)
  let rec aux acc line_number =
    try
      current_line := input_line ic; (* Mettre à jour la référence avec la ligne actuelle *)
      match String.split_on_char ',' (String.trim !current_line) with
      | [x_str; y_str] ->
          let x = float_of_string x_str in
          let y = float_of_string y_str in
          aux (new Point.point x y :: acc) (line_number + 1)
      | _ -> failwith (Printf.sprintf "Format invalide à la ligne %d: %s" line_number !current_line)
    with
    | End_of_file ->
        close_in ic;
        List.rev acc
    | Failure err ->
        close_in_noerr ic;
        failwith (Printf.sprintf "Erreur de lecture du fichier à la ligne %d: %s - %s" line_number !current_line err)
    | exn ->
        close_in_noerr ic;
        raise exn
  in
  aux [] 1
