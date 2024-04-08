let lire_points fichier =
  let ic = open_in fichier in
  let rec aux acc =
    try
      let line = input_line ic in
      match String.split_on_char ',' line with
      | [x_str; y_str] ->
          let x = float_of_string x_str in
          let y = float_of_string y_str in
          aux (new point x y :: acc)
      | _ -> aux acc  (* Ignorer les lignes qui ne correspondent pas au format attendu *)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  aux []
;;
