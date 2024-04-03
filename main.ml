let lire_points fichier =
  (* Même fonction que précédemment, mais retourne une liste d'instances de `point` *)
  []

let () =
  let points = lire_points "chemin/vers/votre/fichier.csv" in
  let d = (* Définissez le degré de votre courbe ici *) in
  let analyse = new analyseMoindresCarres points d in
  let coefficients = analyse#resoudreSysteme in
  (* Utilisez `coefficients` ici, par exemple pour les imprimer *)
