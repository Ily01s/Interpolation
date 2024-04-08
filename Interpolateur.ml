class interpolateur points degree =
object (self)
  val mutable points : (float * float) array = points
  val mutable degree : int = degree

  method private construct_matrix =
    let n = Array.length points in
    let terms_count = (degree + 1) * (degree + 2) / 2 in
    let matrix = Array.make_matrix n terms_count 0. in
    for i = 0 to n - 1 do
      let x, y = points.(i) in
      let index = ref 0 in
      for a = 0 to degree do
        for b = 0 to degree - a do
          matrix.(i).(!index) <- (x ** float_of_int a) *. (y ** float_of_int b);
          incr index
        done
      done
    done;
    matrix

  method private construct_zero_vector =
    let n = Array.length points in
    Array.make n 0.0

  method private solve_system matrix zero_vector =
    let module M = Lacaml.D in
    let a = M.Mat.of_array matrix in
    let b = M.Vec.of_array zero_vector in
    let at = M.lacpy ~trans:`T a in
    let ata = M.gemm ~transa:`T a ~b:a () in
    let atb = M.gemv ~trans:`T a ~y:b () in
    let x = M.gesv ata atb in
    M.Vec.to_array x

  method generate_polynome =
    let matrix = self#construct_matrix in
    let zero_vector = self#construct_zero_vector in
    let coefficients = self#solve_system matrix zero_vector in
    (* Supposant que `polynome` est défini pour prendre un degré et un tableau de coefficients *)
    new polynome degree coefficients
end
