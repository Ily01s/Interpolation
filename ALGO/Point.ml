(*Point.ml*)


class point x_init y_init =
object
  val mutable x : float = x_init
  val mutable y : float = y_init
  method get_x = x
  method get_y = y
  method to_string = "(" ^ string_of_float x ^ ", " ^ string_of_float y ^ ")"
end
