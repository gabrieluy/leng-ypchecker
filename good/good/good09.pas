program good09;
var
   a, b, c : bool;

begin
   a := true;
   b := false;

   c := a and b;       (* 12 = 0000 1100 *)

   c := a or b;       (* 61 = 0011 1101 *)

   c := not a;          (* -61 = 1100 0011 *)

   c := 31.0 < 2;     (* 240 = 1111 0000 *)

   c := 2 > 2 * 3.0;     (* 15 = 0000 1111 *)
end.
