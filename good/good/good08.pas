program good08;
var
  a, b, c, d : integer;
  e: real;

begin
   a := 20;
   b := 10;
   c := 15;
   d := 5;
   e := (a + b) * c / d;      (* ( 30 * 15 ) / 5 *)

   e := ((a + b) * c) / d;    (* (30 * 15 ) / 5  *)

   e := (a + b) * (c / d);   (*  (30) * (15/5)  *)

   e := a + (b * c) / d;     (*  20 + (150/5)  *)
end.
