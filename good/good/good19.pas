program good19 ;
var aux, a, b : string;

  procedure proc (var x, y : string) ;
  begin
  	aux := x;
   	x := y;
   	y := x;
  end;

begin
  a := "pepe";
  b := "juan";
  proc(a, b);
end.
