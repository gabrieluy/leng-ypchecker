program bad05;
var
  a : bool;

begin
	a := false;
	a := a and a;
	a := a + a;
end.
