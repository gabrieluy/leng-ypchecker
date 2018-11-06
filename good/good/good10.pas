program good10;
var
   a, b, c : bool;

begin
   a := true;
   b := false;

   if a > b then
   	a := a and b
   else
   	c := not a;
end.
