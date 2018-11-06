program bad13 ;
var i : integer; z : bool;

  function b (y : integer) : integer;
  begin
    y := i;
    y := +i;
  end;

begin
  b(32);
end.
