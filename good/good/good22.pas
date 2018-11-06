program good22;
var i : integer; z : bool;

  procedure a (var x : bool) ;
  begin
    x := true and x;
  end;

  function b (y : integer) : integer;
  begin
    y := i;
    y := +i;
  end;

begin
  z := false;
  a(z);
  i := b(32);
end.
