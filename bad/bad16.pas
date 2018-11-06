program bad16 ;
var i : integer; y : integer;

  function b () : integer;
  begin
    y := i;
    y := +i;
  end;

begin
  i := b(32);
end.
