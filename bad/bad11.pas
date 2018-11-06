program bad11 ;
var i : integer;

  procedure a (var x : integer) ;
  begin
    x := 3 + x;
  end;

begin
  a(3+4);
end.
