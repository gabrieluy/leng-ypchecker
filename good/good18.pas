program good18;

var
	x, y, aux : char;
	a : integer;
	b : real;

procedure random1();
var
	aux : integer;
begin
	aux := 125;
	a := ((a * 2) + aux) mod 3;
	b := (b + a) / 1;
end;

procedure random2(var m, n : char);
begin
	aux := m;
	m := n;
	n := aux;
end;

begin
	random1;
	random2(x, y);
end.
