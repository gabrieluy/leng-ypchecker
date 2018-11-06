program bad18;

function returnsDoubleOf(n : integer) : integer;
begin
	returnsDoubleOf := n * 2;
end;

function returns2() : integer;
begin
	returns2 := 2;
end;

begin
	returnsDoubleOf(returns2(12121))
end.
