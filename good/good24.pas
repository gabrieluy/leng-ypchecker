program good24;

procedure dummy(n : integer);
begin
end;

function returnsDoubleOf(n : integer) : integer;
begin
	returnsDoubleOf := n * 2;
end;

function returns2() : integer;
begin
	returns2 := 2;
end;

begin
	if (true = false) then
		dummy(returnsDoubleOf(returns2()))
	else
		dummy(returnsDoubleOf(125));
end.
