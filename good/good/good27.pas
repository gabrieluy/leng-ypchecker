program good27;

var
   a : integer;
   b : char;

function a() : string;
begin
   a := "a";
end;

procedure b(a : string);
begin
end;

begin
   b(a());
end.
