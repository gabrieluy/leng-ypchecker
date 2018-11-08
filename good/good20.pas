program good20;

var
  b : bool;
  s : string;
  m, n : integer;

begin
  m := 20;
  n := 25;
  b := true;
  if b then s := "valid" else s := "invalid";
  repeat begin n := n - 1; m := m - 1 end until (n <= 0) and (m <= 0);
  while m < n do m := m + 1;
  if (m = 0) and (n = 0) then s := "complete" else s:= "incomplete";
end.
