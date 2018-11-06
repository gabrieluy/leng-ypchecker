program good27;

var
   a : bool;

begin
   a := ('a' = 'b') and (3 = 2) and (3 = 3.2) and (3.2 = 3) and ("str" = "asd") and (true = false);
end.
