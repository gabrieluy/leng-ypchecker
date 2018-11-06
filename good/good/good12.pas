program good12;

var
	lines : string;
	beginning : string;
	continuation : string;
begin
	beginning := "We play it again ";
	continuation := "and again ";
	lines := beginning;

	repeat
		lines := continuation
	until false;
end.
