program example(input, output);
var x: integer;
var y: integer;
var z: real;
var a: array[0 .. 3] of integer;
var thisidisreallytoolong: integer;

><

function gcd(a: integer; b: integer): integer;
	begin
		if b = 0 
		then
			gcd := a
		else
			gcd := gcd(b, a mod b)
	end;

function lcm(a: integer; b: integer): integer;
	var c: integer;
	function useless: integer;
		begin
		end;
	begin
		c := gcd(a, b);
		lcm := a * b / c
	end;

begin
	x := gcd(123,456);
	y := lcm(12,34);
	z := 3 * -1.123e45;

	i := 10;
	while i > 0 do
		begin
			i := i - 1
		end;

	if not (i = 0) then
		i := 1
	else
		i := 0;

	a[0] := 112358132134;
	a[1] := 2718281828459e-12;
	a[2] := .314159265358979e1;
	a[3] := 1.23e456
end.