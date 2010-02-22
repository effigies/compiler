module Test (samplel, sample, errl, debug) where

import Tape
import Defs ( Token )
import Compute ( Compute )

import Control.Monad.Writer (tell)

debug :: [Token] -> Compute ()
debug (t:ts) = tell ([show t],[])
{-
tellName :: Compute ()
tellName = do
		scope <- getScope
		tell [ show scope ]
-}
samplel :: [String]
samplel = [	"program example(input, output);",
		"var x: integer;",
		"var y: integer;",
		"var z: real;",
		"var a: array[0 .. 1] of integer;",
		"",
		"function gcd(a: integer; b: integer): integer;",
		"\tbegin",
		"\t\tif b = 0 ",
		"\t\tthen",
		"\t\t\tgcd := a",
		"\t\telse",
		"\t\t\tgcd := gcd(b, a mod b)",
		"\tend;",
		"",
		"function lcm(a: integer; b: integer): integer;",
		"\tvar c: integer;",
		"\tfunction useless: integer;",
		"\t\tbegin",
		"\t\tend;",
		"\tbegin",
		"\t\tc := gcd(a, b);",
		"\t\tlcm := a * b / c",
		"\tend;",
		"",
		"begin",
		"\tx := gcd(123,456);",
		"\ty := lcm(12,34);",
		"\tz := 3 * -1.123e45;",
		"",
		"\ti := 10;",
		"\twhile i > 0 do",
		"\t\tbegin",
		"\t\t\ti := i - 1",
		"\t\tend;",
		"",
		"\tif not (i = 0) then",
		"\t\ti := 1",
		"\telse",
		"\t\ti := 0;",
		"",
		"\ta[0] := 1;",
		"\ta[1] := a[0]",
		"end."]

errl :: [String]
errl = [	"program example(input, output);",
		"var x: integer;",
		"var y: integer;",
		"var z: real;",
		"var a: array[0 .. 3] of integer;",
		"var thisidisreallytoolong: integer;",
		"",
		"><",
		"",
		"function gcd(a: integer; b: integer): integer;",
		"\tbegin",
		"\t\tif b = 0 ",
		"\t\tthen",
		"\t\t\tgcd := a",
		"\t\telse",
		"\t\t\tgcd := gcd(b, a mod b)",
		"\tend;",
		"",
		"function lcm(a: integer; b: integer): integer;",
		"\tvar c: integer;",
		"\tfunction useless: integer;",
		"\t\tbegin",
		"\t\tend;",
		"\tbegin",
		"\t\tc := gcd(a, b);",
		"\t\tlcm := a * b / c",
		"\tend;",
		"",
		"begin",
		"\tx := gcd(123,456);",
		"\ty := lcm(12,34);",
		"\tz := 3 * -1.123e45;",
		"",
		"\ti := 10;",
		"\twhile i > 0 do",
		"\t\tbegin",
		"\t\t\ti := i - 1",
		"\t\tend;",
		"",
		"\tif not (i = 0) then",
		"\t\ti := 1",
		"\telse",
		"\t\ti := 0;",
		"",
		"\ta[0] := 112358132134;",
		"\ta[1] := 2718281828459e-12;",
		"\ta[2] := .314159265358979e1;",
		"\ta[3] := 1.23e456",
		"end."]

sample = unlines samplel

samplet = tapify sample

samplets = map (tapify' '\0') samplel


