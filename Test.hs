module Test (samplel,out, errl) where

import Tape
import Token
import Defs
import PreProcess

samplel :: [String]
samplel = [	"program example(input, output);",
		"var x: integer;",
		"var y: integer;",
		"function gcd(a: integer; b: integer): integer;",
		"begin",
		"\tif b = 0 then gcd := a",
		"\telse gcd := gcd(b, a mod b)",
		"end;",
		"",
		"begin",
		"\tj := read(x, y);",
		"\th := write(gcd(x, y))",
		"end."]

errl :: [String]
errl = [	"program exampleriffic(input, output);",
		"var x; integer;",
		"vary y: integer;",
		"function gcd(a: integer; b: integer): integer;",
		"begin",
		"\tif b = 1234567890123456 then gcd := a",
		"\telse gcd := gcd(b, a mod b)",
		"end;",
		"",
		"begin",
		"\tx := 0.123e15;",
		"\ty := .0e0",
		"end."]

sample = unlines samplel

samplet = tapify sample

samplets = map (tapify' '\0') samplel

out = preprocess reserved (tokenize samplet)