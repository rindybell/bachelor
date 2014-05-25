ocamlyacc Parser.mly
ocamllex Lexer.mll
ocamlyacc FormatParser.mly
ocamllex FormatLexer.mll
ocamlc Fundamental.ml Environment.ml Type.ml Parser.mli Parser.ml Lexer.ml FormatParser.mli FormatParser.ml FormatLexer.ml Debug.ml Toptree.ml PrimitiveFunction.ml Converter.ml Functions.ml Eval.ml Main.ml -o main