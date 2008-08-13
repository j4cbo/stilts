smelt: ../util/stream.sml tinyxml/nqx.lex tinyxml/tinyxml.sml tinyxml/tinyxml_parser.sig tinyxml/tinyxml_parser.sml templateattr.sig templateattr.sml templategen.sig templategen.sml templatecompiler.sig templatecompiler.sml smelt.sml smelt-standalone.sml
	mllex tinyxml/nqx.lex
	mlton smelt.mlb

heap:
	ml-build smelt.cm Smelt.main heap

clean:
	rm -rf tinyxml/nqx.lex.sml smelt heap.* .cm tinyxml/.cm
