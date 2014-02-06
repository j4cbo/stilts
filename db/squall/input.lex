structure Tokens = Tokens
type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token

open Tokens

val lineNum = ref 0
fun lines n = lineNum := (!lineNum + n)
fun eof () = EOF (!lineNum, !lineNum)

%%

%header (functor SquallLexFun(structure Tokens : Squall_TOKENS));

%s HEADER SQL ;

whitespace = [\ \t];
token = [a-zA-Z_'];
sqlline = ([^-\n] .* "\n")
        | ("-" ([^-\n] .* "\n" | "\n"))
        | ("--" ([^-\n] .* "\n" | "\n"));

%%

<INITIAL>	^ "#" .*	=>	( lines 1; continue () );
<INITIAL>	^ "---" 	=>	( YYBEGIN HEADER; lex () );
<INITIAL>	^ "engine"	=>	( YYBEGIN HEADER; ENGINE (!lineNum, !lineNum) );

<HEADER>	"*"		=>	( STAR (!lineNum, !lineNum) );
<HEADER>	"list"		=>	( LIST (!lineNum, !lineNum) );
<HEADER>	"fold"		=>	( FOLD (!lineNum, !lineNum) );
<HEADER>	"option"	=>	( OPTION (!lineNum, !lineNum) );
<HEADER>	"array"		=>	( ARRAY (!lineNum, !lineNum) );
<HEADER>	"vector"	=>	( VECTOR (!lineNum, !lineNum) );
<HEADER>	"unit"		=>	( UNIT (!lineNum, !lineNum) );
<HEADER>	"insert_id"	=>	( INSERT_ID (!lineNum, !lineNum) );
<HEADER>	"affected_rows"	=>	( AFFECTED_ROWS (!lineNum, !lineNum) );
<HEADER>	"->"		=>	( ARROW (!lineNum, !lineNum) );
<HEADER>	"->"		=>	( ARROW (!lineNum, !lineNum) );
<HEADER>	":"		=>	( COLON (!lineNum, !lineNum) );
<HEADER>	"{"		=>	( LBRACE (!lineNum, !lineNum) );
<HEADER>	"}"		=>	( RBRACE (!lineNum, !lineNum) );
<HEADER>	"("		=>	( LPAREN (!lineNum, !lineNum) );
<HEADER>	")"		=>	( RPAREN (!lineNum, !lineNum) );
<HEADER>	","		=>	( COMMA (!lineNum, !lineNum) );
<HEADER>	"\n"		=>	( lines 1; YYBEGIN SQL; continue () );
<HEADER>	{token}+	=>	( TOKEN (yytext, !lineNum, !lineNum) );
<HEADER>	{whitespace}+ 	=>	( lex () );

<SQL>		^ "#" .* "\n"	=>	( lines 1; continue () );
<SQL>		^ "---" 	=>	( YYBEGIN HEADER; lex () );
<SQL>		^ {sqlline}	=>	( lines 1; SQLDATA (yytext, !lineNum, !lineNum) );
<SQL>		"\n"		=>	( lines 1; lex () );
