datatype lexresult =
	  DATA of string
	| BEGINTAG of string
	| TAGEND
	| TAGSELFCLOSE
	| ATTRIB of string
	| ATTRIBVALUE of string
	| CLOSETAG of string
	| PI of string
	| EOF

fun eof () = EOF

%%

%s MAIN TAG TAGBEGIN ATTNAME ATTVALUE DQUOT SQUOT CTAGBEGIN PIBEGIN PIDATA PIEND ;

space = ([\t\n] | " "); 
identchar =  [A-Za-z_0-9:-];
pcchar = [^<>];

%%

<INITIAL>	{space}*		=>	(YYBEGIN MAIN; lex());
<MAIN>		{pcchar}+		=>	(DATA yytext);
<MAIN>		"<"			=>	( YYBEGIN TAGBEGIN; continue() );
<MAIN>		"</"			=>	( YYBEGIN CTAGBEGIN; continue() );
<MAIN>		"<?"			=>	( YYBEGIN PIBEGIN; continue() );

<TAGBEGIN>	{identchar}+		=>	( YYBEGIN TAG; BEGINTAG yytext);

<TAG>		{space}+		=>	( continue() );
<TAG>		{identchar}+		=>	( YYBEGIN ATTNAME; ATTRIB yytext);
<TAG>		">"			=>	( YYBEGIN MAIN; TAGEND );
<TAG>		"/>"			=>	( YYBEGIN MAIN; TAGSELFCLOSE );

<ATTNAME>	"="			=>	( YYBEGIN ATTVALUE; continue () );
<ATTVALUE>	"\""			=>	( YYBEGIN DQUOT; continue () );
<ATTVALUE>	"'"			=>	( YYBEGIN SQUOT; continue () );
<DQUOT>		[^"]*			=>	( ATTRIBVALUE yytext );
<DQUOT>		"\""			=>	( YYBEGIN TAG; continue() );
<SQUOT>		[^']*			=>	( ATTRIBVALUE yytext );
<SQUOT>		"'"			=>	( YYBEGIN TAG; continue() );

<CTAGBEGIN>	{identchar}+		=>	( YYBEGIN TAG; CLOSETAG yytext );

<PIBEGIN>	{identchar}+		=>	( YYBEGIN PIDATA; PI yytext );
<PIDATA>	{space}			=>	( continue() );
<PIDATA>	[^\ ] [^?]* 		=>	( YYBEGIN PIEND; DATA yytext (* XXX fix this *) );
<PIEND>		"?>"			=>	( YYBEGIN MAIN; continue() );
