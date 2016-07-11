%option noyywrap yylineno
%option noinput nounput

%{
#include "semantic.tab.h"
%}



letter [a-zA-Z]
digit [0-9]
chc    [[:print:]]{-}[\'\\]
chs    [[:print:]]{-}["]  

%%
"/*"[^*]*"*"+([^/*][^*]*"*"+)*"/"           /*found on stackoverflow!*/
[[:space:]]+                 
{digit}{digit}*    			 {yylval.iVal = atoi(yytext);}return INTCON;
\'{chc}\'					 {yylval.charVal = yytext[1];} return CHARCON;
\'[\\]n\' 					 {yylval.charVal = yytext[0];} return CHARCON;
\'[\\]0\' 					 {yylval.charVal = yytext[0];} return CHARCON;
\"{chs}*\"                   {yylval.strVal = strdup(yytext);}return STRINGCON;
[(]		return LEFTPAREN;
[)]		return RIGHTPAREN;
[\[]	return LEFTBRACK;
[\]]	return RIGHTBRACK;
[{]		return LEFTCURL;
[}]		return RIGHTCURL;
extern 	return EXTERN;
void	return VOID;
return	return RETURN;
while 	return WHILE;
for		return FOR;
[i][f]  return IF;
[\;]		return COLON;
[e][l][s][e] return ELSE;
[=][=]							return DEQ;
[!][=]							return NEQ;
[<][=]							return LTE;
[<]								return LT;	
[>][=]							return GTE;
[>]								return GT;
[&][&]							return AND;
[|][|]							return OR;
\=								return EQ;
\+								return yytext[0];
\-  							return yytext[0];
\*								return yytext[0];
\/								return yytext[0];
\,								return yytext[0];
[i][n][t] 						return INT;
[c][h][a][r] 					return CHAR;
{letter}[a-zA-Z0-9\_]*  		{yylval.idName = strdup(yytext);} 	return ID;				
.                               {fprintf(stderr, "Unknow token:%s", yytext);}
%%
