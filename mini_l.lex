%{
    #include "y.tab.h"
    int yyerror(char *s);
    int currLine = 1;
    int currPos = 1;
%}

DIGIT   [0-9]
UPPERCASE [A-Z]
LOWERCASE [a-z]
ALLCASE  [a-zA-Z]
US   [_]
TAB   [ \t]

%%
"-"                {currPos += yyleng; return SUB;}
"*"                {currPos += yyleng; return MULT;}
"/"                {currPos += yyleng; return DIV;}
"%"                {currPos += yyleng; return MOD;}
"+"                {currPos += yyleng; return ADD;}
"not"              {currPos += yyleng; return NOT;}
"and"              {currPos += yyleng; return AND;}
"or"               {currPos += yyleng; return OR;}
"<"                {currPos += yyleng; return LT;}
"<="               {currPos += yyleng; return LTE;}
">"                {currPos += yyleng; return GT;}
">="               {currPos += yyleng; return GTE;}
"=="               {currPos += yyleng; return EQ;}
"<>"               {currPos += yyleng; return NEQ;}
"("                {currPos += yyleng; return L_PAREN;}
")"                {currPos += yyleng; return R_PAREN;}
"["                {currPos += yyleng; return L_BRACKET;}
"]"                {currPos += yyleng; return R_BRACKET;}
":="               {currPos += yyleng; return ASSIGN;}
"program"          {currPos += yyleng; return PROGRAM;}
"beginprogram"     {currPos += yyleng; return BEGIN_PROGRAM;}
"endprogram"       {currPos += yyleng; return END_PROGRAM;}
"integer"          {currPos += yyleng; return INTEGER;}
"array"            {currPos += yyleng; return ARRAY;}
"of"               {currPos += yyleng; return OF;}
"if"               {currPos += yyleng; return IF;}
"then"             {currPos += yyleng; return THEN;}
"endif"            {currPos += yyleng; return ENDIF;}
"elseif"           {currPos += yyleng; return ELSEIF;}
"else"             {currPos += yyleng; return ELSE;}
"while"            {currPos += yyleng; return WHILE;}
"do"               {currPos += yyleng; return DO;}
"beginloop"        {currPos += yyleng; return BEGINLOOP;}
"endloop"          {currPos += yyleng; return ENDLOOP;}
"continue"         {currPos += yyleng; return CONTINUE;}
"read"             {currPos += yyleng; return READ;}
"write"            {currPos += yyleng; return WRITE;}
"true"             {currPos += yyleng; return TRUE;}
"false"            {currPos += yyleng; return FALSE;}
":"                {currPos += yyleng; return COLON;}
";"                {currPos += yyleng; return SEMICOLON;}
","                {currPos += yyleng; return COMMA;}
\n                 {++currLine; currPos = 1;}
##.*$              {}



{DIGIT}+           {currPos += yyleng; yylval.ival = yytext; return NUMBER;}
{TAB}+             {currPos += yyleng;}
{ALLCASE}+(({US}|{DIGIT}|{ALLCASE})*({DIGIT}|{ALLCASE})+)* {yylval.sval = yytext; currPos += yyleng; return IDENT;}
({DIGIT}|{US})+({US}|{DIGIT}|{ALLCASE})*{ALLCASE}+         {printf("Error at line %i, currPos %i: identifier \"%s\" must begin with a letter\n", currLine, currPos, yytext); return -1;}
{ALLCASE}+({US}|{DIGIT}|{ALLCASE})*{US}+                   {printf("Error at line %i, currPos %i: identifier \"%s\" cannot end with an underscore\n", currLine, currPos, yytext); return -1;}
.                                                          {printf("Error at line %i unrecognized symbol \"%s\"\n",currLine, yytext); return -1;}
%%
