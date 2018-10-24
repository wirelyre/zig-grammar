%option noyywrap

%{
#include <stdio.h>
#include "parser.h"

void yyerror(YYLTYPE *locp, char const *fmt, ...);

#define YY_DECL int yylex(YYSTYPE *yylval, YYLTYPE *yylloc)

#define LOCATION_RESET(Loc)                    \
    (Loc).first_column = (Loc).first_line = 1; \
    (Loc).last_column =  (Loc).last_line = 1;

#define LOCATION_LINES(Loc, Num) \
    (Loc).last_column = 1;       \
    (Loc).last_line += Num;

#define LOCATION_STEP(Loc)                  \
    (Loc).first_column = (Loc).last_column; \
    (Loc).first_line = (Loc).last_line;

#define YY_USER_ACTION  yylloc->last_column += yyleng;
%}

%%
%{
    LOCATION_STEP (*yylloc);
%}
[ ]+       { LOCATION_STEP (*yylloc); }
"//"[^\n]* { LOCATION_STEP (*yylloc); }
\n+        { LOCATION_LINES (*yylloc, yyleng); LOCATION_STEP (*yylloc); }

"test"        { return Keyword_test; }
"pub"         { return Keyword_pub; }
"comptime"    { return Keyword_comptime; }
"extern"      { return Keyword_extern; }
"inline"      { return Keyword_inline; }
"export"      { return Keyword_export; }
"nakedcc"     { return Keyword_nakedcc; }
"stdcallcc"   { return Keyword_stdcallcc; }
"async"       { return Keyword_async; }
"use"         { return Keyword_use; }
"fn"          { return Keyword_fn; }
"noalias"     { return Keyword_noalias; }
"var"         { return Keyword_var; }
"const"       { return Keyword_const; }
"align"       { return Keyword_align; }
"section"     { return Keyword_section; }
"try"         { return Keyword_try; }
"return"      { return Keyword_return; }
"break"       { return Keyword_break; }
"cancel"      { return Keyword_cancel; }
"resume"      { return Keyword_resume; }
"if"          { return Keyword_if; }
"else"        { return Keyword_else; }
"defer"       { return Keyword_defer; }
"while"       { return Keyword_while; }
"for"         { return Keyword_for; }
"suspend"     { return Keyword_suspend; }
"orelse"      { return Keyword_orelse; }
"catch"       { return Keyword_catch; }
"volatile"    { return Keyword_volatile; }
"await"       { return Keyword_await; }
"switch"      { return Keyword_switch; }
"or"          { return Keyword_or; }
"and"         { return Keyword_and; }
"true"        { return Keyword_true; }
"false"       { return Keyword_false; }
"null"        { return Keyword_null; }
"undefined"   { return Keyword_undefined; }
"error"       { return Keyword_error; }
"errorset"    { return Keyword_errorset; }
"unreachable" { return Keyword_unreachable; }
"continue"    { return Keyword_continue; }
"asm"         { return Keyword_asm; }
"packed"      { return Keyword_packed; }
"struct"      { return Keyword_struct; }
"enum"        { return Keyword_enum; }
"union"       { return Keyword_union; }
"promise"     { return Keyword_promise; }
"errdefer"    { return Keyword_errdefer; }

"0b"[01]+("."[01]+|"."?[01]*[eE][+-]?[01]+)                             { return Float; }
"0o"[0-8]+("."[0-8]+|"."?[0-8]*[eE][+-]?[0-8]+)                         { return Float; }
"0x"[0-9a-fA-F]+("."[0-9a-fA-F]+|"."?[0-9a-fA-F]*[pP][+-]?[0-9a-fA-F]+) { return Float; }
[0-9]+("."[0-9]+|"."?[0-9]*[eE][+-]?[0-9]+)                             { return Float; }

"0b"[01]+        { return Integer; }
"0o"[0-8]+       { return Integer; }
"0x"[0-9a-fA-F]+ { return Integer; }
[0-9]+           { return Integer; }

"@"[_a-zA-Z][_a-zA-Z0-9]+                                                                 { return BuiltinIdentifier; }
[_a-zA-Z][_a-zA-Z0-9]*                                                                    { return Identifier; }
"@\""([^\\"\n]|"\\"([nr\\t'"]|"x"[0-9a-fA-F]{2}|u[0-9a-fA-F]{4}|U[0-9a-fA-F]{4}))*"\""    { return Identifier; }
"c"?"\""([^\\"\n]|"\\"([nr\\t'"]|"x"[0-9a-fA-F]{2}|u[0-9a-fA-F]{4}|U[0-9a-fA-F]{4}))*"\"" { return String; }

"c"?"\\\\"[^\n]*                         { return MultilineStringLine; }
"'"[^\\'\n]"'"                           { return Char; }
"'\\"[nr\\t'"]"'"                        { return Char; }
"'\\x"[0-9a-fA-F]{2}"'"                  { return Char; }
"'\\u"[0-9a-fA-F]{4}"'"                  { return Char; }
"'\\U"[0-9a-fA-F]{6}"'"                  { return Char; }

"..." { return Dot3; }
"*="  { return AsteriskEqual; }
"/="  { return SlashEqual; }
"%="  { return PercentEqual; }
"+="  { return PlusEqual; }
"-="  { return MinusEqual; }
"<<=" { return LArrow2Equal; }
">>=" { return RArrow2Equal; }
"->"  { return MinusRArrow; }
"&="  { return AmpersandEqual; }
"^="  { return CaretEqual; }
"|="  { return PipeEqual; }
"*%=" { return AsteriskPercentEqual; }
"+%=" { return PlusPercentEqual; }
"-%=" { return MinusPercentEqual; }
"<="  { return LArrowEqual; }
">="  { return RArrowEqual; }
"=>"  { return EqualRArrow; }
"=="  { return EqualEqual; }
"||"  { return Pipe2; }
"!="  { return ExclamationMarkEqual; }
"<<"  { return LArrow2; }
">>"  { return RArrow2; }
".*"  { return DotAsterisk; }
".?"  { return DotQuestionMark; }
"++"  { return Plus2; }
"+%"  { return PlusPercent; }
".."  { return Dot2; }
"-%"  { return MinusPercent; }
"*%"  { return AsteriskPercent; }
"**"  { return Asterisk2; }
"="   { return Equal; }
":"   { return Colon; }
";"   { return Semicolon; }
","   { return Comma; }
"!"   { return ExclamationMark; }
"."   { return Dot; }
"{"   { return LBrace; }
"}"   { return RBrace; }
"("   { return LParen; }
")"   { return RParen; }
"["   { return LBracket; }
"]"   { return RBracket; }
"<"   { return LArrow; }
">"   { return RArrow; }
"|"   { return Pipe; }
"*"   { return Asterisk; }
"-"   { return Minus; }
"+"   { return Plus; }
"~"   { return Tilde; }
"?"   { return QuestionMark; }
"^"   { return Caret; }
"&"   { return Ampersand; }
"/"   { return Slash; }
"%"   { return Percent; }
"@"   { return AtSign; }

<<EOF>> { return Eof; }
. {
    yyerror(yylloc, "invalid character: %c", *yytext);
    LOCATION_STEP (*yylloc);
    return InvalidToken;
}
