Root <- skip ContainerMembers eof

# *** Top level ***
ContainerMembers
    <- TestDecl ContainerMembers
     / TopLevelComptime ContainerMembers
     / KEYWORD_pub? TopLevelDecl ContainerMembers
     / KEYWORD_pub? ContainerField COMMA ContainerMembers
     / KEYWORD_pub? ContainerField
     /

TestDecl <- KEYWORD_test STRINGLITERAL Block

TopLevelComptime <- KEYWORD_comptime BlockExpr

TopLevelDecl
    <- FnDef
     / FnProto SEMICOLON
     / KEYWORD_extern STRINGLITERAL? FnProto SEMICOLON
     / VarDecl
     / KEYWORD_export VarDecl
     / KEYWORD_extern STRINGLITERAL? VarDecl
     / KEYWORD_use Expr SEMICOLON

FnDef <- (KEYWORD_inline / KEYWORD_export)? FnProto Block

FnProto <- FnCC? KEYWORD_fn IDENTIFIER? LPAREN ParamDecls RPAREN ByteAlign? Section? EXCLAMATIONMARK? ReturnType

VarDecl <- (KEYWORD_const / KEYWORD_var) IDENTIFIER (COLON TypeExpr)? ByteAlign? Section? (EQUAL Expr)? SEMICOLON

ContainerField <- IDENTIFIER (COLON TypeExpr)? (EQUAL Expr)?

# *** Block Level ***
Statement
    <- KEYWORD_comptime? VarDecl
     / KEYWORD_defer BlockExprStatement
     / KEYWORD_errdefer BlockExprStatement
     / KEYWORD_comptime BlockExprStatement
     / KEYWORD_suspend SEMICOLON
     / KEYWORD_suspend BlockExprStatement
     / IfStatement
     / WhileStatement
     / ForStatement
     / SwitchExpr
     / BlockExprStatement

IfStatement
    <- KEYWORD_if GroupedExpr PtrPayload? BlockOrExpr KEYWORD_else Payload? Statement
     / KEYWORD_if GroupedExpr PtrPayload? BlockExprStatement

WhileStatement
    <- BlockLabel? KEYWORD_inline? KEYWORD_while GroupedExpr PtrPayload? WhileContinueExpr? BlockOrExpr KEYWORD_else Payload? Statement
     / BlockLabel? KEYWORD_inline? KEYWORD_while GroupedExpr PtrPayload? WhileContinueExpr? BlockExprStatement

ForStatement
    <- BlockLabel? KEYWORD_inline? KEYWORD_for GroupedExpr PtrIndexPayload? BlockOrExpr KEYWORD_else Statement
     / BlockLabel? KEYWORD_inline? KEYWORD_for GroupedExpr PtrIndexPayload? BlockExprStatement

BlockExprStatement
    <- BlockExpr
     / AssignExpr SEMICOLON

BlockOrExpr
    <- BlockExpr
     / AssignExpr

# *** Expression Level ***
AssignExpr <- Expr (AssignOp Expr)?

Expr <- BoolAndExpr (KEYWORD_or BoolAndExpr)*

BoolAndExpr <- CompareExpr (KEYWORD_and CompareExpr)*

CompareExpr <- BitwiseExpr (CompareOp BitwiseExpr)?

BitwiseExpr <- BitShiftExpr (BitwiseOp BitShiftExpr)*

BitShiftExpr <- AdditionExpr (BitShiftOp AdditionExpr)*

AdditionExpr <- MultiplyExpr (AdditionOp MultiplyExpr)*

MultiplyExpr <- CurlySuffixExpr (MultiplyOp CurlySuffixExpr)*

CurlySuffixExpr <- TypeExpr (LBRACE InitList RBRACE)?

InitList
    <- Expr (COMMA Expr)* COMMA?
     / FieldInit (COMMA FieldInit)* COMMA?
     /

TypeExpr <- PrefixExpr (EXCLAMATIONMARK PrefixExpr)?

PrefixExpr
    <- PrefixOp PrefixExpr
     / SuffixExpr

SuffixExpr
    <- AsyncPrefix PrimaryExpr SuffixOp* FnCallArgumnets
     / PrimaryExpr (SuffixOp / FnCallArgumnets)*

PrimaryExpr
    <- AsmExpr
     / BlockExpr
     / ContainerDecl
     / ErrorSetDecl
     / FnProto
     / ForExpr
     / GroupedExpr
     / IfExpr
     / KEYWORD_break BreakLabel? Expr?
     / KEYWORD_cancel Expr
     / KEYWORD_comptime Expr
     / KEYWORD_continue BreakLabel?
     / KEYWORD_resume Expr
     / KEYWORD_return Expr?
     / SwitchExpr
     / WhileExpr
     / BUILTININDENTIFIER FnCallArgumnets
     / CHAR_LITERAL
     / STRINGLITERAL
     / IDENTIFIER
     / KEYWORD_anyerror
     / KEYWORD_error DOT IDENTIFIER
     / KEYWORD_false
     / KEYWORD_null
     / KEYWORD_promise
     / KEYWORD_true
     / KEYWORD_undefined
     / KEYWORD_unreachable
     / FLOAT
     / INTEGER

BlockExpr <- BlockLabel? Block

Block <- LBRACE Statement* RBRACE

ContainerDecl <- (KEYWORD_extern / KEYWORD_packed)? ContainerDeclAuto

ErrorSetDecl <- KEYWORD_error LBRACE (IDENTIFIER (COMMA IDENTIFIER)* COMMA?)? RBRACE

ForExpr <- BlockLabel? KEYWORD_inline? KEYWORD_for GroupedExpr PtrIndexPayload? Expr (KEYWORD_else Payload? Expr)?

GroupedExpr <- LPAREN Expr RPAREN

IfExpr <- KEYWORD_if GroupedExpr PtrPayload? Expr (KEYWORD_else Payload? Expr)?

SwitchExpr <- KEYWORD_switch GroupedExpr LBRACE (SwitchProng (COMMA SwitchProng)* COMMA?)? RBRACE

WhileExpr <- BlockLabel? KEYWORD_inline? KEYWORD_while GroupedExpr PtrPayload? WhileContinueExpr? Expr (KEYWORD_else Payload? Expr)?

# *** Assembly ***
AsmExpr <- KEYWORD_asm KEYWORD_volatile? LPAREN STRINGLITERAL AsmOutput? RPAREN

AsmOutput <- COLON (AsmOutputItem (COMMA AsmOutputItem)* COMMA?)? AsmInput?

AsmOutputItem <- LBRACKET IDENTIFIER RBRACKET STRINGLITERAL LPAREN (MINUSRARROW TypeExpr / IDENTIFIER) RPAREN

AsmInput <- COLON (AsmInputItem (COMMA AsmInputItem)* COMMA?)? AsmCloppers?

AsmInputItem <- LBRACKET IDENTIFIER RBRACKET STRINGLITERAL LPAREN Expr RPAREN

AsmCloppers <- COLON (STRINGLITERAL (COMMA STRINGLITERAL)* COMMA?)?

# *** Helper grammar ***
BreakLabel <- COLON IDENTIFIER

BlockLabel <- IDENTIFIER COLON

FieldInit <- DOT IDENTIFIER EQUAL Expr

WhileContinueExpr <- COLON LPAREN AssignExpr RPAREN

Section <- KEYWORD_section GroupedExpr

# Fn specific
FnCC
    <- KEYWORD_nakedcc
     / KEYWORD_stdcallcc
     / KEYWORD_extern
     / KEYWORD_async (LARROW TypeExpr RARROW)?

ParamDecls <- (ParamDecl (COMMA ParamDecl)* COMMA?)?

ParamDecl <- (KEYWORD_noalias / KEYWORD_comptime)? (IDENTIFIER COLON)? ParamType

ParamType
    <- KEYWORD_var
     / DOT3
     / TypeExpr

ReturnType
    <- KEYWORD_var
     / TypeExpr

# Payloads
Payload <- PIPE IDENTIFIER PIPE

PtrPayload
    <- PIPE ASTERISK IDENTIFIER PIPE
     / Payload

PtrIndexPayload
    <- PIPE ASTERISK IDENTIFIER COMMA IDENTIFIER PIPE
     / PIPE IDENTIFIER COMMA IDENTIFIER PIPE
     / PtrPayload


# Switch specific
SwitchProng <- SwitchCase EQUALRARROW PtrPayload? AssignExpr

SwitchCase
    <- SwitchItem (COMMA SwitchItem)* COMMA?
     / KEYWORD_else

SwitchItem
    <- Expr DOT3 Expr
     / Expr

# Operators
AssignOp
    <- ASTERISKEQUAL
     / SLASHEQUAL
     / PERCENTEQUAL
     / PLUSEQUAL
     / MINUSEQUAL
     / LARROW2EQUAL
     / RARROW2EQUAL
     / AMPERSANDEQUAL
     / CARETEQUAL
     / PIPEEQUAL
     / ASTERISKPERCENTEQUAL
     / PLUSPERCENTEQUAL
     / MINUSPERCENTEQUAL
     / EQUAL

CompareOp
    <- EQUALEQUAL
     / EXCLAMATIONMARKEQUAL
     / LARROW
     / RARROW
     / LARROWEQUAL
     / RARROWEQUAL

BitwiseOp
    <- AMPERSAND
     / CARET
     / PIPE
     / KEYWORD_orelse
     / KEYWORD_catch Payload?

BitShiftOp
    <- LARROW2
     / RARROW2

AdditionOp
    <- PLUS
     / MINUS
     / PLUS2
     / PLUSPERCENT
     / MINUSPERCENT

MultiplyOp
    <- PIPE2
     / ASTERISK
     / SLASH
     / PERCENT
     / ASTERISK2
     / ASTERISKPERCENT

PrefixOp
    <- EXCLAMATIONMARK
     / MINUS
     / TILDE
     / MINUSPERCENT
     / AMPERSAND
     / QUESTIONMARK
     / KEYWORD_try
     / KEYWORD_await
     / KEYWORD_promise MINUSRARROW
     / PtrStart PtrAttribute*

SuffixOp
    <- LBRACKET Expr DOT2 Expr RBRACKET
     / LBRACKET Expr DOT2 RBRACKET
     / LBRACKET Expr RBRACKET
     / DOT IDENTIFIER
     / DOTASTERISK
     / DOTQUESTIONMARK

AsyncPrefix
    <- KEYWORD_async LARROW TypeExpr RARROW
     / KEYWORD_async

FnCallArgumnets <- LPAREN (Expr (COMMA Expr)* COMMA?)? RPAREN

# Ptr specific
PtrStart
    <- ASTERISK
     / ASTERISK2
     / LBRACKET RBRACKET
     / LBRACKET ASTERISK RBRACKET
     / LBRACKET Expr RBRACKET

PtrAttribute
    <- BitAlign
     / ByteAlign
     / KEYWORD_const
     / KEYWORD_volatile


# ContainerDecl specific
ContainerDeclAuto <- ContainerDeclType LBRACE ContainerMembers RBRACE

ContainerDeclType
    <- KEYWORD_struct GroupedExpr?
     / KEYWORD_union LPAREN KEYWORD_enum GroupedExpr? RPAREN
     / KEYWORD_union GroupedExpr?
     / KEYWORD_enum GroupedExpr?

# Alignment
ByteAlign <- KEYWORD_align GroupedExpr

BitAlign <- KEYWORD_align LPAREN Expr COLON INTEGER COLON INTEGER RPAREN


# *** Tokens ***
eof <- !.
hex <- [0-9a-fA-F]
char_escape
    <- "\\x" hex hex
     / "\\u" hex hex hex hex
     / "\\U" hex hex hex hex hex hex
     / "\\" [nr\\t'"]
char_char
    <- char_escape
     / [^\\'\n]
string_char
    <- char_escape
     / [^\\"\n]

line_comment <- '//'[^\n]*
line_string <- ("\\\\" [^\n]* [ \n]*)+
line_cstring <- ("c\\\\" [^\n]* [ \n]*)+
skip <- ([ \n] / line_comment)*

CHAR_LITERAL <- "'" char_char "'" skip
FLOAT
    <- "0b" [01]+  "." [01]+  ([eE] [-+]? [01]+)?  skip
     / "0o" [0-7]+ "." [0-7]+ ([eE] [-+]? [0-7]+)? skip
     / "0x" hex+   "." hex+   ([pP] [-+]? hex+)?   skip
     /      [0-9]+ "." [0-9]+ ([eE] [-+]? [0-9]+)? skip
     / "0b" [01]+  "."? [eE] [-+]? [01]+  skip
     / "0o" [0-7]+ "."? [eE] [-+]? [0-7]+ skip
     / "0x" hex+   "."? [pP] [-+]? hex+   skip
     /      [0-9]+ "."? [eE] [-+]? [0-9]+ skip
INTEGER
    <- "0b" [01]+  skip
     / "0o" [0-7]+ skip
     / "0x" hex+   skip
     /      [0-9]+ skip
STRINGLITERAL
    <- "c"? "\"" string_char* "\"" skip
     / line_string                 skip
     / line_cstring                skip
IDENTIFIER
    <- !keyword ("c" !"\"\\" / [A-Zabd-z_]) [A-Za-z0-9_]* skip
     / "@\"" string_char* "\""                            skip
BUILTININDENTIFIER <- "@"[A-Za-z_][A-Za-z0-9_]* skip


AMPERSAND            <- '&'      ![=]      skip
AMPERSANDEQUAL       <- '&='               skip
ASTERISK             <- '*'      ![*%=]    skip
ASTERISK2            <- '**'               skip
ASTERISKEQUAL        <- '*='               skip
ASTERISKPERCENT      <- '*%'     ![=]      skip
ASTERISKPERCENTEQUAL <- '*%='              skip
CARET                <- '^'      ![=]      skip
CARETEQUAL           <- '^='               skip
COLON                <- ':'                skip
COMMA                <- ','                skip
DOT                  <- '.'      ![*.?]    skip
DOT2                 <- '..'     ![.]      skip
DOT3                 <- '...'              skip
DOTASTERISK          <- '.*'               skip
DOTQUESTIONMARK      <- '.?'               skip
EQUAL                <- '='      ![>=]     skip
EQUALEQUAL           <- '=='               skip
EQUALRARROW          <- '=>'     ![>]      skip
EXCLAMATIONMARK      <- '!'      ![=]      skip
EXCLAMATIONMARKEQUAL <- '!='               skip
LARROW               <- '<'      ![<=]     skip
LARROW2              <- '<<'     ![=]      skip
LARROW2EQUAL         <- '<<='              skip
LARROWEQUAL          <- '<='               skip
LBRACE               <- '{'                skip
LBRACKET             <- '['                skip
LPAREN               <- '('                skip
MINUS                <- '-'      ![%=>]    skip
MINUSEQUAL           <- '-='               skip
MINUSPERCENT         <- '-%'     ![=]      skip
MINUSPERCENTEQUAL    <- '-%='              skip
MINUSRARROW          <- '->'               skip
PERCENT              <- '%'      ![=]      skip
PERCENTEQUAL         <- '%='               skip
PIPE                 <- '|'      ![|=]     skip
PIPE2                <- '||'               skip
PIPEEQUAL            <- '|='               skip
PLUS                 <- '+'      ![%+=]    skip
PLUS2                <- '++'               skip
PLUSEQUAL            <- '+='               skip
PLUSPERCENT          <- '+%'     ![=]      skip
PLUSPERCENTEQUAL     <- '+%='              skip
QUESTIONMARK         <- '?'                skip
RARROW               <- '>'      ![>=]     skip
RARROW2              <- '>>'     ![=]      skip
RARROW2EQUAL         <- '>>='              skip
RARROWEQUAL          <- '>='               skip
RBRACE               <- '}'                skip
RBRACKET             <- ']'                skip
RPAREN               <- ')'                skip
SEMICOLON            <- ';'                skip
SLASH                <- '/'      ![=]      skip
SLASHEQUAL           <- '/='               skip
TILDE                <- '~'                skip

end_of_word <- ![a-zA-Z0-9_] skip
KEYWORD_align       <- 'align'       end_of_word
KEYWORD_and         <- 'and'         end_of_word
KEYWORD_anyerror    <- 'anyerror'    end_of_word
KEYWORD_asm         <- 'asm'         end_of_word
KEYWORD_async       <- 'async'       end_of_word
KEYWORD_await       <- 'await'       end_of_word
KEYWORD_break       <- 'break'       end_of_word
KEYWORD_cancel      <- 'cancel'      end_of_word
KEYWORD_catch       <- 'catch'       end_of_word
KEYWORD_comptime    <- 'comptime'    end_of_word
KEYWORD_const       <- 'const'       end_of_word
KEYWORD_continue    <- 'continue'    end_of_word
KEYWORD_defer       <- 'defer'       end_of_word
KEYWORD_else        <- 'else'        end_of_word
KEYWORD_enum        <- 'enum'        end_of_word
KEYWORD_errdefer    <- 'errdefer'    end_of_word
KEYWORD_error       <- 'error'       end_of_word
KEYWORD_export      <- 'export'      end_of_word
KEYWORD_extern      <- 'extern'      end_of_word
KEYWORD_false       <- 'false'       end_of_word
KEYWORD_fn          <- 'fn'          end_of_word
KEYWORD_for         <- 'for'         end_of_word
KEYWORD_if          <- 'if'          end_of_word
KEYWORD_inline      <- 'inline'      end_of_word
KEYWORD_nakedcc     <- 'nakedcc'     end_of_word
KEYWORD_noalias     <- 'noalias'     end_of_word
KEYWORD_null        <- 'null'        end_of_word
KEYWORD_or          <- 'or'          end_of_word
KEYWORD_orelse      <- 'orelse'      end_of_word
KEYWORD_packed      <- 'packed'      end_of_word
KEYWORD_promise     <- 'promise'     end_of_word
KEYWORD_pub         <- 'pub'         end_of_word
KEYWORD_resume      <- 'resume'      end_of_word
KEYWORD_return      <- 'return'      end_of_word
KEYWORD_section     <- 'section'     end_of_word
KEYWORD_stdcallcc   <- 'stdcallcc'   end_of_word
KEYWORD_struct      <- 'struct'      end_of_word
KEYWORD_suspend     <- 'suspend'     end_of_word
KEYWORD_switch      <- 'switch'      end_of_word
KEYWORD_test        <- 'test'        end_of_word
KEYWORD_true        <- 'true'        end_of_word
KEYWORD_try         <- 'try'         end_of_word
KEYWORD_undefined   <- 'undefined'   end_of_word
KEYWORD_union       <- 'union'       end_of_word
KEYWORD_unreachable <- 'unreachable' end_of_word
KEYWORD_use         <- 'use'         end_of_word
KEYWORD_var         <- 'var'         end_of_word
KEYWORD_volatile    <- 'volatile'    end_of_word
KEYWORD_while       <- 'while'       end_of_word

keyword <- KEYWORD_align / KEYWORD_and / KEYWORD_anyerror / KEYWORD_asm
         / KEYWORD_async / KEYWORD_await / KEYWORD_break / KEYWORD_cancel
         / KEYWORD_catch / KEYWORD_comptime / KEYWORD_const / KEYWORD_continue
         / KEYWORD_defer / KEYWORD_else / KEYWORD_enum / KEYWORD_errdefer
         / KEYWORD_error / KEYWORD_export / KEYWORD_extern / KEYWORD_false
         / KEYWORD_fn / KEYWORD_for / KEYWORD_if / KEYWORD_inline
         / KEYWORD_nakedcc / KEYWORD_noalias / KEYWORD_null / KEYWORD_or
         / KEYWORD_orelse / KEYWORD_packed / KEYWORD_promise / KEYWORD_pub
         / KEYWORD_resume / KEYWORD_return / KEYWORD_section
         / KEYWORD_stdcallcc / KEYWORD_struct / KEYWORD_suspend
         / KEYWORD_switch / KEYWORD_test / KEYWORD_true / KEYWORD_try
         / KEYWORD_undefined / KEYWORD_union / KEYWORD_unreachable
         / KEYWORD_use / KEYWORD_var / KEYWORD_volatile / KEYWORD_while
