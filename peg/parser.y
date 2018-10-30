Root <- ContainerMembers eof

# *** Top level ***
ContainerMembers
    <- TestDecl ContainerMembers
     / TopLevelComptime ContainerMembers
     / KEYWORD_pub? TopLevelDecl ContainerMembers
     / KEYWORD_pub? ContainerField COMMA ContainerMembers
     / KEYWORD_pub? ContainerField
     /

TestDecl <- KEYWORD_test STRING Block

TopLevelComptime <- KEYWORD_comptime BlockExpr

TopLevelDecl
    <- FnDef
     / FnProto SEMICOLON
     / KEYWORD_extern STRING? FnProto SEMICOLON
     / VarDecl
     / KEYWORD_export VarDecl
     / KEYWORD_extern STRING? VarDecl
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
     / Expr SEMICOLON

BlockOrExpr
    <- BlockExpr
     / Expr

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
    <- AsyncPrefix SuffixExpr FnCallArgumnets
     / PrimaryExpr SuffixOp*

PrimaryExpr
    <- AsmExpr
     / BlockExpr
     / ContainerDecl
     / ErrorSetDecl
     / ForExpr
     / GroupedExpr
     / IfExpr
     / StringLiteral
     / SwitchExpr
     / WhileExpr
     / BUILTINIDENTIFIER FnCallArgumnets
     / CHAR
     / FLOAT
     / IDENTIFIER
     / INTEGER
     / KEYWORD_anyerror
     / KEYWORD_error DOT IDENTIFIER
     / KEYWORD_false
     / KEYWORD_null
     / KEYWORD_promise
     / KEYWORD_true
     / KEYWORD_undefined
     / KEYWORD_unreachable

BlockExpr <- BlockLabel? Block

Block <- LBRACE Statement* RBRACE

ContainerDecl <- (KEYWORD_extern / KEYWORD_packed)? ContainerDeclAuto

ErrorSetDecl <- KEYWORD_error LBRACE (IDENTIFIER (COMMA IDENTIFIER)* COMMA?)? RBRACE

ForExpr <- BlockLabel? KEYWORD_inline? KEYWORD_for GroupedExpr PtrIndexPayload? Expr (KEYWORD_else Payload? Expr)?

GroupedExpr <- LPAREN Expr RPAREN

IfExpr <- KEYWORD_if GroupedExpr PtrPayload? Expr (KEYWORD_else Payload? Expr)?

StringLiteral
    <- STRING
     / MultilineString

MultilineString
    <- MULTILINESTRINGLINE MultilineString
     / MULTILINESTRINGLINE

SwitchExpr <- KEYWORD_switch GroupedExpr LBRACE (SwitchProng (COMMA SwitchProng)* COMMA?)? LBRACE

WhileExpr <- BlockLabel? KEYWORD_inline? KEYWORD_while GroupedExpr PtrPayload? WhileContinueExpr? Expr (KEYWORD_else Payload? Expr)?

# *** Assembly ***
AsmExpr <- KEYWORD_asm KEYWORD_volatile? LPAREN StringLiteral AsmOutput? RPAREN

AsmOutput <- COLON (AsmOutputItem (COMMA AsmOutputItem)* COMMA?)? AsmInput?

AsmOutputItem <- LBRACKET IDENTIFIER RBRACKET StringLiteral LPAREN (MINUSRARROW TypeExpr / IDENTIFIER) RPAREN

AsmInput <- COLON (AsmInputItem (COMMA AsmInputItem)* COMMA?)? AsmCloppers?

AsmInputItem <- LBRACKET IDENTIFIER RBRACKET StringLiteral LPAREN Expr RPAREN

AsmCloppers <- COLON (STRING (COMMA STRING)* COMMA?)?

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
    <- FnCallArgumnets
     / LBRACKET Expr DOT2 Expr RBRACKET
     / LBRACKET Expr DOT2 RBRACKET
     / LBRACKET Expr RBRACKET
     / DOT IDENTIFIER
     / DOTASTERISK
     / DOTQUESTIONMARK

AsyncPrefix
    <- KEYWORD_async
     / KEYWORD_async LARROW TypeExpr RARROW

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


AMPERSAND <- '&' skip
AMPERSANDEQUAL <- '&=' skip
ASTERISK <- '*' skip
ASTERISK2 <- '**' skip
ASTERISKEQUAL <- '*=' skip
ASTERISKPERCENT <- '*%' skip
ASTERISKPERCENTEQUAL <- '*%=' skip
ATSIGN <- '@' skip
BUILTINIDENTIFIER <- '@' [a-zA-Z_][a-zA-Z0-9_]* skip
CARET <- '^' skip
CARETEQUAL <- '^=' skip
CHAR <- "'" charchar "'" skip
COLON <- ':' skip
COMMA <- ',' skip
DOT <- '.' skip
DOT2 <- '..' skip
DOT3 <- '...' skip
DOTASTERISK <- '.*' skip
DOTQUESTIONMARK <- '.?' skip
EQUAL <- '=' skip
EQUALEQUAL <- '==' skip
EQUALRARROW <- '=>' skip
EXCLAMATIONMARK <- '!' skip
EXCLAMATIONMARKEQUAL <- '!=' skip
FLOAT <- '0b'[01]+'.'[01]+ skip
       / '0b'[01]+'.'[01]*[eE][+-]?[01]+ skip
       / '0b'[01]+[eE][+-]?[01]+ skip
       / '0o'[0-8]+'.'[0-8]+ skip
       / '0o'[0-8]+'.'[0-8]*[eE][+-]?[0-8]+ skip
       / '0o'[0-8]+[eE][+-]?[0-8]+ skip
       / '0x'hex+'.'hex+ skip
       / '0x'hex+'.'hex*[eE][+-]?hex+ skip
       / '0x'hex+[eE][+-]?hex+ skip
       / [0-9]+'.'[0-9]+ skip
       / [0-9]+'.'[0-9]*[eE][+-]?[0-9]+ skip
       / [0-9]+[eE][+-]?[0-9]+ skip
IDENTIFIER <- [a-zA-Z_][a-zA-Z0-9_]* skip
INTEGER <- '0b'[01]+ skip
         / '0o'[0-8]+ skip
         / '0x'hex+ skip
         / [0-9]+ skip
KEYWORD_align <- 'align' skip
KEYWORD_and <- 'and' skip
KEYWORD_anyerror <- 'anyerror' skip
KEYWORD_asm <- 'asm' skip
KEYWORD_async <- 'async' skip
KEYWORD_await <- 'await' skip
KEYWORD_break <- 'break' skip
KEYWORD_cancel <- 'cancel' skip
KEYWORD_catch <- 'catch' skip
KEYWORD_comptime <- 'comptime' skip
KEYWORD_const <- 'const' skip
KEYWORD_continue <- 'continue' skip
KEYWORD_defer <- 'defer' skip
KEYWORD_else <- 'else' skip
KEYWORD_enum <- 'enum' skip
KEYWORD_errdefer <- 'errdefer' skip
KEYWORD_error <- 'error' skip
KEYWORD_export <- 'export' skip
KEYWORD_extern <- 'extern' skip
KEYWORD_false <- 'false' skip
KEYWORD_fn <- 'fn' skip
KEYWORD_for <- 'for' skip
KEYWORD_if <- 'if' skip
KEYWORD_inline <- 'inline' skip
KEYWORD_nakedcc <- 'nakedcc' skip
KEYWORD_noalias <- 'noalias' skip
KEYWORD_null <- 'null' skip
KEYWORD_or <- 'or' skip
KEYWORD_orelse <- 'orelse' skip
KEYWORD_packed <- 'packed' skip
KEYWORD_promise <- 'promise' skip
KEYWORD_pub <- 'pub' skip
KEYWORD_resume <- 'resume' skip
KEYWORD_return <- 'return' skip
KEYWORD_section <- 'section' skip
KEYWORD_stdcallcc <- 'stdcallcc' skip
KEYWORD_struct <- 'struct' skip
KEYWORD_suspend <- 'suspend' skip
KEYWORD_switch <- 'switch' skip
KEYWORD_test <- 'test' skip
KEYWORD_true <- 'true' skip
KEYWORD_try <- 'try' skip
KEYWORD_undefined <- 'undefined' skip
KEYWORD_union <- 'union' skip
KEYWORD_unreachable <- 'unreachable' skip
KEYWORD_use <- 'use' skip
KEYWORD_var <- 'var' skip
KEYWORD_volatile <- 'volatile' skip
KEYWORD_while <- 'while' skip
LARROW <- '<' skip
LARROW2 <- '<<' skip
LARROW2EQUAL <- '<<=' skip
LARROWEQUAL <- '<=' skip
LBRACE <- '{' skip
LBRACKET <- '[' skip
LPAREN <- '(' skip
MINUS <- '-' skip
MINUSEQUAL <- '-=' skip
MINUSPERCENT <- '-%' skip
MINUSPERCENTEQUAL <- '-%=' skip
MINUSRARROW <- '->' skip
MULTILINESTRINGLINE <- 'c'?'\\\\'[^\n]* skip
PERCENT <- '%' skip
PERCENTEQUAL <- '%=' skip
PIPE <- '|' skip
PIPE2 <- '||' skip
PIPEEQUAL <- '!=' skip
PLUS <- '+' skip
PLUS2 <- '++' skip
PLUSEQUAL <- '+=' skip
PLUSPERCENT <- '+%' skip
PLUSPERCENTEQUAL <- '+%=' skip
QUESTIONMARK <- '?' skip
RARROW <- '>' skip
RARROW2 <- '>>' skip
RARROW2EQUAL <- '=>>' skip
RARROWEQUAL <- '>=' skip
RBRACE <- '}' skip
RBRACKET <- ']' skip
RPAREN <- ')' skip
SEMICOLON <- ';' skip
SLASH <- '/' skip
SLASHEQUAL <- '/=' skip
STRING <- '"' stringchar* '"' skip
TILDE <- '~' skip

charchar <- [^\\'\n]
         / escape
comment <- '//'[^\n]*
eof <- !.
escape <- '\\' [nrt'"]
        / '\\x' hex hex
        / '\\u' hex hex hex hex
        / '\\U' hex hex hex hex hex hex
hex <- [0-9a-fA-F]
stringchar <- [^\\"\n]
           / escape
skip <- (comment / [ \n])*
