Root <- ContainerMembers eof

# *** Top level ***
ContainerMembers <- TestDecl ContainerMembers
                  / TopLevelComptime ContainerMembers
                  / KEYWORD_pub? TopLevelDecl ContainerMembers
                  / KEYWORD_pub? ContainerField Comma ContainerMembers
                  / KEYWORD_pub? ContainerField
                  /

TestDecl <- KEYWORD_test STRING Block

TopLevelComptime <- KEYWORD_comptime BlockStatement

TopLevelDecl <- FnDef
              / FnProto SEMICOLON
              / KEYWORD_extern STRING? FnProto SEMICOLON
              / VarDecl
              / KEYWORD_export VarDecl
              / KEYWORD_extern STRING? VarDecl
              / KEYOWRD_use Expr SEMICOLON

FnDef <- (KEYWORD_inline | KEYWORD_export)? FnProto Block

FnProto <- FnCC? KEYWORD_fn IDENTIFIER? LPAREN ParamDecls RPAREN ByteAlign? Section? EXCLAMATIONMARK? ReturnType

VarDecl <- (KEYWORD_const | KEYWORD_var) IDENTIFIER (COLON TypeExpr)? ByteAlign? Section? (EQUAL Expr)? SEMICOLON

ContainerField <- IDENTIFIER (COLON TypeExpr)? (EQUAL Expr)?

# *** Block Level ***
Statement <- KEYWORD_comptime? VarDecl
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

IfStatement -> Keyword_if GroupedExpr PtrPayload? BlockOrExpr KEYWORD_else Payload? Statement
             / Keyword_if GroupedExpr PtrPayload? BlockExprStatement

WhileStatement
    : O_BlockLabel O_Inline Keyword_while GroupedExpr O_PtrPayload WhileContinueExpr Statement
    | O_BlockLabel O_Inline Keyword_while GroupedExpr O_PtrPayload WhileContinueExpr SimpleExpr Keyword_else O_Payload Statement
    | O_BlockLabel O_Inline Keyword_while GroupedExpr O_PtrPayload WhileContinueExpr BlockStatement Keyword_else O_Payload Statement

ForStatement
    : O_BlockLabel O_Inline Keyword_for GroupedExpr O_PtrIndexPayload Statement
    | O_BlockLabel O_Inline Keyword_for GroupedExpr O_PtrIndexPayload SimpleExpr Keyword_else Statement
    | O_BlockLabel O_Inline Keyword_for GroupedExpr O_PtrIndexPayload BlockStatement Keyword_else Statement

BlockExprStatement <- BlockStatement
                    / Expr SEMICOLON

BlockOrExpr <- BlockStatement
             / Expr

AMPERSAND <- '&' skip
AMPERSANDEQUAL <- '&=' skip
ASTERISK <- '*' skip
ASTERISK2 <- '**' skip
ASTERISKEQUAL <- '*=' skip
ASTERISKPERCENT <- '*%' skip
ASTERISKPERCENTEQUAL '*%=' skip
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
PIPE <- |' skip
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
eof <- !
escape <- '\\' [nrt'"]
        / '\\x' hex hex
        / '\\u' hex hex hex hex
        / '\\U' hex hex hex hex hex hex
hex <- [0-9a-fA-F]
stringchar <- [^\\"\n]
           / escape
skip <- (comment / space)*
