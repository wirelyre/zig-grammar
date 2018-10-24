%locations
%defines
%define parse.trace
%define parse.error verbose
%define api.pure full

%{
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <unistd.h>
#include <limits.h>

#include "parser.h"

#define YYMAXDEPTH INT_MAX

int yylex(YYSTYPE *yylval, YYLTYPE *yylloc);
extern FILE* yyin;
char *file;

int main(int argc, char **argv) {
    int opt;
    while ((opt = getopt(argc, argv, "v")) != -1) {
        switch (opt) {
        case 'v':
            yydebug = 1;
            break;
        default: /* '?' */
            fprintf(stderr, "Usage: %s [-v] [FILE]...\n", argv[0]);
            exit(EXIT_FAILURE);
        }
    }


    for (; optind < argc; optind++) {
        file = argv[optind];
        yyin = fopen(file, "r");
        if (yyparse() == 1) {
            return 1;
        }
        fclose(yyin);
    }

    return 0;
}

void yyerror(YYLTYPE *locp, char const *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    fprintf(stderr, "%s:%d:%d: ", file, locp->first_line, locp->first_column);
    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n");
    va_end(args);
}
%}

%token Eof 0
%token Keyword_test
%token Keyword_pub
%token Keyword_comptime
%token Keyword_extern
%token Keyword_inline
%token Keyword_export
%token Keyword_nakedcc
%token Keyword_stdcallcc
%token Keyword_async
%token Keyword_use
%token Keyword_fn
%token Keyword_noalias
%token Keyword_var
%token Keyword_const
%token Keyword_align
%token Keyword_section
%token Keyword_try
%token Keyword_return
%token Keyword_break
%token Keyword_cancel
%token Keyword_defer
%token Keyword_while
%token Keyword_for
%token Keyword_suspend
%token Keyword_orelse
%token Keyword_catch
%token Keyword_volatile
%token Keyword_await
%token Keyword_switch
%token Keyword_or
%token Keyword_and
%token Keyword_true
%token Keyword_false
%token Keyword_null
%token Keyword_undefined
%token Keyword_error
%token Keyword_unreachable
%token Integer
%token String
%token MultilineStringLine
%token Identifier
%token AtSign
%token Semicolon
%token Colon
%token Comma
%token ExclamationMark
%token Dot3
%token Dot2
%token Dot
%token LArrowEqual
%token RArrowEqual
%token LBrace
%token RBrace
%token LParen
%token RParen
%token LArrow
%token RArrow
%token LBracket
%token RBracket
%token AsteriskEqual
%token SlashEqual
%token PercentEqual
%token PlusEqual
%token MinusEqual
%token LArrow2Equal
%token RArrow2Equal
%token AmpersandEqual
%token EqualRArrow
%token CaretEqual
%token PipeEqual
%token AsteriskPercentEqual
%token PlusPercentEqual
%token MinusPercentEqual
%token Equal
%token Pipe
%token Minus
%token Tilde
%token QuestionMark
%token DotAsterisk
%token EqualEqual
%token ExclamationMarkEqual
%token MinusRArrow
%token Keyword_packed
%token Keyword_struct
%token Keyword_enum
%token Keyword_union
%token Keyword_promise
%token Caret
%token Ampersand
%token LArrow2
%token RArrow2
%token Plus
%token Plus2
%token PlusPercent
%token Pipe2
%token Slash
%token Percent
%token Asterisk2
%token AsteriskPercent
%token Asterisk
%token MinusPercent
%token Keyword_resume
%token Keyword_if
%token Keyword_else
%token Keyword_continue
%token Char
%token DotQuestionMark
%token Keyword_asm
%token Keyword_errdefer
%token InvalidToken
%token Keyword_errorset
%token Float
%token BuiltinIdentifier

%%

// *** Core grammar ***
Root: ContainerMembers Eof


// Top level
ContainerMembers
    : TestDecl ContainerMembers
    | ComptimeStatement ContainerMembers
    | O_Pub TopLevelDecl ContainerMembers
    | O_Pub ContainerField Comma ContainerMembers
    | O_Pub ContainerField
    | %empty

TestDecl: Keyword_test StringLiteral BlockExpr

TopLevelDecl
    : FnDef
    | FnProto Semicolon
    | Keyword_extern StringLiteral FnProto Semicolon
    | VarDecl
    | Keyword_export VarDecl
    | Keyword_extern VarDecl
    | Keyword_extern StringLiteral VarDecl
    | Keyword_use Expr Semicolon

FnDef
    : FnProto BlockExpr
    | Keyword_inline FnProto BlockExpr
    | Keyword_export FnProto BlockExpr

FnProto: FnProtoCC Keyword_fn O_Identifier LParen L_ParamDecl RParen Placement FnProtoReturnType

VarDecl: VarDeclAttribute Identifier DeclType Placement EqualInitExpr Semicolon

ContainerField: Identifier DeclType EqualInitExpr


// Statements
Statement
    : VarDecl
    | DeferStatement
    | IfStatement
    | WhileStatement
    | ForStatement
    | ComptimeStatement
    | SuspendStatement
    | BlockStatement
    | SwitchExpr
    | LValueExpr Semicolon
    | VoidNoreturnExpr Semicolon

DeferStatement: Defer Statement

IfStatement
    : Keyword_if GroupedExpr O_PtrPayload Statement
    | Keyword_if GroupedExpr O_PtrPayload Expr Keyword_else O_Payload Statement

WhileStatement
    : O_BlockLabel O_Inline Keyword_while GroupedExpr O_PtrPayload WhileContinueExpr Statement
    | O_BlockLabel O_Inline Keyword_while GroupedExpr O_PtrPayload WhileContinueExpr Expr Keyword_else O_Payload Statement

ForStatement
    : O_BlockLabel O_Inline Keyword_for GroupedExpr O_PtrIndexPayload Statement
    | O_BlockLabel O_Inline Keyword_for GroupedExpr O_PtrIndexPayload Expr Keyword_else Statement

ComptimeStatement: Keyword_comptime Statement

SuspendStatement: Keyword_suspend Statement

BlockStatement
    : BlockExpr
    | LabeledBlockExpr


// Expressions
VoidNoreturnExpr
    : Keyword_return O_Expr
    | Keyword_break O_BreakLabel O_Expr
    | Keyword_continue O_BreakLabel
    | Keyword_suspend
    | Keyword_cancel Expr
    | Keyword_resume Expr
    | Keyword_try Expr
    | Keyword_unreachable
    | AsmExpr

ErrorExpr
    : ErrorSetDecl
    | Keyword_error
    | Identifier
    | GroupedExpr

Expr
    : Keyword_return O_Expr
    | Keyword_break O_BreakLabel O_Expr
    | Keyword_continue O_BreakLabel
    | Keyword_suspend
    | Keyword_cancel Expr
    | Keyword_resume Expr
    | BoolOrExpr

BoolOrExpr
    : BoolAndExpr Keyword_or BoolOrExpr
    | BoolAndExpr

BoolAndExpr
    : CompareExpr Keyword_and BoolAndExpr
    | CompareExpr

CompareExpr
    : BinOrExpr CompareOp BinOrExpr
    | BinOrExpr

BinOrExpr
    : BinXorExpr Pipe BinOrExpr
    | BinXorExpr

BinXorExpr
    : BinAndExpr Caret BinXorExpr
    | BinAndExpr

BinAndExpr
    : BitShiftExpr Ampersand BinAndExpr
    | BitShiftExpr

BitShiftExpr
    : AdditionExpr BitShiftOp BitShiftExpr
    | AdditionExpr

AdditionExpr
    : MultiplyExpr AdditionOp AdditionExpr
    | MultiplyExpr

MultiplyExpr
    : PrefixOpExpr MultiplyOp MultiplyExpr
    | PrefixOpExpr

PrefixOpExpr
    : PrefixOp PrefixOpExpr
    | PrimaryExpr

PrimaryExpr
    : Integer
    | Float
    | Char
    | Keyword_true
    | Keyword_false
    | Keyword_null
    | Keyword_unreachable
    | BlockExpr
    | CurlySuffixExpr
    | AsmExpr

BlockExpr: LBrace L_Statement RBrace

CurlySuffixExpr
    : TypeExpr
    | TypeExpr LBrace InitList RBrace

InitList
    : Expr
    | Expr Comma L_Expr
    | FieldInit
    | FieldInit Comma L_FieldInit
    | %empty

TypeExpr
    : ErrorExpr ExclamationMark PrefixTypeExpr
    | PrefixTypeExpr

PrefixTypeExpr
    : PrefixTypeOp PrefixTypeExpr
    | RValueExpr

RValueExpr
    : Keyword_undefined
    | LabeledBlockExpr
    | IfExpr
    | WhileExpr
    | ForExpr
    | SwitchExpr
    | ComptimeExpr
    | LValueExpr

LabeledBlockExpr: BlockLabel BlockExpr

IfExpr
    : Keyword_if GroupedExpr O_PtrPayload Expr
    | Keyword_if GroupedExpr O_PtrPayload Expr Keyword_else O_Payload Expr

WhileExpr
    : O_BlockLabel O_Inline Keyword_while GroupedExpr O_PtrPayload WhileContinueExpr Expr
    | O_BlockLabel O_Inline Keyword_while GroupedExpr O_PtrPayload WhileContinueExpr Expr Keyword_else O_Payload Expr

ForExpr
    : O_BlockLabel O_Inline Keyword_for GroupedExpr O_PtrIndexPayload Expr
    | O_BlockLabel O_Inline Keyword_for GroupedExpr O_PtrIndexPayload Expr Keyword_else Expr

SwitchExpr: Keyword_switch GroupedExpr LBrace L_SwitchProng RBrace

ComptimeExpr: Keyword_comptime Expr

// This is a hack, as the prefix async syntax for calling async functions is a giant pain!
LValueExpr
    : LValueExpr UnwrapOp
    | LValueExpr2

LValueExpr2
//    : AsyncPrefix LValueExpr2 FnCallArgumnets
    : LValueExpr2 SuffixTypeOp
    | LValueExpr2 AssignOp Expr
    | LeafExpr

LeafExpr
    : BuiltinIdentifier FnCallArgumnets
    | Identifier
    | Keyword_error
    | Keyword_promise
    | StringLiteral
    | FnProto
    | ContainerDecl
    | ErrorSetDecl
    | GroupedExpr

StringLiteral
    : MultilineString
    | String

MultilineString
    : MultilineStringLine MultilineString
    | MultilineStringLine

GroupedExpr: LParen Expr RParen

ContainerDecl
    : Keyword_extern ContainerDeclAuto
    | Keyword_packed ContainerDeclAuto
    | ContainerDeclAuto

ErrorSetDecl: Keyword_errorset LBrace L_Identifier RBrace

// Assembly
AsmExpr: Keyword_asm AsmVolatile LParen StringLiteral AsmOutput RParen

AsmVolatile
    : Keyword_volatile
    | %empty

AsmOutput
    : Colon L_AsmOutput AsmInput
    | %empty

AsmOutputItem: LBracket Identifier RBracket StringLiteral LParen AsmOutputVarOrReturn RParen

AsmInput
    : Colon L_AsmInput AsmCloppers
    | %empty

AsmInputItem: LBracket Identifier RBracket StringLiteral LParen Expr RParen

AsmCloppers
    : Colon L_String
    | %empty


// *** Helper grammar ***
BreakLabel: Colon Identifier

BlockLabel: Identifier Colon

VarDeclAttribute
    : Keyword_const
    | Keyword_var

AsmOutputVarOrReturn
    : Identifier
    | MinusRArrow TypeExpr

Defer
    : Keyword_defer
    | Keyword_errdefer

FieldInit: Dot Identifier Equal Expr

WhileContinueExpr
    : Colon LParen Expr RParen
    | %empty


// VarDecl and ContainerField specific
DeclType
    : Colon TypeExpr
    | %empty

EqualInitExpr
    : Equal Expr
    | %empty


// FnProto and VarDecl specific
Placement
    : ByteAlign Section
    | ByteAlign
    | Section
    | %empty

Section: Keyword_section GroupedExpr


// FnProto specific
FnProtoCC
    : Keyword_nakedcc
    | Keyword_stdcallcc
    | Keyword_extern
    | Keyword_async
    | Keyword_async LArrow TypeExpr RArrow
    | %empty

ParamDecl
    : ParamDeclAttribute ParamType
    | ParamDeclAttribute Identifier Colon ParamType

ParamType
    : TypeExpr
    | Keyword_var
    | Dot3

ParamDeclAttribute
    : Keyword_noalias
    | Keyword_comptime
    | %empty

FnProtoReturnType
    : ExclamationMark TypeExpr
    | ExclamationMark Keyword_var
    | TypeExpr
    | Keyword_var


// Payloads
Payload: Pipe Identifier Pipe

PtrPayload
    : Pipe Asterisk Identifier Pipe
    | Payload

PtrIndexPayload
    : Pipe Asterisk Identifier Comma Identifier Pipe
    | Pipe Identifier Comma Identifier Pipe
    | PtrPayload


// Switch specific
SwitchProng: SwitchCase EqualRArrow O_PtrPayload Expr

SwitchCase
    : L_SwitchItem
    | Keyword_else

SwitchItem
    : Expr
    | Expr Dot3 Expr


// Operators
AssignOp
    : AsteriskEqual
    | SlashEqual
    | PercentEqual
    | PlusEqual
    | MinusEqual
    | LArrow2Equal
    | RArrow2Equal
    | AmpersandEqual
    | CaretEqual
    | PipeEqual
    | AsteriskPercentEqual
    | PlusPercentEqual
    | MinusPercentEqual
    | Equal

CompareOp
    : EqualEqual
    | ExclamationMarkEqual
    | LArrow
    | RArrow
    | LArrowEqual
    | RArrowEqual

BitShiftOp
    : LArrow2
    | RArrow2

AdditionOp
    : Plus
    | Minus
    | Plus2
    | PlusPercent
    | MinusPercent

MultiplyOp
    : Pipe2
    | Asterisk
    | Slash
    | Percent
    | Asterisk2
    | AsteriskPercent

PrefixOp
    : ExclamationMark
    | Minus
    | Tilde
    | MinusPercent
    | Ampersand

PrefixTypeOp
    : QuestionMark
    | Keyword_try
    | Keyword_await
    | Keyword_promise MinusRArrow
    | PtrStart L_PtrAttribute

SuffixTypeOp
    : FnCallArgumnets
    | LBracket Expr RBracket
    | LBracket Expr Dot2 RBracket
    | LBracket Expr Dot2 Expr RBracket
    | Dot Identifier
    | DotAsterisk
    | DotQuestionMark

UnwrapOp
    : Keyword_orelse Expr
    | Keyword_catch O_Payload Expr

// Fn call specific
// AsyncPrefix
//     : Keyword_async
//     | Keyword_async LArrow Expr RArrow

FnCallArgumnets: LParen L_Expr RParen


// Ptr specific
PtrStart
    : Asterisk
    | Asterisk2
    | LBracket RBracket
    | LBracket Asterisk RBracket
    | LBracket Expr RBracket

PtrAttribute
    : BitAlign
    | ByteAlign
    | Keyword_const
    | Keyword_volatile


// ContainerDecl specific
ContainerDeclAuto: ContainerDeclType LBrace ContainerMembers RBrace

ContainerDeclType
    : Keyword_struct
    | Keyword_struct GroupedExpr
    | Keyword_union
    | Keyword_union GroupedExpr
    | Keyword_union LParen Keyword_enum RParen
    | Keyword_union LParen Keyword_enum GroupedExpr RParen
    | Keyword_enum
    | Keyword_enum GroupedExpr


// Alignment
ByteAlign: Keyword_align GroupedExpr

BitAlign: Keyword_align LParen Expr Colon Expr Colon Expr RParen


// O_s
O_Identifier
    : Identifier
    | %empty

O_Pub
    : Keyword_pub
    | %empty

O_Inline
    : Keyword_inline
    | %empty

O_Payload
    : Payload
    | %empty

O_PtrPayload
    : PtrPayload
    | %empty

O_PtrIndexPayload
    : PtrIndexPayload
    | %empty

O_BreakLabel
    : BreakLabel
    | %empty

O_BlockLabel
    : BlockLabel
    | %empty

O_Expr
    : Expr
    | %empty

// L_s
L_Expr
    : Expr Comma L_Expr
    | Expr
    | %empty

L_String
    : String Comma L_String
    | String
    | %empty

L_AsmInput
    : AsmInputItem Comma L_AsmInput
    | AsmInputItem
    | %empty

L_AsmOutput
    : AsmOutputItem Comma L_AsmOutput
    | AsmOutputItem
    | %empty

L_ParamDecl
    : ParamDecl Comma L_ParamDecl
    | ParamDecl
    | %empty

L_SwitchProng
    : SwitchProng Comma L_SwitchProng
    | SwitchProng
    | %empty

L_Identifier
    : Identifier Comma L_Identifier
    | Identifier
    | %empty

L_SwitchItem
    : SwitchItem Comma L_SwitchItem
    | SwitchItem
    | %empty

L_FieldInit
    : FieldInit Comma L_FieldInit
    | FieldInit
    | %empty

L_Statement
    : Statement L_Statement
    | %empty

L_PtrAttribute
    : PtrAttribute L_PtrAttribute
    | %empty

%%
