#include <stdio.h>

//#define YY_DEBUG

int yyparse();

int main() {
    if (yyparse() == 0) {
        fprintf(stderr, "Failed!\n");
        return 1;
    }

    return 0;
}
