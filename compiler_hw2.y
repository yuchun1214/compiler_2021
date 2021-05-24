/*	Definition section */
%{
    #include "common.h" //Extern variables that communicate with lex
    // #define YYDEBUG 1
    // int yydebug = 1;

    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;

    void yyerror (char const *s)
    {
        printf("error:%d: %s\n", yylineno, s);
   
    }

    struct operator_t{
        char * _name;
        int _precedence;
    };

    struct stack_t{
        int _idx;
        int _capacity;
        struct operator_t * _operators;
    };
   
    bool initialize_stack(struct stack_t * stack);
    bool push(struct stack_t * stack, char *oper, int prec);
    void pop(struct stack_t *stack);

    int precedence(struct operator_t * opers, int r1, int r2, char * name);
    

    struct stack_t stack;
    // struct entry_t{
    //     unsigned int _index;
    //     char * _name;
    //     char * _type;
    //     unsigned int _address;
    //     unsigned int _lineno;
    //     char * _element_type;
    // };
    // 
    // struct symbol_table_t{
    //     struct entry_t * _entries;
    //     unsigned int _size;
    //     unsigned int _level;
    // };

    // struct stack_t{
    //     struct symbol_table_t * _tables;
    //     unsigned int _size;
    //     unsigned int _capacity;
    // }; 
    // struct stack_t stack;

    
    /* Symbol table function - you can add new function if needed. */
    
    // static void initialize_stack(struct stack_t *stack);
    // static void create_symbol_table(struct stack_t *stack);
    // static void insert_symbol(/* ... */);
    // static void lookup_symbol(/* ... */);
    // static void dump_symbol_table(/* ... */);
%}

%error-verbose

/* Use variable or self-defined structure to represent
 * nonterminal and token type
 */
%union {
    int i_val;
    float f_val;
    char *s_val;
    bool b_val;
    /* ... */
	struct {
		char * id_name;
	} ctr;
}

/* Token without return */
%token INT FLOAT BOOL STRING
%token SEMICOLON
%token PRINT
%token IDENT
%token ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN
%token OR AND
%token EQ NEQ LSL LEQ GTR GEQ
%token ADD SUB
%token MUL QUO REM
%token POS NEG NOT
%token INC DEC
%token TRUE FALSE


/* Token with return, which need to sepcify type */
%token <i_val> INT_LIT
%token <f_val> FLOAT_LIT
%token <s_val> STRING_LIT



/* Nonterminal with return, which need to sepcify type */
%type <s_val> cmp_op add_op mul_op unary_op

/* Yacc will start at this nonterminal */
%start Program

/* Grammar section */
%%

Program
    : StatementList
;

StatementList
    :  StatementList Statement
    |
;

Type
    : TypeName
;

TypeName
    : INT
    | FLOAT
    | STRING
    | BOOL
;



Statement
    : DeclarationStmt { printf("DeclarationStmt\n"); }
	| AssignmentStmt  { printf("AssignmentStmt\n");  }
    | ArithmeticStmt  { printf("ArithmeticStmt\n");  }
    | PrintStmt { pop(&stack); }
;

Expression
	: UnaryExpr
	| Expression binary_op Expression { }
;

UnaryExpr
	: PrimaryExpr
	| unary_op UnaryExpr { push(&stack, $1, 5); }
    | INC UnaryExpr
    | DEC UnaryExpr
    | UnaryExpr INC
    | UnaryExpr DEC
;

PrimaryExpr
	: Operand
	| IndexExpr
	| ConversionExpr
;

Operand
	: Literal
	| IDENT
	| LPExpression Expression RPExpression
;

LPExpression
    : '('    {  push(&stack, "LP", -1); }
;

RPExpression
    : ')'    { pop(&stack);}
;

binary_op
	: OR       { push(&stack, "OR", 0); }
	| AND      { push(&stack, "AND", 1); }
	| cmp_op   { push(&stack, $1, 2); }
	| add_op   { push(&stack, $1, 3); }
	| mul_op   { push(&stack, $1, 4); }
;

cmp_op
	: EQ       { $$ = "EQ"; }
	| NEQ      { $$ = "NEQ"; }
	| LSL      { $$ = "LSL"; }
	| LEQ      { $$ = "LEQ"; }
	| GTR      { $$ = "GTR"; }
	| GEQ      { $$ = "GEQ"; }
;

add_op
	: ADD      { $$ = "ADD"; }
	| SUB      { $$ = "SUB"; }
;

mul_op
	: MUL      { $$ = "MUL"; }
	| QUO      { $$ = "QUO"; }
	| REM      { $$ = "REM"; }
;

unary_op
	: ADD      { $$ = "POS"; }
	| SUB      { $$ = "NEG"; }
	| NOT      { $$ = "NOT"; }
;

ArithmeticStmt
    : Expression binary_op Expression SEMICOLON
    | Expression SEMICOLON
;

PrintStmt
    : PRINT '(' Expression ')' SEMICOLON { }
;


Literal
    : INT_LIT {
        printf("INT_LIT %d\n", $<i_val>$);
    }
    | FLOAT_LIT {
        printf("FLOAT_LIT %f\n", $<f_val>$);
    }
    | STRING_LIT {
        printf("STRING_LIT %s\n", $<s_val>$);
    }
    | BOOL_LIT
;

BOOL_LIT
    : TRUE { printf("TRUE\n"); }
    | FALSE { printf("FALSE\n"); }

IndexExpr
	: PrimaryExpr '[' Expression ']'
;

ConversionExpr
	: Type '(' Expression ')'
	

DeclarationStmt
	: Type IDENT SEMICOLON { printf("ID : %s, type\n", yylval.ctr.id_name);}
	| Type IDENT '='Expression SEMICOLON 
	| Type IDENT '[' Expression ']' SEMICOLON
;

AssignmentExpr
	: Expression assign_op Expression
;

AssignmentStmt
	: AssignmentExpr SEMICOLON
;

assign_op
	: '=' 
	| ADD_ASSIGN
	| SUB_ASSIGN
	| MUL_ASSIGN
	| DIV_ASSIGN
	| MOD_ASSIGN
;

%%

bool initialize_stack(struct stack_t *stack){
    stack->_idx = 0;
    stack->_capacity = 100;
    stack->_operators = (struct operator_t *)malloc(sizeof(struct operator_t)*100);
    return (stack->_operators) ? true : false;
}

bool realloc_stack(struct stack_t * stack){
    stack->_operators = realloc(stack->_operators, sizeof(struct operator_t) * (stack->_capacity += 100));
    return stack->_operators ? true : false;
}


// perform faulty binary search algorithm
int precedence(struct operator_t * opers, int r1, int r2, char * name){
    int mid = (r1 + r2) / 2.0;
    int retval = strcmp(name, opers[mid]._name);
    if(retval == 0){
        return opers[mid]._precedence;
    } else if(retval > 0) {
        return precedence(opers, mid, r2, name);    
    } else{
        return precedence(opers, r1, mid, name);
    }
}

bool push(struct stack_t * stack, char * oper, int prec){
    // printf("push! oper is %s, prec is %u\n", oper, prec);
    
    int i = 0;
    if(prec >= 0){
        for(i = stack->_idx - 1; i >= 0; --i){
            if(stack->_operators[i]._precedence >= prec){
                printf("%s\n", stack->_operators[i]._name); 
            }else
                break;
        }
        stack->_idx = i + 1;
    }

    if(stack->_idx + 1 > stack->_capacity){
        realloc_stack(stack);
        if(!stack->_operators) return false;
    }

    stack->_operators[stack->_idx]._name = oper;
    stack->_operators[stack->_idx]._precedence = prec;
    stack->_idx += 1;
    return true;
}

void pop(struct stack_t * stack){
    // printf("pop!!\n");
    char * text;
    int i;
    for(i = stack->_idx - 1; i >= 0; --i){
        text = stack->_operators[i]._name;
        if(strcmp(text, "LP") == 0)
            break;
        printf("%s\n", stack->_operators[i]._name);
    }
    
    stack->_idx = (i > 0) ? i : 0;
}

// static void initialize_stack(struct stack_t *stack){
//     printf("Initialize stack\n");
//     stack->_tables = (struct symbol_table_t *)malloc(sizeof(struct symbol_table_t) * 100);
//     stack->_capacity = 100;
//     stack->_size = 0;
// }
// 
// static void create_symbol_table(struct stack_t *stack){
//     printf("create symbol table\n");
//     if(stack->_size + 1 > stack->_capacity){
//         stack->_tables = realloc(stack->_tables, sizeof(struct symbol_table_t) * (stack->_capacity += 100));
//     }
// }


/* C code section */
int main(int argc, char *argv[])
{
    if (argc == 2) {
        yyin = fopen(argv[1], "r");
    } else {
        yyin = stdin;
    }

    bool retval = initialize_stack(&stack);
    

    if (!retval)
        exit(-1);

    yyparse();

	printf("Total lines: %d\n", yylineno);
    fclose(yyin);
    return 0;
}
