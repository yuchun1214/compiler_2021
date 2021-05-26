/*	Definition section */
%{
    #include "common.h" //Extern variables that communicate with lex
    
    #define TYPE_AMOUNT 4

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
        bool _is_unary;
    };

    struct symtb_entry_t{
        int _idx;
        char * _name;
        char * _type;
        int _address;
        int _line_no;
        char * _element_type;
        int _scope;
    };

    struct stack_t{
        int _idx;
        int _capacity;
        void * _entries;
        void * _ptr_parent_object;
    };

    struct symbol_table_t{
        int _current_scope;
        int _address;
        struct stack_t _symbols;
    };
   
    bool initialize_operator_stack(struct stack_t * stack);
    
    bool push_operator(
            struct stack_t *operators_stack, 
            struct stack_t * operands_stack, 
            char *name, 
            int prec, 
            bool is_unary
        );

    void pop_operator(struct stack_t * operators_stack, struct stack_t * operands_stack);
    char * pop_operators(struct stack_t *operators_stack, struct stack_t * operands_stack);

    bool initialize_operand_stack(struct stack_t *stack);
    bool push_operand(struct stack_t *stack, char * type);
    void pop_operands(struct stack_t *stack);
    void pop_operand(struct stack_t *stack);
    void conversion(struct stack_t *operands_stack, char * new_type);

    bool initialize_symbol_stack(struct stack_t * stack);
    bool initialize_symbol_table(struct symbol_table_t * table);
    void insert_symbol(struct symbol_table_t * table, char *name, char *type, int line_no, char * element_type);
    int lookup_symbol(struct symbol_table_t * table, char *name);
    void new_scope(struct symbol_table_t * table);
    void delete_scope(struct symbol_table_t * table);
    char * get_operand_type(struct symbol_table_t * table, char *name);

    struct stack_t operators_stack;
    struct stack_t operands_stack;
    struct symbol_table_t table;

    char TYPE[TYPE_AMOUNT][6] = {
        "string", "bool", "int", "float"
    };
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
%token SEMICOLON
%token PRINT
%token ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN
%token OR AND
%token EQ NEQ LSL LEQ GTR GEQ
%token ADD SUB
%token MUL QUO REM
%token POS NEG NOT
%token INC DEC
%token TRUE FALSE
%token INT FLOAT BOOL STRING
%token WHILE FOR
%token IF ELSE


/* Token with return, which need to sepcify type */
%token <i_val> INT_LIT
%token <f_val> FLOAT_LIT
%token <s_val> STRING_LIT
%token <s_val> IDENT



/* Nonterminal with return, which need to sepcify type */
%type <s_val> cmp_op add_op mul_op unary_op assign_op
%type <s_val> Literal
%type <s_val> Type TypeName

/* Yacc will start at this nonterminal */
%start Program

/* Grammar section */
%%

Program
    : StatementList { delete_scope(&table); }
;

StatementList
    :  StatementList Statement
    |
;

Type
    : TypeName { $$ = $1; }
;

TypeName
    : INT { $$ = "int";}
    | FLOAT { $$ = "float"; }
    | STRING { $$ = "string"; }
    | BOOL   { $$ = "bool"; }
;


Statement
    : DeclarationStmt { }
	| AssignmentStmt  { pop_operators(&operators_stack, &operands_stack); }
    | ArithmeticStmt  { pop_operators(&operators_stack, &operands_stack); }
    | PrintStmt { printf("PRINT %s\n", pop_operators(&operators_stack, &operands_stack)); }
    | Block
    | WhileStmt
    | ForStmt
    | IfStmt
;

IfStmt
    : IF Condition Block
    | IF Condition Block ELSE
    | IF Condition Block ELSE IfStmt
;
    

WhileStmt
    : WHILE '(' Condition ')' Block
;

ForStmt
    : FOR '(' ForClause ')' Block
;

ForClause
    : InitStmt SEMICOLON Condition SEMICOLON PostStmt  { pop_operators(&operators_stack, &operands_stack); }
;

Condition
    : Expression { pop_operators(&operators_stack, &operands_stack); /*Expression check if bool type*/}
    | BOOL_LIT
;

InitStmt
    : SimpleExpr
;

PostStmt
    : SimpleExpr
;

SimpleExpr
    : AssignmentExpr
    | Expression
    | UnaryExpr
;


Block
    : LBBlock StatementList RBBlock { }
;

LBBlock
    : '{' { new_scope(&table); }
;

RBBlock
    : '}' { delete_scope(&table); }
;

Expression
	: UnaryExpr
	| Expression binary_op Expression { }
;

UnaryExpr
	: PrimaryExpr
	| unary_op UnaryExpr { push_operator(&operators_stack, &operands_stack, $1, 5, true); }
    | INC UnaryExpr  { push_operator(&operators_stack, &operands_stack, "INC", 5, true); }
    | DEC UnaryExpr{ push_operator(&operators_stack, &operands_stack, "DEC", 5, true); }
    | UnaryExpr INC { push_operator(&operators_stack, &operands_stack, "INC", 5, true); }
    | UnaryExpr DEC { push_operator(&operators_stack, &operands_stack, "DEC", 5, true); }
;

PrimaryExpr
	: Operand
	| IndexExpr 
	| ConversionExpr
;

Operand
	: Literal { push_operand(&operands_stack, $1); }
	| IDENT   { push_operand(&operands_stack, get_operand_type(&table,$1)); }
	| LPExpression Expression RPExpression
;

LPExpression
    : '('    {  push_operator(&operators_stack, &operands_stack, "LP", -1, false); }
;

RPExpression
    : ')'    { pop_operators(&operators_stack, &operands_stack);}
;

binary_op
	: OR       { push_operator(&operators_stack, &operands_stack,  "OR", 0, false); } 
	| AND      { push_operator(&operators_stack, &operands_stack,  "AND", 1, false); }
	| cmp_op   { push_operator(&operators_stack, &operands_stack,  $1, 2, false); }
	| add_op   { push_operator(&operators_stack, &operands_stack,  $1, 3, false); }
	| mul_op   { push_operator(&operators_stack, &operands_stack,  $1, 4, false); }
;

cmp_op
	: EQ       { $$ = "EQL"; }
	| NEQ      { $$ = "NEQ"; }
	| LSL      { $$ = "LSS"; }
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
        $$ = "int";
    }
    | FLOAT_LIT {
        printf("FLOAT_LIT %f\n", $<f_val>$);
        $$ = "float";
    }
    | '"' STRING_LIT '"' {
        printf("STRING_LIT %s\n", $2);
        $$ = "string";
    }
    | BOOL_LIT { $$ = "bool"; }
;

BOOL_LIT
    : TRUE { printf("TRUE\n"); }
    | FALSE { printf("FALSE\n"); }


IndexExpr
	: PrimaryExpr '[' Expression ']' { pop_operators(&operators_stack, &operands_stack); pop_operand(&operands_stack);  }
;

ConversionExpr
	: Type '(' Expression ')'        { conversion(&operands_stack, $1) ; }
    | '(' Type ')' Expression {}     { conversion(&operands_stack, $2); }
	

DeclarationStmt
	: Type IDENT SEMICOLON { insert_symbol(&table, $2, $1, yylineno, "-"); }
	| Type IDENT '=' Expression SEMICOLON  { insert_symbol(&table, $2, $1, yylineno, "-");  pop_operators(&operators_stack, &operands_stack); pop_operands(&operands_stack); }
	| Type IDENT '[' Expression ']' SEMICOLON { insert_symbol(&table, $2, "array", yylineno, $1); pop_operators(&operators_stack, &operands_stack); pop_operands(&operands_stack); }
;

AssignmentExpr
	: Expression assign_op Expression { pop_operators(&operators_stack, &operands_stack); printf("%s\n", $2);}
;

AssignmentStmt
	: AssignmentExpr SEMICOLON
;

assign_op
	: '='   { $$ = "ASSIGN"; }
	| ADD_ASSIGN { $$ = "ADD_ASSIGN"; }
	| SUB_ASSIGN { $$ = "SUB_ASSIGN"; }
	| MUL_ASSIGN { $$ = "MUL_ASSIGN"; }
	| DIV_ASSIGN { $$ = "QUO_ASSIGN"; }
	| MOD_ASSIGN { $$ = "REM_ASSIGN"; }
;
%%

bool initialize_symbol_stack(struct stack_t * stack){
    _initialize_stack(stack, struct symtb_entry_t);
    return (stack->_entries) ? true : false;
}

bool initialize_symbol_table(struct symbol_table_t * table){
    table->_current_scope = 0;
    table->_address = 0;
    int retval = initialize_symbol_stack(&table->_symbols);
    table->_symbols._ptr_parent_object = table;
    return retval;
}

bool initialize_operand_stack(struct stack_t *stack){
    _initialize_stack(stack, char *);
    return (stack->_entries) ? true : false;
}

bool push_operand(struct stack_t *stack, char * type){
    _stack_push(stack, char *, type);
    return (stack->_entries) ? true : false;
}

void pop_operands(struct stack_t *stack){
    stack->_idx = 0;
}

void pop_operand(struct stack_t *stack){
    _stack_pop(stack);
}

bool initialize_operator_stack(struct stack_t *stack){
    _initialize_stack(stack, struct operator_t);
    return (stack->_entries) ? true : false;
}

int check_type(char *operand){
    if(operand){
        for (int i = 0; i < TYPE_AMOUNT; ++i){
            if(strncmp(operand, TYPE[i], 3) == 0){
                return i;
            }
        }
    }
    
    return -1;
}

char * get_type(int index){
    if(index > 0 && index < TYPE_AMOUNT){
        return TYPE[index];
    }
    return NULL;
}

char * get_operand_type(struct symbol_table_t * table, char *name){
    int index = lookup_symbol(table, name);
    if(index >= 0){
        struct stack_t * stack = &(table->_symbols);
        struct symtb_entry_t entry = _stack_get(stack, struct symtb_entry_t, index);
        printf("IDENT (name=%s, address=%d)\n", entry._name, entry._address);
        if(strcmp(entry._type, "array") == 0){
            return entry._element_type;
        }
        return entry._type;
    } else {
        printf("error:%d: undefined variable %s\n", yylineno, name);
        return NULL; 
    }
}

bool push_operator(struct stack_t * operators_stack, struct stack_t * operands_stack,  char * oper, int prec, bool is_unary){
    int i = 0;
    struct operator_t top;
    
    if(prec >= 0){
        for(i = operators_stack->_idx - 1; i >= 0; --i){
            top = _stack_top(operators_stack, struct operator_t);
            if(top._precedence >= prec){
                printf("%s\n", top._name); 
                pop_operator(operators_stack, operands_stack);
            }else
                break;
        }
    }

    top._name = oper;
    top._precedence = prec;
    top._is_unary = is_unary;
    _stack_push(operators_stack, struct operator_t, top); 
    return true;
}

void pop_operator(struct stack_t * operators_stack, struct stack_t * operands_stack){
    struct operator_t operator_top;
    operator_top = _stack_top(operators_stack, struct operator_t);
    int type1, type2, type;
    char * operand1, *operand2, *operand;

    if(!operator_top._is_unary){ // operator is binary, pop two operands
        operand1 = _stack_top(operands_stack, char *);
        type1 = check_type(operand1);
        _stack_pop(operands_stack);

        operand2 = _stack_top(operands_stack, char *);
        type2 = check_type(operand2);
        _stack_pop(operands_stack);
        if(operator_top._precedence >= 3){ // arithmetic, add_op, mul_op
            if (type1 >= 2 && type2 >= 2) // type1 and type2 is int or float
                   type = type1 > type2 ? type1 : type2;           
             else if ( type1 == type2) 
                type = type1;
             else if( type1 < 0 || type2 < 0){
                type = -1;
                // yyerror("Semantic Error : type error!");  
             }
            // if( type1 < 0 || type2 < 0){
            //     type = -1;
            // }else if (type != type2){
            //        
            // }
            
        } else if(operator_top._precedence == 2){ // cmp_op
            // operand1 and operand2 should be int or float
            if(type1 >= 2 && type2 >= 2)
                type = 1; // type is bool
            else
                type = -1;
                // yyerror("Semantic Error : type error!");
        } else { // AND OR
            if(type1 != 1 && type2 != 1)
                yyerror("Semantic Error : type error!");
            type = 1;
        }
        
       operand = get_type(type);
       _stack_push(operands_stack, char *, operand); // push back
    } 
    _stack_pop(operators_stack);
}

char * pop_operators(struct stack_t * operators_stack, struct stack_t * operands_stack){
    struct operator_t top;
    while(operators_stack->_idx){
        top = _stack_top(operators_stack, struct operator_t);
        if(strcmp(top._name, "LP") == 0){
            operators_stack->_idx -= 1;
            break;
        }
        printf("%s\n", top._name);
        pop_operator(operators_stack, operands_stack);
    }

    char * type = _stack_top(operands_stack,char *);
    // printf("type = %s\n", type);
    
    return (type == NULL ? "" : type);
}


void new_scope(struct symbol_table_t * table){
    table->_current_scope += 1;
}

void delete_scope(struct symbol_table_t * table){
    int scope_level = table->_current_scope;
    table->_current_scope -= 1;
    printf("> Dump symbol table (scope level: %d)\n", scope_level);
    printf("%-10s%-10s%-10s%-10s%-10s%s\n", "Index", "Name", "Type", "Address", "Lineno",
    "Element type");
    struct stack_t * stack = &(table->_symbols);
    struct symtb_entry_t entry;
    int pop_amount = 0;
    for(int i = 0; i < table->_symbols._idx; ++i){
        entry = _stack_get(stack, struct symtb_entry_t, i);
        if(entry._scope == scope_level){
            pop_amount += 1;
            printf("%-10d%-10s%-10s%-10d%-10d%s\n", entry._idx, entry._name, entry._type, entry._address, entry._line_no, entry._element_type);
        }
    } 
    table->_symbols._idx -= pop_amount;
}

void insert_symbol(struct symbol_table_t * table, char *name, char *type, int line_no, char * element_type){
    struct symtb_entry_t top;
    struct symtb_entry_t entry;
    struct stack_t * stack = &(table->_symbols);
    
    // check if re-define symbol
    int retval = lookup_symbol(table, name);
    if (retval >= 0){
        entry = _stack_get(stack, struct symtb_entry_t, retval);
        if(entry._scope == table->_current_scope){
            yyerror("Error : re-define symbol\n");
            return ;
        }
    }

    if(table->_symbols._idx == 0){ // stack is empty
        entry._idx = 0; 
    } else { // stack isn't empty
        top = _stack_top(stack, struct symtb_entry_t);
        if( top._scope != table->_current_scope){
            entry._idx = 0;
        }else
            entry._idx = top._idx + 1;
    }

    entry._name = name;
    entry._type = type;
    entry._address = table->_address;
    entry._line_no = line_no;
    entry._element_type = element_type;
    entry._scope = table->_current_scope;
    printf("> Insert {%s} into symbol table (scope level: %d)\n", entry._name, entry._scope);
    table->_address += 1;
    _stack_push(stack, struct symtb_entry_t, entry);
}

int lookup_symbol(struct symbol_table_t * table, char *name){
    int idx = table->_symbols._idx;
    struct symtb_entry_t entry;
    struct stack_t * stack = &(table->_symbols);
    for(int i = idx - 1; i >= 0; --i){
        entry = _stack_get(stack, struct symtb_entry_t, i);
        if(strcmp(entry._name, name) == 0){
            return i;
        }
    }

    return -1;
}

void conversion(struct stack_t * operands_stack, char * new_type){
    char * top = _stack_top(operands_stack, char *);
    printf("%C to %C\n", top[0] ^ 0x20, new_type[0] ^ 0x20);
    pop_operand(operands_stack);
    push_operand(operands_stack, new_type);
}

/* C code section */
int main(int argc, char *argv[])
{
    if (argc == 2) {
        yyin = fopen(argv[1], "r");
    } else {
        yyin = stdin;
    }

    // TYPE = {
    //    "string", "bool", "int", "float"
    // };


    bool retval = initialize_operator_stack(&operators_stack);
    retval &= initialize_operand_stack(&operands_stack); 
    retval &= initialize_symbol_table(&table);

    if (!retval)
        exit(-1);

    yyparse();

	printf("Total lines: %d\n", yylineno);
    fclose(yyin);
    return 0;
}
