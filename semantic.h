#ifndef SEMANTIC_H_INCLUDED
#define SEMANTIC_H_INCLUDED

typedef enum {VAR,FUNC,PROTO,ARRAY,INTEGER,CHARACTER,STRING,VOI,EXT,ENTER,PARAM} Type;
typedef enum {F,EXPR,BOOL,W,IFS,FUNCTION,I,CONSTANT,ASSG,LIST} nodeType;
typedef enum {ADD,SUB,MULT,DIV,UMINUS} Op;
typedef enum {D,NE,AN,R,G,GE,L,LE,NOT} Boolop;
typedef enum {IN,CH,ST} constantType;
typedef enum {INTI,CHARI,EXPRI,ASSGI,STRI,FUNCI,PARAMI} instr_type;


typedef struct symTable{
  int description;
  int type;
  int iVal;
  char charVal;
  char *strVal;
  char *id;
  struct symTable *next;
  struct formalParam *first;
  int size;
  int offset;
  int scope;
} symTabNode;

typedef struct formalParam{
  int type;
  struct formalParam *next;
} paramNode;


typedef struct stack{
  struct symTable *table;
  struct stack *next;
} stackNode;

typedef struct ifStatment{
  struct tree * condition;
  struct tree * if_branch;
  struct tree * else_branch;
}if_node;

typedef struct expression{
  Op op;
  struct tree *left;
  struct tree *right;
  struct codeNode * code;
  struct symTabNode * place;
}expr_node;

typedef struct bool{
  Boolop op;
  struct tree *left;
  struct tree *right;
}bool_node;

typedef struct constant{
  constantType t;
  int iVal;
  char cVal;
  char * sVal;
}con_node;

typedef struct assignment{
  struct symTable * symbol;
  struct tree * value;
}assign_node;

typedef struct whileStatement{
  struct tree * condition;
  struct tree * while_branch;
}while_node;

typedef struct forStatement{
  struct tree * assignment;
  struct tree * condition;
  struct tree * expression;
  struct tree * body;
}for_node;

typedef struct functionCall{
  struct tree * arguments;
  struct symTable * symbol;
}function_node;

typedef struct id{
  struct symTable * symbol;
}id_node;

typedef struct expr_instr{
  int opType;
  struct symTable * src1;
  struct symTable * src2;
  struct symTable * dest;
}expr_instr_node;

typedef struct int_instr
{
  struct symTable * dest;
  int src;
}int_instr_node;

typedef struct char_instr
{
  struct symTable * dest;
  char src;
}char_instr_node;
typedef struct str_instr
{
  struct symTable * dest;
  char* src;
}str_instr_node;
typedef struct param_instr{
  struct symTable * ref;
  char * label;
  int i;
  char c;
  char * s;
  int type;
}param_instr_node;

typedef struct assign_instr
{
  struct symTable * dest;
  struct symTable * src;

}assign_instr_node;

typedef struct func_instr{
  struct symTable * function; 
  struct symTable * returnAdd;
}funct_instr_node;

typedef struct instructions{
  union{
     struct expr_instr e;
     struct int_instr i;
     struct char_instr c;
     struct assign_instr assign;
     struct func_instr fc;
     struct str_instr s;
     struct param_instr p;
  }instr;
  struct instructions * next;
  struct instructions * last;
  instr_type type;
}instruction_node;

typedef struct tree{
  int node_type;
  union {
    struct expression e;
    struct forStatement f;
    struct whileStatement w;
    struct constant c;
    struct bool b;
    struct assignment a;
    struct ifStatment i;
    struct functionCall fc;
    struct id s;
  }u;
  struct instructions * code;
  struct symTable * place;
} tree_node;
/*
struct neg_node{
  int node_type;
  struct expr_node *left;
  struct codeNode * code;
  struct symTabNode * place;
};

struct not_node{
  int node_type;
  struct expr_node *left;
 struct codeNode * code;
  struct symTabNode * place;
};

struct and_node{
  int node_type;
  struct expr_node *left;
  struct expr_node *right;
  struct codeNode * code;
  struct symTabNode * place;
};

struct or_node{
  int node_type;
  struct expr_node *left;
  struct expr_node *right;
  struct codeNode * code;
  struct symTabNode * place;
};

struct gt_node{
  int node_type;
  struct expr_node *left;
  struct expr_node *right;
  struct codeNode * code;
  struct symTabNode * place;
};

struct gte_node{
  int node_type;
  struct expr_node *left;
  struct expr_node *right;
  struct codeNode * code;
  struct symTabNode * place;
};

struct lt_node{
  int node_type;
  struct expr_node *left;
  struct expr_node *right;
  struct codeNode * code;
  struct symTabNode * place;
};

struct d_node{
  int node_type;
  struct expr_node *left;
  struct expr_node *right;
  struct codeNode * code;
  struct symTabNode * place;
};

struct n_node{
  int node_type;
  struct expr_node *left;
  struct expr_node *right;
  struct codeNode * code;
  struct symTabNode * place;
};

struct lte_node{
  int node_type;
  struct expr_node *left;
  struct expr_node *right;
  struct codeNode * code;
  struct symTabNode * place;
};


struct integer{
  int node_type;
  int value;
  struct codeNode * code;
  struct symTabNode * place;
};

struct character{
  int node_type;
  char valueC;
  struct codeNode * code;
  struct symTabNode * place;
};

struct string{
  int node_type;
  char * valueS;
  struct codeNode * code;
  struct symTabNode * place;
};

struct if_node {
  int node_type;
  struct expr_node * condition;
  struct expr_node * if_branch;
  struct expr_node * else_branch;
  struct codeNode * code;
  struct symTabNode * place;
};

struct while_node {
  int node_type;
  struct expr_node * condition;
  struct expr_node * while_branch;
  struct codeNode * code;
  struct symTabNode * place;
};

struct id_node{
  int node_type;
  struct symTable * symbol;
  struct codeNode * code;
  struct symTabNode * place;
};

struct for_node{
  int node_type;
  struct expr_node * assignment;
  struct expr_node * expression1;
  struct expr_node * expression2;
  struct expr_node * statement;
  struct codeNode * code;
  struct symTabNode * place;
};

struct assign_node{
  int node_type;
  struct symTable * symbol;
  struct expr_node * value;
  struct codeNode * code;
  struct symTabNode * place;
};

struct idf_node{
  int node_type;
  struct symTable * symbol;
  struct expr_node * arguments;
  struct codeNode * code;
  struct symTabNode * place;
};

struct ref_node{
  int node_type;
  struct symTable * symbol;
  struct codeNode * code;
  struct symTabNode * place;
};
*/

typedef struct list_node{
  struct tree * root;
  struct list_node * next;
}listNode;
#endif 




