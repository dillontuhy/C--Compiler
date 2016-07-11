%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "semantic.h"
extern int yylex();
extern char *yytext;
extern int yylineno;
void yyerror();
symTabNode *table;
stackNode *state;
paramNode *first;
listNode *head;
void addToTable(Type type, int iVal, char charVal, char *strVal, char *name);
int checkTable();
void changeDesc(char* name, Type returntype);
void addFuncParams();
void addToParams();
void printTable();
void checkVoid(char *name);
symTabNode *makeTable();
int checkParams(symTabNode *first, symTabNode *second);
void pop();
void push();
/*void assignIdValue(char *name, struct expr_node *calc);*/
/*struct expr_node *checkType( struct expr_node *expr1, struct expr_node *expr2, char op);*/
tree_node * makeBoolNode(Boolop bo, tree_node * expr1, tree_node * expr2); //boolean
tree_node * makeExprNode(Op op, tree_node * expr1, tree_node * expr2); //expr
tree_node * makeIntNode(int i); //constant
tree_node * makeCharNode(char c); //constant
tree_node * makeStrNode(char * s); //constant
tree_node * makeIDNode(symTabNode * symbol); //id
tree_node * makeForNode(tree_node * assignment, tree_node * expr1, tree_node * expr2,  tree_node * statement); //for
tree_node * makeWhileNode(tree_node * condition, tree_node * while_branch); //while
tree_node * makeIfNode(tree_node * condition, tree_node * if_branch, tree_node * else_branch);//if
tree_node * makeFuncNode(symTabNode * symbol, tree_node * arguments);//Functions
tree_node * makeAssignNode(symTabNode * symbol, tree_node * value); //Assignment node
instruction_node * newExprIntr(Op opType, symTabNode * src1,symTabNode * src2, symTabNode * dest);
instruction_node * newAssgnIntr(symTabNode * dest, symTabNode * src);
instruction_node * newCharIntr(symTabNode * dest, char src);
instruction_node * newIntIntr(symTabNode * dest, int src);
instruction_node * newFuncIntr(symTabNode * functionCall, symTabNode * ret);
instruction_node * newStrIntr(symTabNode * dest, char * src);
instruction_node * newParamIntr(int type, char * label,symTabNode * ref, int i, char c , char * s);
char * newLabel();
void codeGenExprList(tree_node * node);
symTabNode * newTemp(int t);
int tempVariablesSize();
void enterFunc(symTabNode * function);
void makeGlobals();

symTabNode * findSymbolFromID(char * name);
/*struct expr_node * makeIDFNode(char * name, struct expr_node * arguments);*/
void appendStmtNode(tree_node * stmt);
void printFunc();
void printCode(tree_node * root);


int tVal=0;
int desc=0;
int size=0;
int err_occurred=0;
int scope= 0;
int returnVal=VOI;
int tempCounter=0;
int paramOffsetCounter = 0;
int labelCounter = 0;


%}

%union{
	int iVal;
	char charVal;
  	char *strVal;
	char *idName;
	int valType;
	struct tree * tree_node;
	struct list_node * list;
}

%token <iVal>INTCON <charVal>CHARCON <strVal>STRINGCON  
%token <idName>ID CHAR INT
%token COLON EQ NEQ OR AND DEQ LT LTE GT GTE 
%token LEFTBRACK RIGHTBRACK LEFTPAREN RIGHTPAREN EXTERN VOID LEFTCURL RIGHTCURL IF ELSE RETURN FOR WHILE
%start prog
%left OR
%left AND
%left DEQ NEQ
%left LT LTE GT GTE
%left '+' '-'
%left '*' '/'
%right '!'

%type <valType> type 
%type <idName> addID
%type <tree_node> expr stmt assg more_expr more_stmt more_id
%type <list> body

%%
prog	:	func {makeGlobals();}
		|   dcl  {makeGlobals();}
		|   func more_prog {makeGlobals();}
		|   dcl more_prog  {makeGlobals();}
		|
		;
more_prog : func
		  | dcl
		  | func more_prog
		  | dcl  more_prog
		  ;

dcl	: type var_decl                                                     
	| EXTERN type addID LEFTPAREN push parm_types RIGHTPAREN pop addparams COLON {changeDesc($3, EXT); checkTable();}		 			 
	| EXTERN type addID LEFTPAREN push parm_types RIGHTPAREN pop addparams ',' more_dcl COLON  {changeDesc($3, EXT); checkTable();}
	| type addID LEFTPAREN push parm_types RIGHTPAREN pop addparams ',' more_dcl COLON         {changeDesc($2, PROTO); checkTable();}
	| type addID LEFTPAREN push parm_types RIGHTPAREN pop addparams COLON                      {changeDesc($2, PROTO); checkTable();}
	| error COLON
 	;

addID : ID {addToTable(tVal,0,' '," ",$1); $$ = $1;}

addparams: /* epsilon */ {addFuncParams();};


more_dcl: addID LEFTPAREN push parm_types RIGHTPAREN pop addparams     {changeDesc($1,PROTO); checkTable();}
        | addID LEFTPAREN push parm_types RIGHTPAREN pop addparams ',' more_dcl  {changeDesc($1,PROTO); checkTable();}
        ;

var_decl	: 	addID COLON                                      {changeDesc($1,VAR); checkTable();}
			|   addID ',' var_decl			                     {changeDesc($1,VAR); checkTable();}
			|	addID LEFTBRACK INTCON RIGHTBRACK COLON          {changeDesc($1,ARRAY); checkTable();}
			|  	addID LEFTBRACK INTCON RIGHTBRACK ',' var_decl   {changeDesc($1,ARRAY); checkTable();} 
			;

parm_types /* we are inside of a function declaration, or a function definitition so we push our stack */
	:	type                            
 	|	type addID                        {addToParams($1); changeDesc($2,PARAM); checkTable();} 
 	|   type addID LEFTBRACK RIGHTBRACK   {addToParams($1); changeDesc($2,PARAM); checkTable();}
 	|   type addID LEFTBRACK RIGHTBRACK ',' more_parm {addToParams($1); changeDesc($2,PARAM); }
 	|   type addID ',' more_parm                      {addToParams($1); changeDesc($2,PARAM); }
 	;

more_parm /* all in same scope as the other parameters */
	:   type addID                                        {addToParams($1); changeDesc($2,PARAM);   checkTable();}
	|   type addID ',' more_parm                          {addToParams($1); changeDesc($2,PARAM);   checkTable();}
	| 	type addID LEFTBRACK RIGHTBRACK                   {addToParams($1); changeDesc($2,PARAM); checkTable();}
	|   type addID LEFTBRACK RIGHTBRACK ',' more_parm     {addToParams($1); changeDesc($2,PARAM); checkTable();}
	;

func	/* when we shift into our para_types we are pushing a new scope, this is the same scope as our interior values of our function definition*/
	:	type addID LEFTPAREN push parm_types RIGHTPAREN addparams LEFTCURL body {enterFunc(findSymbolFromID($2));} RIGHTCURL pop  {changeDesc($2,FUNC);}
	|   error RIGHTCURL
 	;
/*We are going to create a list of list_nodes which contain stmts and a reference to the next stmt node, thus we are going to be returning list nodes*/
body    : stmt               {appendStmtNode($1);}    
		| type var_decl        {$$ =NULL;}  
		| stmt body          {appendStmtNode($1);}    
	    | type var_decl body    {$$ =NULL;}  
		;

type: CHAR {$$ = CHARACTER; tVal=CHARACTER;}
	| INT  {$$ = INTEGER;   tVal=INTEGER;}
	| VOID {$$ = VOID;      tVal=VOI;}	
	;

stmt	
	:   IF LEFTPAREN expr RIGHTPAREN stmt ELSE stmt       				{$$ = makeIfNode($3,$5,$7);}
	|	IF LEFTPAREN expr RIGHTPAREN stmt  				  				{$$ = makeIfNode($3,$5,NULL);}
 	|	WHILE LEFTPAREN expr RIGHTPAREN stmt              				{$$ = makeWhileNode($3,$5);}
 	|	FOR LEFTPAREN assg  COLON  expr  COLON assg  RIGHTPAREN stmt    {$$ = makeForNode($3,$5,$7,$9);}
 	|	FOR LEFTPAREN  COLON expr  COLON  assg  RIGHTPAREN stmt        	{$$ = makeForNode(NULL,$4,$6,$8);}
 	|	FOR LEFTPAREN  COLON  COLON assg RIGHTPAREN stmt                {$$ = makeForNode(NULL,NULL,$5,$7);}
 	|	FOR LEFTPAREN assg  COLON COLON assg RIGHTPAREN stmt            {$$ = makeForNode($3,NULL,$6,$8);}
 	|	FOR LEFTPAREN assg  COLON COLON RIGHTPAREN stmt 				{$$ = makeForNode($3,NULL,NULL,$7);}
 	|	FOR LEFTPAREN COLON  expr  COLON RIGHTPAREN stmt                {$$ = makeForNode(NULL,$4,NULL,$7);}
 	|	FOR LEFTPAREN COLON COLON RIGHTPAREN stmt 						{$$ = makeForNode(NULL,NULL,NULL,$6);}
 	|	FOR LEFTPAREN assg  COLON  expr  COLON RIGHTPAREN stmt          {$$ = makeForNode($3,$5,NULL,$8);}
 	|  	RETURN COLON                                     				{$$ = NULL;}
 	|   RETURN expr COLON												{$$ = NULL;}
 	|	assg COLON     									                {$$ = $1;}
 	|	ID LEFTPAREN more_expr RIGHTPAREN COLON     					{$$ = makeFuncNode(findSymbolFromID($1),$3);}
 	|   ID LEFTPAREN RIGHTPAREN COLON                    				{$$ = makeFuncNode(findSymbolFromID($1),NULL);}
 	|	LEFTCURL more_stmt RIGHTCURL									{$$ = $2;}
 	|	COLON                                            				{$$ = NULL;}
 	|   error RIGHTCURL													{$$ = NULL;}
 	|   error RIGHTPAREN												{$$ = NULL;}
 	|   error COLON														{$$ = NULL;}
 	;

more_stmt :  stmt more_stmt												{$$ = $1;}
		  |																{$$ = NULL;}
		  ; 

assg :	ID LEFTBRACK expr RIGHTBRACK EQ expr                            {$$ = NULL;}
	|   ID EQ expr                       								{$$ = makeAssignNode(findSymbolFromID($1),$3);}
	;


expr 
	:	'-' expr  %prec '*'   {$$ = makeExprNode(UMINUS,$2,NULL);}
	|   '!' expr              {$$ = makeBoolNode(NOT,$2,NULL);}
 	|	expr DEQ expr         { $$ = makeExprNode(DEQ,$1,$3);}
 	|	expr NEQ expr         { $$ = makeExprNode(NEQ,$1,$3);}
 	|   expr LTE expr         { $$ = makeExprNode(LTE,$1,$3);}
 	|	expr LT expr 	      { $$ = makeExprNode(LT,$1,$3);}
 	|   expr GTE expr    	  { $$ = makeExprNode(GTE,$1,$3);}
 	|   expr GT expr     	  { $$ = makeExprNode(GT,$1,$3);}
 	|   expr AND expr         { $$ = makeExprNode(AND,$1,$3);}
 	|	expr OR expr     	  { $$ = makeExprNode(OR,$1,$3);}
 	|   expr '+' expr         { $$ = makeExprNode(ADD,$1,$3);}
 	|	expr '-' expr    	  { $$ = makeExprNode(SUB,$1,$3);}
 	|	expr '*' expr         { $$ = makeExprNode(MULT,$1,$3);}
 	|	expr '/' expr         { $$ = makeExprNode(DIV,$1,$3);}
 	|	LEFTPAREN expr RIGHTPAREN {$$ = $2;}
 	|   more_id     {$$ = $1;}     		  
 	|	INTCON      {$$ = makeIntNode($1);}     
 	|	CHARCON     {$$ = makeCharNode($1);}     
 	|	STRINGCON	{$$ = makeStrNode($1);}	 
 	|   ID          {$$ = makeIDNode(findSymbolFromID($1));}
 	;
more_expr :   expr      
		  |   expr ',' more_expr  {$$ = makeExprNode(LIST,$1,$3);}
		  ;

more_id : ID LEFTPAREN more_expr RIGHTPAREN  	   {$$ = makeFuncNode(findSymbolFromID($1),$3);}
		| ID LEFTPAREN RIGHTPAREN 				   {$$ = makeFuncNode(findSymbolFromID($1),NULL);}
		| ID LEFTBRACK expr RIGHTBRACK             {$$ = NULL;}
		;
push :/*epsilon*/   {push();} 
	 ;

pop : /*epsilon*/   {pop();} 
	;


 %%

int main(){
	printf("fn_print_int:\n");
	printf("\tli   $v0, 1\n");
	printf("\tlw   $a0, 0($sp)\n");    
	printf("\tsyscall\n");         
	printf("\tjr $ra\n");
	printf("fn_print_string:\n");
	printf("\tli   $v0, 4\n");
	printf("\tlw   $a0, 0($sp)\n");
	printf("\tsyscall\n");
	printf("\tjr $ra\n");

  yydebug = 0;  /* set to 1 to debug the parser */
  table = malloc(sizeof(symTabNode));
  table->next = NULL;
  table->iVal =0;
  table->charVal= ' ';
  table->strVal = " ";
  table->type = VOI;
  table->id = "$";
  table->first = NULL;
  table->size = 0;
  state = malloc(sizeof(stackNode));
  state->table = table;
  state->next=NULL;
  first = NULL;
  head = NULL;
  yyparse();
  return err_occurred;
}

void yyerror(char *s){
  if( yytext[0] == 0){
      fprintf(stderr, "%d, End of File found too early\n", yylineno);
   }
  else{ 
  	fprintf(stderr, "Syntax error on line %d ", yylineno);
  	fprintf(stderr, "Error token %s", yytext);
  	fprintf(stderr, "\n");
  }
  err_occurred = 1;
  return;
}

void addToTable(Type type, int iVal, char charVal, char *strVal, char *name){
	/*printf("calling addToTable\n");*/
    //printf("We're adding id= %s symbol to table in scope %d\n", name, scope);
  	symTabNode *tmpNode;
  	tmpNode = malloc(sizeof(symTabNode));
  	tmpNode->type = type;
  	tmpNode->iVal = iVal;
  	tmpNode->charVal = charVal;
  	tmpNode->strVal = strVal;
  	tmpNode->id = strdup(name);
  	tmpNode->next = table;
  	tmpNode->first = NULL;
  	tmpNode->scope = scope;
  	state->table = tmpNode;
  	table = tmpNode;
    //printTable();
}

int checkTable(){
	/*printf("calling checkTable\n"); */
	symTabNode *outer;
	symTabNode *inner;
	outer = table;
	inner = outer->next;
		while(strcmp(inner->id, "$") != 0){
			/*printf("We are comparing ID's %s and %s and descriptions %d and %d \n", outer->id, inner->id, outer->description, inner->description);*/
			if(strcmp(outer->id, inner->id) == 0){ /* if the ID's are equal*/
				if(outer->description == FUNC && inner->description == PROTO){/*correct order check params*/
					 if(outer->type != inner->type){
					 	fprintf(stderr, "Return type of function and prototype do not match %d \n", yylineno);
					 	table = table->next;
					 	err_occurred=1;
					 }
					 else if(checkParams(outer,inner) == 1){
					    fprintf(stderr, "Parameters did not match on line %d \n", yylineno);
					    table = table->next;
					    err_occurred=1;
					 }
				}else if(outer->description == FUNC && inner->description == EXT){
					fprintf(stderr, "External function can not be defined on line %d \n", yylineno);
					table= table->next;
					err_occurred=1;
				}
				else if(outer->description == PROTO ){
					fprintf(stderr, "A prototype must come before a definition on line  %d \n", yylineno);
					table= table->next;
					err_occurred=1;
				} 
				else{
				    fprintf(stderr, "Identifier already in use on line %d \n", yylineno);
				    table = table->next;
				    err_occurred=1;
				}
			}
		inner= inner->next;
		}
  return 0;
}


void push(){
  scope = scope + 1;
  stackNode *tmpstate;
  tmpstate = malloc(sizeof(stackNode));
  tmpstate->table = makeTable();
  tmpstate->next  = state;
  state = tmpstate;
  table = tmpstate->table;
}

void pop(){
  scope = scope - 1;
  state = state->next;
  table = state->table;  
  paramOffsetCounter = 0;
  tempCounter = 0;
  head=NULL;
}

symTabNode *makeTable(){
  /*printf("calling makeTable\n");*/
  symTabNode *tmptable = malloc(sizeof(symTabNode));
  tmptable->next = NULL;
  tmptable->iVal =0;
  tmptable->charVal= ' ';
  tmptable->strVal = " ";
  tmptable->type = VOI;
  tmptable->id = "$";
  return tmptable;
}

void printTable(){
	/*printf("calling printTable\n");*/
	symTabNode *symTabPtr;
	symTabPtr = table;
	while( symTabPtr != NULL ){
      printf("%s " , symTabPtr->id);
      printf(" offset = %d ", symTabPtr->offset);
      if (symTabPtr->first != NULL){
      	paramNode *paramPtr;
		paramPtr = symTabPtr->first;
		while(paramPtr != NULL ){
             printf("%d ", paramPtr->type);
             paramPtr = paramPtr->next;
		}
      }
      printf("\n"); 
    symTabPtr = symTabPtr->next;
    }
}

void addToParams(Type type){
	/*printf("added a parameter of type %d \n", type);*/
     paramNode *temp = malloc(sizeof(paramNode));
     size = size + 1;
     temp->type = type;
     temp->next = first;
     first = temp; 
}

void addFuncParams(){
	/*printf("We are adding paramters to our table value %s \n", table->id);*/
	if (scope > 0){
		symTabNode * tmp = malloc(sizeof(symTabNode));
		tmp = state->next->table;
		tmp->first = first;
		tmp->size  = size;
		size = 0;
		first = NULL; 
	}else {
		table->first = first;
		table->size = size;
		size = 0;
		first = NULL;
   	}
	
}


/*
Look up the ID value, and add the desired type to the identifier:
possible options are VAR, ARRAY, PROTO, FUNC, EXT.
*/
void changeDesc(char* name, Type returntype){
	printf("calling changeDesc, on ID %s , with type %d, and scope %d\n", name, returntype, scope);
	symTabNode *symTabPtr;
	symTabPtr = table;
	while(strcmp(symTabPtr->id, name) != 0){
		symTabPtr= symTabPtr->next;
	}
	symTabPtr->description = returntype;
	if(returntype == PARAM){
		symTabPtr->offset = 8 * paramOffsetCounter;
		paramOffsetCounter = paramOffsetCounter + 1;
	}else if(returntype == VAR){
			if(tempCounter==0){
				symTabPtr->offset = (-4);
			}else{
				symTabPtr->offset = (-4*tempCounter);
			}
		tempCounter = tempCounter + 1;
	}
}

int checkParams(symTabNode *first, symTabNode *second){
	/*printf("calling checkParams\n");*/
	paramNode *fptr;
	paramNode *sptr;
	fptr = first->first;
	sptr = second->first;
	int flag = 0;
	if(first->size != second->size){
		flag = 1;
	}else{
		while(fptr != NULL && sptr != NULL){
			if(fptr->type != sptr->type){
			   flag = 1;
			}
			fptr= fptr->next;
			sptr=sptr->next;
		}
	}
	return flag;
}
//For regular expression
tree_node * makeExprNode(Op opt, tree_node * expr1, tree_node * expr2){
	tree_node * tmp = 
			malloc(sizeof(tree_node));
	switch(opt){
		case ADD:
		case SUB:
		case MULT:
		case DIV:
			tmp->node_type = EXPR;
			tmp->u.e.left = expr1;
			tmp->u.e.right = expr2;
			tmp->u.e.op = opt;
			return tmp;
			break;
		case UMINUS:
			tmp->node_type = EXPR;
			tmp->u.e.left = expr1;
			tmp->u.e.op = opt;
			return tmp;
			break;
	} 
	return tmp;
}

//For boolean expression
tree_node * makeBoolNode(Boolop bo, tree_node * expr1, tree_node * expr2){
	tree_node * tmp = malloc(sizeof(tree_node));
	switch(bo){
		case D:
		case NE:
		case AN:
		case R:
		case G:
		case GE:
		case L:
		case LE:
			tmp->node_type = BOOL;
			tmp->u.b.left = expr1;
			tmp->u.b.right = expr2;
			tmp->u.e.op = bo;
			return tmp;
			break;
		case NOT:
			tmp->node_type = BOOL;
			tmp->u.b.left = expr1;
			tmp->u.e.op = bo;
			return tmp;
			break;
	}
	return tmp;
}
//int constant
tree_node * makeIntNode(int i){
	tree_node * tmp = malloc(sizeof(tree_node));
	tmp->node_type = CONSTANT;
	tmp->u.c.iVal = i;
	tmp->u.c.t = IN;
	return tmp;
}
//char constant
tree_node * makeCharNode(char  c){
	tree_node * tmp = malloc(sizeof(tree_node));
	tmp->node_type = CONSTANT;
	tmp->u.c.cVal = c;
	tmp->u.c.t = CH;
	return tmp;
}

//string constant
tree_node * makeStrNode(char * s){
	tree_node * tmp = malloc(sizeof(tree_node));
	tmp->node_type = CONSTANT;
	tmp->u.c.sVal = s;
	tmp->u.c.t= ST;
	return tmp;
}

//make ID node
tree_node * makeIDNode(symTabNode * symbol){
   tree_node * tmp = malloc(sizeof(tree_node));
   tmp->node_type=I;
   tmp->u.s.symbol = symbol; // assigns the symbol to the node
   return tmp;
}
//For Node
tree_node * makeForNode(tree_node * assignment, tree_node * expr1, tree_node * expr2,  tree_node * statement){
	tree_node * tmp = malloc(sizeof(tree_node));
	tmp->node_type=FOR;
	tmp->u.f.assignment = assignment;
	tmp->u.f.condition = expr1;
	tmp->u.f.expression = expr2;
	tmp->u.f.body = statement;
	return tmp;
}
//While node
tree_node * makeWhileNode(tree_node * condition, tree_node * while_branch){
	tree_node * tmp = malloc(sizeof(tree_node));
	tmp->node_type=WHILE;
	tmp->u.w.condition=condition;
	tmp->u.w.while_branch=while_branch;
	return tmp;
}
//If Node
tree_node * makeIfNode(tree_node * condition, tree_node * if_branch, tree_node * else_branch)
{
  tree_node * tmp = malloc(sizeof(tree_node));
  tmp->node_type = IFS;
  tmp->u.i.condition = condition;
  tmp->u.i.if_branch = if_branch;
  tmp->u.i.else_branch = else_branch;
  return tmp;
}
//Function call
tree_node * makeFuncNode(symTabNode * symbol, tree_node * arguments){
	//printf("made a function node with id %s\n",symbol->id);
	tree_node * tmp = malloc(sizeof(tree_node));
	tmp->node_type = FUNCTION;
	tmp->u.fc.arguments=arguments;
	tmp->u.fc.symbol=symbol;
	return tmp;
}
tree_node * makeAssignNode(symTabNode * symbol, tree_node * value){
	//printf("called makeAssignNode\n");
	tree_node * tmp = malloc(sizeof(tree_node));
	tmp->node_type = ASSG;
	tmp->u.a.symbol = symbol;
	tmp->u.a.value = value;
	return tmp;
}


symTabNode * findSymbolFromID(char * name){
	//printf("searching the table for %s\n",name);
	stackNode *stack = state;
	while(stack != NULL){
		symTabNode *symTabPtr;
		symTabPtr = stack->table;
		while( symTabPtr != NULL ){ 
			if( strcmp(symTabPtr->id, name) == 0){
				return symTabPtr;
			}
			symTabPtr = symTabPtr->next;
		}
		
		stack = stack->next;
	}
	fprintf(stderr, "Unable to find the ID from the symbol table\n");
	err_occurred = 1;
	return NULL;
}

void appendStmtNode(tree_node * stmt){
	/*Right now we want to add stmt to our list given head*/
	//printf("We are adding a node to our list, of type %d\n", stmt->node_type);
	listNode * tmp = malloc(sizeof(listNode));
	tmp->root = stmt;
	tmp->next = head;
	head = tmp;
}

/*The idea here is that we want to print out the correct number of childern for each node in our tree */
void printSyntaxTree(tree_node * node, int depth){
	int i;
	for( i = 0; i < depth; i++ ){
		printf("   ");
	}
	switch(node->node_type){
		case EXPR:
			switch(node->u.e.op){
				case ADD:
					printf("+{\n");
					printSyntaxTree( node->u.e.left, depth + 1);
					printSyntaxTree( node->u.e.right, depth + 1);
					for( i = 0; i < depth; i++ ){
						printf("   ");
					}
					printf("}\n");
				break;
				case SUB:
					printf("-{\n");
					printSyntaxTree( node->u.e.left, depth + 1);
					printSyntaxTree( node->u.e.right, depth + 1);
					for( i = 0; i < depth; i++ ){
						printf("   ");
					}
					printf("}\n");
					break;
				case MULT:
					printf("*{\n");
					printSyntaxTree( node->u.e.left, depth + 1);
					printSyntaxTree( node->u.e.right, depth + 1);
					for( i = 0; i < depth; i++ ){
						printf("   ");
					}
					printf("}\n");
					break;
				case DIV:
					printf("/{\n");
					printSyntaxTree( node->u.e.left, depth + 1);
					printSyntaxTree( node->u.e.right, depth + 1);
					for( i = 0; i < depth; i++ ){
						printf("   ");
					}
					printf("}\n");
					break;
				case UMINUS:
					printf("UMINUS{\n");
					printSyntaxTree( node->u.e.left, depth + 1);
					for( i = 0; i < depth; i++ ){
						printf("   ");
					}
					printf("}\n");
					break;
			}
			break;

		case BOOL:
			switch(node->u.b.op){
				case D:
					printf("=={\n");
					printSyntaxTree( node->u.b.left, depth + 1);
					printSyntaxTree( node->u.b.right, depth + 1);
					for( i = 0; i < depth; i++ ){
						printf("   ");
					}
					printf("}\n");
					break;
				case NE:
					printf("!={\n");
					printSyntaxTree( node->u.b.left, depth + 1);
					printSyntaxTree( node->u.b.right, depth + 1);
					for( i = 0; i < depth; i++ ){
						printf("   ");
					}
					printf("}\n");
					break;

				case G:
					printf(">{\n");
					printSyntaxTree( node->u.b.left, depth + 1);
					printSyntaxTree( node->u.b.right, depth + 1);
					for( i = 0; i < depth; i++ ){
						printf("   ");
					}
					printf("}\n");
					break;

				case GE:
					printf(">={\n");
					printSyntaxTree( node->u.b.left, depth + 1);
					printSyntaxTree( node->u.b.right, depth + 1);
					for( i = 0; i < depth; i++ ){
						printf("   ");
					}
					printf("}\n");
					break;

				case LE:
					printf("<={\n");
					printSyntaxTree( node->u.b.left, depth + 1);
					printSyntaxTree( node->u.b.right, depth + 1);
					for( i = 0; i < depth; i++ ){
						printf("   ");
					}
					printf("}\n");
					break;
				case L:
					printf("<{\n");
					printSyntaxTree( node->u.b.left, depth + 1);
					printSyntaxTree( node->u.b.right, depth + 1);
					for( i = 0; i < depth; i++ ){
						printf("   ");
					}
					printf("}\n");
					break;
				case NOT:
					printf("!{\n");
					printSyntaxTree( node->u.b.left, depth + 1);
					printSyntaxTree( node->u.b.right, depth + 1);
					for( i = 0; i < depth; i++ ){
						printf("   ");
					}
					printf("}\n");
					break;
				case AN:
					printf("&&{\n");
					printSyntaxTree( node->u.b.left, depth + 1);
					printSyntaxTree( node->u.b.right, depth + 1);
					for( i = 0; i < depth; i++ ){
						printf("   ");
					}
					printf("}\n");
					break;
				case R:
					printf("||{\n");
					printSyntaxTree( node->u.b.left, depth + 1);
					printSyntaxTree( node->u.b.right, depth + 1);
					for( i = 0; i < depth; i++ ){
						printf("   ");
					}
					printf("}\n");
					break;
			}
			break;

		case CONSTANT:
			switch(node->u.c.t){
				case IN:
					printf("%d\n",node->u.c.iVal);
					break;
				case CH:
					printf("%c",node->u.c.cVal);
					break;
				case ST:
					printf("%s",node->u.c.sVal);
					break;
			}
			break;	

		case IFS:/* if statment*/
			{
				printf("IF{\n");
				printSyntaxTree( node->u.i.condition, depth + 1);
				printSyntaxTree( node->u.i.if_branch, depth + 1);
				if(node->u.i.else_branch != NULL){
					printSyntaxTree( node->u.i.else_branch, depth + 1);
				}
				for( i = 0; i < depth; i++ ){
					printf("   ");
				}
				printf("}\n");
			}
			break;
		/***************************/
		case W:/* while statement*/
			{	
				printf("while{\n");
				printSyntaxTree( node->u.w.condition, depth + 1);
				printSyntaxTree( node->u.w.while_branch, depth + 1);
				for( i = 0; i < depth; i++ ){
					printf("   ");
				}
				printf("}\n");
			}
			break;
		/***************************/
		case F:/*for statment*/
			printf("For{\n");
			if(node->u.f.assignment != NULL){
				printSyntaxTree(node->u.f.assignment,depth + 1);
			}
			if(node->u.f.condition != NULL){
				printSyntaxTree(node->u.f.condition, depth +1);
			}
			if(node->u.f.expression != NULL){
				printSyntaxTree(node->u.f.expression, depth +1);
			}
			if(node->u.f.body != NULL){
				printSyntaxTree(node->u.f.body, depth + 1);
			}
			for( i = 0; i < depth; i++ ){
					printf("   ");
			}
			printf("}\n");
			break;
		/***************************/
		case I:/*ID constant*/
			{
				printf("%s", node->u.s.symbol->id);
			}
			break;
		/***************************/
		case ASSG:/*assignment*/
			{
				printf("={\n");
				printf("   %s\n", node->u.a.symbol->id);
				printSyntaxTree( node->u.a.value, depth + 1);
				for( i = 0; i < depth; i++ ){
					printf("   ");
				}
				printf("}\n");
			}
			break;
		/***************************/
		case FUNCTION:/* function Idenitifier*/
			printf("function{\n");
			printf("%s\n", node->u.fc.symbol->id);
			printSyntaxTree(node->u.fc.arguments, depth + 1);
			for( i = 0; i < depth; i++ ){
					printf("   ");
				}
			printf("}\n");
		break;

		default:
		    printf("We have a case to handle still\n");
		break;
	}
}



void codeGen(tree_node * node){
	//printf("We called code gen with node_type %d\n", node->node_type);
	instruction_node * newCode = malloc(sizeof(instruction_node));
	switch(node->node_type){
		case EXPR:
			switch(node->u.e.op){
				case ADD:
					//printf("We are generating for add\n");
					codeGen(node->u.e.left); //generate the right code first
					codeGen(node->u.e.right); //generate the left code next
					node->place = newTemp(node->node_type);
					node->code = node->u.e.left->code;
					node->code->last->next = node->u.e.right->code;
					newCode = newExprIntr(ADD,node->u.e.left->place,node->u.e.right->place,node->place);
					newCode->next = NULL;
					newCode->last = NULL;
					node->code->last->next->last->next = newCode;
					node->code->last = newCode;
					// connected them all and properly added the instruction to the node
				break;
				case SUB:
					//printf("We are generating for sub\n");
					codeGen(node->u.e.left); //generate the right code first
					codeGen(node->u.e.right); //generate the left code next
					node->place = newTemp(node->node_type);
					node->code = node->u.e.left->code;
					node->code->last->next = node->u.e.right->code;
					newCode = newExprIntr(SUB,node->u.e.left->place,node->u.e.right->place,node->place);
					newCode->next = NULL;
					newCode->last = NULL;
					node->code->last->next->last->next = newCode;
					node->code->last = newCode;
					// connected them all and properly added the instruction to the node
					break;
				case MULT:
					//printf("We are generating for mult\n");
					codeGen(node->u.e.left); //generate the right code first
					codeGen(node->u.e.right); //generate the left code next
					node->place = newTemp(node->node_type);
					node->code = node->u.e.left->code;
					node->code->last->next = node->u.e.right->code;
					newCode = newExprIntr(MULT,node->u.e.left->place,node->u.e.right->place,node->place);
					newCode->next = NULL;
					newCode->last = NULL;
					node->code->last->next->last->next = newCode;
					node->code->last = newCode;
					// connected them all and properly added the instruction to the node
					break;
				case DIV:
					//printf("We are generating for div\n");
					codeGen(node->u.e.left); //generate the right code first
					codeGen(node->u.e.right); //generate the left code next
					node->place = newTemp(node->node_type);
					node->code = node->u.e.left->code;
					node->code->last->next = node->u.e.right->code;
					newCode = newExprIntr(DIV,node->u.e.left->place,node->u.e.right->place,node->place);
					newCode->next = NULL;
					newCode->last = NULL;
					node->code->last->next->last->next = newCode;
					node->code->last = newCode;
					// connected them all and properly added the instruction to the node
					break;
				case UMINUS:
					//printf("We are generating for UMINUS\n");
					codeGen(node->u.e.left); //generate the right code first
					node->place = newTemp(node->node_type);
					node->code = node->u.e.left->code;
					newCode = newExprIntr(UMINUS,node->u.e.left->place,NULL,node->place);
					newCode->next = NULL;
					newCode->last = NULL;
					node->code->last->next = newCode;
					node->code->last = newCode;
					// connected them all and properly added the instruction to the node
					break;
			}
			break;
	
		

		case CONSTANT:
			switch(node->u.c.t){
				case IN:{
					//printf("We are generating for intcon\n");
					node->place = newTemp(node->node_type);
					newCode = newIntIntr(node->place,node->u.c.iVal);
					node->code = newCode;
					node->code->next = NULL;
					node->code->last = newCode;
					}
					break;
				case CH:
					//printf("We are generating for charcon\n");
					node->place = newTemp(node->node_type);
					newCode = newCharIntr(node->place,node->u.c.cVal);	
					node->code = newCode;
					node->code->next = NULL;
					node->code->last = newCode;
					}				
					break;
				case ST:
					//printf("We are generating for strcon\n");
					node->place = newTemp(node->node_type);
					newCode = newStrIntr(node->place, node->u.c.sVal);	
					node->code = newCode;
					node->code->next = NULL;
					node->code->last = newCode;
					break;
			}
			break;	
		case I:/*ID constant*/
			{
				//printf("We are generating for constant\n");
				node->place = node->u.s.symbol;
				node->code = NULL; 
				//node->code->next = NULL;
				//node->code->last = newCode;

			}
			break;
		/***************************/
		case ASSG:/*assignment*/
			{	
				//printf("We are generating code for an assignment\n");
				codeGen(node->u.a.value);//find the right side node
				node->place = node->u.a.symbol;
				newCode = newAssgnIntr(node->place,node->u.a.value->place);
				node->code = node->u.a.value->code;
				if(node->code != NULL){
		    		node->code->last->next = newCode;
		    		node->code->last = newCode;
		    	}else{
		    		node->code = newCode;
		    		node->code->next = NULL;
		    		node->code->last = newCode;
		    	}
				//node->code->next = newCode;
				//node->code->last = newCode;
				
			}
			break;
		/***************************/
		case FUNCTION:
		    {
		    	//printf("Generating code for a function %s\n",node->u.fc.symbol->id);
		    	//printf("The number of expressions = %d\n",node->u.fc.symbol->size);
		    	codeGenExprList(node->u.fc.arguments);//generate the code for the arguments recursively
		    	//node->place = newTemp(node->u.fc.symbol->type);
		    	node->code = node->u.fc.arguments->code;
		    	newCode = newFuncIntr(node->u.fc.symbol,node->place);//call the function
		    	if(node->code != NULL){
		    		node->code->last = newCode;
		    		node->code->next = newCode;
		    	}
		    }
		break;
		default:
			printf("We have a case we are not handling yet");
		break;

	}
}
void codeGenExprList(tree_node * node){
	instruction_node * newCode = malloc(sizeof(instruction_node));
	switch (node->node_type){
	case CONSTANT:
			switch(node->u.c.t){
				case IN:{
					//printf("We are generating a param intcon\n");
					newCode = newParamIntr(IN," ",NULL,node->u.c.iVal,' '," ");
					node->code = newCode;
					node->code->next = NULL;
					node->code->last = newCode;
					}
					break;
				case CH:{
					//printf("We are generating a param charcon\n");
					newCode = newParamIntr(CH," ",NULL,0,node->u.c.cVal," ");	
					node->code = newCode;
					node->code->next = NULL;
					node->code->last = newCode;
					}				
					break;
				case ST:
					//printf("We are generating a parameter for strcon\n");
					//node->place = newTemp(node->node_type);
					newCode = newParamIntr(ST,newLabel(),NULL,0,' ',node->u.c.sVal);	
					node->code = newCode;
					node->code->next = NULL;
					node->code->last = newCode;
					break;
			}
			break;	
		case I:/*ID constant*/
			{
				//printf("We are generating a parameter for an ID constant, %s\n",node->u.s.symbol->id);
				//printf("The offset of this symbol is at %d \n",node->u.s.symbol->offset);
				newCode = newParamIntr(ID," ",node->u.s.symbol,0,' '," ");	
				node->code = newCode;
				node->code->next = NULL;
				node->code->last = newCode;
				break;
			}
			break;
	}
}



symTabNode * newTemp(int t){
	symTabNode * ntmp = malloc(sizeof(symTabNode));
	ntmp->scope = scope;
	char buff[7];
	sprintf(buff, "$tmp%d", tempCounter);
	ntmp->type=t;
	ntmp->id = strdup(buff);
	if(tempCounter==0){
		ntmp->offset = (-4);
	}else{
		ntmp->offset = (-4*tempCounter);
	}
	ntmp->next =table;//add it to the local symbol table
	table = ntmp;
	tempCounter = tempCounter + 1; //add to our temp counter
	//printf("We created a new temp with id %s and offset %d \n",buff,(-4*tempCounter));
	return ntmp;
}

instruction_node * newExprIntr(Op opType, symTabNode * src1,symTabNode * src2, symTabNode * dest){
	instruction_node * ninstr = malloc(sizeof(instruction_node));
	ninstr->instr.e.opType = opType;
	ninstr->instr.e.src1 = src1;
	ninstr->instr.e.src2 = src2;
	ninstr->instr.e.dest = dest;
	ninstr->type = EXPRI;
	return ninstr;
}

instruction_node * newIntIntr(symTabNode * dest, int src){
	instruction_node * ninstr = malloc(sizeof(instruction_node));
	ninstr->instr.i.dest = dest;
	ninstr->instr.i.src  = src;
	ninstr->type = INTI;
	return ninstr;
}

instruction_node * newCharIntr(symTabNode * dest, char src){
	instruction_node * ninstr = malloc(sizeof(instruction_node));
	ninstr->instr.c.dest = dest;
	ninstr->instr.c.src  = src;
	ninstr->type = CHARI;
	return ninstr;
}
instruction_node * newStrIntr(symTabNode * dest, char * src){
	instruction_node * ninstr = malloc(sizeof(instruction_node));
	ninstr->instr.s.dest = dest;
	ninstr->instr.s.src  = src;
	ninstr->type = STRI;
	return ninstr;
}

instruction_node * newAssgnIntr(symTabNode * dest, symTabNode * src){
	instruction_node * ninstr = malloc(sizeof(instruction_node));
	ninstr->instr.assign.dest = dest;
	ninstr->instr.assign.src  = src;
	ninstr->type = ASSGI;
	return ninstr;
}
instruction_node * newFuncIntr(symTabNode * functionCall, symTabNode * ret){
	instruction_node * ninstr = malloc(sizeof(instruction_node));
	ninstr->instr.fc.function = functionCall;
	ninstr->instr.fc.returnAdd = ret;
	ninstr->type = FUNCI;
	return ninstr;
}
instruction_node * newParamIntr(int type, char * label ,symTabNode * ref, int i, char c , char * s){
	instruction_node * ninstr = malloc(sizeof(instruction_node));
	ninstr->instr.p.type = type;
	ninstr->instr.p.ref = ref;
	ninstr->instr.p.i = i;
	ninstr->instr.p.c = c;
	ninstr->instr.p.s = s;
	ninstr->instr.p.label = label;
	ninstr->type = PARAMI;
	return ninstr;//we now have a parameter with it's type
}
/*
instruction_node * newEnterIntr(int Type, symTabNode * function){
	instruction_node * ninstr = malloc(sizeof(instruction_node));
	ninstr->instr.enter.type = Type;
	ninstr->instr.enter.function = function;
	return ninstr;
}*/

void printFunc(){
	struct list_node * listPtr  = malloc(sizeof(struct list_node));
	listPtr=head;
	//printf("calling printFunc\n");
	while ( listPtr != NULL){
		printSyntaxTree(listPtr->root, 0);
		codeGen(listPtr->root);
		printf("\n");
		printf("Moving to next statment\n");
		printf("\n");
		listPtr = listPtr->next;
	}
	listPtr=head;
}

void enterFunc(symTabNode * function){
	struct list_node * listPtr  = malloc(sizeof(struct list_node));
	listPtr=head;
	while ( listPtr != NULL){
		codeGen(listPtr->root);
		listPtr = listPtr->next;
	}
	printf(".text\n");
	if(strcmp(function->id, "main") == 0){
		printf("main:\n");
	}else{
		printf("fn_%s:\n", function->id); //print the functio name
	}
	int localAllocation =0;
    //localAllocation = function->size * 8;
	int tempsize = 4 * tempVariablesSize();
	localAllocation = localAllocation + tempsize;
	printf("    la $sp, -8($sp)\n");  
	printf("    sw $fp, 4($sp)\n");   
	printf("    sw $ra, 0($sp)\n");   
	printf("    la $fp, 0($sp)\n");
	if(localAllocation != 0){   
		printf("    la $sp, -%d($sp)\n",localAllocation);
	}else{
		printf("    la $sp, 0($sp)\n");
	}	
	printf("\n");
	/*load the paramaters, we will only have one in this case
	printf("lb $t0, 8($fp)\n");
	printf("la $sp, -4($sp)\n");
	printf("sw $t0, 0($sp)\n");
	*/
	//loadParams(function);
	
	listPtr = head;
	while ( listPtr != NULL){
		printCode(listPtr->root);
		listPtr = listPtr->next;
	}
	
	printf("    la $sp, 0($fp)\n");
    printf("    lw $ra, 0($sp)\n");	
    printf("    lw $fp, 4($sp)\n"); 	
    printf("    la $sp, 8($sp)\n");	
    printf("    jr $ra\n");  
    printTable();   
}
int tempVariablesSize(){
	//printf("creating temp var size\n");
	int size = 0;
	//int off = 0;
	symTabNode * ptr = malloc(sizeof(symTabNode));
	ptr = table;
	while(strcmp(ptr->id, "$") != 0){//iterate over the table and get the size of the temps
		if(ptr->description == PARAM){
		} 
		else if(ptr->type != FUNC){
			size = size + 1;
				
		}
		//printf("tablenode id = %s with type =%d\n",ptr->id,ptr->type);		
		ptr = ptr->next;
	}
	//printf("temp var size =%d \n",size);
	return size;
}

//we want to make a tree node with 
/*
void loadParams(symTabNode * function){
	int iter = 0;
	symTabNode * param = malloc(sizeof(symTabNode));
	int stackLocation = iter + 8;
}*/
void printCode(tree_node * root){
	instruction_node * code = root->code;
	while(code != NULL){
		//printf("code loop type is %d\n", code->type);
		switch(code->type){
			case INTI:
				//	printf("#IntPARAM");
					printf("    li $t0, %d\n",code->instr.i.src);
					printf("    sw $t0, %d($fp)\n",code->instr.i.dest->offset);
					//printf("    sw $t0, 0($sp)\n");
				break;
			case CHARI:
				//printf("#CHARACTER\n");
				//printf("storeage destination is %s\n",code->instr.c.dest->id);
				//printf("storage offset = %d\n",code->instr.c.dest->offset);

					printf("    li $t0 %d\n", (int) code->instr.c.src);
					//printf("    la $t0, %d($sp)\n",code->instr.c.dest->offset);
					printf("    sb $t0, %d($fp)\n",code->instr.c.dest->offset);
				break;
			case STRI:
				//printf("#STRING\n");
				printf(".data\n");
				printf("%s: .asciiz %s\n",code->instr.c.dest->id,code->instr.s.src);
				printf("\n.text\n");
				printf("    la $t0, %s\n",code->instr.c.dest->id);
				printf("    la $sp, %d($sp)\n", code->instr.c.dest->offset);
				printf("    sw $t0, 0($sp)\n");
				break;
			case ASSGI:
				printf("\n");
				//printf("assignment type will be %d\n", code->instr.assign.dest->type);
				if(code->instr.assign.src->scope == 0){ //global variable
					printf("    la $t2, _%s\n",code->instr.assign.src->id);
				}else if(code->instr.assign.dest->type == INTEGER){
					printf("    lw $t0, %d($fp)\n", code->instr.assign.src->offset);
				}else{
					printf("    lb $t0, %d($fp)\n", code->instr.assign.src->offset);
				}

				if(code->instr.assign.dest->scope == 0){ //global variable
					printf("    la $t2, _%s\n",code->instr.assign.dest->id);
					if(code->instr.assign.dest->type == INTEGER){
						printf("    sw  $t0, 0($t2)\n");
					}else{
						printf("    sb  $t0, 0($t2)\n");
					}
				}else{
					if(code->instr.assign.dest->type == INTEGER){			
						printf("    sw $t0, %d($fp)\n",code->instr.assign.dest->offset);
					}else{
						printf("    sb $t0, %d($fp)\n",code->instr.assign.dest->offset);
					}
				}
				/*else if(code->instr.assign.dest->type == INTEGER){
					printf("    la $sp, %d($sp)\n",code->instr.assign.dest->offset);
				}else{
					printf("    lb $sp, %d($sp)\n",code->instr.assign.dest->offset);
				}*/
				
				break;
			case EXPRI:
				//printf("#EXPR\n");
				//printf("\n");
				//printf("Expression code\n");
				break;
			case FUNCI:{
				//printf("#FUNC\n");
				printf("\n");
				int params=0;
				params= 4 * code->instr.fc.function->size;
				printf("    jal fn_%s\n",code->instr.fc.function->id);
				printf("    la $sp, %d($sp)\n",params);
				printf("\n");
				}
			break;
			case PARAMI:{
				//printf("#PARAM\n");
				printf("\n");
				if(code->instr.p.type == IN){
					//printf("#Int parameter\n");
					printf("    li $t0, %d\n",code->instr.p.i);
					printf("    sw $sp, -4($fp)\n");
					printf("    lb $t0, -4($fp)\n");
					printf("    la $t0, -4($fp)\n");
					printf("    sw $t0, 0($sp)\n");
				}else if(code->instr.p.type == CH){
					//printf("#Char parameter\n");
					printf("    li $t0, %d\n", (int) code->instr.p.c);
					printf("    sb $t0, -4($fp)\n");
					printf("    lb $t0, -4($fp)\n");
					printf("    la $sp, -4($sp)\n");
					printf("    sw $t0,  0($sp)\n");
				}else if(code->instr.p.type == ST){
					//printf("#string parameter\n");
					printf(".data\n");
					printf("%s: .asciiz %s\n",code->instr.p.label,code->instr.p.s);
					printf("\n.text\n");
					printf("    la $t0, %s\n",code->instr.p.label);
					printf("    la  $sp, -4($sp)\n");
					printf("    sw $t0, 0($sp)\n");
				}else if(code->instr.p.type ==ID){
					//printf("Id parameter\n");
					if(code->instr.p.ref->scope > 0){
						if(code->instr.p.ref->type == INTEGER){
							printf("    lw $t0, %d($fp)\n",code->instr.p.ref->offset);
						}else if(code->instr.p.ref->type == CHARACTER){
							printf("    lb $t0, %d($fp)\n",code->instr.p.ref->offset);
						}
					}else{
						printf("    la $t1, _%s\n",code->instr.p.ref->id);
						if(code->instr.p.ref->type == CHARACTER){
							printf("    lb $t0, 0($t1)\n");
						}else{
							printf("    lw $t0, 0($t1)\n");
						}
					}
					printf("    la $sp, -4($sp)\n");
					printf("    sw $t0, 0($sp)\n");
				}
				
			}
			break;
		}
		code=code->next;
	}
}

void makeGlobals(){
	symTabNode * ptr = malloc(sizeof(symTabNode));
	ptr = table;
	printf(".data\n");
	while(strcmp(ptr->id, "$") != 0){
		if(ptr->type == CHARACTER){
			if(ptr->description != FUNC || ptr->description != PROTO){
				printf("_%s: .space 1\n",ptr->id);
				printf(".align 2\n");	
			}
		}
		else if(ptr->type == INTEGER){
			if(ptr->description != FUNC || ptr->description != PROTO){
				printf("_%s: .space 4\n",ptr->id);
				printf(".align 2\n");	
			}
		}else{
			//not handled
		}
		ptr=ptr->next;
	}
}
char * newLabel(){
	char buff[7];
	sprintf(buff, "$tmp%d", labelCounter);
	char * temp = malloc(sizeof(char) * strlen(buff));
	strcpy(temp,buff);
	labelCounter = labelCounter + 1;
	return temp;
}