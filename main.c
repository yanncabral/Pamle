#import <stdio.h>
#import <stdlib.h>
#import <string.h>
#import <math.h>

typedef struct {
  char name[30];
  int count;
  int size;
  float* value;
} *PVar, Var;

char data[256], *current = data;
Var vars[256];
float last = 0;

enum token {
  tkEOF, //expr
  tkSet, tkSetPlus, tkSetMinus, tkSetDiv, tkSetMul, tkSetMod, tkSetExp,
  tkLBrace, tkRBrace,
  tkAnd, tkOr, tkXor,
  tkMajor, tkMinor, tkMajorEq, tkMinorEq, tkEqual, //rvalue
  tkPlus, tkMinus,     //part
  tkMul, tkDiv, tkMod, //term
  tkFat,               //fat
  tkExp,               //pot
  tkNumber, tkLParen, tkRParen, tkId,//factor
  tkComma, 
  tkUnknow 
};

int isDigit(){
  if(*current=='\0') return 0;
  return (strchr("0123456789", *current)?1:0);
}

int isId(){
  if(*current=='\0') return 0;
  return ((*current<='z'&&*current>='a')||*current=='_');
}

float getNumber(){
  float result = 0;
  while(isDigit()){
    result = result * 10 + *current - '0';
    current++;
  }
  if(*current=='.'){
    current++;
    float frac = getNumber();
    while(frac>=1) frac /= 10;
    result += frac;
  }
  return result;
}

void getName(char* p){
  do *(p++) = *(current++);
  while(isId());
  *p = '\0';
}

char peek(){
  return *(current+1);
}

void skipWhite(){
  while(*current==' ') current++;
}

int getToken(){
  skipWhite();
  if(isDigit()) return tkNumber;
  if(isId()) return tkId;
  switch(*current){
    case '\0':return tkEOF;
    case '!': return tkFat;
    case '(': return tkLParen;
    case ')': return tkRParen;
    case '&': return tkAnd;
    case '|': return tkOr;
    case '[': return tkLBrace;
    case ']': return tkRBrace;
    case ',': return tkComma;
    case '+': return (peek()=='='?tkSetPlus:tkPlus);
    case '-': return (peek()=='='?tkSetMinus:tkMinus);
    case '/': return (peek()=='='?tkSetDiv:tkDiv);
    case '%': return (peek()=='='?tkSetMod:tkMod);
    case '^': return (peek()=='='?tkSetExp:tkXor);
    case '*': return (peek()=='*'?tkExp:peek()=='='?tkSetMul:tkMul);
    case '>': return (peek()=='='?tkMajorEq:tkMajor);
    case '<': return (peek()=='='?tkMinorEq:tkMinor);
    case '=': return (peek()=='='?tkEqual:tkSet);
  }
  return tkUnknow;
}

float expr(int n);

PVar growVar(PVar var){
  if(!var->size) var->size = 8;
  else if(var->count>=var->size) var->size *= 2;
  var->value = (float*) realloc(var->value, var->size *= 2 );
  return var;
}

PVar setVar(PVar var){
  if(getToken()==tkSet) current++;
  if(getToken()==tkLBrace){
      do{
        growVar(var)->value[var->count++] = expr(1);
      } while(getToken()==tkComma);
    if(getToken()==tkRBrace) current++; 
    else printf("Syntax error");
    } else growVar(var)->value[var->count++] = expr(0);
  return var;
}

PVar createVar(char* name){
  PVar var = vars;
  while(var->name[0]) var++;
  strncpy(var->name, name, 30);
  var->size = var->count = 0;
  return setVar(var);
}

float* indexVar(PVar var){
  if(getToken()==tkLBrace){
    int index = expr(1);
    if(getToken()==tkRBrace) current++; else printf("Syntax error!");
    if(index>var->count) var->count = index;
    return &growVar(var)->value[index];
  }
  return var->value;
}

PVar getVar(char* name){
  PVar var = 0;
  for(int i=0; i<256; i++){
    if(!strcmp(vars[i].name, name)){
      var = &vars[i];
      break;
    }
  }
  return var;
}

float constant(char* name){
  if(!strcmp(name, "pi")) return M_PI;
  else if(!strcmp(name, "e")) return exp(1);
  return 0;
}

float variable(char* name){
  PVar var = getVar(name);
  if(!var){
    if(getToken()==tkLBrace) do{} while(*(current++)!=']');
    if(getToken()==tkSet) return *indexVar(createVar(name));
    float result = constant(name);
    if(result) return result;
    else {
      printf("error: i don't know what's %s, man!\n", name);
      return 0;
    }
  } else {
    float* v = indexVar(var);
    switch(getToken()){
      case tkSet: return *v = expr(1);
      case tkSetPlus: return *v += expr(2);
      case tkSetMinus: return *v -= expr(2);
      case tkSetMul: return *v *= expr(2);
      case tkSetDiv: return *v /= expr(2);
      case tkSetMod: return *v = (int) var->value % (int) expr(2);
      case tkSetExp: return *v = pow(*v, expr(2));
    }
    return *v;
  }
  return 0;
}

float func(char* name){
  if(getToken()!=tkLParen) return variable(name);
  float result = 0;
  if(!strcmp(name, "twice")) result = expr(1)*2; 
  else if(!strcmp(name, "sin")) result = sin(expr(1));
  else if(!strcmp(name, "cos")) result = cos(expr(1));
  else if(!strcmp(name, "tan")) result = tan(expr(1));
  else if(!strcmp(name, "sqrt")) result = sqrt(expr(1));
  else if(!strcmp(name, "floor")) result = floor(expr(1));
  else if(!strcmp(name, "ceil")) result = ceil(expr(1));
  else if(!strcmp(name, "exp")) result = exp(expr(1));
  else if(!strcmp(name, "floor")) result = floor(expr(1));
  else if(!strcmp(name, "sinh")) result = sinh(expr(1));
  else if(!strcmp(name, "cosh")) result = cosh(expr(1));
  else if(!strcmp(name, "tanh")) result = tanh(expr(1));
  else if(!strcmp(name, "ln")) result = log(expr(1));
  else if(!strcmp(name, "log")) result = log10(expr(1));
  else if(!strcmp(name, "trunc")) result = trunc(expr(1));
  else if(!strcmp(name, "degree")) result = expr(1)*(180/M_PI);
  else if(!strcmp(name, "radian")) result = expr(1)*(M_PI/180);
  else if(!strcmp(name, "abs")) result = fabsf(expr(1));
  else if(!strcmp(name, "last")) (current++, result = last);
  if(getToken()==tkRParen) current++; else printf("Syntax error.\n");
  return result;
}

float atom(){
  char name[30];
  getName(name);
  if(!strcmp(name, "not")) return !expr(1); 
  return func(name);
}

float factor(int n){
  current += n;
  int token = getToken();
  if(token==tkNumber) return getNumber();
  if(token==tkLParen) {
    float result = expr(1);
    if(getToken()==tkRParen) current++;
    return result;
  }
  if(token==tkMinus) return -factor(1);
  if(token==tkId) return atom();
  if(token==tkUnknow) printf("Unexpected token: %i.\n", *current);
  return last;
}

float pot(int n){
  current += n;
  float result = factor(0);
  if(getToken()==tkExp) result = pow(result, pot(2)); 
  return result;
}

int fatorial(int n){
  if(n) return n * fatorial(n-1); else return 1;
}

float fat(int n){
  current += n;
  float result = pot(0), token;
  while(getToken()==tkFat) result = fatorial((current++, result));
  return result;
}

float term(int n){
  current += n;
  float result = fat(0), token;
  do{
    token = getToken();
    if(token==tkMul) result *= fat(1);
    else if(token==tkDiv) result /= fat(1);
    else if(token==tkMod) result = (int)(result) % (int) fat(1);
  } while(token==tkMul||token==tkDiv||token==tkMod);
  return result;
}

float part(int n){
  current += n;
  float result = term(0), token;
  do{
    token = getToken();
    if(token==tkPlus)  result += term(1);
    else if(token==tkMinus) result -= term(1);
  } while(token==tkPlus||token==tkMinus);
  return result;
}

float rvalue(int n){
  current += n;
  float result = part(0), token;
  do{
    token = getToken();
    if(token==tkEqual) result = (part(2) == result);
    if(token==tkMajor) result = (part(1) < result);
    if(token==tkMinor) result = (part(1) > result);
    if(token==tkMajorEq) result = (part(2) <= result);
    if(token==tkMinorEq) result = (part(2) >= result);
  } while(token==tkMajor||token==tkMinor||token==tkMajorEq||token==tkMinorEq||token==tkEqual);
  return result;
}

float expr(int n){
  current += n;
  float result = rvalue(0);
  switch(getToken()){
    case tkAnd: return (int) result & (int) rvalue(1);
    case tkOr:  return (int) result | (int) rvalue(1);
    case tkXor: return (int) result ^ (int) rvalue(1);
  }
  return last = result;
}

void repl(){
  do{
    printf("%G\n", expr(0));
    skipWhite();
  } while(*current!='\n');
}

int main(){
  system("@cls||clear");
  printf("Welcome to Pamle math software.\nIt was developed by YANN, the most pica developer from Brazil.\n\n");
  printf("Try some math:\n");
  while(1){
    printf(">>> ");
    fgets(data, sizeof(data), stdin);
    current = data;
    if(!strncmp(current, "exit", 4)) break;
    if(!strncmp(current, "clear", 5)) system("@cls||clear");
    else repl();
  }
  return 0;
}