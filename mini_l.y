/*
  Hongyi Zhang (hzhan014)
  Andrew Kruczek (akruc001)
  project 3
*/
%{
  #include <iostream>
  #include <stdio.h>
  #include <string>
  #include <sstream>
  #include <map>
  #include <stack>
  #include <fstream>
  using namespace std;
  
  /*prototypes*/
  int yyerror(const char *s);
  int yylex(void);
  string numtostr(int num);
  
  /*extern variables*/
  extern int currLine;
  extern char* yytext;
  extern int currPos;
  
  /*stacks and hash_table*/
  map<string, int> hash_table;
  stack<string> compares; // compares signs
  stack<string> indices;
  stack<string> identifiers;
  stack<string> variables;
  stack<string> read_write;
  stack<int> labels; 
  stack<int> loops; //loop stack
  stack<int> predicates;

  int pred = 0;
  int term = 0;
  int label = 0;
  bool Error_check = false;
  stringstream stream_out;
  string milname = "";
  string error_string;
%}

%union{
  char* ival;
  char* sval; 
}
%error-verbose
%start input
%token <sval> IDENT
%token <ival> NUMBER
%token PROGRAM BEGIN_PROGRAM END_PROGRAM 
%token INTEGER ARRAY
%token OF IF THEN ENDIF ELSE ELSEIF WHILE DO BEGINLOOP ENDLOOP CONTINUE 
%token READ WRITE
%token AND OR NOT TRUE FALSE 
%token SUB ADD MULT DIV MOD EQ NEQ LT GT LTE GTE
%token SEMICOLON COLON COMMA ASSIGN
%token L_PAREN L_BRACKET
%token R_PAREN R_BRACKET

%%
block : declaration_loop begin_program statement_loop;
declaration_loop : declaration SEMICOLON | declaration SEMICOLON declaration_loop;
statement_loop : statement SEMICOLON statement_loop | statement SEMICOLON;
identifier_loop : identifier COMMA identifier_loop | identifier;
statement : s1 | s2 | s3 | s4 ;
relation_exp : re_1 | re_2 | re_3;
re_3 : L_PAREN bool_exp R_PAREN | NOT L_PAREN bool_exp R_PAREN ;
expression : multiplicative_exp e_loop;
multiplicative_exp : term m_e_loop;
relation_and_exp : relation_exp rne_loop;
var_loop : var COMMA var_loop | var;
s1 : sub_while bool_exp begin_loop statement_loop end_loop;
s2 : IF bool_exp sub_then statement_loop stt2_loop;
s3 : var ASSIGN exp_1;
stt2_loop : sub_else statement_loop sub_endif | sub_elseif bool_exp statement_loop stt2_loop | sub_endif;
begin_program : BEGIN_PROGRAM {stream_out << ": START" << endl;};
    
input
  : PROGRAM IDENT SEMICOLON block END_PROGRAM {
      string holder = string($2);
      size_t pos = holder.find(";");
      milname = holder.substr(0,pos);
      milname = milname + ".mil";
      const char* filename = milname.c_str();
      ofstream createfile;
      createfile.open(milname.c_str());
      createfile.close();
      freopen(filename,"w",stdout);
      for(int i = 0; i < term; i++){cout << "\t. t" << i << endl;}
      for(int i = 0; i < pred; i++){cout << "\t. p" << i << endl;}
      cout << stream_out.str();
      cout << ": EndLabel" << endl;
    };

declaration : identifier_loop COLON INTEGER {
      while(!identifiers.empty()) {
        stream_out << "\t. " << identifiers.top() << endl;
        identifiers.pop();
      }
    }
  | identifier_loop COLON ARRAY L_BRACKET NUMBER R_BRACKET OF INTEGER {
      if(atoi($5) <= 0) {
        error_string = "Error: declaring an array of size <= 0";
        yyerror(error_string.c_str());
      }
      while(!identifiers.empty()) {
        stream_out << "\t.[] " << identifiers.top() << ", " << atoi($5) << endl;
        hash_table[identifiers.top()] = atoi($5);
        identifiers.pop();
      }
    };

identifier : IDENT {
      if(hash_table.find("_" + string($1)) != hash_table.end()) {
        error_string = "Error: " + string($1) + " was previously defined";
        yyerror(error_string.c_str());
      }
      hash_table["_" + string($1)] = -1;
      identifiers.push("_" + string($1));
    };

s4 : READ var_loop {
      while(!variables.empty()) {
        if(indices.top() == "-1") {
          stringstream std_hold;
          std_hold << "\t.< " << variables.top() << endl;
          read_write.push(std_hold.str());
        }
        else {
          stringstream std_hold;
          std_hold << "\t.[]< " << variables.top() << ", "
                 << indices.top() << endl;
          read_write.push(std_hold.str());
        }
        variables.pop();
        indices.pop();
      }
      while(!read_write.empty()) {
        stream_out << read_write.top();
        read_write.pop();
      }
    }
    | WRITE var_loop {
      while(!variables.empty()) {
        if(indices.top() == "-1") {
          stringstream std_hold;
          std_hold << "\t.> " << variables.top() << endl;
          read_write.push(std_hold.str());
        }
        else {
          stringstream std_hold;
          std_hold << "\t.[]> " << variables.top() << ", "
                 << indices.top() << endl;
          read_write.push(std_hold.str());
        }
        variables.pop();
        indices.pop();
      }
      while(!read_write.empty()) {
        stream_out << read_write.top();
        read_write.pop();
      }
    }
  | sub_do BEGINLOOP statement_loop loop_end WHILE bool_exp {
      int hold_1 = predicates.top();
      predicates.pop();
      int l1 = labels.top();
      stream_out << "\t== p" << pred << ", p" << hold_1 << ", 0" << endl;
      stream_out << "\t?:= label" << l1 << ", p" << pred << endl;
      ++pred;
      labels.pop();
    }
  | CONTINUE {
      if(!loops.empty()) {
        int s = loops.top();
        stream_out << "\t:= label" << s << endl;
      }
    };
    
exp_1 : expression {
      string tmp2 = variables.top();
      if(indices.top() != "-1") {
        stream_out << "\t=[] t" << numtostr(term) << ", " << tmp2 << ", "
               << indices.top() << endl;
        tmp2 = "t" + numtostr(term);
        ++term;
      }
      variables.pop();
      indices.pop();
      string tmp1 = variables.top();
      if(indices.top() != "-1") {
        stream_out << "\t[]= " << tmp1 << ", " << indices.top()
               << ", " << tmp2 << endl;
      }
      else {
        stream_out << "\t= " << tmp1 << ", " << tmp2 << endl;
      }
      variables.pop();
      indices.pop();
    };

sub_then : THEN {
      int hold_2 = predicates.top();
      predicates.pop();
      stream_out << "\t?:= L" << label << ", p" << hold_2 << endl;
      labels.push(label);
      label++;
    };

sub_elseif : ELSEIF{
      stream_out << "\t:= L" << label << endl;
      stream_out << ": L" << labels.top() << endl;
      labels.pop();
      labels.push(label);
      label++;
    };
    
sub_else : ELSE {
      stream_out << "\t:= L" << label << endl;
      stream_out << ": L" << labels.top() << endl;
      labels.pop();
      labels.push(label);
      label++;
    };

sub_endif : ENDIF {
      stream_out << ": L" << labels.top() << endl;
      labels.pop();
    };

sub_while : WHILE {
      stream_out << ": L" << label << endl;
      labels.push(label);
      loops.push(label);
      label++;
    };
sub_do : DO {
      stream_out << ": L" << label << endl;
      labels.push(label);
      label++;
      loops.push(label);
      label++;
    };

begin_loop : BEGINLOOP {
      int hold_2 = predicates.top();
      predicates.pop();
      stream_out << "\t?:= L" << label << ", p" << hold_2 << endl;
      labels.push(label);
      label++;
    };

end_loop : ENDLOOP {
      int hold_2 = labels.top();
      labels.pop();
      int hold_1 = labels.top();
      labels.pop();
      stream_out << "\t:= L" << hold_1 << endl << ": L" << hold_2 << endl;
      loops.pop();
    };
    
bool_loop : OR relation_and_exp bool_loop {
      int hold_2 = predicates.top();
      predicates.pop();
      int hold_1 = predicates.top();
      predicates.pop();
      stream_out << "\t|| p" << pred << ", p" << hold_1 << ", p" << hold_2 << endl;
      predicates.push(pred);
      ++pred;
    } | {/*epsilon*/};




loop_end : ENDLOOP {
      int l1 = loops.top(); 
      stream_out << ": L" << l1 << endl;
      loops.pop();
    };

bool_exp : relation_and_exp bool_loop {
      int hold_2 = predicates.top();
      predicates.pop();
      stream_out << "\t== p" << pred << ", p" << hold_2 << ", 0" << endl;
      predicates.push(pred);
      ++pred;
    };


rne_loop : AND relation_exp rne_loop {
      int hold_2 = predicates.top();
      predicates.pop();
      int hold_1 = predicates.top();
      predicates.pop();
      stream_out << "\t&& p" << pred << ", p" << hold_1 << ", p" << hold_2 << endl;
      predicates.push(pred);
      ++pred;
    } | {/*epsilon*/};

re_1 : expression comp expression {
      string tmp2 = variables.top();
      if(indices.top() != "-1") {
        stream_out << "\t=[] t" << numtostr(term) << ", " << tmp2 << ", " << indices.top() << endl;
        tmp2 = "t" + numtostr(term);
        ++term;
      }
      indices.pop();
      variables.pop();
      string tmp1 = variables.top();
      if(indices.top() != "-1") {
        stream_out << "\t=[] t" << numtostr(term) << ", " << tmp1 << ", " << indices.top() << endl;
        tmp1 = "t" + numtostr(term);
        ++term;
      }
      indices.pop();
      variables.pop();
      string c = compares.top();
      compares.pop();
      stream_out << "\t" << c << " p" << pred << ", " << tmp1 << ", " << tmp2 << endl;
      predicates.push(pred);
      ++pred;
    }
  | NOT expression comp expression {
      string tmp2 = variables.top();
      if(indices.top() != "-1") {
        
        stream_out << "  =[] t" << numtostr(term) << ", " << tmp2 << ", " << indices.top() << endl;
        tmp2 = "t" + numtostr(term);
        ++term;
      }
      indices.pop();
      variables.pop();
      string tmp1 = variables.top();
      if(indices.top() != "-1") {

        stream_out << "\t=[] t" << numtostr(term) << ", " << tmp1 << ", "
               << indices.top() << endl;
        tmp1 = "t" + numtostr(term);
        ++term;
      }
      indices.pop();
      variables.pop();
      string c = compares.top();
      compares.pop();
      stream_out << "\t" << c << " p" << pred << ", " << tmp1 << ", " << tmp2 << endl;
      ++pred;
      stream_out << "\t== p" << pred << ", p" << pred-1 << ", 0" << endl;
      predicates.push(pred);
      ++pred;
    };

re_2 : FALSE {
      stream_out << "\t== p" << pred << ", 1, 0" << endl;
      predicates.push(pred);
      ++pred;
    }
  | NOT FALSE {
      stream_out << "\t== p" << pred << ", 1, 1" << endl;
      predicates.push(pred);
      ++pred;
  }
  | TRUE {
      stream_out << "\t== p" << pred << ", 1, 1" << endl;
      predicates.push(pred);
      ++pred;
    }
  | NOT TRUE {
    stream_out << "\t== p" << pred << ", 1, 0" << endl;
      predicates.push(pred);
      ++pred;
    };

comp : EQ {compares.push("==");}
  | NEQ {compares.push("!=");}
  | LT {compares.push("<");}
  | GT {compares.push(">");}
  | LTE {compares.push("<=");}
  | GTE {compares.push(">=");};

e_loop : SUB multiplicative_exp e_loop {
      string tmp2 = variables.top();
      if(indices.top() != "-1") {
        
        stream_out << "\t=[] t" << numtostr(term) << ", " << tmp2 << ", " << indices.top() << endl;
        tmp2 = "t" + numtostr(term);
        ++term;
      }
      indices.pop();
      variables.pop();
      string tmp1 = variables.top();
      if(indices.top() != "-1") {

        stream_out << "\t=[] t" << numtostr(term) << ", " << tmp1 << indices.top() << endl;
        tmp1 = "t" + numtostr(term);
        ++term;
      }
      indices.pop();
      variables.pop();
      stream_out << "\t- t" << term << ", " << tmp1 << ", " << tmp2 << endl;

      variables.push("t" + numtostr(term));
      indices.push("-1");
      ++term;
    }
  | ADD multiplicative_exp e_loop {
      string tmp2 = variables.top();
      if(indices.top() != "-1") {

        stream_out << "\t=[] t" << numtostr(term) << ", " << tmp2 << ", " << indices.top() << endl;
        tmp2 = "t" + numtostr(term);
        ++term;
      }
      indices.pop();
      variables.pop();
      string tmp1 = variables.top();
      if(indices.top() != "-1") {

        stream_out << "\t=[] t" << numtostr(term) << ", " << tmp1 << ", " << indices.top() << endl;
        tmp1 = "t" + numtostr(term);
        ++term;
      }
      variables.pop();
      indices.pop();
      stream_out << "\t+ t" << term << ", " << tmp1 << ", " << tmp2 << endl;
      
      variables.push("t" + numtostr(term));
      indices.push("-1");
      ++term;
    } | {/*epsilon*/};

m_e_loop : DIV term m_e_loop {
      string tmp2 = variables.top();
      if(indices.top() != "-1") {
        stream_out << "  =[] t" << numtostr(term) << ", " << tmp2 << ", " << indices.top() << endl;
        tmp2 = "t" + numtostr(term);
        ++term;
      }
      indices.pop();
      variables.pop();
      string tmp1 = variables.top();
      if(indices.top() != "-1") {

        stream_out << "  =[] t" << numtostr(term) << ", " << tmp1 << ", " << indices.top() << endl;
        tmp1 = "t" + numtostr(term);
        ++term;
      }
      indices.pop();
      variables.pop();
      stream_out << "  / t" << term << ", " << tmp1 << ", " << tmp2 << endl;
      variables.push("t" + numtostr(term));
      indices.push("-1");
      ++term;
    }
  | MULT term m_e_loop {
      string tmp2 = variables.top();
      if(indices.top() != "-1") {
        stream_out << "\t=[] t" << numtostr(term) << ", " << tmp2 << ", " << indices.top() << endl;
        tmp2 = "t" + numtostr(term);
        ++term;
      }
      indices.pop();
      variables.pop();
      string tmp1 = variables.top();
      if(indices.top() != "-1") {
        stream_out << "\t=[] t" << numtostr(term) << ", " << tmp1 << ", " << indices.top() << endl;
        tmp1 = "t" + numtostr(term);
        ++term;
      }
      indices.pop();
      variables.pop();
      stream_out << "  * t" << term << ", " << tmp1 << ", "  << tmp2 << endl;
      variables.push("t" + numtostr(term));
      indices.push("-1");
      ++term;
    }

  | MOD term m_e_loop {
      string tmp2 = variables.top();
      if(indices.top() != "-1") {
        stream_out << "  =[] t" << numtostr(term) << ", " << tmp2 << ", " << indices.top() << endl;
        tmp2 = "t" + numtostr(term);
        ++term;
      }
      variables.pop();
      indices.pop();
      string tmp1 = variables.top();
      if(indices.top() != "-1") {
        stream_out << "  =[] t" << numtostr(term) << ", " << tmp1 << ", " << indices.top() << endl;
        tmp1 = "t" + numtostr(term);
        ++term;
      }
      variables.pop();
      indices.pop();
      stream_out << "  % t" << term << ", " << tmp1 << ", " << tmp2 << endl;
      variables.push("t" + numtostr(term));
      indices.push("-1");
      ++term;
    } | {/*epsilon*/};

term : SUB var {
    string tmp2 = variables.top();
    if(indices.top() != "-1") {
      stream_out << "  =[] t" << numtostr(term) << ", " << tmp2 << indices.top() << endl;
      tmp2 = "t" + numtostr(term);
      ++term;
    }
    variables.pop();
    stream_out << "  - t" << term << ", 0, " << tmp2 << endl;
    variables.push("t" + numtostr(term));
    indices.push("-1");
    ++term;
  }
  | SUB L_PAREN expression R_PAREN {
      string tmp2 = variables.top();
      variables.pop();
      stream_out << "  - t" << term << ", 0, " << tmp2 << endl;
      variables.push("t" + numtostr(term));
      indices.push("-1");
      ++term;
    }
  | L_PAREN expression R_PAREN
  
  | NUMBER {
      variables.push(string($1));
      indices.push("-1");
    }
  | SUB sub_number
  | var
    ;

sub_number : NUMBER {
      stream_out << "  - t" << term << ", 0, " << string($1) << endl;
      variables.push("t" + numtostr(term));
      indices.push("-1");
      ++term;
    };

var : sub_ident {
      map<string, int>::iterator it;
      it = hash_table.find(variables.top());
      if(it != hash_table.end()) {
        if((*it).second != -1) {
          error_string = "Error: array " + variables.top().substr(1, variables.top().length()-1) + " requires an index";
          yyerror(error_string.c_str());
        }
      }
      indices.push("-1");
    }
  | sub_ident sub_l_bracket expression R_BRACKET {
      indices.pop();
      indices.push(variables.top());
      variables.pop();
    };

sub_l_bracket : L_BRACKET {
      map<string, int>::iterator it;
      it = hash_table.find(variables.top());
      if(it != hash_table.end()) {
        if((*it).second == -1) {
          error_string = "Error: variable " + variables.top().substr(1, variables.top().length()-1) + " does not require an index";
          yyerror(error_string.c_str());
        }
      }
  };

sub_ident : IDENT {
      string keywords = string($1);
      if(keywords == "program"    
              ||  keywords == "beginprogram" 
              ||  keywords == "endprogram" 
              ||  keywords == "of" 
              ||  keywords == "do" 
              ||  keywords == "while"      
              ||  keywords == "beginloop"  
              ||  keywords == "endloop" 
              ||  keywords == "continue"   
              ||  keywords == "read" 
              ||  keywords == "write"      
              ||  keywords == "and" 
              ||  keywords == "or"         
              ||  keywords == "not" 
              ||  keywords == "if"   
              ||  keywords == "elseif"      
              ||  keywords == "else" 
              ||  keywords == "then" 
              ||  keywords == "endif" 
              ||  keywords == "integer" 
              ||  keywords == "array"      
              ||  keywords == "true"       
              ||  keywords == "false") {
        error_string = "Error: " + string($1) + " is a keyword";
        yyerror(error_string.c_str());
      }
      else if(hash_table.find("_" + keywords) == hash_table.end()) {
        error_string = "Error: " + keywords + " was not declared";
        yyerror(error_string.c_str());
      }
      variables.push("_" + string($1));
    };
%%
int yyerror(const char *error_string)
{
  printf("%s, at symbol %s on line %d\n", error_string, yytext, currLine);
  return 1;
}
string numtostr(int num){
    ostringstream ss;
    ss << num;
    return ss.str();
}
int main(int argc, char **argv)
{
  if((argc > 1) && (freopen(argv[1], "r", stdin) == NULL))
  {
    cerr << argv[0] << ": File " << argv[1] << " cannot be opened.\n";
    exit(1);
  }
  yyparse();
  return 1;
}
