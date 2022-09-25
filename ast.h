#include <string>

// types
class Exp;
class Value;
class Program;
class Defs;
class Def;
typedef std::string name;
typedef Exp* exp;
typedef Value* value;
typedef Program* program;
typedef Defs* defs;
typedef Def* def;

// AST construction: expression
exp num(int);           // 42
exp add(exp,exp);       // A + B
exp mul(exp,exp);       // A * B
exp sub(exp,exp);       // A - B
exp less(exp,exp);      // A < B
exp ite(exp,exp,exp);   // (1) if A then B else C  *or*  (2) A ? B : C
exp var(name);          // x
exp str(std::string);   // "foo"
exp append(exp,exp);    // A ++ B
exp let(name,exp,exp);  // let X = RHS in BODY
exp call1(name f,exp arg); // f(arg)

// AST construction: programs and defs
program makeProgram(defs, exp main);
defs nilDefs();
defs consDefs(def,defs);
def def1(name func, name arg, exp body);

// values
value vInt(int);
value vString(std::string);
std::string showV(value);
bool equalV(value,value);

// environments
class Env;
typedef Env* env;
env emptyEnv();
env extendEnv(env,name,value);
value lookupEnv(env,name);

// pretty-printing, evaluation, execution
std::string show(exp e);
value eval(exp,env);
value execute(program);
