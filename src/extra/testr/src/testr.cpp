#include "testr.h"
#define USE_RINTERNALS
using namespace Rcpp;
using namespace std;

CCODE get_internal(string name)
{ 
    FUNTAB* p = R_FunTab ;
    for( ; p->name != NULL; ++p ){
        if( name == p->name )
            return p->cfun ;
    } 
    return NULL ;
}

SEXP search(){
      CCODE search_fun = get_internal("search");
      Language call("search") ;    
      return search_fun(call, Rf_ScalarInteger(0), NULL,  R_GlobalEnv ) ; 
}

SEXP deparse(SEXP x)
{
      CCODE deparse_fun = get_internal("deparse");
      Language call("deparse", x) ;    
      return deparse_fun(call, Rf_ScalarInteger(0), CDR(call), R_GlobalEnv ) ; 
}


bool missing(SEXP x, SEXP env)
{
      CCODE missing_fun = get_internal("missing");
      Language call("missing", x);
      LogicalVector res(missing_fun(call, Rf_ScalarInteger(0), CDR(call), env));
      return res[0] == TRUE; 
}

bool contains(CharacterVector v, string elem){
  for (int i = 0; i < v.length(); i++){
    if (elem == as<string>(v[i]))
    return true;
  } 
  return false;
}

string getFunctionEnvironmentName(string &functionName) {
  string envir_name = "";
  Environment envir;
  CharacterVector packages = search();
  for (int i = 0; i < packages.length(); i++){
    envir = Environment(as<std::string>(packages[i]));
    if (envir.exists(functionName)) {
      envir_name = as<std::string>(packages[i]);
      break;
    }
  }
  return envir_name;
}
