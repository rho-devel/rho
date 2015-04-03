expected <- eval(parse(text="list(NULL, NULL, c(\"a\", \"b\", \"c\"), NULL, c(\"V5\", \"V6\", \"V7\", \"V8\", \"V9\"))"));        
test(id=0, code={        
argv <- eval(parse(text="list(list(NULL), list(NULL, c(\"a\", \"b\", \"c\"), NULL, c(\"V5\", \"V6\", \"V7\", \"V8\", \"V9\")))"));        
do.call(`c`, argv);        
}, o=expected);        

