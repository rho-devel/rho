expected <- eval(parse(text="structure(-2.87322644268389, .Names = \"Var2\")"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(-0.224489795918367, .Names = \"Var2\"), structure(-0.816326530612245, .Names = \"Var1\"))"));          
.Internal(atan2(argv[[1]], argv[[2]]));          
}, o=expected);          

