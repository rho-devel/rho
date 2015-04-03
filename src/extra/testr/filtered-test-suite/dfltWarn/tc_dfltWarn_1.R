expected <- eval(parse(text="NULL"));    
test(id=0, code={    
argv <- eval(parse(text="list(\"'f' is deprecated.\\nUse 'convertY' instead.\\nSee help(\\\"Deprecated\\\")\", NULL)"));    
.Internal(.dfltWarn(argv[[1]], argv[[2]]));    
}, o=expected);    

