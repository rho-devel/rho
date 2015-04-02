expected <- eval(parse(text="NULL"));    
test(id=0, code={    
argv <- eval(parse(text="list(\"‘graphics’ namespace cannot be unloaded:\\n  namespace ‘graphics’ is imported by ‘stats’ so cannot be unloaded\", NULL)"));    
.Internal(.dfltWarn(argv[[1]], argv[[2]]));    
}, o=expected);    

