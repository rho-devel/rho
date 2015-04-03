expected <- eval(parse(text="NULL"));   
test(id=0, code={   
argv <- eval(parse(text="list(\"Invalid file name(s) for R code in ./myTst/R:\\n  'file55711ba85492'\\n are now renamed to 'z<name>.R'\", quote(package.skeleton(\"myTst\", code_files = tmp)))"));   
.Internal(`.dfltWarn`(argv[[1]], argv[[2]]));   
}, o=expected);   

