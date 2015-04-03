expected <- eval(parse(text="-1L"));      
test(id=0, code={      
argv <- eval(parse(text="list(\"/home/lzhao/R/x86_64-unknown-linux-gnu-library/3.0/FALSE\", 5)"));      
.Internal(file.access(argv[[1]], argv[[2]]));      
}, o=expected);      

