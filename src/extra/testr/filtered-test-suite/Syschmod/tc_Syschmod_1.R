expected <- eval(parse(text="logical(0)"));      
test(id=0, code={      
argv <- eval(parse(text="list(character(0), structure(integer(0), class = \"octmode\"), TRUE)"));      
.Internal(Sys.chmod(argv[[1]], argv[[2]], argv[[3]]));      
}, o=expected);      

