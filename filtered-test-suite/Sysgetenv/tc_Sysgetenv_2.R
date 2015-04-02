expected <- eval(parse(text="NA_character_"));          
test(id=0, code={          
argv <- eval(parse(text="list(\"SWEAVE_OPTIONS\", NA_character_)"));          
.Internal(Sys.getenv(argv[[1]], argv[[2]]));          
}, o=expected);          

