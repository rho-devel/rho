expected <- eval(parse(text="NA_integer_"));     
test(id=0, code={     
argv <- eval(parse(text="list(\"package:methods\", c(\".GlobalEnv\", \"package:graphics\", \"package:stats\", \"Autoloads\", \"package:base\"), NA_integer_)"));     
.Internal(charmatch(argv[[1]], argv[[2]], argv[[3]]));     
}, o=expected);     

