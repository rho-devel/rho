expected <- eval(parse(text="8L"));     
test(id=0, code={     
argv <- eval(parse(text="list(\"package:methods\", c(\".GlobalEnv\", \"CheckExEnv\", \"package:stats\", \"package:graphics\", \"package:grDevices\", \"package:utils\", \"package:datasets\", \"package:methods\", \"Autoloads\", \"package:base\"), NA_integer_)"));     
.Internal(charmatch(argv[[1]], argv[[2]], argv[[3]]));     
}, o=expected);     

