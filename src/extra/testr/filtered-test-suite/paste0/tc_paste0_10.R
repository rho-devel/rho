expected <- eval(parse(text="\"$y\""));       
test(id=0, code={       
argv <- eval(parse(text="list(list(character(0), \"$y\"), NULL)"));       
.Internal(`paste0`(argv[[1]], argv[[2]]));       
}, o=expected);       

