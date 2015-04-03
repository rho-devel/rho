expected <- eval(parse(text="\"\""));       
test(id=0, code={       
argv <- eval(parse(text="list(list(character(0), character(0)), \"\\n\")"));       
.Internal(`paste0`(argv[[1]], argv[[2]]));       
}, o=expected);       

