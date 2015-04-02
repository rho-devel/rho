expected <- eval(parse(text="\"\""));       
test(id=0, code={       
argv <- eval(parse(text="list(list(character(0)), \" \", \" \")"));       
.Internal(`paste`(argv[[1]], argv[[2]], argv[[3]]));       
}, o=expected);       

