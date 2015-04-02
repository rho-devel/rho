expected <- eval(parse(text="\"yaxp\""));       
test(id=0, code={       
argv <- eval(parse(text="list(list(\"yaxp\"), TRUE, TRUE)"));       
.Internal(`unlist`(argv[[1]], argv[[2]], argv[[3]]));       
}, o=expected);       

