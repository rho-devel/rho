expected <- eval(parse(text="\"1 y value <= 0 omitted from logarithmic plot\""));  
test(id=0, code={  
argv <- eval(parse(text="list(\"%d y value <= 0 omitted from logarithmic plot\", 1L)"));  
.Internal(`sprintf`(argv[[1]], argv[[2]]));  
}, o=expected);  

