expected <- eval(parse(text="TRUE"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE), .Names = c(\"1\", \"2\", \"3\", \"4\", \"5\", \"6\", \"7\", \"8\")))"));                
do.call(`is.atomic`, argv);                
}, o=expected);                

