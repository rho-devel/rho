expected <- eval(parse(text="c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(c(1208822400, 1208908800, 1208995200, 1209081600, 1209168000, 1209254400), class = c(\"POSIXct\", \"POSIXt\"), tzone = \"GMT\"), structure(1209168000, class = c(\"POSIXct\", \"POSIXt\"), tzone = \"GMT\"))"));  
do.call(`<=`, argv);  
}, o=expected);  

