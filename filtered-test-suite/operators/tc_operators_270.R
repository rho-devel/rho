expected <- eval(parse(text="c(TRUE, TRUE, TRUE, TRUE, TRUE)"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(c(13991, 13992, 13993, 13994, 13995), class = \"Date\", labels = c(\"Apr 22\", \"Apr 23\", \"Apr 24\", \"Apr 25\", \"Apr 26\")), 13991)"));  
do.call(`>=`, argv);  
}, o=expected);  

