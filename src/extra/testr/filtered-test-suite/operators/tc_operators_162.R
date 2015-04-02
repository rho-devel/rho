expected <- eval(parse(text="c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(c(18000, 315550800, 631170000, 946702800, 1262322000, 1577854800), class = c(\"POSIXct\", \"POSIXt\"), tzone = \"\"), 28304640)"));          
do.call(`>=`, argv);          
}, o=expected);          

