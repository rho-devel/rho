expected <- eval(parse(text="\"%%  ~~objects to See Also as \\\\code{\\\\link{~~fun~~}}, ~~~\""));       
test(id=0, code={       
argv <- eval(parse(text="list(list(\"%%  ~~objects to See Also as\", \"\\\\code{\\\\link{~~fun~~}}, ~~~\"), \" \", NULL)"));       
.Internal(`paste`(argv[[1]], argv[[2]], argv[[3]]));       
}, o=expected);       

