expected <- eval(parse(text="structure(\"# everything \", Rd_tag = \"VERB\")"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(\"# everything \", Rd_tag = \"VERB\"), TRUE, TRUE)"));                 
.Internal(unlist(argv[[1]], argv[[2]], argv[[3]]));                 
}, o=expected);                 

