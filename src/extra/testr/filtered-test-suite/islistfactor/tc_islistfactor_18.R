expected <- eval(parse(text="FALSE"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(list(structure(list(structure(\"rm\", Rd_tag = \"VERB\")), Rd_tag = \"\\\\alias\"), structure(list(structure(\"remove\", Rd_tag = \"VERB\")), Rd_tag = \"\\\\alias\")), TRUE)"));                 
.Internal(islistfactor(argv[[1]], argv[[2]]));                 
}, o=expected);                 

