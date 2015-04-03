expected <- eval(parse(text="structure(\".\", Rd_tag = \"TEXT\")"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(\"to be supported).\", Rd_tag = \"TEXT\"), 17L, 17L)"));                
.Internal(substr(argv[[1]], argv[[2]], argv[[3]]));                
}, o=expected);                

