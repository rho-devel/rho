expected <- eval(parse(text="FALSE"));       
test(id=0, code={       
argv <- eval(parse(text="list(list(structure(list(structure(\"text\", Rd_tag = \"VERB\")), Rd_tag = \"\\\\alias\"), structure(list(structure(\"text.default\", Rd_tag = \"VERB\")), Rd_tag = \"\\\\alias\")), TRUE)"));       
.Internal(`islistfactor`(argv[[1]], argv[[2]]));       
}, o=expected);       

