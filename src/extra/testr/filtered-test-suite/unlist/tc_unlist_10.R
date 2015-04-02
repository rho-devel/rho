expected <- eval(parse(text="structure(\"     \\\"Jetz no chli züritüütsch: (noch ein bißchen Zürcher deutsch)\\\")\\n\", Rd_tag = \"RCODE\")"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(\"     \\\"Jetz no chli züritüütsch: (noch ein bißchen Zürcher deutsch)\\\")\\n\", Rd_tag = \"RCODE\"), TRUE, TRUE)"));       
.Internal(`unlist`(argv[[1]], argv[[2]], argv[[3]]));       
}, o=expected);       

