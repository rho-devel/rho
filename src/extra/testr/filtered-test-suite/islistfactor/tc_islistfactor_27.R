expected <- eval(parse(text="FALSE"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(list(V1 = structure(c(\"head\", \"1\", \"3\", \"6\"), class = \"AsIs\"), V2 = structure(c(\"NA\", \" 2\", \" 4\", \" 7\"), class = \"AsIs\"), V3 = structure(c(\"NA\", \"NA\", \" 5\", \" 8\"), class = \"AsIs\"), V4 = structure(c(\"NA\", \"NA\", \"NA\", \" 9\"), class = \"AsIs\")), .Names = c(\"V1\", \"V2\", \"V3\", \"V4\"), row.names = c(\"1\", \"2\", \"3\", \"4\")), FALSE)"));       
.Internal(`islistfactor`(argv[[1]], argv[[2]]));       
}, o=expected);       

