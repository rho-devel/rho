expected <- eval(parse(text="structure(\"print(.leap.seconds, tz = \\\"PST8PDT\\\")  # and in Seattle's\\n\", Rd_tag = \"RCODE\")"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(\"print(.leap.seconds, tz = \\\"PST8PDT\\\")  # and in Seattle's\\n\", Rd_tag = \"RCODE\"), TRUE, TRUE)"));       
.Internal(`unlist`(argv[[1]], argv[[2]], argv[[3]]));       
}, o=expected);       

