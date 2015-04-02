expected <- eval(parse(text="structure(\"plot(1:10, 1:10, main = \\\"text(...) examples\\\\n~~~~~~~~~~~~~~\\\",\\n\", Rd_tag = \"RCODE\")"));     
test(id=0, code={     
argv <- eval(parse(text="list(\"\\\\\\\\(l|)dots\", \"...\", structure(\"plot(1:10, 1:10, main = \\\"text(...) examples\\\\n~~~~~~~~~~~~~~\\\",\\n\", Rd_tag = \"RCODE\"), FALSE, TRUE, FALSE, TRUE)"));     
.Internal(`gsub`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));     
}, o=expected);     

