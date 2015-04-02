expected <- eval(parse(text="\"text> ## The following two examples use latin1 characters: these may not\\ntext> ## appear correctly (or be omitted entirely).\\ntext> plot(1:10, 1:10, main = \\\"text(...) examples\\\\n~~~~~~~~~~~~~~\\\",\\ntext+      sub = \\\"R is GNU ©, but not ® ...\\\")\""));       
test(id=0, code={       
argv <- eval(parse(text="list(list(c(\"text> \", \"text> \", \"text> \", \"text+ \"), c(\"## The following two examples use latin1 characters: these may not\", \"## appear correctly (or be omitted entirely).\", \"plot(1:10, 1:10, main = \\\"text(...) examples\\\\n~~~~~~~~~~~~~~\\\",\", \"     sub = \\\"R is GNU ©, but not ® ...\\\")\")), \"\\n\")"));       
.Internal(`paste0`(argv[[1]], argv[[2]]));       
}, o=expected);       

