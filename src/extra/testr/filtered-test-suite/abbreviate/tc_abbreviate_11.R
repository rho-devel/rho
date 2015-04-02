expected <- eval(parse(text="c(\"Svnst\", \"N.462\", \"Mnchr\", \"N.475\", \"Velvt\", \"Ptlnd\", \"Glbrn\", \"N.457\", \"WN.38\", \"Trebi\")"));     
test(id=0, code={     
argv <- eval(parse(text="list(c(\"Svansota\", \"No. 462\", \"Manchuria\", \"No. 475\", \"Velvet\", \"Peatland\", \"Glabron\", \"No. 457\", \"Wisconsin No. 38\", \"Trebi\"), 5, TRUE)"));     
.Internal(abbreviate(argv[[1]], argv[[2]], argv[[3]]));     
}, o=expected);     

