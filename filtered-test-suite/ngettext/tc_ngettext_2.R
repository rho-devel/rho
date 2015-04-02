expected <- eval(parse(text="\"%s are not all TRUE\""));     
test(id=0, code={     
argv <- eval(parse(text="list(2L, \"%s is not TRUE\", \"%s are not all TRUE\", NULL)"));     
.Internal(ngettext(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));     
}, o=expected);     

