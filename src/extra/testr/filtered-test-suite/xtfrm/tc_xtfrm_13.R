expected <- eval(parse(text="structure(2.91666666666667, base = 12, lens = 3L, .classes = \"numeric_version\")"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(list(c(2L, 11L, 0L)), class = \"numeric_version\"))"));     
do.call(`xtfrm`, argv);     
}, o=expected);     

