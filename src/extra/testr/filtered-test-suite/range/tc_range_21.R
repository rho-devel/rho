expected <- eval(parse(text="structure(c(12053, 12062), class = \"Date\")"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(c(12053, 12054, 12055, 12056, 12057, 12058, 12059, 12060, 12061, 12062), class = \"Date\"), na.rm = TRUE)"));           
do.call(`range`, argv);           
}, o=expected);           

