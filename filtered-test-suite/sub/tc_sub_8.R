expected <- eval(parse(text="\"msgstr\\\\[0\\\\]\""));               
test(id=0, code={               
argv <- eval(parse(text="list(\"^(msgstr)\\\\[([[:digit:]]+)\\\\].*$\", \"\\\\1\\\\\\\\[\\\\2\\\\\\\\]\", \"msgstr[0] \\\"%d ligne de poids nul non comptabilis<U+00E9>e\\\"\", FALSE, FALSE, FALSE, FALSE)"));               
.Internal(sub(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));               
}, o=expected);               

