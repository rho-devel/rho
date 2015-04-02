expected <- eval(parse(text="TRUE"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(2L, .Label = c(\"Northeast\", \"South\", \"North Central\", \"West\"), class = \"factor\"))"));              
do.call(`is.finite`, argv);              
}, o=expected);              

