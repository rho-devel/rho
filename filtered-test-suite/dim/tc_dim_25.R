expected <- eval(parse(text="NULL"));       
test(id=0, code={       
argv <- eval(parse(text="list(c(NA, NA, NA, NA, NA, \"Ripley\", \"Venables & Smith\"))"));       
do.call(`dim`, argv);       
}, o=expected);       

