expected <- eval(parse(text="NULL"));       
test(id=0, code={       
argv <- eval(parse(text="list(c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))"));       
do.call(`dim`, argv);       
}, o=expected);       

