expected <- eval(parse(text="23L"));                   
test(id=0, code={                   
argv <- eval(parse(text="list(c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))"));                   
do.call(`length`, argv);                   
}, o=expected);                   

