expected <- eval(parse(text="c(1, 1, 1, 2, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1)"));               
test(id=0, code={               
argv <- eval(parse(text="list(1, c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE))"));               
do.call(`+`, argv);               
}, o=expected);               

