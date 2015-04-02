expected <- eval(parse(text="c(105L, 108L, 111L)"));   
test(id=0, code={   
argv <- eval(parse(text="list(105L, 112L, 3L)"));   
do.call(`seq.int`, argv);   
}, o=expected);   

