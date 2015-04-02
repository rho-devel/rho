expected <- eval(parse(text="c(Inf, -Inf)"));           
test(id=0, code={           
argv <- list();           
do.call(`range`, argv);           
}, o=expected);           

