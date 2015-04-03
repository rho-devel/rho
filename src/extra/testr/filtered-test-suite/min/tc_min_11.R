expected <- eval(parse(text="0L"));             
test(id=0, code={             
argv <- eval(parse(text="list(c(FALSE, FALSE))"));             
do.call(`min`, argv);             
}, o=expected);             

