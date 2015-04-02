expected <- eval(parse(text="c(-3.21402130636699, 101.08748330158, -8.50234284659562)"));       
test(id=0, code={       
argv <- eval(parse(text="list(c(-3.21402130636699, 101.08748330158, -8.50234284659562), value = NULL)"));       
do.call(`names<-`, argv);       
}, o=expected);       

