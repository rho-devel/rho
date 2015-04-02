expected <- eval(parse(text="c(-1.18395294127004, -5.78596479931973e-16)"));         
test(id=0, code={         
argv <- eval(parse(text="list(c(0.0654707112145738, 0.999999999999999))"));         
do.call(`log10`, argv);         
}, o=expected);         

