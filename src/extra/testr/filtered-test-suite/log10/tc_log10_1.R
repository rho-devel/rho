expected <- eval(parse(text="c(-1.32790214206428, -1.88605664769316, -2.69897000433602, -4, -4.63827216398241, -5.34678748622466)"));   
test(id=0, code={   
argv <- eval(parse(text="list(c(0.047, 0.013, 0.002, 1e-04, 2.3e-05, 4.5e-06))"));   
do.call(`log10`, argv);   
}, o=expected);   

