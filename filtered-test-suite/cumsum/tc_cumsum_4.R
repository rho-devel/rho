expected <- eval(parse(text="structure(c(-191.999930599838, -191.999930599838), .Names = c(\"\", \"\"))"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(c(-191.999930599838, 7.71626352011359e-309), .Names = c(\"\", \"\")))"));             
do.call(`cumsum`, argv);             
}, o=expected);             

