expected <- eval(parse(text="-7.12801378828154e+22"));               
test(id=0, code={               
argv <- eval(parse(text="list(7.12801378828154e+22)"));               
do.call(`-`, argv);               
}, o=expected);               

