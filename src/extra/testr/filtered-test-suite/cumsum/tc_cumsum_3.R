expected <- eval(parse(text="structure(c(79.3831968838961, 8.55983483385341e+101), .Names = c(\"\", \"\"))"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(c(79.3831968838961, 8.55983483385341e+101), .Names = c(\"\", \"\")))"));             
do.call(`cumsum`, argv);             
}, o=expected);             

