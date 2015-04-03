expected <- eval(parse(text="2L"));                   
test(id=0, code={                   
argv <- eval(parse(text="list(c(-167.089651989438, -122.420302709026))"));                   
do.call(`length`, argv);                   
}, o=expected);                   

