expected <- eval(parse(text="\"complex\""));              
test(id=0, code={              
argv <- eval(parse(text="list(complex(0))"));              
do.call(`class`, argv);              
}, o=expected);              

