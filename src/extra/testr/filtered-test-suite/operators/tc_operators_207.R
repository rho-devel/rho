expected <- eval(parse(text="c(4.5241870901798, 0.211646098116025, 1.86003798801034e-43)"));     
test(id=0, code={     
argv <- eval(parse(text="list(5, c(0.90483741803596, 0.042329219623205, 3.72007597602068e-44))"));     
do.call(`*`, argv);     
}, o=expected);     

