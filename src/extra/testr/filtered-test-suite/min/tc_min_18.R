expected <- eval(parse(text="1.86003798801034e-43"));     
test(id=0, code={     
argv <- eval(parse(text="list(c(4.5241870901798, 0.211646098116025, 1.86003798801034e-43))"));     
do.call(`min`, argv);     
}, o=expected);     

