expected <- eval(parse(text="c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)"));          
test(id=0, code={          
argv <- eval(parse(text="list(c(2.00256647265648e-308, 2.22284878464869e-308, 2.22507363599982e-308, 2.2250738585072e-308, 2.22507408101459e-308, 2.22729893236571e-308, 2.44758124435792e-308, 1.61792382137608e+308, 1.79589544172745e+308, 1.797692955093e+308, 1.79769313486232e+308), 1)"));          
do.call(`>=`, argv);          
}, o=expected);          

