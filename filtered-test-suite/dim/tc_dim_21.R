expected <- eval(parse(text="NULL"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(c(0, 1, 131072, 129140163, 17179869184, 762939453125, 16926659444736, 232630513987207, 2251799813685248, 16677181699666568, 1e+17))"));                  
do.call(`dim`, argv);                  
}, o=expected);                  

