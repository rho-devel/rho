expected <- eval(parse(text="c(1.17424751923643+0i, 0.38794894958494+1.52883589793151i, 0.38794894958494-1.52883589793151i)"));      
test(id=0, code={      
argv <- eval(parse(text="list(c(1, -1.16348488318732, 0.667550726251972, -0.342308178637008))"));      
.Internal(polyroot(argv[[1]]));      
}, o=expected);      

