expected <- eval(parse(text="c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(c(0, 0.414213562373095, 0.732050807568877, 0, 0.23606797749979, 0.449489742783178, 0.645751311064591, 0.82842712474619, 0, 0.16227766016838), id = \"test 1\", class = structure(\"withId\", package = \".GlobalEnv\")), 0.01)"));          
do.call(`<`, argv);          
}, o=expected);          

