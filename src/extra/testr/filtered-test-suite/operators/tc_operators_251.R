expected <- eval(parse(text="structure(c(0, 0.414213562373095, 0.732050807568877, 0, 0.23606797749979, 0.449489742783178, 0.645751311064591, 0.82842712474619, 0, 0.16227766016838), id = \"test 1\", class = structure(\"withId\", package = \".GlobalEnv\"))"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(c(1, 1.4142135623731, 1.73205080756888, 2, 2.23606797749979, 2.44948974278318, 2.64575131106459, 2.82842712474619, 3, 3.16227766016838), id = \"test 1\", class = structure(\"withId\", package = \".GlobalEnv\")), 1)"));           
do.call(`%%`, argv);           
}, o=expected);           

