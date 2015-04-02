expected <- eval(parse(text="structure(list(id = character(0), class = structure(\"withId\", package = \".GlobalEnv\")), .Names = c(\"id\", \"class\"))"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(c(1+1i, 2+1.4142135623731i, 3+1.73205080756888i, 4+2i, 5+2.23606797749979i, 6+2.44948974278318i, 7+2.64575131106459i, 8+2.82842712474619i, 9+3i, 10+3.1622776601684i), id = character(0), class = structure(\"withId\", package = \".GlobalEnv\")))"));               
do.call(`attributes`, argv);               
}, o=expected);               

