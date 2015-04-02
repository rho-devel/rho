expected <- eval(parse(text="structure(c(1, 1.4142135623731, 1.73205080756888, 2, 2.23606797749979, 2.44948974278318, 2.64575131106459, 2.82842712474619, 3, 3.16227766016838), id = \"test 1\", class = structure(\"withId\", package = \".GlobalEnv\"))"));            
test(id=0, code={            
argv <- eval(parse(text="list(structure(1:10, id = \"test 1\", class = structure(\"withId\", package = \".GlobalEnv\")))"));            
do.call(`sqrt`, argv);            
}, o=expected);            

