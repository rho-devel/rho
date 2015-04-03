expected <- eval(parse(text="c(2L, 5L)"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(structure(list(V1 = c(0.497699242085218, 0.991906094830483), V2 = c(0.668466738192365, 0.107943625887856), V3 = c(0.0994661601725966, 0.518634263193235), V4 = c(0.892198335845023, 0.389989543473348), V5 = c(0.79730882588774, 0.410084082046524)), .Names = c(\"V1\", \"V2\", \"V3\", \"V4\", \"V5\"), row.names = c(16L, 18L), class = \"data.frame\"))"));                  
do.call(`dim`, argv);                  
}, o=expected);                  

