expected <- eval(parse(text="TRUE"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(79.5323303457107, 6, 86.1989970123773, 6, 69.7732394366197, 5, 98.0323303457106, 6, 108.032330345711, 6, 89.1989970123773, 6, 114.198997012377, 6, 116.698997012377, 6, 110.365663679044, 6, 124.365663679044, 6, 126.365663679044, 6, 118.032330345711, 6), .Dim = c(6L, 4L), .Dimnames = structure(list(V = c(\"Golden.rain\", \"rep        \", \"Marvellous \", \"rep        \", \"Victory    \", \"rep        \"), N = c(\"0.0cwt\", \"0.2cwt\", \"0.4cwt\", \"0.6cwt\")), .Names = c(\"V\", \"N\"))))"));      
do.call(`is.numeric`, argv);      
}, o=expected);      

