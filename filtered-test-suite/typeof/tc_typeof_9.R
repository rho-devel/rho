expected <- eval(parse(text="\"complex\""));                
test(id=0, code={                
argv <- eval(parse(text="list(c(-21.222245139688+176.377752294836i, -21.222245139688-176.377752294836i, 61.0965873274467+76.779430575699i, 61.0965873274467-76.779430575699i, -11.7486843755171+0i))"));                
.Internal(typeof(argv[[1]]));                
}, o=expected);                

