expected <- eval(parse(text="NULL"));                   
test(id=0, code={                   
argv <- eval(parse(text="list(c(-21.222245139688+176.377752294836i, -21.222245139688-176.377752294836i, 61.0965873274464+76.7794305756989i, 61.0965873274464-76.7794305756989i, -11.748684375517+0i))"));                   
do.call(`names`, argv);                   
}, o=expected);                   

