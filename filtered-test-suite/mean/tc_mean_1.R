expected <- eval(parse(text="98.0323303457106"));   
test(id=0, code={   
argv <- eval(parse(text="list(c(95.4489970123773, 98.5489970123773, 98.5489970123773, 98.5489970123773, 98.5489970123773, 98.5489970123773))"));   
.Internal(`mean`(argv[[1]]));   
}, o=expected);   

