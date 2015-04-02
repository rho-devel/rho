expected <- eval(parse(text="c(1, 0.5, -0.125, 0.0625, -0.0390625, 0.02734375, -0.0205078125, 0.01611328125, -0.013092041015625, 0.0109100341796875, -0.00927352905273438)"));      
test(id=0, code={      
argv <- eval(parse(text="list(0.5, 0:10)"));      
.Internal(choose(argv[[1]], argv[[2]]));      
}, o=expected);      

