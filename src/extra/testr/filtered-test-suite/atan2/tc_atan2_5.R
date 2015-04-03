expected <- eval(parse(text="2.4378374329021e-05"));          
test(id=0, code={          
argv <- eval(parse(text="list(2.43782895752771e-05, 0.999996523206508)"));          
.Internal(atan2(argv[[1]], argv[[2]]));          
}, o=expected);          

