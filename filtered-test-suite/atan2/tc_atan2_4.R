expected <- eval(parse(text="1.5707963267949+0i"));          
test(id=0, code={          
argv <- eval(parse(text="list(0+1i, 0+0i)"));          
.Internal(atan2(argv[[1]], argv[[2]]));          
}, o=expected);          

