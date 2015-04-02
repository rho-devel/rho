expected <- eval(parse(text="4.9306115419259e+108"));             
test(id=0, code={             
argv <- eval(parse(text="list(4.9306115419259e+108)"));             
.Internal(mean(argv[[1]]));             
}, o=expected);             

