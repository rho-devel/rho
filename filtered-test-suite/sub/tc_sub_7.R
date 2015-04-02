expected <- eval(parse(text="\"UseRS may fly into JFK or laGuardia\""));               
test(id=0, code={               
argv <- eval(parse(text="list(\"(\\\\w)(\\\\w*)(\\\\w)\", \"\\\\U\\\\1\\\\E\\\\2\\\\U\\\\3\", \"useRs may fly into JFK or laGuardia\", FALSE, TRUE, FALSE, FALSE)"));               
.Internal(sub(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));               
}, o=expected);               

