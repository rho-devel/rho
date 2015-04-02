expected <- eval(parse(text="\"UseRS MaY FlY IntO JFK OR LaGuardiA\""));                   
test(id=0, code={                   
argv <- eval(parse(text="list(\"(\\\\w)(\\\\w*)(\\\\w)\", \"\\\\U\\\\1\\\\E\\\\2\\\\U\\\\3\", \"useRs may fly into JFK or laGuardia\", FALSE, TRUE, FALSE, FALSE)"));                   
.Internal(gsub(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));                   
}, o=expected);                   

