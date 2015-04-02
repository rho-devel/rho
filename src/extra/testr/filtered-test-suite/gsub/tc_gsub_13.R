expected <- eval(parse(text="\"|The| |quick| |brown| èé\""));                   
test(id=0, code={                   
argv <- eval(parse(text="list(\"\\\\b\", \"|\", \"The quick brown èé\", FALSE, TRUE, FALSE, FALSE)"));                   
.Internal(gsub(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));                   
}, o=expected);                   

