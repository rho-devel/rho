expected <- eval(parse(text="-3L"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(list(c(8.44399377410362, 28.4640218366572, 12.2441566485997)), row.names = c(NA, -3L), class = \"data.frame\"), 1L)"));       
.Internal(`shortRowNames`(argv[[1]], argv[[2]]));       
}, o=expected);       

