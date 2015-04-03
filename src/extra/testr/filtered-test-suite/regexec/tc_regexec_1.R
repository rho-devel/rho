expected <- eval(parse(text="list(structure(c(1L, 1L, 1L, 8L, 20L, 21L, 23L), match.length = c(26L, 7L, 4L, 12L, 3L, 2L, 4L)))"));  
test(id=0, code={  
argv <- eval(parse(text="list(\"^(([^:]+)://)?([^:/]+)(:([0-9]+))?(/.*)\", \"http://stat.umn.edu:80/xyz\", FALSE, FALSE, FALSE)"));  
.Internal(regexec(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));  
}, o=expected);  

