expected <- eval(parse(text="structure(c(0, 0.693147180559945, 0, 0, 1.79175946922805, 1.79175946922805, 6.5792512120101, 0, 15.1044125730755, 15.1044125730755, 25.1912211827387, 12.8018274800815, 6.5792512120101, 8.52516136106541, 19.9872144956619, 17.5023078458739), .Dim = c(4L, 4L), .Dimnames = structure(list(income = c(\"< 15k\", \"15-25k\", \"25-40k\", \"> 40k\"), satisfaction = c(\"VeryD\", \"LittleD\", \"ModerateS\", \"VeryS\")), .Names = c(\"income\", \"satisfaction\")))"));            
test(id=0, code={            
argv <- eval(parse(text="list(structure(c(2, 3, 2, 1, 4, 4, 7, 2, 11, 11, 15, 10, 7, 8, 13, 12), .Dim = c(4L, 4L), .Dimnames = structure(list(income = c(\"< 15k\", \"15-25k\", \"25-40k\", \"> 40k\"), satisfaction = c(\"VeryD\", \"LittleD\", \"ModerateS\", \"VeryS\")), .Names = c(\"income\", \"satisfaction\"))))"));            
do.call(`lgamma`, argv);            
}, o=expected);            

