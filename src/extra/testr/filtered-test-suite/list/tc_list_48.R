expected <- eval(parse(text="list(structure(list(stats = c(7, 35, 60, 80, 135), n = 26L, conf = c(46.0561427916751, 73.9438572083249), out = integer(0)), .Names = c(\"stats\", \"n\", \"conf\", \"out\")))"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(structure(list(stats = c(7, 35, 60, 80, 135), n = 26L, conf = c(46.0561427916751, 73.9438572083249), out = integer(0)), .Names = c(\"stats\", \"n\", \"conf\", \"out\")))"));                  
do.call(`list`, argv);                  
}, o=expected);                  

