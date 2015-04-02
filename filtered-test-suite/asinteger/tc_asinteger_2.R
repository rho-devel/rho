expected <- eval(parse(text="c(33L, 34L, 35L, 36L, 37L, 38L, 18L, 19L, 20L, 21L, 22L, 23L, 36L, 37L, 38L, 39L)"));              
test(id=0, code={              
argv <- eval(parse(text="list(c(\"   33\", \"   34\", \"   35\", \"   36\", \"   37\", \"   38\", \"   18\", \"   19\", \"   20\", \"   21\", \"   22\", \"   23\", \"   36\", \"   37\", \"   38\", \"   39\"))"));              
do.call(`as.integer`, argv);              
}, o=expected);              

