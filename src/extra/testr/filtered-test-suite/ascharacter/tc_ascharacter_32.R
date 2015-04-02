expected <- eval(parse(text="c(\"I\", \"II\", \"III\", \"IV\")"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(1:4, class = \"roman\"))"));                 
do.call(`as.character`, argv);                 
}, o=expected);                 

