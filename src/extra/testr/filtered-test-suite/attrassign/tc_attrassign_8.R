expected <- eval(parse(text="structure(4, \"`Object created`\" = \"Sat Dec  7 00:26:20 2013\")"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(4, \"`Object created`\" = \"Sat Dec  7 00:26:20 2013\"), \"Object created\", value = \"Sat Dec  7 00:26:20 2013\")"));           
do.call(`attr<-`, argv);           
}, o=expected);           

