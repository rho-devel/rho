expected <- eval(parse(text="structure(list(\"*\", \" \", \"skipping installation test\", \"\\n\", sep = \"\"), .Names = c(\"\", \"\", \"\", \"\", \"sep\"))"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(list(\"*\", \" \", \"skipping installation test\", \"\\n\"), sep = \"\")"));                  
do.call(`c`, argv);                  
}, o=expected);                  

