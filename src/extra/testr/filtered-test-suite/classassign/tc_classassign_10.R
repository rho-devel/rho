expected <- eval(parse(text="structure(c(\"o\", \"p\", \"v\", \"i\", \"r\", \"w\", \"b\", \"m\", \"f\", \"s\"), date = structure(1224086400, class = c(\"POSIXct\", \"POSIXt\"), tzone = \"\"), class = \"stamped\")"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(c(\"o\", \"p\", \"v\", \"i\", \"r\", \"w\", \"b\", \"m\", \"f\", \"s\"), date = structure(1224086400, class = c(\"POSIXct\", \"POSIXt\"), tzone = \"\"), class = \"stamped\"), value = \"stamped\")"));   
do.call(`class<-`, argv);   
}, o=expected);   

