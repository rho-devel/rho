expected <- TRUE       
test(id=17, code={       
argv <- list("\"class\" is a reserved slot name and cannot be redefined")       
do.call('is.character', argv);       
},  o = expected);       
       
