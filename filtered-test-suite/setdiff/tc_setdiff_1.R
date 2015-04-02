expected <- c("bibtex", "tex")    
test(id=0, code={    
argv <- structure(list(x = c("bibtex", "tex"), y = ".svn"), .Names = c("x",     
"y"))    
do.call('setdiff', argv);    
},  o = expected);    
    
