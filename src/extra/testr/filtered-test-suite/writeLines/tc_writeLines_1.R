expected <- NULL      
test(id=45, code={      
argv <- structure(list(text = " \"  A  \"; \"B\" ;\"C\";\" D \";\"E \";  F  ;G  ",       
    con = "foo"), .Names = c("text", "con"))      
do.call('writeLines', argv);      
},  o = expected);      
      
