expected <- eval(parse(text="NULL"));      
test(id=0, code={      
cat<-  function (..., file = "", sep = " ", fill = FALSE, labels = NULL, append = FALSE)        
  {       
      if (is.character(file))        
          if (file == "")        
              file <- stdout()       
          else if (substring(file, 1L, 1L) == "|") {       
              file <- pipe(substring(file, 2L), "w")       
              on.exit(close(file))       
          }       
          else {       
              file <- file(file, ifelse(append, "a", "w"))       
              on.exit(close(file))       
          }       
      .Internal(cat(list(...), file, sep, fill, labels, append))       
  }       
argv <- eval(parse(text="list(\"#comment\\n\\n#another\\n#\\n#\\n\", \"C1\\tC2\\tC3\\n\\\"Panel\\\"\\t\\\"Area Examined\\\"\\t\\\"# Blemishes\\\"\\n\", \"\\\"1\\\"\\t\\\"0.8\\\"\\t\\\"3\\\"\\n\", \"\\\"2\\\"\\t\\\"0.6\\\"\\t\\\"2\\\"\\n\", \"\\\"3\\\"\\t\\\"0.8\\\"\\t\\\"3\\\"\\n\", file = \"test.dat\", sep = \"\")"));      
do.call(`cat`, argv);      
}, o=expected);      

