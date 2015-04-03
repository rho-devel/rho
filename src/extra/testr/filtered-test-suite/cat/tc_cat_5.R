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
argv <- eval(parse(text="list(\"head\\n\", file = \"foo2\")"));      
do.call(`cat`, argv);      
}, o=expected);      

