expected <- eval(parse(text="NULL"));  
test(id=0, code={  
check_names<-  function ()    
  {   
      x <- array(1:24, c(4, 6))   
      nms <- list(happy = letters[1:4], sad = LETTERS[1:6])   
      dimnames(x) <- nms   
      tmp <- aperm(x, c(2, 1))   
      stopifnot(all.equal(dimnames(tmp), nms[c(2, 1)]))   
      dimnames(x) <- c(nms[1], list(NULL))   
      tmp <- aperm(x, c(2, 1))   
      stopifnot(all.equal(dimnames(tmp), c(list(NULL), nms[1])))   
      names(nms) <- c("happy", "sad")   
      dimnames(x) <- nms   
      tmp <- aperm(x, c(2, 1))   
      stopifnot(all.equal(names(dimnames(tmp)), names(nms[c(2, 1)])))   
      dimnames(x) <- c(nms[1], list(NULL))   
      tmp <- aperm(x, c(2, 1))   
      stopifnot(all.equal(names(dimnames(tmp)), c("", names(nms)[1])))   
  }   
argv <- list();  
do.call(`check_names`, argv);  
}, o=expected);  

