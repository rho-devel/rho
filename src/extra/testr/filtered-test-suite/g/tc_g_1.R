expected <- eval(parse(text="function (foo) foo"));  
test(id=0, code={  
g<-  function (fitted)    
  {   
      on.exit(remove(fitted))   
      return(function(foo) foo)   
  }   
argv <- eval(parse(text="list(1)"));  
do.call(`g`, argv);  
}, o=expected);  

