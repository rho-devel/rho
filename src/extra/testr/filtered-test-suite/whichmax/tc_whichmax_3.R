expected <- eval(parse(text="1L"));    
test(id=0, code={    
argv <- eval(parse(text="list(c(7985.84636551931, 7366.07281363396, 7342.71367123673, 7315.48787041648, 7290.90503004105))"));    
.Internal(which.max(argv[[1]]));    
}, o=expected);    

