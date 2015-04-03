expected <- c("foo", "bar")         
test(id=11, code={         
argv <- list(structure(c(0.909297426825682, 0.141120008059867, -0.756802495307928         
), class = c("foo", "bar")))         
do.call('class', argv);         
},  o = expected);         
         
