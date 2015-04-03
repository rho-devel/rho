expected <- eval(parse(text="FALSE"));    
test(id=0, code={    
argv <- eval(parse(text="list(c(0.367649186507741, -0.792514168501158, 0.0770550181313438, 0.193209990320579, 0.556026088821232, -1.90995675991293, 1.21007077813812, -1.22764970620883))"));    
do.call(`is.raw`, argv);    
}, o=expected);    

