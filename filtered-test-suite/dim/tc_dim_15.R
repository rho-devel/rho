expected <- eval(parse(text="c(2L, 4L)"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(structure(c(\"‘[,1]’\", \"‘[,2]’\", \"‘height’\", \"‘weight’\", \"numeric\", \"numeric\", \"Height (in) \", \"Weight (lbs)\"), .Dim = c(2L, 4L)))"));                  
do.call(`dim`, argv);                  
}, o=expected);                  

