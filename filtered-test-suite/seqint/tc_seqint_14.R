expected <- eval(parse(text="c(0.95, 0.829639050387597, 0.709278100775194, 0.588917151162791, 0.468556201550388, 0.348195251937984)"));            
test(id=0, code={            
argv <- eval(parse(text="list(from = 0.95, by = -0.120360949612403, length.out = 6)"));            
do.call(`seq.int`, argv);            
}, o=expected);            

