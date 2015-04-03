expected <- eval(parse(text="TRUE"));                
test(id=0, code={                
argv <- eval(parse(text="list(c(19.7787405591752, 12504507.4953993, 12504507.4953993, 5.96190157728191e+41))"));                
do.call(`is.atomic`, argv);                
}, o=expected);                

