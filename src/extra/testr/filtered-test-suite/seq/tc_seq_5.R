expected <- eval(parse(text="integer(0)"));                        
test(id=0, code={                        
argv <- eval(parse(text="list(FALSE)"));                        
do.call(`seq_len`, argv);                        
}, o=expected);                        

