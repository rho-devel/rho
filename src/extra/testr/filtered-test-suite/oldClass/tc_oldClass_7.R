expected <- eval(parse(text="NULL"));                
test(id=0, code={                
argv <- eval(parse(text="list(3.18309886183791e-301)"));                
do.call(`oldClass`, argv);                
}, o=expected);                

