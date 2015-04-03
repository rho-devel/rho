expected <- eval(parse(text="TRUE"));              
test(id=0, code={              
argv <- eval(parse(text="list(3.18309886183776e-301)"));              
do.call(`is.finite`, argv);              
}, o=expected);              

