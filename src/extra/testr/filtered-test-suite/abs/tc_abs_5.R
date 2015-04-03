expected <- eval(parse(text="structure(c(7.0990260398094, 6.52913885777653, 3.11767063409183, 18.6913646342089), .Dim = c(4L, 1L))"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(c(-7.0990260398094, -6.52913885777653, -3.11767063409183, -18.6913646342089), .Dim = c(4L, 1L)))"));              
do.call(`abs`, argv);              
}, o=expected);              

