expected <- eval(parse(text="TRUE"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L), .Label = c(\"freckle\", \"superficial\", \"nodular\", \"indeterminate\"), class = \"factor\", contrasts = \"contr.treatment\"))"));                
do.call(`is.atomic`, argv);                
}, o=expected);                

