expected <- eval(parse(text="FALSE"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, NA, NA, NA, NA, NA), .Label = c(\"[0,2)\", \"[2,4)\", \"[4,6)\", \"[6,8)\"), class = \"factor\"))"));                  
do.call(`is.list`, argv);                  
}, o=expected);                  

