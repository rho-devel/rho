expected <- eval(parse(text="c(NA, 2467L, 2468L, 2470L, 2470L, 2477L, 2478L, 2478L, 2480L, 2480L, 2482L, 2482L, 2482L, 2484L, 2484L, 2486L, 2486L, 2486L, 2490L, 2491L)"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(frow = c(NA, 2467L, 2468L, 2470L, 2470L, 2477L, 2478L, 2478L, 2480L, 2480L, 2482L, 2482L, 2482L, 2484L, 2484L, 2486L, 2486L, 2486L, 2490L, 2491L)), .Names = \"frow\"), 1L)"));                 
do.call(`.subset2`, argv);                 
}, o=expected);                 

