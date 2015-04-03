expected <- eval(parse(text="structure(list(structure(9L, members = 1L, height = 0, label = 9L, leaf = TRUE, value = 2L), structure(list(structure(10L, label = 10L, members = 1L, height = 0, leaf = TRUE, value = 1L), structure(1L, label = 1L, members = 1L, height = 0, leaf = TRUE, value = 10L)), members = 2L, midpoint = 0.5, height = 0.114813676452255, value = 5.5)), members = 3L, midpoint = 0.75, height = 0.241190881793568, value = 3.75)"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(structure(9L, members = 1L, height = 0, label = 9L, leaf = TRUE, value = 2L), structure(list(structure(10L, label = 10L, members = 1L, height = 0, leaf = TRUE, value = 1L), structure(1L, label = 1L, members = 1L, height = 0, leaf = TRUE, value = 10L)), members = 2L, midpoint = 0.5, height = 0.114813676452255, value = 5.5)), members = 3L, midpoint = 0.75, height = 0.241190881793568, value = 3.75))"));                 
do.call(`unclass`, argv);                 
}, o=expected);                 

