expected <- eval(parse(text="1:2"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(list(Topic = c(\"myTst-package\", \"foo-class\", \"myTst\", \"show,foo-method\", \"show,foo-method\", \"show-methods\"), File = c(\"myTst-package\", \"foo-class\", \"myTst-package\", \"foo-class\", \"show-methods\", \"show-methods\")), .Names = c(\"Topic\", \"File\"), row.names = c(3L, 1L, 4L, 2L, 6L, 5L)))"));               
do.call(`seq_along`, argv);               
}, o=expected);               

