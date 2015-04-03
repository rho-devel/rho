expected <- eval(parse(text="146L"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(c(42L, 1L, 0L, 16L, 84L, 0L, 3L, 0L, 0L), .Dim = c(3L, 3L), .Dimnames = structure(list(c(\"(0,2.5]\", \"(2.5,4]\", NA), c(\"(2,5.5]\", \"(5.5,10]\", NA)), .Names = c(\"\", \"\")), class = \"table\"))"));                 
do.call(`sum`, argv);                 
}, o=expected);                 

