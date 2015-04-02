expected <- eval(parse(text="c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)"));             
test(id=0, code={             
argv <- eval(parse(text="list(c(\"  \\036 The other major change was an error for asymmetric loss matrices,\", \"    prompted by a user query.  With L=loss asymmetric, the altered\", \"    priors were computed incorrectly - they were using L' instead of L.\", \"    Upshot - the tree would not not necessarily choose optimal splits\", \"    for the given loss matrix.  Once chosen, splits were evaluated\", \"    correctly.  The printed “improvement” values are of course the\", \"    wrong ones as well.  It is interesting that for my little test\", \"    case, with L quite asymmetric, the early splits in the tree are\", \"    unchanged - a good split still looks good.\"))"));             
do.call(`nzchar`, argv);             
}, o=expected);             

