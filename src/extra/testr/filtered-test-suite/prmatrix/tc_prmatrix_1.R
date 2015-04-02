expected <- eval(parse(text="structure(c(1, 6, 9, 6, 4, 1, 1, 2, 1, 0.305397625390859, 0.00170825768891124, 8.51556634078892e-12, 0.64987756971621, 0.0197968749793939, 5.28672163823767e-10, 0.00471555351643001, 2.33367394341443e-13, 1.21630438148624e-64, 1, 1, 1), .Dim = c(3L, 7L), .Dimnames = list(NULL, c(\"time\", \"n.risk\", \"n.event\", \"survival\", \"std.err\", \"lower 95% CI\", \"upper 95% CI\")))"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(1, 6, 9, 6, 4, 1, 1, 2, 1, 0.305397625390859, 0.00170825768891124, 8.51556634078892e-12, 0.64987756971621, 0.0197968749793939, 5.28672163823767e-10, 0.00471555351643001, 2.33367394341443e-13, 1.21630438148624e-64, 1, 1, 1), .Dim = c(3L, 7L), .Dimnames = list(NULL, c(\"time\", \"n.risk\", \"n.event\", \"survival\", \"std.err\", \"lower 95% CI\", \"upper 95% CI\"))), c(\"\", \"\", \"\"), c(\"time\", \"n.risk\", \"n.event\", \"survival\", \"std.err\", \"lower 95% CI\", \"upper 95% CI\"), TRUE, FALSE, NULL)"));     
.Internal(prmatrix(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]]));     
}, o=expected);     

