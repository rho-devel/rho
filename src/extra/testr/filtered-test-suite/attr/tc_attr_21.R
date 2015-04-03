expected <- eval(parse(text="NULL"));                   
test(id=0, code={                   
argv <- eval(parse(text="list(structure(list(ID = c(63L, 63L, 63L, 63L, 63L), Age = c(30L, 30L, 30L, 30L, 30L), OME = structure(c(3L, 3L, 3L, 3L, 3L), .Label = c(\"N/A\", \"high\", \"low\"), class = \"factor\"), Loud = c(35L, 40L, 45L, 50L, 55L), Noise = structure(c(2L, 2L, 2L, 2L, 2L), .Label = c(\"coherent\", \"incoherent\"), class = \"factor\"), Correct = c(1L, 1L, 1L, 3L, 1L), Trials = c(2L, 1L, 1L, 3L, 1L), UID = c(67L, 67L, 67L, 67L, 67L), UIDn = c(67.1, 67.1, 67.1, 67.1, 67.1)), .Names = c(\"ID\", \"Age\", \"OME\", \"Loud\", \"Noise\", \"Correct\", \"Trials\", \"UID\", \"UIDn\"), row.names = c(635L, 639L, 643L, 647L, 651L), class = \"data.frame\"), \"na.action\")"));                   
do.call(`attr`, argv);                   
}, o=expected);                   

