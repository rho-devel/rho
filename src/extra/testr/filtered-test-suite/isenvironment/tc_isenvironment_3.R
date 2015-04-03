expected <- eval(parse(text="FALSE"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(list(ID = c(65L, 65L), Age = c(18L, 18L), OME = structure(c(1L, 1L), .Label = c(\"N/A\", \"high\", \"low\"), class = \"factor\"), Loud = c(35L, 50L), Noise = structure(c(2L, 2L), .Label = c(\"coherent\", \"incoherent\"), class = \"factor\"), Correct = 0:1, Trials = c(1L, 1L), UID = c(71L, 71L), UIDn = c(71.1, 71.1)), .Names = c(\"ID\", \"Age\", \"OME\", \"Loud\", \"Noise\", \"Correct\", \"Trials\", \"UID\", \"UIDn\"), row.names = c(691L, 701L), class = \"data.frame\"))"));          
do.call(`is.environment`, argv);          
}, o=expected);          

