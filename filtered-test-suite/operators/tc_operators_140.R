expected <- eval(parse(text="structure(c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE), .Dim = 18L, .Dimnames = list(c(\"5\", \"8\", \"9\", \"12\", \"13\", \"16\", \"18\", \"23\", \"27\", \"28\", \"30\", \"31\", \"33\", \"34\", \"43\", \"45\", \"48\", \"161\")))"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(c(2, 2, 1, 1, 1, 0, 1, 2, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0), .Dim = 18L, .Dimnames = list(c(\"5\", \"8\", \"9\", \"12\", \"13\", \"16\", \"18\", \"23\", \"27\", \"28\", \"30\", \"31\", \"33\", \"34\", \"43\", \"45\", \"48\", \"161\"))), 0)"));           
do.call(`>`, argv);           
}, o=expected);           

