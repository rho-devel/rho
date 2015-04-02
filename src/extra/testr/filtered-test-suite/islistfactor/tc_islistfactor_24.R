expected <- eval(parse(text="FALSE"));       
test(id=0, code={       
argv <- eval(parse(text="list(list(structure(list(structure(c(\"\", \"+ M.user\", \"+ Temp\", \"+ M.user:Temp\"), class = \"AsIs\")), row.names = c(NA, -4L), class = \"data.frame\"), structure(list(c(NA, -1, -1, -1)), row.names = c(NA, -4L), class = \"data.frame\"), structure(list(c(NA, 20.5814660332393, 3.80016287449608, 2.78794934284365)), row.names = c(NA, -4L), class = \"data.frame\"), structure(list(c(11, 10, 9, 8)), row.names = c(NA, -4L), class = \"data.frame\"), structure(list(c(32.825622681839, 12.2441566485997, 8.44399377410362, 5.65604443125997)), row.names = c(NA, -4L), class = \"data.frame\"), structure(list(c(92.5235803967766, 73.9421143635373, 72.1419514890413, 71.3540021461976)), row.names = c(NA, -4L), class = \"data.frame\")), FALSE)"));       
.Internal(`islistfactor`(argv[[1]], argv[[2]]));       
}, o=expected);       

