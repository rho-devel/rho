expected <- eval(parse(text="FALSE"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(visible = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), from = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), .Label = \"registered S3method for summary\", class = \"factor\")), .Names = c(\"visible\", \"from\"), row.names = c(\"summary.aspell\", \"summary.ecdf\", \"summary.loess\", \"summary.nls\", \"summary.packageStatus\", \"summary.PDF_Dictionary\", \"summary.PDF_Stream\", \"summary.ppr\", \"summary.prcomp\", \"summary.princomp\", \"summary.stl\", \"summary.tukeysmooth\"), class = \"data.frame\"))"));                 
do.call(`is.matrix`, argv);                 
}, o=expected);                 

