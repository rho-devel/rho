expected <- eval(parse(text="FALSE"));            
test(id=0, code={            
argv <- eval(parse(text="list(structure(c(-5.96781464124519, -6.49437440734601, -3.09795335180399, -6.0516983940436, 2.94181419227242, 1.32243907887975, -6.14000748997388, -1.17705131190311), .Dim = c(4L, 2L), .Dimnames = list(c(\"Murder\", \"Assault\", \"UrbanPop\", \"Rape\"), c(\"PC1\", \"PC2\"))))"));            
do.call(`is.language`, argv);            
}, o=expected);            

