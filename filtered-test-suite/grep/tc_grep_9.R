expected <- eval(parse(text="integer(0)"));                
test(id=0, code={                
argv <- eval(parse(text="list(\"-package$\", structure(c(\"bkde\", \"bkde2D\", \"bkfe\", \"dpih\", \"dpik\", \"dpill\", \"locpoly\"), .Names = c(\"/home/lzhao/tmp/RtmphvE7Uy/ltxf49c4960bf/bkde.tex\", \"/home/lzhao/tmp/RtmphvE7Uy/ltxf49c4960bf/bkde2D.tex\", \"/home/lzhao/tmp/RtmphvE7Uy/ltxf49c4960bf/bkfe.tex\", \"/home/lzhao/tmp/RtmphvE7Uy/ltxf49c4960bf/dpih.tex\", \"/home/lzhao/tmp/RtmphvE7Uy/ltxf49c4960bf/dpik.tex\", \"/home/lzhao/tmp/RtmphvE7Uy/ltxf49c4960bf/dpill.tex\", \"/home/lzhao/tmp/RtmphvE7Uy/ltxf49c4960bf/locpoly.tex\")), FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)"));                
.Internal(grep(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));                
}, o=expected);                

