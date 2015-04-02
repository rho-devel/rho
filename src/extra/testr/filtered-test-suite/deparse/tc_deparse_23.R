expected <- eval(parse(text="\"cor(Z[, FALSE], use = \\\"pairwise.complete.obs\\\", method = \\\"kendall\\\")\""));     
test(id=0, code={     
argv <- eval(parse(text="list(quote(cor(Z[, FALSE], use = \"pairwise.complete.obs\", method = \"kendall\")), 60L, TRUE, 69, -1L)"));     
.Internal(`deparse`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));     
}, o=expected);     

