expected <- eval(parse(text="TRUE"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(\"\\n\", structure(\"c(person(\\\"José\\\", \\\"Pinheiro\\\", role = \\\"aut\\\",\\n                    comment = \\\"S version\\\"),\\n             person(\\\"Douglas\\\", \\\"Bates\\\", role = \\\"aut\\\",\\n                    comment = \\\"up to 2007\\\"),\\n             person(\\\"Saikat\\\", \\\"DebRoy\\\", role = \\\"ctb\\\",\\n                    comment = \\\"up to 2002\\\"),\\n             person(\\\"Deepayan\\\", \\\"Sarkar\\\", role = \\\"ctb\\\",\\n                    comment = \\\"up to 2005\\\"),\\n\\t     person(\\\"R-core\\\", email = \\\"R-core@R-project.org\\\",\\n                    role = c(\\\"aut\\\", \\\"cre\\\")),\\n             person(\\\"EISPACK authors\\\", role = \\\"ctb\\\"))\", .Names = \"Authors@R\"), FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)"));                 
.Internal(grepl(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));                 
}, o=expected);                 

