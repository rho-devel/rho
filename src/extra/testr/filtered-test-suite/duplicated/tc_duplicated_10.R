expected <- eval(parse(text="c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)"));           
test(id=0, code={           
argv <- eval(parse(text="list(c(\"\\\\title\", \"\\\\name\", \"\\\\alias\", \"\\\\alias\", \"\\\\keyword\", \"\\\\keyword\", \"\\\\description\", \"\\\\usage\", \"\\\\arguments\", \"\\\\details\", \"\\\\value\", \"\\\\section\", \"\\\\section\", \"\\\\seealso\", \"\\\\examples\"), FALSE, FALSE, NA)"));           
.Internal(duplicated(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));           
}, o=expected);           

