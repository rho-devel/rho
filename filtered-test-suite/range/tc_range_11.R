expected <- eval(parse(text="structure(c(1L, 5L), .Label = c(\"a\", \"c\", \"i\", \"s\", \"t\"), class = c(\"ordered\", \"factor\"))"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(c(4L, 5L, 1L, 5L, 3L, 4L, 5L, 3L, 2L, 4L), .Label = c(\"a\", \"c\", \"i\", \"s\", \"t\"), class = c(\"ordered\", \"factor\")), structure(c(4L, 2L, 3L, 5L, 4L, 3L, 5L, 1L, 5L, 4L), .Label = c(\"a\", \"c\", \"i\", \"s\", \"t\"), class = c(\"ordered\", \"factor\")), na.rm = FALSE)"));           
do.call(`range`, argv);           
}, o=expected);           

