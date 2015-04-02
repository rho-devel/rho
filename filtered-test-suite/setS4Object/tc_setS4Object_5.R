expected <- eval(parse(text="structure(c(\"nonStructure\", \"ANY\", \"ANY\", \"ANY\"), .Names = c(NA_character_, NA_character_, NA_character_, NA_character_), package = character(0), class = structure(\"signature\", package = \"methods\"))"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(\"nonStructure\", \"ANY\", \"ANY\", \"ANY\"), .Names = c(NA_character_, NA_character_, NA_character_, NA_character_), package = character(0), class = structure(\"signature\", package = \"methods\")), TRUE, 0L)"));     
.Internal(`setS4Object`(argv[[1]], argv[[2]], argv[[3]]));     
}, o=expected);     

