expected <- eval(parse(text="structure(list(quote(list), quote(expand.grid(Hair = lab$Hair, Eye = lab$Eye, Sex = lab$Sex, stringsAsFactors = TRUE)), Fr = quote(as.vector(HairEyeColor))), .Names = c(\"\", \"\", \"Fr\"))"));       
test(id=0, code={       
argv <- eval(parse(text="list(quote(list(expand.grid(Hair = lab$Hair, Eye = lab$Eye, Sex = lab$Sex, stringsAsFactors = TRUE), Fr = as.vector(HairEyeColor))), \"list\")"));       
.Internal(`as.vector`(argv[[1]], argv[[2]]));       
}, o=expected);       

