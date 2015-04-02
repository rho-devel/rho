expected <- eval(parse(text="structure(FALSE, .Names = NA_character_)"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(TRUE, .Names = NA_character_))"));        
do.call(`is.na`, argv);        
}, o=expected);        

