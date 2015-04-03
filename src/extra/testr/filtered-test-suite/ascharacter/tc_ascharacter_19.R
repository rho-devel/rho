expected <- eval(parse(text="c(\"George E. P. Box\", \"David R. Cox\")"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(structure(list(given = c(\"George\", \"E.\", \"P.\"), family = \"Box\", role = NULL, email = NULL, comment = NULL), .Names = c(\"given\", \"family\", \"role\", \"email\", \"comment\")), structure(list(given = c(\"David\", \"R.\"), family = \"Cox\", role = NULL, email = NULL, comment = NULL), .Names = c(\"given\", \"family\", \"role\", \"email\", \"comment\"))), class = \"person\"))"));                 
do.call(`as.character`, argv);                 
}, o=expected);                 

