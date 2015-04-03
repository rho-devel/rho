expected <- eval(parse(text="list(structure(list(structure(\"vpl1\", class = c(\"vpListing\", \"gridVectorListing\", \"gridListing\")), structure(\"1\", class = c(\"vpUpListing\", \"gridVectorListing\", \"gridListing\"))), class = c(\"gridListListing\", \"gridListing\")), structure(\"vpl2\", class = c(\"vpListing\", \"gridVectorListing\", \"gridListing\")))"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(list(structure(list(structure(\"vpl1\", class = c(\"vpListing\", \"gridVectorListing\", \"gridListing\")), structure(\"1\", class = c(\"vpUpListing\", \"gridVectorListing\", \"gridListing\"))), class = c(\"gridListListing\", \"gridListing\"))), list(structure(\"vpl2\", class = c(\"vpListing\", \"gridVectorListing\", \"gridListing\"))))"));                  
do.call(`c`, argv);                  
}, o=expected);                  

