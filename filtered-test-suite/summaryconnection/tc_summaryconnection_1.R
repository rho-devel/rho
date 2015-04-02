expected <- eval(parse(text="structure(list(description = \"stderr\", class = \"terminal\", mode = \"w\", text = \"text\", opened = \"opened\", `can read` = \"no\", `can write` = \"yes\"), .Names = c(\"description\", \"class\", \"mode\", \"text\", \"opened\", \"can read\", \"can write\"))"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(2L, class = c(\"terminal\", \"connection\")))"));  
.Internal(summary.connection(argv[[1]]));  
}, o=expected);  

