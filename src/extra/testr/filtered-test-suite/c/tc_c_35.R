expected <- eval(parse(text="c(\"‘?’ for shortcuts to help topics.\", \"\", \"  ‘help.search()’ or ‘??’ for finding help pages\", \"  on a vague topic;\", \"  ‘help.start()’ which opens the HTML version of the R\", \"  help pages;\", \"  ‘library()’ for listing available packages and the\", \"  help objects they contain;\", \"  ‘data()’ for listing available data sets;\", \"  ‘methods()’.\", \"\", \"  Use ‘prompt\")"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(c(\"‘?’ for shortcuts to help topics.\", \"\", \"  ‘help.search()’ or ‘??’ for finding help pages\", \"  on a vague topic;\", \"  ‘help.start()’ which opens the HTML version of the R\", \"  help pages;\", \"  ‘library()’ for listing available packages and the\", \"  help objects they contain;\", \"  ‘data()’ for listing available data sets;\", \"  ‘methods()’.\", \"\", \"  Use ‘prompt\"), character(0))"));                  
do.call(`c`, argv);                  
}, o=expected);                  

