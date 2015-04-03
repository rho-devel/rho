expected <- eval(parse(text="structure(list(date = structure(1224086400, class = c(\"POSIXct\", \"POSIXt\"), tzone = \"\"), .S3Class = \"stamped\", class = structure(\"stamped\", package = \".GlobalEnv\")), .Names = c(\"date\", \".S3Class\", \"class\"))"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(c(\"o\", \"p\", \"v\", \"i\", \"r\", \"w\", \"b\", \"m\", \"f\", \"s\"), date = structure(1224086400, class = c(\"POSIXct\", \"POSIXt\"), tzone = \"\"), .S3Class = \"stamped\", class = structure(\"stamped\", package = \".GlobalEnv\")))"));               
do.call(`attributes`, argv);               
}, o=expected);               

