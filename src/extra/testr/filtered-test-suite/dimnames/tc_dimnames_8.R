expected <- eval(parse(text="list(c(\"[[.data.frame\", \"[[.Date\", \"[[.factor\", \"[[.numeric_version\", \"[[.POSIXct\"), c(\"visible\", \"from\"))"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(list(visible = c(TRUE, TRUE, TRUE, TRUE, TRUE), from = structure(c(2L, 2L, 2L, 2L, 2L), .Label = c(\"CheckExEnv\", \"package:base\", \"package:datasets\", \"package:graphics\", \"package:grDevices\", \"package:methods\", \"package:stats\", \"package:utils\"), class = \"factor\")), .Names = c(\"visible\", \"from\"), row.names = c(\"[[.data.frame\", \"[[.Date\", \"[[.factor\", \"[[.numeric_version\", \"[[.POSIXct\"), class = \"data.frame\"))"));               
do.call(`dimnames`, argv);               
}, o=expected);               

