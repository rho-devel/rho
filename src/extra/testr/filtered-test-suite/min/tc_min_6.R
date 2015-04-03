expected <- eval(parse(text="structure(1338544800L, class = c(\"POSIXct\", \"POSIXt\"))"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(c(1338544800L, 1338566400L, 1338588000L, 1338609600L, 1338631200L, 1338652800L, 1338674400L, 1338696000L, 1338717600L, 1338739200L, 1338760800L, 1338782400L, 1338804000L, 1338825600L, 1338847200L, 1338868800L, 1338890400L, 1338912000L, 1338933600L, 1338955200L, 1338976800L, 1338998400L, 1339020000L, 1339041600L), class = c(\"POSIXct\", \"POSIXt\"), tzone = \"\"), na.rm = TRUE)"));             
do.call(`min`, argv);             
}, o=expected);             

