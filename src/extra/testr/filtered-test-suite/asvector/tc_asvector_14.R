expected <- eval(parse(text="structure(list(Package = \"myTst\", Type = \"Package\", Title = \"What the package does (short line)\", Version = \"1.0\", Date = \"2014-03-17\", Author = \"Who wrote it\", Maintainer = \"Who to complain to <yourfault@somewhere.net>\", Description = \"More about what it does (maybe more than one line)\", License = \"What license is it under?\", Depends = \"methods\"), .Names = c(\"Package\", \"Type\", \"Title\", \"Version\", \"Date\", \"Author\", \"Maintainer\", \"Description\", \"License\", \"Depends\"))"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(c(\"myTst\", \"Package\", \"What the package does (short line)\", \"1.0\", \"2014-03-17\", \"Who wrote it\", \"Who to complain to <yourfault@somewhere.net>\", \"More about what it does (maybe more than one line)\", \"What license is it under?\", \"methods\"), .Names = c(\"Package\", \"Type\", \"Title\", \"Version\", \"Date\", \"Author\", \"Maintainer\", \"Description\", \"License\", \"Depends\")), \"list\")"));       
.Internal(`as.vector`(argv[[1]], argv[[2]]));       
}, o=expected);       

