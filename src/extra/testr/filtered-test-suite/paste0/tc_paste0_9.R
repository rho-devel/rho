expected <- eval(parse(text="c(\"Package: \\\\tab myTst2\\\\cr\", \"Type: \\\\tab Package\\\\cr\", \"Version: \\\\tab 1.0\\\\cr\", \"Date: \\\\tab 2014-03-17\\\\cr\", \"License: \\\\tab What license is it under?\\\\cr\", \"Depends: \\\\tab methods\\\\cr\")"));       
test(id=0, code={       
argv <- eval(parse(text="list(list(c(\"Package:\", \"Type:\", \"Version:\", \"Date:\", \"License:\", \"Depends:\"), \" \\\\tab \", structure(c(\"myTst2\", \"Package\", \"1.0\", \"2014-03-17\", \"What license is it under?\", \"methods\"), .Names = c(\"Package\", \"Type\", \"Version\", \"Date\", \"License\", \"Depends\")), \"\\\\cr\"), NULL)"));       
.Internal(`paste0`(argv[[1]], argv[[2]]));       
}, o=expected);       

