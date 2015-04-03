expected <- TRUE        
test(id=15, code={        
argv <- list(structure(list(onefile = TRUE, family = "Helvetica", title = "R Graphics Output",         
    fonts = NULL, encoding = "default", bg = "transparent", fg = "black",         
    width = 0, height = 0, horizontal = TRUE, pointsize = 12,         
    paper = "default", pagecentre = TRUE, print.it = FALSE, command = "default",         
    colormodel = "srgb", useKerning = TRUE, fillOddEven = FALSE), .Names = c("onefile",         
"family", "title", "fonts", "encoding", "bg", "fg", "width",         
"height", "horizontal", "pointsize", "paper", "pagecentre", "print.it",         
"command", "colormodel", "useKerning", "fillOddEven")))        
do.call('is.list', argv);        
},  o = expected);        
        
