expected <- FALSE            
test(id=681, code={            
argv <- list(structure(list(platform = "x86_64-unknown-linux-gnu", arch = "x86_64",             
    os = "linux-gnu", system = "x86_64, linux-gnu", status = "",             
    major = "3", minor = "1.1", year = "2014", month = "07",             
    day = "10", "svn rev" = "66115", language = "R", version.string = "R version 3.1.1 (2014-07-10)",             
    nickname = "Sock it to Me"), .Names = c("platform", "arch",             
"os", "system", "status", "major", "minor", "year", "month",             
"day", "svn rev", "language", "version.string", "nickname"), class = "simple.list"))            
do.call('is.function', argv);            
},  o = expected);            
            
