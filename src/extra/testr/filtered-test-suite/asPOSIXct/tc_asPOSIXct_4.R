expected <- eval(parse(text="c(43200, 157809600, 315576000, 473428800, 631195200, 788961600, 946728000, 1104580800, 1262347200, 1420113600)"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(list(sec = 0, min = 0L, hour = 12L, mday = 1L, mon = 0L, year = c(70L, 75L, 80L, 85L, 90L, 95L, 100L, 105L, 110L, 115L), wday = 4L, yday = 0L, isdst = -1L), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = \"GMT\"), \"GMT\")"));      
.Internal(as.POSIXct(argv[[1]], argv[[2]]));      
}, o=expected);      

