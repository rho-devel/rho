expected <- eval(parse(text="TRUE"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(structure(list(Ozone = c(NA, NA, NA, NA, NA, NA, 29L, NA, 71L, 39L, NA, NA, 23L, NA, NA, 21L, 37L, 20L, 12L, 13L, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), Solar.R = c(286L, 287L, 242L, 186L, 220L, 264L, 127L, 273L, 291L, 323L, 259L, 250L, 148L, 332L, 322L, 191L, 284L, 37L, 120L, 137L, 150L, 59L, 91L, 250L, 135L, 127L, 47L, 98L, 31L, 138L), Wind = c(8.6, 9.7, 16.1, 9.2, 8.6, 14.3, 9.7, 6.9, 13.8, 11.5, 10.9, 9.2, 8, 13.8, 11.5, 14.9, 20.7, 9.2, 11.5, 10.3, 6.3, 1.7, 4.6, 6.3, 8, 8, 10.3, 11.5, 14.9, 8), Temp = c(78L, 74L, 67L, 84L, 85L, 79L, 82L, 87L, 90L, 87L, 93L, 92L, 82L, 80L, 79L, 77L, 72L, 65L, 73L, 76L, 77L, 76L, 76L, 76L, 75L, 78L, 73L, 80L, 77L, 83L), Month = c(6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L), Day = 1:30), .Names = c(\"Ozone\", \"Solar.R\", \"Wind\", \"Temp\", \"Month\", \"Day\"), row.names = 32:61, class = \"data.frame\"))"));                  
do.call(`is.list`, argv);                  
}, o=expected);                  

