## Ensure consistent "diss.." class --- make "namespace-private-global !
dissiCl <- c("dissimilarity", "dist")

## consistent error / warning messages; could use for internationalization
..msg <-
    list(error =
         c(NAdiss = "NA-values in the dissimilarity matrix not allowed.",
           non.diss="x is not and cannot be converted to class dissimilarity"
           ),
         warn = c()
         )
