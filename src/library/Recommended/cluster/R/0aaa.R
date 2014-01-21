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

## Not exported, and only used because CRAN checks must be faster
doExtras <- function() {
    interactive() || nzchar(Sys.getenv("R_CLUSTER_CHECK_EXTRA")) ||
        identical("true", unname(Sys.getenv("R_PKG_CHECKING_doExtras")))
}
