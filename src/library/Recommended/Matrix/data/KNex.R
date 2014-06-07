stopifnot(require(Matrix)) # at least its classes

KNex <-
    local({
	load(system.file(file.path("external", "KNex_slots.rda"), package = "Matrix"))
	## -> 'L'
	r <- list(mm = new("dgCMatrix"), y = L[["y"]])
	for (n in c("Dim", "i","p","x"))
	    slot(r$mm, n) <- L[[n]]
	r
    })
