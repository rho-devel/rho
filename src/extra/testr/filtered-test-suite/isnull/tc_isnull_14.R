expected <- FALSE
test(id=34841, code={
argv <- list(function (file = ifelse(onefile, "Rplots.pdf", "Rplot%03d.pdf"), 
    width, height, onefile, family, title, fonts, version, paper, 
    encoding, bg, fg, pointsize, pagecentre, colormodel, useDingbats, 
    useKerning, fillOddEven, compress) 
{
    initPSandPDFfonts()
    new <- list()
    if (!missing(width)) 
        new$width <- width
    if (!missing(height)) 
        new$height <- height
    if (!missing(onefile)) 
        new$onefile <- onefile
    if (!missing(title)) 
        new$title <- title
    if (!missing(fonts)) 
        new$fonts <- fonts
    if (!missing(version)) 
        new$version <- version
    if (!missing(paper)) 
        new$paper <- paper
    if (!missing(encoding)) 
        new$encoding <- encoding
    if (!missing(bg)) 
        new$bg <- bg
    if (!missing(fg)) 
        new$fg <- fg
    if (!missing(pointsize)) 
        new$pointsize <- pointsize
    if (!missing(pagecentre)) 
        new$pagecentre <- pagecentre
    if (!missing(colormodel)) 
        new$colormodel <- colormodel
    if (!missing(useDingbats)) 
        new$useDingbats <- useDingbats
    if (!missing(useKerning)) 
        new$useKerning <- useKerning
    if (!missing(fillOddEven)) 
        new$fillOddEven <- fillOddEven
    if (!missing(compress)) 
        new$compress <- compress
    old <- check.options(new, name.opt = ".PDF.Options", envir = .PSenv)
    if (!missing(family) && (inherits(family, "Type1Font") || 
        inherits(family, "CIDFont"))) {
        enc <- family$encoding
        if (inherits(family, "Type1Font") && !is.null(enc) && 
            enc != "default" && (is.null(old$encoding) || old$encoding == 
            "default")) 
            old$encoding <- enc
        family <- family$metrics
    }
    if (is.null(old$encoding) || old$encoding == "default") 
        old$encoding <- guessEncoding()
    if (!missing(family)) {
        if (length(family) == 4L) {
            family <- c(family, "Symbol.afm")
        }
        else if (length(family) == 5L) {
        }
        else if (length(family) == 1L) {
            pf <- pdfFonts(family)[[1L]]
            if (is.null(pf)) 
                stop(gettextf("unknown family '%s'", family), 
                  domain = NA)
            matchFont(pf, old$encoding)
        }
        else stop("invalid 'family' argument")
        old$family <- family
    }
    version <- old$version
    versions <- c("1.1", "1.2", "1.3", "1.4", "1.5", "1.6", "1.7", 
        "2.0")
    if (version %in% versions) 
        version <- as.integer(strsplit(version, "[.]")[[1L]])
    else stop("invalid PDF version")
    onefile <- old$onefile
    if (!checkIntFormat(file)) 
        stop(gettextf("invalid 'file' argument '%s'", file), 
            domain = NA)
    .External(C_PDF, file, old$paper, old$family, old$encoding, 
        old$bg, old$fg, old$width, old$height, old$pointsize, 
        onefile, old$pagecentre, old$title, old$fonts, version[1L], 
        version[2L], old$colormodel, old$useDingbats, old$useKerning, 
        old$fillOddEven, old$compress)
    invisible()
})
do.call('is.null', argv);
},  o = expected);

