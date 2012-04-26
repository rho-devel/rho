# SCCS @(#)text.rpart.s	1.12 06/06/01
# This is a modification of text.tree.
# Fancy option has been added in (to mimic post.tree)
#

text.rpart <-
    function(x, splits = TRUE, label, FUN = text, all=FALSE,
             pretty = NULL, digits = getOption("digits") - 3,
             use.n=FALSE, fancy=FALSE, fwidth=.8, fheight =.8, ...)
{
    if(!inherits(x, "rpart")) stop("Not legitimate rpart")
    if(!is.null(x$frame$splits)) x <- rpconvert(x)#Backwards compatability
    if (nrow(x$frame) <= 1)
        stop("fit is not a tree, just a root")

    frame <- x$frame
    # col <- names(frame)
    # ylevels <- attr(x,'ylevels')
    # if(!is.null(ylevels <- attr(x, "ylevels")))
    #    col <- c(col, ylevels)
    # if(is.na(match(label, col)))
    #    stop("Label must be a column label of the frame component of the tree")
    if(!missing(label))
        warning("argument 'label' is currently unused")
    cxy <- par("cxy")                   #character width and height
    if(!is.null(srt <- list(...)$srt) && srt == 90)
        cxy <- rev(cxy)
    xy <- rpartco(x)

    node <- as.numeric(row.names(x$frame))
    is.left <- (node%%2 ==0)            #left hand sons
    node.left <- node[is.left]
    parent <- match(node.left/2, node)

    ##Put left splits at the parent node

    if(splits) {
        left.child <- match(2 * node, node)
        right.child <- match(node * 2 + 1, node)
        rows <- labels(x, pretty = pretty)

        if(fancy) {
            ## put split labels on branches instead of nodes

            xytmp <- rpart.branch(x=xy$x,y=xy$y,node=node)
            leftptx <- (xytmp$x[2L,]+xytmp$x[1L,])/2
            leftpty <- (xytmp$y[2L,]+xytmp$y[1L,])/2
            rightptx <- (xytmp$x[3L,]+xytmp$x[4,])/2
            rightpty <- (xytmp$y[3L,]+xytmp$y[4L,])/2

            FUN(leftptx,leftpty+.52*cxy[2L],
                rows[left.child[!is.na(left.child)]],...)
            FUN(rightptx,rightpty-.52*cxy[2L],
                rows[right.child[!is.na(right.child)]],...)
        }

        else FUN(xy$x, xy$y + 0.5 * cxy[2L], rows[left.child], ...)
    }
    leaves <- if(all) rep(TRUE, nrow(frame)) else frame$var == "<leaf>"

    ylevels <- attr(x,'ylevels')
    stat <- if (is.null(frame$yval2))
        x$functions$text(yval=frame$yval[leaves],
                         dev=frame$dev[leaves],
                         wt=frame$wt[leaves],
                         ylevel=ylevels, digits=digits,
                         n=frame$n[leaves], use.n=use.n)
    else
        x$functions$text(yval=frame$yval2[leaves,],
                         dev=frame$dev[leaves],
                         wt=frame$wt[leaves],
                         ylevel=ylevels, digits=digits,
                         n=frame$n[leaves], use.n=use.n)


    oval <- function(middlex,middley,a,b) {

        theta <- seq(0,2*pi,pi/30)
        newx <- middlex + a*cos(theta)
        newy <- middley + b*sin(theta)

        polygon(newx,newy,border=TRUE,col=0)
        ##	     polygon(newx,newy,border=T)
    }

    rectangle <- function(middlex, middley,a,b) {

        newx <- middlex + c(a,a,-a,-a)
        newy <- middley + c(b,-b,-b,b)

        polygon(newx,newy,border=TRUE,col=0)
        ##	  polygon(newx,newy,border=T)
    }

    if(fancy) {

        ## find maximum length of stat
        maxlen <- max(string.bounding.box(stat)$columns) + 1L
        maxht <- max(string.bounding.box(stat)$rows) + 1L

        if(fwidth<1)  a.length <- fwidth*cxy[1L]*maxlen
        else a.length <- fwidth*cxy[1L]

        if(fheight<1) b.length <- fheight*cxy[2L]*maxht
        else b.length <- fheight*cxy[2L]

### create ovals and rectangles here
        ## sqrt(2) creates the smallest oval that fits around the
        ## best fitting rectangle
        for(i in parent) oval(xy$x[i],xy$y[i],
                              a=sqrt(2)*a.length/2, b=sqrt(2)*b.length/2)
        child <- match(node[frame$var=="<leaf>"],node)
        for(i in child) rectangle(xy$x[i],xy$y[i],
                                  a=a.length/2,b=b.length/2)
    }

    ##if FUN=text then adj=1 puts the split label to the left of the
    ##    split rather than centered
    ##Allow labels at all or just leaf nodes

    ## stick values on nodes
    if(fancy) FUN(xy$x[leaves], xy$y[leaves] + .5 * cxy[2], stat, ...)
    else FUN(xy$x[leaves], xy$y[leaves] - 0.5 * cxy[2], stat, adj=.5, ...)

    invisible()
}
