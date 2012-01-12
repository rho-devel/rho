
### Copyright (C) 2001-2006  Deepayan Sarkar <Deepayan.Sarkar@R-project.org>
###
### This file is part of the lattice package for R.
### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
###
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
###
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
### MA 02110-1301, USA





## Plan: to make things more modular than they are now. As a first
## step, get a function that does a 3d transformation. Probably a good
## idea to do things in terms of homogeneous coordinates


ltransform3dMatrix <- function(screen, R.mat = diag(4))
{
    rot.mat <- diag(3)
    screen.names <- names(screen)
    screen <- lapply(screen, "*", pi/180)

    for(i in seq_along(screen.names)) {
        th <- screen[[i]]
        cth <- cos(th)
        sth <- sin(th)
        tmp.mat <-
            (if (screen.names[i]=="x")
             matrix(c(1, 0, 0, 0, cth, sth, 0, -sth, cth), 3, 3)
            else if (screen.names[i]=="y")
             matrix(c(cth, 0, -sth, 0, 1, 0, sth, 0, cth), 3, 3)
            else if (screen.names[i]=="z")
             matrix(c(cth, sth, 0, -sth, cth, 0, 0, 0, 1), 3, 3))
        rot.mat <- tmp.mat %*% rot.mat
    }
    rot.mat <- cbind(rot.mat, c(0,0,0))
    rot.mat <- rbind(rot.mat, c(0,0,0,1))
    if (!missing(R.mat)) rot.mat <- rot.mat %*% R.mat
    rot.mat
}





ltransform3dto3d <- function(x, R.mat, dist = 0)
{
    if (length(x) == 0) return(x)
    tdata <- R.mat %*% rbind(x, 1)

    ## back to 3d
    tdata[1,] <- tdata[1,]/tdata[4,]
    tdata[2,] <- tdata[2,]/tdata[4,]
    tdata[3,] <- tdata[3,]/tdata[4,]

    ## now 'perspective' x,y coordinates. z remains unmodified
    if (dist != 0)  ## 1/dist = distance of eye from center
    {
        tdata[1,] <- tdata[1,] / (1/dist - tdata[3,])
        tdata[2,] <- tdata[2,] / (1/dist - tdata[3,])
    }

    tdata[1:3, , drop = FALSE]
}










prepanel.default.cloud <-
    function(perspective = TRUE,
             distance = if (perspective) 0.2 else 0, 
             xlim, ylim, zlim,
             screen = list(z = 40, x = -60),
             R.mat = diag(4),
             aspect = c(1, 1),
             panel.aspect = 1,
             ...,
             zoom = 0.8)
{
    rot.mat <- ltransform3dMatrix(screen = screen, R.mat = R.mat)
    aspect <- rep(aspect, length.out = 2)
    corners <-
        rbind(x = c(-1,1,1,-1,-1,1,1,-1),
              y = c(-1,-1,-1,-1,1,1,1,1) * aspect[1],
              z = c(-1,-1,1,1,-1,-1,1,1) * aspect[2])
    corners <- corners / (2 * max(corners)) ## contain in [-.5, .5] cube
    corners <- ltransform3dto3d(corners, rot.mat, dist = distance)

    xrng <- range(corners[1,])
    yrng <- range(corners[2,])
    slicelen <- max(diff(xrng), diff(yrng)/ panel.aspect)

    list(xlim = extend.limits(xrng, length = slicelen) / zoom,
         ylim = extend.limits(yrng, length = panel.aspect * slicelen) / zoom,
         dx = 1, dy = 1)
}




panel.3dscatter <-
    function(x, y, z, rot.mat = diag(4), distance,
             groups = NULL,
             type = "p",
             xlim, ylim, zlim,
             xlim.scaled,
             ylim.scaled,
             zlim.scaled,
             zero.scaled,
             col,
             ## eventually make all these cloud.3d$col etc (or not)
             col.point = if (is.null(groups)) plot.symbol$col else superpose.symbol$col,
             col.line = if (is.null(groups)) plot.line$col else superpose.line$col,
             lty = if (is.null(groups)) plot.line$lty else superpose.line$lty,
             lwd = if (is.null(groups)) plot.line$lwd else superpose.line$lwd,
             cex = if (is.null(groups)) plot.symbol$cex else superpose.symbol$cex,
             pch = if (is.null(groups)) "+" else superpose.symbol$pch,
             cross,
             ...,
             .scale = FALSE,
             subscripts = TRUE,
             identifier = "3dscatter")
{
    if (.scale)
    {
        ## x, y, z, are usually already scaled to lie within a
        ## bounding box.  However, setting .scale=TRUE will do the
        ## scaling here; this may be helpful for calls to
        ## panel.3dscatter() in user-supplied panel functions.
        x <- xlim.scaled[1] + diff(xlim.scaled) * (x-xlim[1])/diff(xlim)
        y <- ylim.scaled[1] + diff(ylim.scaled) * (y-ylim[1])/diff(ylim)
        z <- zlim.scaled[1] + diff(zlim.scaled) * (z-zlim[1])/diff(zlim)
    }

    ##cloud.3d <- list(col=1, cex=1, lty=1, lwd=1, pch=1)
    plot.symbol <- trellis.par.get("plot.symbol")
    plot.line <- trellis.par.get("plot.line")
    superpose.symbol <- trellis.par.get("superpose.symbol")
    superpose.line <- trellis.par.get("superpose.line")
    if (!missing(col))
    {
        col.point <- col
        col.line <- col
    }

    n <- length(x)
    if (n > 0)
    {
        if (is.null(groups))
        {
            col.point <- rep(col.point, length.out = n)
            col.line <- rep(col.line, length.out = n)
            lty <- rep(lty, length.out = n)
            lwd <- rep(lwd, length.out = n)
            cex <- rep(cex, length.out = n)
            pch <- rep(pch, length.out = n)
        }
        else
        {
            nvals <- nlevels(as.factor(groups))
            groups <- as.numeric(as.factor(groups))[subscripts]

            col.point <- rep(col.point, length.out = nvals)[groups]
            col.line <- rep(col.line, length.out = nvals)[groups]
            lty <- rep(lty, length.out = nvals)[groups]
            lwd <- rep(lwd, length.out = nvals)[groups]
            cex <- rep(cex, length.out = nvals)[groups]
            pch <- rep(pch, length.out = nvals)[groups]
        }


        ## The following code deals with different 'type's. (We allow
        ## for multiple types, but do them sequentially, so
        ## overplotting may happen. The amount of work required to fix
        ## this is too much to make it worth the effort.)

        ## 'points' type
        if (any(c("p", "b", "o") %in% type))
        {
            id <-
                ((x >= xlim.scaled[1]) & (x <= xlim.scaled[2]) &
                 (y >= ylim.scaled[1]) & (y <= ylim.scaled[2]) &
                 (z >= zlim.scaled[1]) & (z <= zlim.scaled[2]) &
                 !is.na(x) & !is.na(y) & !is.na(z))

            m <- ltransform3dto3d(rbind(x, y, z), rot.mat, distance)
            ord <- sort.list(m[3,])
            ord <- ord[id[ord]]

            if (missing(cross)) cross <- all(pch == "+")
            if (cross) ## plot symbols are 3d cross hairs
            {
                tmpx0 <- rep(x[ord], each = 3) - rep(cex[ord], each = 3) * c(0.02, 0, 0)
                tmpx1 <- rep(x[ord], each = 3) + rep(cex[ord], each = 3) * c(0.02, 0, 0)
                tmpy0 <- rep(y[ord], each = 3) - rep(cex[ord], each = 3) * c(0, 0.02, 0)
                tmpy1 <- rep(y[ord], each = 3) + rep(cex[ord], each = 3) * c(0, 0.02, 0)
                tmpz0 <- rep(z[ord], each = 3) - rep(cex[ord], each = 3) * c(0, 0, 0.02)
                tmpz1 <- rep(z[ord], each = 3) + rep(cex[ord], each = 3) * c(0, 0, 0.02)

                m0 <- ltransform3dto3d(rbind(tmpx0, tmpy0, tmpz0), rot.mat, distance)
                m1 <- ltransform3dto3d(rbind(tmpx1, tmpy1, tmpz1), rot.mat, distance)

                lsegments(x0 = m0[1,], y0 = m0[2,],
                          x1 = m1[1,], y1 = m1[2,],
                          col = rep(col.line[ord], each = 3),
                          ...,
                          identifier = paste(identifier, "points", sep = "."))
            }
            else
            {
                lpoints(x = m[1, ord], y = m[2, ord],
                        col = col.point[ord],
                        pch = pch[ord],
                        cex = cex[ord], ...,
                        identifier = identifier)
            }
        }

        ## 'lines' type
        if (any(c("l", "b", "o") %in% type))
        {
            ord <- if (is.null(groups)) TRUE else sort.list(groups)
            tmplen <- length(x)
            tmpx0 <- x[ord][-1]
            tmpx1 <- x[ord][-tmplen]
            tmpy0 <- y[ord][-1]
            tmpy1 <- y[ord][-tmplen]
            tmpz0 <- z[ord][-1]
            tmpz1 <- z[ord][-tmplen]
            tmpcol0 <- col.line[ord][-1]
            tmpcol1 <- col.line[ord][-tmplen]

            ## segments shouldn't join points in different groups 
            tmpcol0[ groups[ord][-1] != groups[ord][-tmplen] ] <- "transparent"

            m0 <- ltransform3dto3d(rbind(tmpx0, tmpy0, tmpz0), rot.mat, distance)
            m1 <- ltransform3dto3d(rbind(tmpx1, tmpy1, tmpz1), rot.mat, distance)

            ## a collection of line segments in 3d space is not well
            ## ordered. This is just a naive heuristic:

            ord <- sort.list(pmax(m0[3,], m1[3,]))
            lsegments(x0 = m0[1, ord], y0 = m0[2, ord],
                      x1 = m1[1, ord], y1 = m1[2, ord],
                      col = tmpcol0[ord],
                      lwd = lwd[ord],
                      lty = lty[ord], ...,
                      identifier = paste(identifier, "lines", sep = "."))
        }


        ## 'histogram' type
        if ("h" %in% type)
        {
            id <-
                ((x >= xlim.scaled[1]) & (x <= xlim.scaled[2]) &
                 (y >= ylim.scaled[1]) & (y <= ylim.scaled[2]) &
                 !is.na(x) & !is.na(y) & !is.na(z))
            m <- ltransform3dto3d(rbind(x, y, z), rot.mat, distance)
            ord <- sort.list(m[3,])
            ord <- ord[id[ord]]
            zero.scaled <-
                if (zero.scaled < zlim.scaled[1]) zlim.scaled[1]
                else if (zero.scaled > zlim.scaled[2]) zlim.scaled[2]
                else zero.scaled
            other.end <- ltransform3dto3d(rbind(x, y, zero.scaled), rot.mat, distance)
            lsegments(m[1,ord], m[2,ord],
                      other.end[1,ord], other.end[2,ord],
                      col = col.line[ord],
                      lty = lty[ord],
                      lwd = lwd[ord], ...,
                      identifier = paste(identifier, "hist", sep = "."))
        }
        if (any(!(type %in% c("p", "h", "l", "b", "o"))))
        {
            warning("'type' has unsupported values")
        }
    }
}




####################################################################
##                     Interface to C code                        ##
####################################################################


## the following is now part of the settings

# palette.shade <-
#     function(irr, ref, height, saturation = .9)
# {
#     hsv(h = height,
#         s = 1 - saturation * (1 - (1-ref)^0.5),
#         v = irr)
# }




panel.3dwire <-
    function(x, y, z, rot.mat = diag(4), distance,
             shade = FALSE,
             shade.colors.palette = trellis.par.get("shade.colors")$palette,
             light.source = c(0, 0, 1000),
             xlim, ylim, zlim,
             xlim.scaled,
             ylim.scaled,
             zlim.scaled,
             col = if (shade) "transparent" else "black",
             lty = 1, lwd = 1,
             alpha,
             col.groups = superpose.polygon$col,
             polynum = 100,
             ...,
             .scale = FALSE,
             drape = FALSE,
             at,
             col.regions = regions$col,
             alpha.regions = regions$alpha,
             identifier = "3dwire")
{

    ## a faster version of panel.3dwire that takes advantage of grid
    ## in R >= 1.8.1's multiple polygon drawing capabilities. The
    ## solution is a bit hackish, it basically keeps track of the
    ## polygons to be drawn in a 'global' variable and draws them all
    ## at once when 'sufficiently many' have been collected.

    ## x, y, z are in a special form here (compared to most other
    ## places in lattice). x and y are short ascending, describing the
    ## grid, and z is the corresponding z values in the order (x1,y1),
    ## (x1,y2), ... . length(z) == length(x) * length(y). Sometimes, z
    ## might be a matrix, which indicates multiple surfaces. Above
    ## description true for each column in that case.

    ## things are slightly different depending on whether shading is
    ## being done. If not, things are relatively simple, and in
    ## particular polygons are drawn one quadrilateral at a
    ## time. However, if shade=T, facets are drawn triangles at a
    ## time. The difference is mostly in C code, but some distinctions
    ## need to be made here as well

    if (.scale)
    {
        ## x, y, z, are usually already scaled to lie within a
        ## bounding box.  However, setting .scale=TRUE will do the
        ## scaling here; this may be helpful for calls to
        ## panel.3dscatter() in user-supplied panel functions.
        x[] <- xlim.scaled[1] + diff(xlim.scaled) * (x-xlim[1])/diff(xlim)
        y[] <- ylim.scaled[1] + diff(ylim.scaled) * (y-ylim[1])/diff(ylim)
        z[] <- zlim.scaled[1] + diff(zlim.scaled) * (z-zlim[1])/diff(zlim)
    }



    if (shade) drape <- FALSE ## shade overrides drape

    regions <-
        if (drape)
            trellis.par.get("regions")
        else {
            bg <- trellis.par.get("background")
            if (bg[["col"]] == "transparent")
                bg[["col"]] <- "white"
            bg
        }

    numcol <- length(at) - 1
    numcol.r <- length(col.regions)
    col.regions <-
        if (numcol.r <= numcol)
            rep(col.regions, length.out = numcol)
        else col.regions[floor(1+(1:numcol-1)*(numcol.r-1)/(numcol-1))]


    ## 2004-03-12 new experimental stuff: when x, y, z are all
    ## matrices of the same dimension, they represent a 3-D surface
    ## parametrized on a 2-D grid (the details of the parametrizing
    ## grid are unimportant). 

    isParametrizedSurface <- is.matrix(x) && is.matrix(y) && is.matrix(z)

    if (isParametrizedSurface)
    {
        is.na(x) <- (x < xlim.scaled[1] | x > xlim.scaled[2])
        is.na(y) <- (y < ylim.scaled[1] | y > ylim.scaled[2])
        is.na(z) <- (z < zlim.scaled[1] | z > zlim.scaled[2])
        htrange <- extend.limits(sqrt(range(x^2 + y^2 + z^2, finite = TRUE)), prop = 0.01)
        ngroups <- 1
    }
    else
    {
        ngroups <- if (is.matrix(z)) ncol(z) else 1
        superpose.polygon <- trellis.par.get("superpose.polygon")
        col.groups <- rep(col.groups, length.out = ngroups)
        if (length(col) > 1) col <- rep(col, length.out = ngroups)


        ## remove things outside xlim and ylim bounds

        id.x <- x >= xlim.scaled[1] & x <= xlim.scaled[2]
        id.y <- y >= ylim.scaled[1] & y <= ylim.scaled[2]

        id.z <- rep(id.y, length(id.x)) & rep(id.x, each = length(id.y))

        x <- x[id.x]
        y <- y[id.y]
        z <- z[id.z]

        htrange <- zlim.scaled
    }


    if (shade)
    {
        shade.colors.palette <- getFunctionOrName(shade.colors.palette)
        pol.x <- numeric(polynum * 3)
        pol.y <- numeric(polynum * 3)

        pol.fill <- character(polynum)
        pol.col <- col
        pol.alpha <- trellis.par.get("shade.colors")$alpha
        if (!missing(alpha)) pol.alpha <- alpha

        count <- 0 ## counts number of polygons stacked up so far
        ## counts number of times grid.polygon has been called
        gridpolycount <- 0 

        wirePolygon <-

            function(xx, yy, misc)
            {

                ## misc:
                ## 1: cos angle between normal and incident light
                ## 2: cos angle between reflected light and eye
                ## 3: z-height averaged
                ## 4: group indicator

                height <- (misc[3] - htrange[1]) / diff(htrange)
                invalid <- (is.na(height) || any(is.na(xx)) ||
                            any(is.na(yy)) || height > 1 || height < 0)

                if (!invalid)
                {
                    pol.x[3 * count + 1:3] <<- xx
                    pol.y[3 * count + 1:3] <<- yy

                    count <<- count + 1
                    pol.fill[count] <<-
                        shade.colors.palette(misc[1], misc[2], height)

                    if (count == polynum)
                    {
                        gridpolycount <<- gridpolycount + 1
                        grid.polygon(x = pol.x, y = pol.y, id.lengths = rep(3, polynum),
                                     default.units = "native",
                                     name = trellis.grobname(paste(identifier,
                                       "polygons", gridpolycount, sep = "."),
                                       type = "panel"),
                                     gp = gpar(fill = pol.fill,
                                     col = pol.col,
                                     lty = lty, lwd = lwd,
                                     alpha = pol.alpha))
                        count <<- 0
                    }
                }
            }


        .Call(wireframePanelCalculations,
              as.double(x),
              as.double(y),
              as.double(z),
              as.double(rot.mat),
              as.double(distance),
              if (isParametrizedSurface) as.integer(ncol(x)) else as.integer(length(x)),
              if (isParametrizedSurface) as.integer(nrow(x)) else as.integer(length(y)),
              as.integer(ngroups),
              as.double(light.source),
              environment(),
              as.integer(shade),
              as.integer(isParametrizedSurface))





        if (count > 0)
        {
            grid.polygon(x = pol.x[1:(count * 3)], y = pol.y[1:(count * 3)],
                         default.units = "native", id.lengths = rep(3, count),
                         name = trellis.grobname(paste(identifier,
                           "polygons", gridpolycount + 1, sep = "."),
                           type = "panel"),
                         gp = gpar(fill = rep(pol.fill, length.out = count),
                         col = rep(pol.col, length.out = count),
                         lty = lty, lwd = lwd,
                         alpha = pol.alpha))
        }

    }
    else  ## no shade
    {

        pol.x <- numeric(polynum * 4)
        pol.y <- numeric(polynum * 4)

        if (length(col.regions) > 1)
        {
            pol.fill <- vector(mode(col.regions), polynum)
            pol.col <-
                if (ngroups == 1 || length(col) == 1) col[1]
                else vector(mode(col), polynum)
            pol.alpha <- alpha.regions
        }
        else if (ngroups == 1)
        {
            pol.fill <- col.regions[1]
            pol.col <- col[1]
            pol.alpha <- alpha.regions
        }
        else
        {
            pol.fill <- vector(mode(col.groups), polynum)
            pol.col <-
                if (length(col) == 1) col[1]
                else vector(mode(col), polynum)
            pol.alpha <- superpose.polygon$alpha
        }
        ## FIXME: should alpha override alpha.regions? NO.
        ## if (!missing(alpha)) pol.alpha <- alpha

        count <- 0 ## counts number of polygons stacked up so far
        ## counts number of times grid.polygon has been called
        gridpolycount <- 0 

        wirePolygon <-

            function(xx, yy, misc)
            {
                ## misc:
                ## 3: z-height averaged
                ## 4: group indicator

                height <- (misc[3] - htrange[1]) / diff(htrange)
                invalid <- (is.na(height) || any(is.na(xx)) || any(is.na(yy)) ||
                            height > 1 || height < 0)

                if (!invalid)
                {
                    pol.x[4 * count + 1:4] <<- xx
                    pol.y[4 * count + 1:4] <<- yy

                    count <<- count + 1

                    if (length(col.regions) > 1)
                    {
                        pol.fill[count] <<- col.regions[(seq_along(at)[at > misc[3]])[1] - 1 ]
                        if (ngroups > 1 && length(col) > 1) pol.col[count] <<- col[as.integer(misc[4])]
                    }
                    ## nothing to do if ngroups == 1
                    else if (ngroups > 1)
                    {
                        pol.fill[count] <<- col.groups[as.integer(as.integer(misc[4]))]
                        if (length(col) > 1) pol.col[count] <<- col[as.integer(misc[4])]
                    }


                    if (count == polynum) {

                        gridpolycount <<- gridpolycount + 1
                        grid.polygon(x = pol.x, y = pol.y, id.lengths = rep(4, polynum),
                                     default.units = "native",
                                     name = trellis.grobname(paste(identifier,
                                       "polygons", gridpolycount, sep = "."),
                                       type = "panel"),
                                     gp = gpar(fill = pol.fill,
                                               col = pol.col, ## FIXME: should be adjustcolor(col, alpha.f = alpha)
                                               lty = lty, lwd = lwd,
                                               alpha = pol.alpha))
                        count <<- 0
                    }
                }
            }


        .Call(wireframePanelCalculations,
              as.double(x),
              as.double(y),
              as.double(z),
              as.double(rot.mat),
              as.double(distance),
              if (isParametrizedSurface) as.integer(ncol(x)) else as.integer(length(x)),
              if (isParametrizedSurface) as.integer(nrow(x)) else as.integer(length(y)),
              as.integer(ngroups),
              as.double(light.source),
              environment(),
              as.integer(shade),
              as.integer(isParametrizedSurface))


        if (count > 0)
        {
            grid.polygon(x = pol.x[1:(count * 4)], y = pol.y[1:(count * 4)],
                         default.units = "native", id.lengths = rep(4, count),
                         name = trellis.grobname(paste(identifier,
                           "polygons", gridpolycount + 1, sep = "."),
                           type = "panel"),
                         gp = gpar(fill = rep(pol.fill, length.out = count),
                         col = rep(pol.col, length.out = count),
                         lty = lty, lwd = lwd,
                         alpha = pol.alpha))
        }

    }

}













panel.cloud <-
    function(x, y, subscripts, z,
             groups = NULL,
             perspective = TRUE,
             distance = if (perspective) 0.2 else 0, 
             xlim, ylim, zlim,
             panel.3d.cloud = "panel.3dscatter",
             panel.3d.wireframe = "panel.3dwire",
             screen = list(z = 40, x = -60),
             R.mat = diag(4), aspect = c(1, 1),
             par.box = NULL,

             xlab, ylab, zlab,
             xlab.default, ylab.default, zlab.default,

             scales.3d,
             proportion = 0.6, wireframe = FALSE,
             ## zoom = 0.8, ## ignored in panel.cloud

             ## The main problem with scales is that it is difficult
             ## to figure out the best way to place the scales.  They
             ## can be specified explicitly using scpos if default is
             ## not OK

             scpos,
             ...,
             at,    ## this is same as 'at' in wireframe
             identifier = "cloud")
{

    ## x, y, z can be matrices and we want to retain them as matrices.
    ## The only reasonable things (other than already numeric) are
    ## factors.  Other things will hopefully cause an error down the
    ## road, and we may want to catch them here some day.

    if (is.factor(x)) x <- as.numeric(x)
    if (is.factor(y)) y <- as.numeric(y)
    if (is.factor(z)) z <- as.numeric(z)

    ## calculate rotation matrix:
    rot.mat <- ltransform3dMatrix(screen = screen, R.mat = R.mat)



    ## 2004-03-12 new experimental stuff: when x, y, z are all
    ## matrices of the same dimension, they represent a 3-D surface
    ## parametrized on a 2-D grid (the details of the parametrizing
    ## grid are unimportant). This is meant only for wireframe

    ## In this case, subscripts will be ignored, because it's not
    ## clear how they should interact

    isParametrizedSurface <-
        wireframe && is.matrix(x) && is.matrix(y) && is.matrix(z)

    if (isParametrizedSurface)
        zrng <- extend.limits(sqrt(range(x^2 + y^2 + z^2, finite = TRUE)))





    if (length(subscripts) > 0)  ## otherwise nothing to draw (not even box ?)
    {

        ## figure out data ranges and tick locations / labels
        ## Information needed: *lim, scales.3d

        ## For now, keep *lim as they are, since in cloud / wireframe
        ## extend.limits type adjustments don't happen by
        ## default. Even if that's done, this may not be the place to
        ## do it (shouldn't really need anything more than *lim and
        ## *axs)

        ## So, get tick labels, and then convert *lim to numeric
        ## Although, this is all unnecessary if arrows = TRUE

        xlabelinfo <-
            calculateAxisComponents(xlim,
                                    at = scales.3d$x.scales$at,
                                    num.limit = NULL,
                                    labels = scales.3d$x.scales$labels,
                                    logsc = scales.3d$x.scales$log,
                                    abbreviate = scales.3d$x.scales$abbreviate,
                                    minlength = scales.3d$x.scales$minlength,
                                    format.posixt = scales.3d$x.scales$format,
                                    n = scales.3d$x.scales$tick.number)


        ylabelinfo <-
            calculateAxisComponents(ylim,
                                    at = scales.3d$y.scales$at,
                                    num.limit = NULL,
                                    labels = scales.3d$y.scales$labels,
                                    logsc = scales.3d$y.scales$log,
                                    abbreviate = scales.3d$y.scales$abbreviate,
                                    minlength = scales.3d$y.scales$minlength,
                                    format.posixt = scales.3d$y.scales$format,
                                    n = scales.3d$y.scales$tick.number)


        zlabelinfo <-
            calculateAxisComponents(zlim,
                                    at = scales.3d$z.scales$at,
                                    num.limit = NULL,
                                    labels = scales.3d$z.scales$labels,
                                    logsc = scales.3d$z.scales$log,
                                    abbreviate = scales.3d$z.scales$abbreviate,
                                    minlength = scales.3d$z.scales$minlength,
                                    format.posixt = scales.3d$z.scales$format,
                                    n = scales.3d$z.scales$tick.number)


        x.at <- xlabelinfo$at
        y.at <- ylabelinfo$at
        z.at <- zlabelinfo$at

        x.at.lab <- xlabelinfo$labels
        y.at.lab <- ylabelinfo$labels
        z.at.lab <- zlabelinfo$labels

        xlim <- xlabelinfo$num.limit
        ylim <- ylabelinfo$num.limit
        zlim <- zlabelinfo$num.limit

        par.box.final <- trellis.par.get("box.3d")
        if (!is.null(par.box)) par.box.final[names(par.box)] <- par.box

        aspect <- rep(aspect, length.out = 2)

        if (!isParametrizedSurface)
        {
            x <- x[subscripts]
            y <- y[subscripts]
            z <- z[subscripts]
        }

        corners <-
            data.frame(x = c(-1, 1, 1,-1,-1, 1, 1,-1),
                       y = c(-1,-1,-1,-1, 1, 1, 1, 1) * aspect[1],
                       z = c(-1,-1, 1, 1,-1,-1, 1, 1) * aspect[2])
        corners <- corners / (2 * max(corners)) ## contain in [-.5, .5] cube

        xlim.scaled <- range(corners$x)
        ylim.scaled <- range(corners$y)
        zlim.scaled <- range(corners$z)

        ## denotes scaled ranges of bounding box. passed to
        ## panel.3dscatter and panel.3dwire in case they are useful

        ## center of bounding box:
        box.center <- matrix(unlist(lapply(corners, mean)), 3, 1)

        ## these are box boundaries:
        pre <- c(1,2,4,1,2,3,4,1,5,6,8,5)
        nxt <- c(2,3,3,4,6,7,8,5,6,7,7,8)

        ## The corners are defined in terms of coordinates in 3-D
        ## space as above. The actual choice of coordinates ideally
        ## should not affect anything, but I haven't checked. Box
        ## boundaries are defined as pairs of corners. The numbers of
        ## the corners and boundaries are helpful in keeping track of
        ## things, and are described in the diagram below.


        ## 1, 2, ..., 8 are the corners, L-1, ..., L-12 the boundaries
        ##
        ##                                   L-11
        ##                           8------------------------7
        ##                         / |                       / |
        ##                        /  |                      /  |
        ##                    L-7/   |L-12              L-6/   |
        ##                      /    |                    /    |
        ##                     /     |                   /     |
        ##                    /      |        L-3       /      |L-10
        ##                   4-------------------------3       |
        ##                   |       |                 |       |
        ##                   |       |                 |       |
        ##                   |       |                 |       |
        ##                   |       |    L-9          |       |
        ##                L-4|       5-----------------|-------6
        ##                   |      /                  |      /
        ##                   |     /                   |     /
        ##                   |    /                 L-2|    /L-5
        ##                   |   /                     |   /
        ##                   |  /L-8                   |  /
        ##                   | /                       | /
        ##                   |/                        |/
        ##                   1-------------------------2
        ##                (0,0,0)          L-1
        ##
        ##
        ## Also the 6 FACES are defined in terms of corners (lines)
        ## as follows:
        ##
        ## F-1 : 1,2,3,4 (1,2,3,4)
        ## F-2 : 2,6,7,3 (5,10,6,2)
        ## F-3 : 6,5,8,7 (9,12,11,10)
        ## F-4 : 5,1,4,8 (8,4,7,12)
        ## F-5 : 1,2,6,5 (1,5,9,8)
        ## F-6 : 4,3,7,8 (3,6,11,7)

        face.corners <- list(c(1,2,3,4),
                             c(2,6,7,3),
                             c(6,5,8,7),
                             c(5,1,4,8),
                             c(1,2,6,5),
                             c(4,3,7,8))

        face.lines <- list(c(1,2,3,4),
                           c(5,10,6,2),
                           c(9,12,11,10),
                           c(8,4,7,12),
                           c(1,5,9,8),
                           c(3,6,11,7))

        ## SCALES : very beta

        tmp <- ltransform3dto3d(t(as.matrix(corners)), rot.mat)
        farthest <- 1  ## used later also
        farval <- tmp[3,1]

        for (i in 2:8)
            if (tmp[3,i] < farval) {
                farthest <- i
                farval <- tmp[3,i]
            }

        ## not foolproof, need to revisit this later
        scale.position <-
            if (farthest == 1) list(x = 3, y = 7, z = 2)
            else if (farthest == 2) list(x = 9, y = 8, z = 10)
            else if (farthest == 3) list(x = 11, y = 7, z = 10)
            else if (farthest == 4) list(x = 11, y = 6, z = 2)
            else if (farthest == 5) list(x = 1, y = 5, z = 4)
            else if (farthest == 6) list(x = 1, y = 8, z = 12)
            else if (farthest == 7) list(x = 3, y = 7, z = 2)
            else if (farthest == 8) list(x = 3, y = 6, z = 10)

        ##original:
            #if (farthest == 1) list(x = 9, y = 5, z = 2)
            #else if (farthest == 2) list(x = 9, y = 8, z = 10)
            #else if (farthest == 3) list(x = 11, y = 7, z = 10)
            #else if (farthest == 4) list(x = 11, y = 6, z = 2)
            #else if (farthest == 5) list(x = 1, y = 5, z = 4)
            #else if (farthest == 6) list(x = 1, y = 8, z = 12)
            #else if (farthest == 7) list(x = 3, y = 7, z = 2)
            #else if (farthest == 8) list(x = 3, y = 6, z = 10)

        if (!missing(scpos))
            scale.position[names(scpos)] <- scpos

        scpos <- scale.position


        labs <- rbind(x = c(0, corners$x[pre[scpos$y]], corners$x[pre[scpos$z]]),
                      y = c(corners$y[pre[scpos$x]], 0, corners$y[pre[scpos$z]]),
                      z = c(corners$z[pre[scpos$x]], corners$z[pre[scpos$y]], 0))

        labs[,1] <- labs[,1] * (1 + scales.3d$x.scales$distance/3)
        labs[,2] <- labs[,2] * (1 + scales.3d$y.scales$distance/3)
        labs[,3] <- labs[,3] * (1 + scales.3d$z.scales$distance/3)

        axes <- rbind(x =
                      c(proportion * corners$x[c(pre[scpos$x], nxt[scpos$x])],
                        corners$x[c(pre[scpos$y], nxt[scpos$y])],
                        corners$x[c(pre[scpos$z], nxt[scpos$z])]),
                      y =
                      c(corners$y[c(pre[scpos$x], nxt[scpos$x])],
                        proportion * corners$y[c(pre[scpos$y], nxt[scpos$y])],
                        corners$y[c(pre[scpos$z], nxt[scpos$z])]),
                      z =
                      c(corners$z[c(pre[scpos$x], nxt[scpos$x])],
                        corners$z[c(pre[scpos$y], nxt[scpos$y])],
                        proportion * corners$z[c(pre[scpos$z], nxt[scpos$z])]))

        axes[,1:2] <- axes[,1:2] * (1 + scales.3d$x.scales$distance/10)
        axes[,3:4] <- axes[,3:4] * (1 + scales.3d$y.scales$distance/10)
        axes[,5:6] <- axes[,5:6] * (1 + scales.3d$z.scales$distance/10)

        ## box ranges and lengths
        cmin <- lapply(corners, min)
        cmax <- lapply(corners, max)
        clen <- lapply(corners, function(x) diff(range(x, finite = TRUE)))


        ## scaled (to bounding box) data
        x <- cmin$x + clen$x * (x-xlim[1])/diff(xlim)
        y <- cmin$y + clen$y * (y-ylim[1])/diff(ylim)
        z <- cmin$z + clen$z * (z-zlim[1])/diff(zlim)
        at <-
            if (isParametrizedSurface)
            {
                zrng.scaled <- extend.limits(sqrt(range(x^2 + y^2 + z^2, finite = TRUE)))
                zrng.scaled[1] + diff(zrng.scaled) * (at - zrng[1])/diff(zrng)
            }
            else cmin$z + clen$z * (at - zlim[1])/diff(zlim)

        ## same?: zero.scaled <- cmin$z + clen$z * (0 - zlim[1])/diff(zlim)
        ## yes.                  cmin$z - clen$z *       zlim[1]/diff(zlim)
        zero.scaled <- cmin$z - clen$z * zlim[1]/diff(zlim)
        ## needed in panel.3dscatter for type = 'h'


        x.at <- cmin$x + clen$x * (x.at-xlim[1])/diff(xlim)
        y.at <- cmin$y + clen$y * (y.at-ylim[1])/diff(ylim)
        z.at <- cmin$z + clen$z * (z.at-zlim[1])/diff(zlim)
        at.len <- length(x.at)
        x.at <- rbind(x = x.at,
                      y = rep(corners$y[pre[scpos$x]], at.len),
                      z = rep(corners$z[pre[scpos$x]], at.len))
        at.len <- length(y.at)
        y.at <- rbind(x = rep(corners$x[pre[scpos$y]], at.len),
                      y = y.at,
                      z = rep(corners$z[pre[scpos$y]], at.len))
        at.len <- length(z.at)
        z.at <- rbind(x = rep(corners$x[pre[scpos$z]], at.len),
                      y = rep(corners$y[pre[scpos$z]], at.len),
                      z = z.at)

        x.at.end <- x.at + scales.3d$x.scales$tck * .05 * labs[,1]
        y.at.end <- y.at + scales.3d$y.scales$tck * .05 * labs[,2]
        z.at.end <- z.at + scales.3d$z.scales$tck * .05 * labs[,3]

        x.labs <- x.at + 2 * scales.3d$x.scales$tck * .05 * labs[,1]
        y.labs <- y.at + 2 * scales.3d$y.scales$tck * .05 * labs[,2]
        z.labs <- z.at + 2 * scales.3d$z.scales$tck * .05 * labs[,3]


        corners <- ltransform3dto3d(t(as.matrix(corners)), rot.mat, distance)

        taxes <- ltransform3dto3d(axes, rot.mat, distance)
        x.at <- ltransform3dto3d(x.at, rot.mat, distance)
        x.labs <- ltransform3dto3d(x.labs, rot.mat, distance)
        x.at.end <- ltransform3dto3d(x.at.end, rot.mat, distance)

        y.at <- ltransform3dto3d(y.at, rot.mat, distance)
        y.labs <- ltransform3dto3d(y.labs, rot.mat, distance)
        y.at.end <- ltransform3dto3d(y.at.end, rot.mat, distance)

        z.at <- ltransform3dto3d(z.at, rot.mat, distance)
        z.labs <- ltransform3dto3d(z.labs, rot.mat, distance)
        z.at.end <- ltransform3dto3d(z.at.end, rot.mat, distance)

        tlabs <- ltransform3dto3d(labs, rot.mat, distance)

        box.center <- ltransform3dto3d(box.center, rot.mat, distance)

        ## Shall now determine which bounding lines should be 'hidden'
        ## (by the data, and hence need to be drawn before the data),
        ## and which should be 'visible'. Will actually consider each
        ## face (one at a time), determine if it is 'visible' (had the
        ## bounding cube been opaque), and if so, mark the lines
        ## forming that face as 'visible'

        ## The logical vector 'mark' will correspond to the 12 lines
        ## (indexing explained in the diagram above). mark = TRUE will
        ## mean that the line will be drawn AFTER the data is
        ## drawn. Start off with all mark = FALSE.

        ## The idea is that for visible faces, the z-value of the
        ## center of the face will be greater than the z-value of the
        ## center of the whole box. This doesn't always work for
        ## perspective plots.

        ##print(box.center)
        mark <- rep(FALSE, 12)
        box.center.z <- box.center[3]

        for (face in 1:6)
            if (mean(corners[3, face.corners[[face]] ]) > box.center.z) ## i.e., face visible
                mark[1:12 %in% face.lines[[face]] ] <- TRUE

        #for (j in 1:12)
        #    if (pre[j]==farthest || nxt[j]==farthest)
        #        mark[j] <- FALSE

        ## This draws the 'back' of the box, i.e., the portion that
        ## should be hidden by the data. This doesn't always work
        ## properly

        lsegments(corners[1, pre[!mark]],
                  corners[2, pre[!mark]],
                  corners[1, nxt[!mark]],
                  corners[2, nxt[!mark]],
                  col = par.box.final$col,
                  lwd = par.box.final$lwd,
                  lty = 2,
                  identifier = paste(identifier, "back.box", sep = "."))


        ## The following portion of code is responsible for drawing
        ## the part of the plot driven by the data. The modus operandi
        ## will be different for cloud and wireframe, since they have
        ## essentially different purpose. For cloud, the data is
        ## unstructured, and x, y and z are all passed to the
        ## panel.3d.cloud function. For wireframe, on the other hand,
        ## x and y must form a regular grid, which sort(unique(<x|y>))
        ## is enough to describe (o.w., greater chances of memory
        ## problems). z would then have to be supplied in a very
        ## particular order. All this is fine, but a problem arises if
        ## we want to allow groups -- multiple surfaces. One option is
        ## to supply a matrix (nx * ny by no.of.groups) for z. This is
        ## OK, but it precludes the posibility of supplying x and y as
        ## only their unique values from the very beginning. So that's
        ## not allowed for grouped displays


        if (wireframe)
        {
            if (isParametrizedSurface)
            {
                ## FIXME: unnecessary copy
                tmp <- z
            }
            else if (is.null(groups))
            {
                nx <- length(unique(x))
                ny <- length(unique(y))
                len <- length(z)
                if (nx * ny == len)
                {
                    ord <- order(x, y)
                    tmp <- z[ord]
                    x <- sort(unique(x[!is.na(x)]))
                    y <- sort(unique(y[!is.na(y)]))
                }
                else
                {
                    ## which means some rows missing, should be NA

                    ## convert z into a (conceptual) matrix, with NA
                    ## entries for those 'missing' from data
                    ## frame. There's scope for ambiguity here, which
                    ## can be avoided by the user.

                    tmp <- rep(NA_real_, nx * ny)
                    ux <- sort(unique(x[!is.na(x)]))
                    uy <- sort(unique(y[!is.na(y)]))
                    idx <- match(x, ux)
                    idy <- match(y, uy)
                    tmp[(idx - 1) * length(uy) + idy] <- z

                    x <- ux
                    y <- uy
                }
            }
            else {

                ## all surfaces have to be on the same regular
                ## grid. No row can be missing, though some z-values
                ## can be NA. Needs a lot of change otherwise

                vals <- sort(unique(groups))
                nvals <- length(vals)
                tmp <- numeric(0)

                for (i in seq_along(vals)) {
                    id <- (groups[subscripts] == vals[i])
                    if (any(id)) {
                        ord <- order(x[id], y[id])
                        tmp <- cbind(tmp, z[id][ord])
                    }
                }

                x <- sort(unique(x))
                y <- sort(unique(y))
            }
            z <- list(NULL) ## hopefully becomes garbage, collected if necessary
            panel.3d.wireframe <- getFunctionOrName(panel.3d.wireframe)

            pargs <- list(x = x, y = y, z = tmp,
                          rot.mat = rot.mat,
                          distance = distance,
                          at = at,
##                          col.regions = col.regions,
                          xlim = xlim,
                          ylim = ylim,
                          zlim = zlim,
                          xlim.scaled = xlim.scaled,
                          ylim.scaled = ylim.scaled,
                          zlim.scaled = zlim.scaled,
                          zero.scaled = zero.scaled,
                          ...)

            if (!("..." %in% names(formals(panel.3d.wireframe))))
                pargs <- pargs[intersect(names(pargs), names(formals(panel.3d.wireframe)))]
            do.call("panel.3d.wireframe", pargs)
        }
        else
        {
            panel.3d.cloud <- getFunctionOrName(panel.3d.cloud)
            pargs <- list(x = x, y = y, z = z,
                          rot.mat = rot.mat,
                          distance = distance,
                          groups = groups,
                          subscripts = subscripts,
                          xlim = xlim,
                          ylim = ylim,
                          zlim = zlim,
                          xlim.scaled = xlim.scaled,
                          ylim.scaled = ylim.scaled,
                          zlim.scaled = zlim.scaled,
                          zero.scaled = zero.scaled,
                          ...)
            if (!("..." %in% names(formals(panel.3d.cloud))))
                pargs <- pargs[intersect(names(pargs), names(formals(panel.3d.cloud)))]
            do.call("panel.3d.cloud", pargs)
        }

        ## This draws the front of the bounding box

        lsegments(corners[1, pre[mark]],
                  corners[2, pre[mark]],
                  corners[1, nxt[mark]],
                  corners[2, nxt[mark]],
                  col = par.box.final$col,
                  lty = par.box.final$lty,
                  lwd = par.box.final$lwd,
                  identifier = paste(identifier, "front.box", sep = "."))

        ## Next part for axes. FIXME: ignoring axis.text$lineheight
        ## because seems overkill, but could add that too.

        axis.text <- trellis.par.get("axis.text")
        axis.line <- trellis.par.get("axis.line")

        xaxis.col.line <-
            if (is.logical(scales.3d$x.scales$col.line)) axis.line$col
            else scales.3d$x.scales$col.line
        xaxis.lty <-
            if (is.logical(scales.3d$x.scales$lty)) axis.line$lwd
            else scales.3d$x.scales$lty
        xaxis.lwd <-
            if (is.logical(scales.3d$x.scales$lwd)) axis.line$lty
            else scales.3d$x.scales$lwd
        xaxis.col.text <-
            if (is.logical(scales.3d$x.scales$col)) axis.text$col
            else scales.3d$x.scales$col
        xaxis.font <-
            if (is.logical(scales.3d$x.scales$font)) axis.text$font
            else scales.3d$x.scales$font
        xaxis.fontface <-
            if (is.logical(scales.3d$x.scales$fontface)) axis.text$fontface
            else scales.3d$x.scales$fontface
        xaxis.fontfamily <-
            if (is.logical(scales.3d$x.scales$fontfamily)) axis.text$fontfamily
            else scales.3d$x.scales$fontfamily
        xaxis.cex <-
            if (is.logical(scales.3d$x.scales$cex)) rep(axis.text$cex, length.out = 1)
            else scales.3d$x.scales$cex
        xaxis.rot <-
            if (is.logical(scales.3d$x.scales$rot)) 0
            else scales.3d$x.scales$rot


        yaxis.col.line <-
            if (is.logical(scales.3d$y.scales$col.line)) axis.line$col
            else scales.3d$y.scales$col.line
        yaxis.lty <-
            if (is.logical(scales.3d$y.scales$lty)) axis.line$lwd
            else scales.3d$y.scales$lty
        yaxis.lwd <-
            if (is.logical(scales.3d$y.scales$lwd)) axis.line$lty
            else scales.3d$y.scales$lwd
        yaxis.col.text <-
            if (is.logical(scales.3d$y.scales$col)) axis.text$col
            else scales.3d$y.scales$col
        yaxis.font <-
            if (is.logical(scales.3d$y.scales$font)) axis.text$font
            else scales.3d$y.scales$font
        yaxis.fontface <-
            if (is.logical(scales.3d$y.scales$fontface)) axis.text$fontface
            else scales.3d$y.scales$fontface
        yaxis.fontfamily <-
            if (is.logical(scales.3d$y.scales$fontfamily)) axis.text$fontfamily
            else scales.3d$y.scales$fontfamily
        yaxis.cex <-
            if (is.logical(scales.3d$y.scales$cex)) rep(axis.text$cex, length.out = 1)
            else scales.3d$y.scales$cex
        yaxis.rot <-
            if (is.logical(scales.3d$y.scales$rot)) 0
            else scales.3d$y.scales$rot


        zaxis.col.line <-
            if (is.logical(scales.3d$z.scales$col.line)) axis.line$col
            else scales.3d$z.scales$col.line
        zaxis.lty <-
            if (is.logical(scales.3d$z.scales$lty)) axis.line$lwd
            else scales.3d$z.scales$lty
        zaxis.lwd <-
            if (is.logical(scales.3d$z.scales$lwd)) axis.line$lty
            else scales.3d$z.scales$lwd
        zaxis.col.text <-
            if (is.logical(scales.3d$z.scales$col)) axis.text$col
            else scales.3d$z.scales$col
        zaxis.font <-
            if (is.logical(scales.3d$z.scales$font)) axis.text$font
            else scales.3d$z.scales$font
        zaxis.fontface <-
            if (is.logical(scales.3d$z.scales$fontface)) axis.text$fontface
            else scales.3d$z.scales$fontface
        zaxis.fontfamily <-
            if (is.logical(scales.3d$z.scales$fontfamily)) axis.text$fontfamily
            else scales.3d$z.scales$fontfamily
        zaxis.cex <-
            if (is.logical(scales.3d$z.scales$cex)) rep(axis.text$cex, length.out = 1)
            else scales.3d$z.scales$cex
        zaxis.rot <-
            if (is.logical(scales.3d$z.scales$rot)) 0
            else scales.3d$z.scales$rot


        if (scales.3d$x.scales$draw) {
            if (scales.3d$x.scales$arrows) {
                larrows(x0 = taxes[1, 1], y0 = taxes[2, 1],
                        x1 = taxes[1, 2], y1 = taxes[2, 2],
                        length = 0.02, unit = "npc",
                        lty = xaxis.lty,
                        lwd = xaxis.lwd,
                        col = xaxis.col.line,
                        identifier = paste(identifier, "x.axis.arrow",
                          sep = "."))
            }
            else {
                lsegments(x0 = x.at[1,], y0 = x.at[2,], x1 = x.at.end[1,], y1 = x.at.end[2,],
                          lty = xaxis.lty,
                          col = xaxis.col.line,
                          lwd = xaxis.lwd,
                          identifier = paste(identifier, "x.axis.ticks",
                            sep = "."))
                ltext(x.at.lab, x = x.labs[1,], y = x.labs[2,],
                      cex = xaxis.cex,
                      srt = xaxis.rot,
                      font = xaxis.font,
                      fontfamily = xaxis.fontfamily,
                      fontface = xaxis.fontface,
                      col = xaxis.col.text,
                      identifier = paste(identifier, "x.axis.labels",
                        sep = "."))
            }
        }

        if (scales.3d$y.scales$draw) {
            if (scales.3d$y.scales$arrows) {
                larrows(x0 = taxes[1, 3], y0 = taxes[2, 3],
                        x1 = taxes[1, 4], y1 = taxes[2, 4],
                        length = 0.02, unit = "npc",
                        lty = yaxis.lty,
                        lwd = yaxis.lwd,
                        col = yaxis.col.line,
                        identifier = paste(identifier, "y.axis.arrow",
                            sep = "."))

            }
            else {
                lsegments(x0 = y.at[1,], y0 = y.at[2,], x1 = y.at.end[1,], y1 = y.at.end[2,],
                          lty = yaxis.lty,
                          col = yaxis.col.line,
                          lwd = yaxis.lwd,
                          identifier = paste(identifier, "y.axis.ticks",
                            sep = "."))

                ltext(y.at.lab, x = y.labs[1,], y = y.labs[2,],
                      cex = yaxis.cex,
                      srt = yaxis.rot,
                      font = yaxis.font,
                      fontfamily = yaxis.fontfamily,
                      fontface = yaxis.fontface,
                      col = yaxis.col.text,
                      identifier = paste(identifier, "y.axis.labels",
                        sep = "."))

            }
        }
        if (scales.3d$z.scales$draw) {
            if (scales.3d$z.scales$arrows) {
                larrows(x0 = taxes[1, 5], y0 = taxes[2, 5],
                        x1 = taxes[1, 6], y1 = taxes[2, 6],
                        length = 0.02, unit = "npc",
                        lty = zaxis.lty,
                        lwd = zaxis.lwd,
                        col = zaxis.col.line,
                        identifier = paste(identifier, "z.axis.arrow",
                          sep = "."))

            }
            else {
                lsegments(x0 = z.at[1,], y0 = z.at[2,], x1 = z.at.end[1,], y1 = z.at.end[2,],
                          lty = zaxis.lty,
                          col = zaxis.col.line,
                          lwd = zaxis.lwd,
                          identifier = paste(identifier, "z.axis.ticks",
                            sep = "."))

                ltext(z.at.lab, x = z.labs[1,], y = z.labs[2,],
                      cex = zaxis.cex,
                      srt = zaxis.rot,
                      font = zaxis.font,
                      fontfamily = zaxis.fontfamily,
                      fontface = zaxis.fontface,
                      col = zaxis.col.text,
                      identifier = paste(identifier, "z.axis.labels",
                        sep = "."))

            }
        }

        xlab <- getLabelList(xlab, trellis.par.get("par.xlab.text"), xlab.default)
        ylab <- getLabelList(ylab, trellis.par.get("par.ylab.text"), ylab.default)
        zlab <- getLabelList(zlab, trellis.par.get("par.zlab.text"), zlab.default)

        ## slightly different frm xyplot etc, in that rot can be
        ## supplied in the *lab lists

        xlab <-
            grobFromLabelList(xlab, name = trellis.grobname("xlab", type=""))
#                              rot = if (is.null(xlab$rot)) 0 else xlab$rot)
        ylab <-
            grobFromLabelList(ylab, name = trellis.grobname("ylab", type=""))
#                              rot = if (is.null(ylab$rot)) 0 else ylab$rot)
        zlab <-
            grobFromLabelList(zlab, name = trellis.grobname("zlab", type=""))
#                              rot = if (is.null(zlab$rot)) 0 else zlab$rot)

        if (!is.null(xlab))
        {
            pushViewport(viewport(x = tlabs[1, 1], y = tlabs[2, 1],
                                  default.units = "native"))
            grid.draw(xlab)
            upViewport()
        }

        if (!is.null(ylab))
        {
            pushViewport(viewport(x = tlabs[1, 2], y = tlabs[2, 2],
                                  default.units = "native"))
            grid.draw(ylab)
            upViewport()
        }


        if (!is.null(zlab))
        {
            pushViewport(viewport(x = tlabs[1, 3], y = tlabs[2, 3],
                                  default.units = "native"))
            grid.draw(zlab)
            upViewport()
        }

    }
}



panel.wireframe <- function(...)
    panel.cloud(..., wireframe = TRUE)




wireframe <- function(x, data, ...) UseMethod("wireframe")


wireframe.matrix <-
    function(x, data = NULL,
             zlab = deparse(substitute(x)),
             aspect,
             ..., xlim, ylim,
             row.values = seq_len(nrow(x)),
             column.values = seq_len(ncol(x)))
{
    stopifnot(length(row.values) == nrow(x),
              length(column.values) == ncol(x))
    if (!is.null(data)) warning("explicit 'data' specification ignored")
    form <- eval(z ~ row * column)
    if (missing(aspect)) aspect <- pmin(ncol(x) / nrow(x), c(Inf, 1))
    data <- expand.grid(row = row.values, column = column.values)
    data$z <- as.vector(as.numeric(x))
    ## if rownames/colnames are non-null, use them to label
    if (missing(xlim))
        xlim <-
            if (!is.null(rownames(x))) rownames(x)
            else range(row.values, finite = TRUE)
    if (missing(ylim))
        ylim <-
            if (!is.null(colnames(x))) colnames(x)
            else range(column.values, finite = TRUE)
    wireframe(form, data, zlab = zlab, aspect = aspect,
              xlim = xlim, ylim = ylim,
              ...)
}



wireframe.formula <-
    function(x,
             data = NULL,
             panel = lattice.getOption("panel.wireframe"),
             default.prepanel = lattice.getOption("prepanel.default.wireframe"),
             ...)
{
    ocall <- sys.call(sys.parent()); ocall[[1]] <- quote(wireframe)
    ccall <- match.call()
    ccall$data <- data
    ccall$panel <- panel
    ccall$default.prepanel <- default.prepanel
    ccall[[1]] <- quote(lattice::cloud)
    ans <- eval.parent(ccall)
    ans$call <- ocall
    ans
}



## FIXME: need settings for wireframe line colors (wires), cloud
## points/cross lines (cloud.3d),


cloud <- function(x, data, ...) UseMethod("cloud")


## FIXME: made xlim/ylim similar to levelplot.matrix? What about
## cloud.table?

cloud.matrix <-
    function(x, data = NULL, type = 'h',
             zlab = deparse(substitute(x)),
             aspect,
             ..., xlim, ylim, 
             row.values = seq_len(nrow(x)),
             column.values = seq_len(ncol(x)))
{
    stopifnot(length(row.values) == nrow(x),
              length(column.values) == ncol(x))
    if (!is.null(data)) warning("explicit 'data' specification ignored")
    form <- eval(z ~ row * column)
    if (missing(aspect)) aspect <- pmin(ncol(x) / nrow(x), c(Inf, 1))
    data <-
        expand.grid(row = seq_len(nrow(x)),
                    column = seq_len(ncol(x)))
    data$z <- as.vector(as.numeric(x))
    ## if rownames/colnames are non-null, use them to label
    if (missing(xlim))
        xlim <-
            if (!is.null(rownames(x))) rownames(x)
            else range(row.values, finite = TRUE)
    if (missing(ylim))
        ylim <-
            if (!is.null(colnames(x))) colnames(x)
            else range(column.values, finite = TRUE)
    cloud(form, data, type = type, zlab = zlab, aspect = aspect, 
          xlim = xlim, ylim = ylim,
          ...)
}



cloud.table <-
    function(x, data = NULL, groups = FALSE,
             zlab = deparse(substitute(x)),
             type = "h", ...)
{
    stopifnot(length(dim(x)) > 1)
    data <- as.data.frame(x)
    nms <- names(data)
    freq <- which(nms == "Freq")
    nms <- nms[-freq]
    form <- sprintf("Freq ~ %s * %s", nms[1], nms[2])
    nms <- nms[-c(1, 2)]
    len <- length(nms)
    if (is.logical(groups) && groups && len > 0)
    {
        groups <- as.name(nms[len])
        nms <- nms[-len]
        len <- length(nms)
    }
    else groups <- NULL
    if (len > 0)
    {
        rest <- paste(nms, collapse = "+")
        form <- paste(form, rest, sep = "|")
    }
    cloud(as.formula(form), data,
          groups = eval(groups),
          zlab = zlab,
          type = type, ...,
          default.scales =
          list(x = list(arrows = FALSE),
               y = list(arrows = FALSE)))
}





cloud.formula <-
    function(x,
             data = NULL,
             allow.multiple = is.null(groups) || outer,
             outer = FALSE,
             auto.key = FALSE,
             aspect = c(1,1),
             panel.aspect = 1,
             panel = lattice.getOption("panel.cloud"),
             prepanel = NULL,
             scales = list(),
             strip = TRUE,
             groups = NULL,
             xlab,
             ylab,
             zlab,
             xlim = if (is.factor(x)) levels(x) else range(x, finite = TRUE),
             ylim = if (is.factor(y)) levels(y) else range(y, finite = TRUE),
             zlim = if (is.factor(z)) levels(z) else range(z, finite = TRUE),
             at,
             drape = FALSE,

             pretty = FALSE,
             drop.unused.levels = lattice.getOption("drop.unused.levels"),
             ...,
             lattice.options = NULL,
             default.scales = list(distance = c(1, 1, 1), arrows = TRUE, axs = axs.default),
             default.prepanel = lattice.getOption("prepanel.default.cloud"),
             colorkey = any(drape),
             col.regions,
             alpha.regions,
             cuts = 70,
             subset = TRUE,
             axs.default = "r")

    ## the axs.default is to (by default) enable scale extension for
    ## cloud, but not for wireframe. Needs work to be actually
    ## implemented.
{
    stopifnot(is.numeric(panel.aspect))
    formula <- x
    dots <- list(...)
    groups <- eval(substitute(groups), data, environment(formula))
    subset <- eval(substitute(subset), data, environment(formula))
    if (!is.null(lattice.options))
    {
        oopt <- lattice.options(lattice.options)
        on.exit(lattice.options(oopt), add = TRUE)
    }

    ## Step 1: Evaluate x, y, z etc. and do some preprocessing

    form <-
        latticeParseFormula(formula, data, dimension = 3,
                            subset = subset, groups = groups,
                            multiple = allow.multiple,
                            outer = outer, subscripts = TRUE,
                            drop = drop.unused.levels)

    ## We need to be careful with subscripts here. It HAS to be there,
    ## and it's to be used to index x, y, z (and not only groups,
    ## unlike in xyplot etc). This means we have to subset groups as
    ## well, which is about the only use for the subscripts calculated
    ## in latticeParseFormula, after which subscripts is regenerated
    ## as a straight sequence indexing the variables

    if (!is.null(form$groups))
        groups <-
            if (is.matrix(form$groups)) as.vector(form$groups)[form$subscr]
            else if (is.data.frame(form$groups))
                as.vector(as.matrix(form$groups))[form$subscr]
            else form$groups[form$subscr]

    subscr <- seq_len(length(form$left))

    if (!is.function(panel)) panel <- eval(panel)
    if (!is.function(strip)) strip <- eval(strip)
    cond <- form$condition
    z <- form$left
    x <- form$right.x
    y <- form$right.y

    ## (2004-03-12) experimental stuff: when x, y, z are all matrices
    ## of the same dimension, they represent a 3-D surface
    ## parametrized on a 2-D grid (the details of the parametrizing
    ## grid are unimportant). This is meant only for wireframe

    isParametrizedSurface <-
        is.matrix(x) && is.matrix(y) && is.matrix(z)

    if (length(cond) == 0)
    {
        strip <- FALSE
        cond <- list(gl(1, length(x)))
    }

    if (missing(xlab)) xlab <- form$right.x.name
    if (missing(ylab)) ylab <- form$right.y.name
    if (missing(zlab)) zlab <- form$left.name

    zrng <-
        if (isParametrizedSurface)
            extend.limits(sqrt(range(x^2 + y^2 + z^2, finite = TRUE)))
        else
            extend.limits(range(as.numeric(z), finite = TRUE))


    if (missing(at))
        at <-
            if (drape)
            {
                if (pretty) pretty(zrng, cuts)
                else seq(zrng[1], zrng[2], length.out = cuts+2)
            }
            else zrng

    ## create a skeleton trellis object with the
    ## less complicated components:

    foo <-
        do.call("trellis.skeleton",
                c(list(formula = formula, 
                       cond = cond,
                       aspect = panel.aspect,
                       strip = strip,
                       panel = panel,
                       xlab = NULL,
                       ylab = NULL,
                       lattice.options = lattice.options), dots))

    ##----------------------------------------------------------------+
    ## xlab, ylab, zlab have special meaning in cloud / wireframe, and|
    ## need to be passed to the panel function to be processed. These |
    ## xlab / ylab are dummies to satisfy the usual processing        |
    ## routines ------------------------------------------------------+

    dots <- foo$dots # arguments not processed by trellis.skeleton
    foo <- foo$foo
    foo$call <- sys.call(sys.parent()); foo$call[[1]] <- quote(cloud)

    ## Step 2: Compute scales.common (leaving out limits for now)

    foo <- c(foo, do.call("construct.scales", list(draw=FALSE)))

    ## scales has to be interpreted differently. Nothing needs to be
    ## done for the usual scales, but need a scales for panel.cloud
    ## S-PLUS probably doesn't allow x-y-z-specific scales, but I see
    ## no reason not to allow that (will not allow limits, though)

    scales <- updateList(default.scales, scales)
    scales.3d <- do.call("construct.3d.scales", scales)


    ## Step 3: Decide if limits were specified in call
    ## Here, always FALSE (in the 2d panel sense)
    have.xlim <- FALSE
    have.ylim <- FALSE

    ## Step 4: Decide if log scales are being used: !!!

    have.xlog <- !is.logical(scales.3d$x.scales$log) || scales.3d$x.scales$log
    have.ylog <- !is.logical(scales.3d$y.scales$log) || scales.3d$y.scales$log
    have.zlog <- !is.logical(scales.3d$z.scales$log) || scales.3d$z.scales$log
    if (have.xlog)
    {
        xlog <- scales.3d$x.scales$log
        xbase <-
            if (is.logical(xlog)) 10
            else if (is.numeric(xlog)) xlog
            else if (xlog == "e") exp(1)

        x <- log(x, xbase)
        if (!missing(xlim)) xlim <- logLimits(xlim, xbase)
    }
    if (have.ylog)
    {
        ylog <- scales.3d$y.scales$log
        ybase <-
            if (is.logical(ylog)) 10
            else if (is.numeric(ylog)) ylog
            else if (ylog == "e") exp(1)

        y <- log(y, ybase)
        if (!missing(ylim)) ylim <- logLimits(ylim, ybase)
    }
    if (have.zlog)
    {
        zlog <- scales.3d$z.scales$log
        zbase <-
            if (is.logical(zlog)) 10
            else if (is.numeric(zlog)) zlog
            else if (zlog == "e") exp(1)

        z <- log(z, zbase)
        if (!missing(zlim)) zlim <- logLimits(zlim, zbase)
    }

    ## Step 5: Process cond

    cond.max.level <- unlist(lapply(cond, nlevels))

    if (is.logical(colorkey))
    {
        if (colorkey)
        {
            colorkey <- list(at = at, space = "right")
            if (!missing(col.regions)) colorkey$col <- col.regions
            if (!missing(alpha.regions)) colorkey$alpha <- alpha.regions
        }
        else colorkey <- NULL
    }
    else if (is.list(colorkey))
    {
        tmp <- ## FIXME: does the inside thing work? probably not 
            list(space = if (any(c("x", "y", "corner") %in% names(colorkey))) "inside" else "right",
                 at = at)
        if (!missing(col.regions)) tmp$col <- col.regions
        if (!missing(alpha.regions)) tmp$alpha <- alpha.regions
        colorkey <- updateList(tmp, colorkey)
    }
    foo$legend <-
        construct.legend(foo$legend,
                         colorkey,
                         fun = "draw.colorkey")





####################################
#     if (!drape) col.regions <- trellis.par.get("background")$col

#     ## region
#     numcol <- length(at) - 1
#     numcol.r <- length(col.regions)

#     col.regions <-
#         if (numcol.r <= numcol)
#             rep(col.regions, length.out = numcol)
#         else col.regions[floor(1+(1:numcol-1)*(numcol.r-1)/(numcol-1))]


#     if (is.logical(colorkey))
#     {
#         if (colorkey) colorkey <-
#             list(space = "right", col = col.regions,
#                  at = at, tick.number = 7)
#         else colorkey <- NULL
#     }
#     else if (is.list(colorkey))
#     {
#         ##foo$colorkey <- colorkey
#         if (is.null(colorkey$col)) colorkey$col <- col.regions
#         if (is.null(colorkey$at)) colorkey$at <- at
#         if (is.null(colorkey$space)) colorkey$space <-
#             if (any(c("x", "y", "corner") %in% names(colorkey))) "inside" else "right"
#     }

#     foo$legend <-
#         construct.legend(foo$legend,
#                          colorkey,
#                          fun = "draw.colorkey")
#################################



    ## maybe *lim = NULL with relation = "free" ?


    ## Process limits here ? Needs some thought



    ## Step 6: Determine packets

    foo$panel.args.common <-
        c(list(x = x, y = y, z = z,

               ## rot.mat = rot.mat,
               ## zoom = zoom,

               xlim = xlim, ylim = ylim, zlim = zlim,
               xlab = xlab, ylab = ylab, zlab = zlab,
               xlab.default = form$right.x.name,
               ylab.default = form$right.y.name,
               zlab.default = form$left.name,

               ##distance = if (perspective) distance else 0,

               aspect = aspect,
               panel.aspect = panel.aspect,
               drape = drape,
               scales.3d = scales.3d,
               at = at),
          dots)

    if (!missing(col.regions)) foo$panel.args.common$col.regions <- col.regions
    if (!missing(alpha.regions)) foo$panel.args.common$alpha.regions <- alpha.regions


    if (!is.null(groups)) foo$panel.args.common$groups <- groups

    npackets <- prod(cond.max.level)
    if (npackets != prod(sapply(foo$condlevels, length))) 
        stop("mismatch in number of packets")
    foo$panel.args <- vector(mode = "list", length = npackets)


    foo$packet.sizes <- numeric(npackets)
    if (npackets > 1)
    {
        dim(foo$packet.sizes) <- sapply(foo$condlevels, length)
        dimnames(foo$packet.sizes) <- lapply(foo$condlevels, as.character)
    }

    cond.current.level <- rep(1, length(cond))


    for (packet.number in seq_len(npackets))
    {
        id <- compute.packet(cond, cond.current.level)
        foo$packet.sizes[packet.number] <- sum(id)

        foo$panel.args[[packet.number]] <-
            list(subscripts = subscr[id])

        cond.current.level <-
            cupdate(cond.current.level,
                    cond.max.level)

    }


    more.comp <-
        c(limits.and.aspect(default.prepanel,
                            prepanel = prepanel,
                            have.xlim = have.xlim, xlim = xlim,
                            have.ylim = have.ylim, ylim = ylim,
                            x.relation = foo$x.scales$relation,
                            y.relation = foo$y.scales$relation,
                            panel.args.common = foo$panel.args.common,
                            panel.args = foo$panel.args,
                            aspect = panel.aspect,
                            npackets = npackets),
          cond.orders(foo))
    foo[names(more.comp)] <- more.comp


    if (is.null(foo$legend) && needAutoKey(auto.key, groups))
    {
        foo$legend <-
            list(list(fun = "drawSimpleKey",
                      args =
                      updateList(list(text = levels(as.factor(groups)),
                                      points = TRUE,
                                      rectangles = FALSE,
                                      lines = FALSE), 
                                 if (is.list(auto.key)) auto.key else list())))
        foo$legend[[1]]$x <- foo$legend[[1]]$args$x
        foo$legend[[1]]$y <- foo$legend[[1]]$args$y
        foo$legend[[1]]$corner <- foo$legend[[1]]$args$corner

        names(foo$legend) <-
            if (any(c("x", "y", "corner") %in% names(foo$legend[[1]]$args)))
                "inside"
            else
                "top"
        if (!is.null(foo$legend[[1]]$args$space))
            names(foo$legend) <- foo$legend[[1]]$args$space
    }

    class(foo) <- "trellis"
    foo
}

