## code for soap film smoothing to deal with difficult boundary regions
## Copyright Simon Wood 2006-2012

unconstrain <- function(object,beta) {
## function to produce full version of constrained coefficients of
## smooth object. Returned vector may have an attribute "constant"
## to be subtraccted from results.
## NOTE: sum to zero on some parameters only branch is not fully
##       tested (also unused at present)! 
  del.index <- attr(object,"del.index") 
  if (!is.null(del.index)) {
    beta.full <- rep(0,length(beta)+length(del.index))
    k <- 1;j <- 1
    for (i in 1:length(beta.full)) {
      if (j <= length(del.index) && i==del.index[j]) {
        beta.full[i] <- 0;j <- j + 1
      } else { 
        beta.full[i] <- beta[k];k <- k + 1
      }
    } 
    beta <- beta.full
  } ## end of del.index handling
  
  qrc <- attr(object,"qrc")
  if (!is.null(qrc)) { ## then smoothCon absorbed constraints
    j <- attr(object,"nCons")
    if (j>0) { ## there were constraints to absorb - need to untransform
      k <- length(beta) + j
      if (inherits(qrc,"qr")) {
        indi <- attr(object,"indi") ## index of constrained parameters
        if (is.null(indi)) {
        ##    X <- t(qr.qty(qrc,t(X))[(j+1):k,,drop=FALSE]) ## XZ
            beta <- qr.qy(qrc,c(rep(0,j),beta)) 
        } else { ## only some parameters are subject to constraint
          ## NOTE: this branch not fully tested
          nx <- length(indi)
          nc <- j;nz <- nx - nc
          Xbeta <- qr.qy(qrc,c(rep(0,j),beta[indi]))
          beta.full <- rep(0,length(beta)+j)
          ib <- 1;ii <- 1
          for (i in 1:length(beta.full)) {
            if (i==indi[ii]) {
              beta.full[i] <- Xbeta[ii]; ii <- ii + 1
            } else {
              beta.full[i] <- beta[ib]; ib <- ib + 1
            }
          }
          ##X[,indi[1:nz]]<-t(qr.qty(qrc,t(X[,indi,drop=FALSE]))[(nc+1):nx,,drop=FALSE])
          ##  X <- X[,-indi[(nz+1):nx]]
          beta <- beta.full
        }
      } else if (inherits(qrc,"sweepDrop")) {
        ## Sweep and drop constraints. First element is index to drop. 
        ## Remainder are constants to be swept out of remaining columns 
        ## X <- sweep(X[,-qrc[1],drop=FALSE],2,qrc[-1])
        #X <- X[,-qrc[1],drop=FALSE] - matrix(qrc[-1],nrow(X),ncol(X)-1,byrow=TRUE)
        cnst <- sum(beta*qrc[-1])
        if (qrc[1]==1) beta <- c(0,beta) else 
        if (qrc[1]==length(beta)+1) beta <- c(beta,0) else
        beta <- c(beta[1:(qrc[1]-1)],0,beta[qrc[1]:length(beta)])
        attr(beta,"constant") <- cnst
      } else if (qrc>0) { ## simple set to zero constraint
        ##X <- X[,-qrc] 
        if (qrc==1) beta <- c(0,beta) else 
        if (qrc==length(beta)+1) beta <- c(beta,0) else
        beta <- c(beta[1:(qrc-1)],0,beta[qrc:length(beta)])
      } else if (qrc<0) { ## params sum to zero
        # X <- t(diff(t(X)))
        beta <- t(diff(diag(length(beta)+1)))%*%beta
      }
    } ## end if (j>0)
  } ## end if qrc exists
  beta
} ## end of unconstrain

bnd2C <- function(bnd) {
## converts boundary loop list to form required in C code.
 n.loop <- 1
 if (is.null(bnd$x)) { ## translate into form that C routine needs
    bn <- list(x=bnd[[1]]$x,y=bnd[[1]]$y)
    n.loop <- length(bnd)
    if (length(bnd)>1) for (i in 2:n.loop) { 
      bn$x <- c(bn$x,NA,bnd[[i]]$x);bn$y <- c(bn$y,NA,bnd[[i]]$y)
    }
    bnd <- bn
  }
  ## replace NA segment separators with a numeric code 
  lowLim <- min(c(bnd$x,bnd$y),na.rm=TRUE)-1
  ind <- is.na(bnd$x)|is.na(bnd$y)
  bnd$x[ind] <- bnd$y[ind] <- lowLim - 1
  bnd$n <- length(bnd$x)
  if (bnd$n != length(bnd$y)) stop("x and y must be same length")
  bnd$breakCode <-lowLim
  bnd$n.loop <- n.loop
  bnd
} ## end bnd2C

inSide <- function(bnd,x,y)
## tests whether each point x[i],y[i] is inside the boundary defined
## by bnd$x, bnd$y, or by multiple boundary loops in bnd[[1]]$x,
## bnd[[1]]$y, bnd[[2]]$x, ... etc. 
## names in bnd must match those of x and y, but do not need to be "x" and "y"
{ ## match the names up first...
  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))
  bnd.name <- names(bnd)
  if (is.null(bnd.name)) for (i in 1:length(bnd)) { 
    bnd.name <- names(bnd[[i]])
    if (xname%in%bnd.name==FALSE||yname%in%bnd.name==FALSE) stop("variable names don't match boundary names")
    bnd.name[xname==bnd.name] <- "x"
    bnd.name[yname==bnd.name] <- "y"   
    names(bnd[[i]]) <- bnd.name
  } else {
    if (xname%in%bnd.name==FALSE||yname%in%bnd.name==FALSE) stop("variable names don't match boundary names")
    bnd.name[xname==bnd.name] <- "x"
    bnd.name[yname==bnd.name] <- "y"   
    names(bnd) <- bnd.name
  }  
  ## now do the real stuff...
  bnd <- bnd2C(bnd)
  um <-.C(C_in_out,bx=as.double(bnd$x),by=as.double(bnd$y),break.code=as.double(bnd$breakCode),
          x=as.double(x),y=as.double(y),inside=as.integer(y*0),nb=as.integer(bnd$n),
          n=as.integer(length(x)))
  as.logical(um$inside)
} ## end inSide


process.boundary <- function(bnd)
## takes a list of boundary loops, makes sure that they join up
## and add a distance along loop array, d to each list element.
{ for (i in 1:length(bnd)) {
    x <- bnd[[i]]$x;y<-bnd[[i]]$y;n <- length(x)
    if (length(y)!=n) stop("x and y not same length")
    if (x[1]!=x[n]||y[1]!=y[n]) { ## then loop not closed, so close it
      n<-n+1;x[n] <- x[1];y[n] <- y[1] 
      if (inherits(bnd[[i]],"data.frame")) bnd[[i]][n,] <-bnd[[i]][1,] 
      else { ## hopefully a list!
        bnd[[i]]$x[n] <- x[1];bnd[[i]]$y[n] <- y[1] 
        if (!is.null(bnd[[i]]$f)) bnd[[i]]$f[n] <- bnd[[i]]$f[1]
      }
    }
    len <- c(0,sqrt((x[1:(n-1)]-x[2:n])^2+(y[1:(n-1)]-y[2:n])^2)) ## seg lengths
    bnd[[i]]$d<-cumsum(len) ## distance along boundary
  }
  bnd
} ## end process.boundary



crunch.knots <- function(G,knots,x0,y0,dx,dy)
## finds indices of knot locations in solution grid
## the knot x,y locations are given in the `knots' argument.
{ nk <- length(knots$x)
  nx <- ncol(G);ny <- nrow(G)
  ki <- rep(0,nk)
  if (nk==0) return(ki)
  for (k in 1:nk) {
    i <- round((knots$x[k]-x0)/dx)+1
    j <- round((knots$y[k]-y0)/dy)+1
    if (i>1&&i<=nx&&j>1&&j<=ny) {
      ki[k] <- G[j,i]
      if (ki[k] <= 0) {
        str <- paste("knot",k,"is on or outside boundary")
        stop(str)
      }
    }
  } ## all knots done
  ki ## ki[k] indexes kth knot in solution grid
} ## end crunch.knots

setup.soap <- function(bnd,knots,nmax=100,k=10,bndSpec=NULL) {
## setup soap film  smooth - nmax is number of grid cells for longest side
## it's important that grid cells are square!
 
  ## check boundary...

  if (!inherits(bnd,"list")) stop("bnd must be a list.")
  n.loops <- length(bnd)
  if (n.loops!=length(k)) {
    if (length(k)==1) k <- rep(k,n.loops) 
    else stop("lengths of k and bnd are not compatible.") 
  }
  bnd <- process.boundary(bnd) ## add distances and close any open loops

  ## create grid on which to solve Laplace equation 
  ## Obtain grid limits from boundary 'bnd'....
  x0 <- min(bnd[[1]]$x);x1 <- max(bnd[[1]]$x)
  y0 <- min(bnd[[1]]$y);y1 <- max(bnd[[1]]$y)
  if (length(bnd)>1) for (i in 2:length(bnd)) {
    x0 <- min(c(x0,bnd[[i]]$x)); x1 <- max(c(x1,bnd[[i]]$x))
    y0 <- min(c(y0,bnd[[i]]$y)); y1 <- max(c(y1,bnd[[i]]$y))
  } ## now got the grid limits, can set it up
 
  if (x1-x0>y1-y0) { ## x is longest side
    dy <- dx <- (x1-x0) /(nmax-1)
    nx <- nmax
    ny <- ceiling((y1-y0)/dy)+1
  } else { ## y is longest side
    dy <- dx <- (y1-y0) /(nmax-1)
    ny <- nmax
    nx <- ceiling((x1-x0)/dy)+1   
  }
  ## so grid is now nx by ny, cell size is dx by dy (but dx=dy)
  ## x0, y0 is "lower left" cell centre

  ## Create grid index G 
  bnc <- bnd2C(bnd) ## convert boundary to form required in C code

  G <- matrix(0,ny,nx)
  nb <- rep(0,bnc$n.loop)

  oo <- .C(C_boundary,G=as.integer(G), d=as.double(G), dto=as.double(G), x0=as.double(x0), 
                y0 = as.double(y0), dx=as.double(dx), dy = as.double(dy),
                nx=as.integer(nx),as.integer(ny), x=as.double(bnc$x),y=as.double(bnc$y),
                breakCode=as.double(bnc$breakCode),n=as.integer(bnc$n),nb=as.integer(nb))

  ret <- list(G=matrix(oo$G,ny,nx),nb=oo$nb,d=oo$d[oo$d >= 0],x0=x0,y0=y0,dx=dx,dy=dy,bnd=bnd)
  rm(oo)
  ## Now create the PDE coefficient matrix 
  n.inside <- sum(ret$G > - nx*ny)
  xx <- rep(0,5*n.inside)
  o1 <- .C(C_pde_coeffs,as.integer(ret$G),xx=as.double(xx),ii=as.integer(xx),jj=as.integer(xx),
            n=as.integer(0),as.integer(nx),as.integer(ny),as.double(dx),as.double(dy))
  ind <- 1:o1$n
  X <- sparseMatrix(i=o1$ii[ind]+1,j=o1$jj[ind]+1,x=o1$xx[ind])
  er <- expand(lu(X))
  ret$Q <- er$Q;ret$U <- er$U;ret$L <- er$L;ret$P <- er$P
  ret$ng <- n.inside ## the number of cells to solve for 
  rm(er);rm(X)
  ## ... so the sparse LU decomposition of X can be used to solve PDE.
  ## X = PLUQ where P and Q are permuation matrices.

  ## now obtain location of knots in solution ... 
  ret$ki <- crunch.knots(ret$G,knots,x0,y0,dx,dy)

  ## setup the boundary conditions/boundary splines
  bc <- list() ## to hold boundary conditions
  start <- 1
  for (i in 1:length(bnd)) {
    stop <- start - 1 + ret$nb[i] ## ret$d[start:stop] = dist along boundary loop i
    if (is.null(bnd[[i]]$f)) { ## this boundary is free
      d <- c(ret$d[start:stop],0) # boundary gridpoint distances along smooth
      if (is.null(bndSpec)) {
        bsm <- smooth.construct(s(d,bs="cc",k=k[i]),data=data.frame(d=d),knots=NULL) 
      } else if (bndSpec$bs=="cc"){
        if (bndSpec$knot.space=="even") 
          knots <- seq(min(d),max(d),length=k[i])  
        else
          knots <- quantile(unique(d),seq(0,1,length=k[i]))
        bsm <- smooth.construct(s(d,bs="cc",k=k[i]),data=data.frame(d=d),knots=NULL)  

      } else { ## use "cp" P-spline
        bsm <- smooth.construct(s(d,bs="cp",k=k[i],m=bndSpec$m),data=data.frame(d=d),knots=NULL) 
      }
      bc[[i]] <- list(bsm=bsm,X=bsm$X[1:ret$nb[i],],S=bsm$S[[1]],free.bound=TRUE)
    } else { ## boundary is fixed
      d <- ret$d[start:stop] 
      ui <- !duplicated(bnd[[i]]$d)
      ff <- approx(bnd[[i]]$d[ui],bnd[[i]]$f[ui],d)$y ## fixed values for BC
      bc[[i]] <- list(f=ff,free.bound=FALSE)
    }
    start <- stop + 1
  }
  ret$bc <- bc
  ret  
} ## end of setup.soap

soap.basis <- function(sd,x=NA,y=NA,film=TRUE,wiggly=TRUE,penalty=TRUE,plot=FALSE,beta=1) {
## function to evaluate soap basis using soap definition object 'sd' 
## returned by setup.soap. x and y are values at which to evaluate.
## If plot==TRUE then then data suitable for plotting are returned at the resolution
## of the solution grid. Then beta contains either the coefficients, or a single number 
## representing the single basis function to return (0 for the offset). 

  if (!plot) {
    indout <- inSide(sd$bnd,x,y); n <- length(x)
  } else {
    penalty <- FALSE
    ## Some constraints result in the need to add a constant
    ## to the field (e.g. sweep and drop)
    cnst <- attr(beta,"constant")
    if (is.null(cnst)) cnst <- 0 else cnst <- -cnst
  }
  offset.needed <- FALSE;
  nc <- length(sd$ki)*as.numeric(wiggly) ## number of interior knots 
  nb <- 0 ## boundary basis dimension
  offset <- NULL
 
  if (film) {
    stop <- 0
    for (i in 1:length(sd$bc)) { ## work through boundary loops
      start <- stop + 1;stop <- start - 1 + sd$nb[i]
      if (sd$bc[[i]]$free.bound) nb <- nb + ncol(sd$bc[[i]]$X) 
      else { ## fixed boundary, so offset required  
        if (!offset.needed) { 
          bndOff <- rep(0,sd$ng) ## array for fixed boundary conditions 
          offset.needed <- TRUE
        }
        bndOff[start:stop] <- sd$bc[[i]]$f
      } ## fixed boundary done
    } ## finished first pass through loops
  } ## finished first if film

  if (plot) { ## preliminaries for plotting info
      if (length(beta)==1) { ## just one basis function to be returned
        if (beta<0||beta>nc+nb||(beta==0&&!offset.needed)) stop("attempt to select non existent basis function")
        select.basis <- TRUE    
      } else { ## whole smooth to be returned
        if (length(beta)!=nc+nb) stop("coefficient vector wrong length")
        select.basis <- FALSE
      }
      G <- sd$G ## solution grid
      G[G < - length(G)] <- NA ## exterior marked as NA
      ind <- !is.na(G)
      gind <- G[ind] <- abs(G[ind])+1
      ## need to create the indices such that G[gind] <- g is correct...
      gind[G[ind]] <- (1:length(G))[ind]
      G[ind] <- cnst ## now clear interior of G
  } ## finished preliminary if (plot)

  if (film) {
    if (offset.needed) { ## solve for offset soap film
      bndOff <- solve(sd$Q,solve(sd$U,solve(sd$L,solve(t(sd$P),bndOff))))
      if (plot) { ## grid is all that's needed
        if (select.basis&&beta==0||!select.basis) {
          G[gind] <- bndOff
        } 
      } else { ## need full interpolation
        NAcode <- max(bndOff)*2
        offset <- .C(C_gridder,z=as.double(x),as.double(x),as.double(y),as.integer(length(x)),as.double(bndOff),
                   as.integer(sd$G),nx=as.integer(ncol(sd$G)),ny=as.integer(nrow(sd$G)),as.double(sd$x0), 
                   as.double(sd$y0),as.double(sd$dx),as.double(sd$dy),as.double(NAcode*2))$z
        offset[offset>NAcode] <- NA
        offset[!indout] <- NA
      }
    }
  } ## finished preliminary if (film)
  
  if (!plot) {
    X <- matrix(0,n,nb+nc) ## model matrix
    if (penalty) { S <- list();off <- 1;nS=0} else {off <- S <- NULL}
  }

  k <- 1 ## model matrix column

  if (film&&nb>0) {
    ## now work through boundary bases
    stop <- 0
    for (i in 1:length(sd$bc)) { ## work through boundary loops
      start <- stop + 1;stop <- start - 1 + sd$nb[i]
      ind <- start:stop ## index of this loop in solution grid
      if (sd$bc[[i]]$free.bound) {
        if (penalty) {
          nS <- nS + 1
          off[nS] <- k
          S[[nS]] <- sd$bc[[i]]$S
        } ## penalty done

        for (j in 1:ncol(sd$bc[[i]]$X)) { ## loop over loop basis cols
           z <- rep(0,sd$ng)
           z[ind] <- sd$bc[[i]]$X[,j] ## PDE rhs
           z <- solve(sd$Q,solve(sd$U,solve(sd$L,solve(t(sd$P),z))))
           if (plot) {
             if (select.basis) { 
               if (beta==k) G[gind] <- z 
             } else G[gind] <- G[gind] + beta[k]*z
           } else {
             NAcode <- max(z)*2
             Xj <- .C(C_gridder,z=as.double(x),as.double(x),as.double(y),as.integer(length(x)),as.double(z),
                   as.integer(sd$G),nx=as.integer(ncol(sd$G)),ny=as.integer(nrow(sd$G)),as.double(sd$x0), 
                   as.double(sd$y0),as.double(sd$dx),as.double(sd$dy),as.double(NAcode*2))$z
           
             Xj[Xj>NAcode] <- NA;X[,k] <- Xj;
           }
           k <- k + 1
        } ## basis done        
      } ## end of free boundary 
    } ## end of boundary loops
  } ## end of film processing

  if (wiggly) { ## interior basis functions required
    g <- matrix(0,sd$ng,nc)
    for (i in 1:nc) g[sd$ki[i],i] <- 1
    g <- as(solve(sd$Q,solve(sd$U,solve(sd$L,solve(t(sd$P),g)))),"matrix")
    g <- sweep(g,2,apply(g,2,max),"/") ## normalize - not really needed
    if (penalty) { ## get soap penalty
      nS <- nS + 1;off[nS] <- k
      S[[nS]] <- crossprod(g) * sd$dx * sd$dy
    }
    g <- solve(sd$Q,solve(sd$U,solve(sd$L,solve(t(sd$P),g))))
    NAcode <- max(g)*2
    for (i in 1:nc) {
      if (plot) {
        if (select.basis) {
          if (k==beta) G[gind] <- g[,i] 
        } else G[gind] <- G[gind] + beta[k]*g[,i]
      } else {
        Xj <- .C(C_gridder,z=as.double(x),as.double(x),as.double(y),as.integer(length(x)),as.double(g[,i]),
                as.integer(sd$G),nx=as.integer(ncol(sd$G)),ny=as.integer(nrow(sd$G)),as.double(sd$x0), 
                as.double(sd$y0),as.double(sd$dx),as.double(sd$dy),as.double(NAcode*2))$z
        Xj[Xj>NAcode] <- NA;X[,k] <- Xj
      }
      k <- k + 1
    }
  }
  if (plot) {
    return(t(G))
  } else {
    X[!indout,] <- NA
    return(list(X=X,S=S,off=off,offset=offset))
  }   
} ## end soap.basis


smooth.construct.so.smooth.spec<-function(object,data,knots)
## a full soap film smooth constructor method function for 
## integration with mgcv::gam
{ if (is.null(knots)) stop("knots must be specified for soap")
  if (object$dim!=2) stop("soap films are bivariate only")

  x <- data[[object$term[1]]]
  y <- data[[object$term[2]]]
  
  knt <- list(x=knots[[object$term[1]]],y=knots[[object$term[2]]])

  if (length(knt$x)<1) stop("need at least one interior knot")  

  bnd <- object$xt$bnd
  if (is.null(bnd)) stop("can't soap smooth without a boundary")
  if (!inherits(bnd,"list")) stop("bnd must be a list of boundary loops")
  
  for (i in 1:length(bnd)) { ## re-lable boundary
    nm <- names(bnd[[i]])
    ind <- nm==object$term[1]
    if (sum(ind)!=1) stop("faulty bnd")
    names(bnd[[i]])[ind] <- "x"
    ind <- nm==object$term[2]
    if (sum(ind)!=1) stop("faulty bnd")
    names(bnd[[i]])[ind] <- "y"
  }

  if (length(object$bs.dim)==1) k <- rep(object$bs.dim,length(bnd)) 
  else {
    if (length(object$bs.dim)==length(bnd)) k <- object$bs.dim else
    stop("k and bnd lengths are inconsistent")
  }

  if (is.null(object$xt$nmax)) nmax <- 200 else nmax <- object$xt$nmax

  ## setup the soap defining structures

  sd <- setup.soap(bnd,knots=knt,nmax=nmax,k=k,bndSpec=object$xt$bndSpec)

  b <- soap.basis(sd,x,y,film=TRUE,wiggly=TRUE,penalty=TRUE)

  if (sum(is.na(b$X))>0) stop("data outside soap boundary")

 # b <- soap.construct(x,y,bnd,knots=knt,k=k,n.grid=n.grid,basis.type=2,
 #                     depth=depth,rel.eps=rel.eps,abs.eps=abs.eps,
 #                     bndSpec=object$xt$bndSpec)



  ## get penalty null space for the term...
  ns.dim <- 0;n <- length(sd$bc)
  if (n>0) for (i in 1:n) if (sd$bc[[i]]$free.bound) 
           ns.dim <- ns.dim + sd$bc[[i]]$bsm$null.space.dim
  object$null.space.dim <- ns.dim
  need.con <- TRUE
  for (i in 1:length(sd$bc)) 
  if (!sd$bc[[i]]$free.bound) need.con <- FALSE

  ## rescale basis for nice conditioning....
  irng <- 1/as.numeric(apply(b$X,2,max)-apply(b$X,2,min))
  b$X <- t(t(b$X)*irng)  
  ## now apply rescaling
  for (i in  1:length(b$S)) {
    a <- irng[b$off[i]:(b$off[i]+ncol(b$S[[i]])-1)]
    b$S[[i]] <- diag(a)%*%b$S[[i]]%*%diag(a)
  }

  object$irng <- irng ## the column scaling factor 

  object$X <- b$X ## model matrix
  attr(object$X,"offset") <- b$offset

  if (!object$fixed) { ## have to unpack a bit...
    S <- list();n <- ncol(object$X)
    for (i in 1:length(b$S)) {
      S[[i]] <- matrix(0,n,n)
      m <- ncol(b$S[[i]])
      ind <- b$off[i]:(b$off[i]+m-1)
      S[[i]][ind,ind] <- b$S[[i]]
    }
    object$S <- S ## penalties
  }

  rr <- ncol(b$S[[1]])-1 
  if (length(b$S)>1) for (i in 2:length(b$S)) rr <- c(rr,ncol(b$S[[i]])-1)
  rr[length(rr)] <- rr[length(rr)]+1
  object$rank <- rr # penalty ranks

  if (!need.con)  object$C <- matrix(0,0,ncol(object$X)) ## no con

  object$df <- ncol(object$X) # -nrow(object$C)
  
  for (i in 1:length(sd$bc)) {
    sd$bc[[i]]$bsm <- sd$bc[[i]]$S <- NULL
  }
  
  object$sd <- sd
  
  class(object)<-"soap.film"  # Give object a class
  object
} ## end of full soap constructor



smooth.construct.sf.smooth.spec<-function(object,data,knots)
## a soap film smooth boundary interpolating film only constructor 
## method function for integration with mgcv::gam
{ if (is.null(knots)) stop("knots must be specified for soap")
  if (object$dim!=2) stop("soap films are bivariate only")

  x <- data[[object$term[1]]]
  y <- data[[object$term[2]]]
  
  knt <- list(x=knots[[object$term[1]]],y=knots[[object$term[2]]])

  ## if (length(knt$x)<1) stop("need at least one interior knot")  

  bnd <- object$xt$bnd
  if (is.null(bnd)) stop("can't soap smooth without a boundary")
  if (!inherits(bnd,"list")) stop("bnd must be a list of boundary loops")
  
  for (i in 1:length(bnd)) { ## re-lable boundary
    nm <- names(bnd[[i]])
    ind <- nm==object$term[1]
    if (sum(ind)!=1) stop("faulty bnd")
    names(bnd[[i]])[ind] <- "x"
    ind <- nm==object$term[2]
    if (sum(ind)!=1) stop("faulty bnd")
    names(bnd[[i]])[ind] <- "y"
  }

  if (length(object$bs.dim)==1) k <- rep(object$bs.dim,length(bnd)) 
  else {
    if (length(object$bs.dim)==length(bnd)) k <- object$bs.dim else
    stop("k and bnd lengths are inconsistent")
  }

  if (is.null(object$xt$nmax)) nmax <- 200 else nmax <- object$xt$nmax

  ## setup the soap defining structures

  sd <- setup.soap(bnd,knots=knt,nmax=nmax,k=k,bndSpec=object$xt$bndSpec)

  b <- soap.basis(sd,x,y,film=TRUE,wiggly=FALSE,penalty=TRUE)
  if (sum(is.na(b$X))>0) stop("data outside soap boundary")

  if (ncol(b$X)==0) stop("no free coefs in sf smooth")

#  b <- soap.construct(x,y,bnd,knots=knt,k=k,n.grid=n.grid,basis.type=2,
#                      depth=depth,rel.eps=rel.eps,abs.eps=abs.eps,film=TRUE,
#                      wiggly=FALSE,bndSpec=object$xt$bndSpec)

  ## get penalty null space for term
  ns.dim <- 0;n <- length(sd$bc)
  k <- 0 ## counter for b$S
  rr <- rep(0,length(b$S))
  if (n>0) for (i in 1:n) if (sd$bc[[i]]$free.bound) { 
           nsd <-  sd$bc[[i]]$bsm$null.space.dim
           ns.dim <- ns.dim + nsd
           k <- k + 1
           rr[k] <- ncol(b$S[[k]]) - nsd ## rank of b$S[[k]] 
  }
  object$null.space.dim <- ns.dim
  object$rank <- rr # penalty ranks

  need.con <- TRUE
  for (i in 1:length(sd$bc)) 
  if (!sd$bc[[i]]$free.bound) need.con <- FALSE

  ## rescale basis for nice conditioning....
  irng <- 1/as.numeric(apply(b$X,2,max)-apply(b$X,2,min))
  b$X <- t(t(b$X)*irng)  
  ## now apply rescaling
  if (length(b$S)>0) for (i in  1:length(b$S)) {
    a <- irng[b$off[i]:(b$off[i]+ncol(b$S[[i]])-1)]
    b$S[[i]] <- diag(a)%*%b$S[[i]]%*%diag(a)
  }

  object$irng <- irng ## the column scaling factor 

  object$X <- b$X ## model matrix
  attr(object$X,"offset") <- b$offset

  if (!object$fixed) { ## have to unpack a bit...
    S <- list();n <- ncol(object$X)
    if (length(b$S)>0) for (i in 1:length(b$S)) {
      S[[i]] <- matrix(0,n,n)
      m <- ncol(b$S[[i]])
      ind <- b$off[i]:(b$off[i]+m-1)
      S[[i]][ind,ind] <- b$S[[i]]
    }
    object$S <- S ## penalties
  }

  if (!need.con)  object$C <- matrix(0,0,ncol(object$X)) ## no con

  object$df <- ncol(object$X) # -nrow(object$C)
  
  for (i in 1:length(sd$bc)) {
    sd$bc[[i]]$bsm <- sd$bc[[i]]$S <- NULL
  }
  
  object$sd <- sd

  class(object)<-c("sf","soap.film")  # Give object a class
  object
} ## end of boundary film component soap constructor


smooth.construct.sw.smooth.spec<-function(object,data,knots)
## a soap film smooth wiggly component only constructor method function for 
## integration with mgcv::gam
{ if (is.null(knots)) stop("knots must be specified for soap")
  if (object$dim!=2) stop("soap films are bivariate only")

  x <- data[[object$term[1]]]
  y <- data[[object$term[2]]]
  
  knt <- list(x=knots[[object$term[1]]],y=knots[[object$term[2]]])

  if (length(knt$x)<1) stop("need at least one interior knot")  

  bnd <- object$xt$bnd
  if (is.null(bnd)) stop("can't soap smooth without a boundary")
  if (!inherits(bnd,"list")) stop("bnd must be a list of boundary loops")
  
  for (i in 1:length(bnd)) { ## re-lable boundary
    nm <- names(bnd[[i]])
    ind <- nm==object$term[1]
    if (sum(ind)!=1) stop("faulty bnd")
    names(bnd[[i]])[ind] <- "x"
    ind <- nm==object$term[2]
    if (sum(ind)!=1) stop("faulty bnd")
    names(bnd[[i]])[ind] <- "y"
  }

  if (length(object$bs.dim)==1) k <- rep(object$bs.dim,length(bnd)) 
  else {
    if (length(object$bs.dim)==length(bnd)) k <- object$bs.dim else
    stop("k and bnd lengths are inconsistent")
  }
  
  if (is.null(object$xt$nmax)) nmax <- 200 else nmax <- object$xt$nmax

  ## setup the soap defining structures

  sd <- setup.soap(bnd,knots=knt,nmax=nmax,k=k,bndSpec=object$xt$bndSpec)

  b <- soap.basis(sd,x,y,film=FALSE,wiggly=TRUE,penalty=TRUE)

  if (sum(is.na(b$X))>0) stop("data outside soap boundary")

  object$null.space.dim <- 0 ## penalty is full rank, for this case

 
  ## rescale basis for nice conditioning....
  irng <- 1/as.numeric(apply(b$X,2,max)-apply(b$X,2,min))
  b$X <- t(t(b$X)*irng)  
  ## now apply rescaling
  for (i in  1:length(b$S)) {
    a <- irng[b$off[i]:(b$off[i]+ncol(b$S[[i]])-1)]
    b$S[[i]] <- diag(a)%*%b$S[[i]]%*%diag(a)
  }

  object$irng <- irng ## the column scaling factor 

  object$X <- b$X ## model matrix

  if (!object$fixed) { ## have to unpack a bit...
    S <- list();n <- ncol(object$X)
    for (i in 1:length(b$S)) {
      S[[i]] <- matrix(0,n,n)
      m <- ncol(b$S[[i]])
      ind <- b$off[i]:(b$off[i]+m-1)
      S[[i]][ind,ind] <- b$S[[i]]
    }
    object$S <- S ## penalties
  }

  rr <- ncol(b$S[[1]])-1 
  if (length(b$S)>1) for (i in 2:length(b$S)) rr <- c(rr,ncol(b$S[[i]])-1)
  rr[length(rr)] <- rr[length(rr)]+1
  object$rank <- rr # penalty ranks

  
  object$df <- ncol(object$X) # -nrow(object$C)
  
  for (i in 1:length(sd$bc)) {
    sd$bc[[i]]$bsm <- sd$bc[[i]]$S <- NULL
  }
  
  object$sd <- sd
  object$C <- matrix(0,0,ncol(object$X)) ## this is tied to zero

  class(object)<-c("sw","soap.film")  # Give object a class
  object
} ## end of wiggly component of soap constructor



Predict.matrix.soap.film<-function(object,data)
# prediction method function for the soap.film smooth class
{ x <- get.var(object$term[1],data)
  y <- get.var(object$term[2],data) 
  b <- soap.basis(object$sd,x,y,film=TRUE,wiggly=TRUE,penalty=FALSE)
  X <- t(object$irng*t(b$X))
  attr(X,"offset") <- b$offset
  X
}

Predict.matrix.sf <- function(object,data)
# prediction method function for the sf smooth class --- the boundary interpolating film
# component of a soap film smooth 
{ x <- get.var(object$term[1],data)
  y <- get.var(object$term[2],data)
  b <- soap.basis(object$sd,x,y,film=TRUE,wiggly=FALSE,penalty=FALSE)
  X <- t(object$irng*t(b$X))
  attr(X,"offset") <- b$offset
  X
}

Predict.matrix.sw <- function(object,data)
# prediction method function for the sw smooth class --- the wiggly
# component of a soap film smooth 
{ x <- get.var(object$term[1],data)
  y <- get.var(object$term[2],data)
  X <- soap.basis(object$sd,x,y,film=FALSE,wiggly=TRUE,penalty=FALSE)$X
  X <- t(object$irng*t(X))
  X
}

plot.soap.film <- function(x,P=NULL,data=NULL,label="",se1.mult=1,se2.mult=2,
                     partial.resids=FALSE,rug=TRUE,se=TRUE,scale=-1,n=100,n2=40,
                     pers=FALSE,theta=30,phi=30,jit=FALSE,xlab=NULL,ylab=NULL,main=NULL,
                     ylim=NULL,xlim=NULL,too.far=0.1,shade=FALSE,shade.col="gray80",
                     shift=0,trans=I,by.resids=FALSE,scheme=0,...) {
## plot method function for soap.smooth terms
  if (scheme==3) {  
    if (is.null(P)) outline <- FALSE else outline <- TRUE   
            if (is.null(xlim)) xlim <- c(x$sd$x0,x$sd$x0+ncol(x$sd$G)*x$sd$dx)
            if (is.null(ylim)) ylim <- c(x$sd$y0,x$sd$y0+nrow(x$sd$G)*x$sd$dy)
       
            P0 <- plot.mgcv.smooth(x=x,P=P,data=data,label=label,se1.mult=se1.mult,se2.mult=se2.mult,
                     partial.resids=partial.resids,rug=rug,se=se,scale=scale,n=n,n2=n2,
                     pers=pers,theta=theta,phi=phi,jit=jit,xlab=xlab,ylab=ylab,main=main,
                     ylim=ylim,xlim=xlim,too.far=too.far,shade=shade,shade.col=shade.col,
                     shift=shift,trans=trans,by.resids=by.resids,scheme=scheme,...)
            if (outline) { if (is.null(names(P$bnd))) {
                for (i in 1:length(P$bnd)) lines(P$bnd[[i]],lwd=2)
              } else lines(P$bnd,lwd=2)
            } else { P0$bnd <- x$sd$bnd}
            return(P0)
  }
  if (is.null(P)) { ## get plotting information...
    if (!x$plot.me) return(NULL) ## shouldn't or can't plot
    ## get basic plot data 
    beta <- unconstrain(x,attr(x,"coefficients"))*x$irng ## coefs
    raw <- data[x$term]
    film <- wiggly <- TRUE
    if (inherits(x,"sw")) film <- FALSE else if (inherits(x,"sf")) wiggly <- FALSE
    soap.basis(x$sd,film=film,wiggly=wiggly,plot=TRUE,beta=beta) -> G
    if (is.null(xlab)) xlabel<- x$term[1] else xlabel <- xlab
    if (is.null(ylab)) ylabel <- x$term[2] else ylabel <- ylab
    xscale <- x$sd$x0 + 0:(nrow(G)-1) * x$sd$dx 
    yscale <- x$sd$y0 + 0:(ncol(G)-1) * x$sd$dy
    return(list(fit=G,scale=FALSE,se=FALSE,raw=raw,xlab=xlabel,ylab=ylabel,
                xscale=xscale,yscale=yscale,main=label,bnd=x$sd$bnd))
    } else { ## do plot
      if (scheme==0) {
        xlim <- range(P$xscale);dx = xlim[2] - xlim[1]
        ylim <- range(P$yscale);dy = ylim[2] - ylim[1]
        plot(P$xscale[1],P$yscale[1],xlab=P$xlab,ylab=P$ylab,main=P$main,xlim=xlim,ylim=ylim,...)
        rect(xlim[1]-dx,ylim[1]-dy,xlim[2]+dx,ylim[2]+dy,col="lightgrey")
        image(P$xscale,P$yscale,P$fit,add=TRUE,col=heat.colors(50),...)
        contour(P$xscale,P$yscale,P$fit,add=TRUE,...)
      } else if (scheme==1) {
        image(P$xscale,P$yscale,P$fit,col=grey(0:50/50),xlab=P$xlab,
              ylab=P$ylab,main=P$main,...)
        contour(P$xscale,P$yscale,P$fit,add=TRUE,...)
      } else if (scheme==2) {
        contour(P$xscale,P$yscale,P$fit,xlab=P$xlab,
              ylab=P$ylab,main=P$main,...)
        if (is.null(names(P$bnd))) {
          for (i in 1:length(P$bnd)) lines(P$bnd[[i]],lwd=2)
        } else lines(P$bnd,lwd=2)
      }
    }

} ## end plot.soap.smooth



fs.test <- function(x,y,r0=.1,r=.5,l=3,b=1,exclude=TRUE)
## test function based on Tim Ramsay (2002) J.R.Statist. Soc. B
## 64(2):307-319 "Spline smoothing over difficult regions"
{ q <- pi*r/2 ## 1/2 length of semi-circle part of centre curve
  a <- d <- x*0 ## along and distance to arrays

  ## convert x,y to along curve and distance to curve (a,d) 
  ## co-ordinates. 0 distance along is at (x=-r,y=0)  
  
  ind <- x>=0 & y>0
  a[ind] <- q + x[ind]
  d[ind] <- y[ind]-r

  ind <- x>=0 & y<=0
  a[ind] <- -q - x[ind]
  d[ind] <- -r - y[ind]
  
  
  ind <- x < 0 
  a[ind] <- -atan(y[ind]/x[ind])*r
  d[ind] <- sqrt(x[ind]^2+y[ind]^2) - r
  
  ## create exclusion index
  
  ind <- abs(d)>r-r0 | (x>l & (x-l)^2+d^2 > (r-r0)^2)

 # f <- a*b # the original
  f <- a*b+d^2
  
  if (exclude) f[ind] <- NA
  attr(f,"exclude") <- ind
  f
}


fs.boundary <- function(r0=.1,r=.5,l=3,n.theta=20)
## produce boundary file for fs.test
{ rr <- r+(r-r0)
  theta <- seq(pi,pi/2,length=n.theta)
  x <- rr*cos(theta); y <- rr*sin(theta)
  
  theta <- seq(pi/2,-pi/2,length=2*n.theta)
  x <- c(x,(r-r0)*cos(theta)+l); y <- c(y,(r-r0)*sin(theta)+r)
  
  theta <- seq(pi/2,pi,length=n.theta)
  x <- c(x,r0*cos(theta)); y <- c(y,r0*sin(theta))

  n<-length(x)
  x <- c(x,x[n:1]);y <- c(y,-y[n:1])

  return(list(x=x,y=y))
}

