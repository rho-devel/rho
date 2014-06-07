## R routines for gam fitting with calculation of derivatives w.r.t. sp.s
## (c) Simon Wood 2004-2013

## These routines are for type 3 gam fitting. The basic idea is that a P-IRLS
## is run to convergence, and only then is a scheme for evaluating the 
## derivatives via the implicit function theorem used. 


gam.reparam <- function(rS,lsp,deriv) 
## Finds an orthogonal reparameterization which avoids `dominant machine zero leakage' between 
## components of the square root penalty.
## rS is the list of the square root penalties: last entry is root of fixed. 
##    penalty, if fixed.penalty=TRUE (i.e. length(rS)>length(sp))
## lsp is the vector of log smoothing parameters.
## *Assumption* here is that rS[[i]] are in a null space of total penalty already;
## see e.g. totalPenaltySpace & mini.roots
## Ouputs:
## S -- the total penalty matrix similarity transformed for stability
## rS -- the component square roots, transformed in the same way
## Qs -- the orthogonal transformation matrix S = t(Qs)%*%S0%*%Qs, where S0 is the 
##       untransformed total penalty implied by sp and rS on input
## E -- the square root of the transformed S (obtained in a stable way by pre-conditioning)
## det -- log |S|
## det1 -- dlog|S|/dlog(sp) if deriv >0
## det2 -- hessian of log|S| wrt log(sp) if deriv>1  
{ q <- nrow(rS[[1]])
  rSncol <- unlist(lapply(rS,ncol))
  M <- length(lsp) 
  if (length(rS)>M) fixed.penalty <- TRUE else fixed.penalty <- FALSE
  
  d.tol <- .Machine$double.eps^.3 ## group `similar sized' penalties, to save work

  r.tol <- .Machine$double.eps^.75 ## This is a bit delicate -- too large and penalty range space can be supressed.

  oo <- .C("get_stableS",S=as.double(matrix(0,q,q)),Qs=as.double(matrix(0,q,q)),sp=as.double(exp(lsp)),
                  rS=as.double(unlist(rS)), rSncol = as.integer(rSncol), q = as.integer(q),
                  M = as.integer(M), deriv=as.integer(deriv), det = as.double(0), 
                  det1 = as.double(rep(0,M)),det2 = as.double(matrix(0,M,M)), 
                  d.tol = as.double(d.tol),
                  r.tol = as.double(r.tol),
                  fixed_penalty = as.integer(fixed.penalty))
  S <- matrix(oo$S,q,q)
  S <- (S+t(S))*.5
  p <- abs(diag(S))^.5            ## by Choleski, p can not be zero if S +ve def
  p[p==0] <- 1                    ## but it's possible to make a mistake!!
  ##E <-  t(t(chol(t(t(S/p)/p)))*p) 
  St <- t(t(S/p)/p)
  St <- (St + t(St))*.5 ## force exact symmetry -- avoids very rare mroot fails 
  E <- t(mroot(St,rank=q)*p) ## the square root S, with column separation
  Qs <- matrix(oo$Qs,q,q)         ## the reparameterization matrix t(Qs)%*%S%*%Qs -> S
  k0 <- 1
  for (i in 1:length(rS)) { ## unpack the rS in the new space
    crs <- ncol(rS[[i]]);
    k1 <- k0 + crs * q - 1 
    rS[[i]] <- matrix(oo$rS[k0:k1],q,crs)
    k0 <- k1 + 1
  }
  ## now get determinant + derivatives, if required...
  if (deriv > 0) det1 <- oo$det1 else det1 <- NULL
  if (deriv > 1) det2 <- matrix(oo$det2,M,M) else det2 <- NULL  
  list(S=S,E=E,Qs=Qs,rS=rS,det=oo$det,det1=det1,det2=det2,fixed.penalty = fixed.penalty)
}


get.Eb <- function(rS,rank) 
## temporary routine to get balanced sqrt of total penalty
## should eventually be moved to estimate.gam, or gam.setup,
## as it's sp independent, but that means re doing gam.fit3 call list,
## which should only be done after method is tested
{ q <- nrow(rS[[1]])
  S <- matrix(0,q,q)
  for (i in 1:length(rS)) { 
    Si <- rS[[i]]%*%t(rS[[i]])
    S <- S + Si/sqrt(sum(Si^2)) 
  }
  t(mroot(S,rank=rank)) ## E such that E'E = S
}

gam.fit3 <- function (x, y, sp, Eb,UrS=list(),
            weights = rep(1, nobs), start = NULL, etastart = NULL, 
            mustart = NULL, offset = rep(0, nobs),U1=diag(ncol(x)), Mp=-1, family = gaussian(), 
            control = gam.control(), intercept = TRUE,deriv=2,
            gamma=1,scale=1,printWarn=TRUE,scoreType="REML",null.coef=rep(0,ncol(x)),
            pearson.extra=0,dev.extra=0,n.true=-1,...) {
## Inputs:
## * x model matrix
## * y response
## * sp log smoothing parameters
## * Eb square root of nicely balanced total penalty matrix used for rank detection
## * UrS list of penalty square roots in range space of overall penalty. UrS[[i]]%*%t(UrS[[i]]) 
##   is penalty. See 'estimate.gam' for more.
## * weights prior weights (reciprocal variance scale)
## * start initial values for parameters
## * etastart initial values for eta
## * mustart initial values for mu... only one of last## * control - control list.
## * intercept - indicates whether model has one.
## * deriv - order 0,1 or 2 derivatives are to be returned (lower is cheaper!)
## * gamma - multiplier for effective degrees of freedom in GCV/UBRE.
## * scale parameter. Negative signals to estimate.
## * printWarn print or supress?
## * scoreType - type of smoothness selection to use.
## * null.coef - coefficients for a null model, in order to be able to check for immediate 
##   divergence.
## * pearson.extra is an extra component to add to the pearson statistic in the P-REML/P-ML 
##   case, only.
## * dev.extra is an extra component to add to the deviance in the REML and ML cases only.
## * n.true is to be used in place of the length(y) in ML/REML calculations,
##   and the scale.est only.
## 
## Version with new reparameterization and truncation strategy. Allows iterative weights 
## to be negative. Basically the workhorse routine for Wood (2011) JRSSB.
## A much modified version of glm.fit. Purpose is to estimate regression coefficients 
## and compute a smoothness selection score along with its derivatives.
##
    if (control$trace) { t0 <- proc.time();tc <- 0} 

    if (family$link==family$canonical) fisher <- TRUE else fisher=FALSE 
    ## ... if canonical Newton = Fisher, but Fisher cheaper!
    if (scale>0) scale.known <- TRUE else scale.known <- FALSE
    if (!scale.known&&scoreType%in%c("REML","ML")) { ## the final element of sp is actually log(scale)
      nsp <- length(sp)
      scale <- exp(sp[nsp])
      sp <- sp[-nsp]
    }
    if (!deriv%in%c(0,1,2)) stop("unsupported order of differentiation requested of gam.fit3")
    x <- as.matrix(x)  
    nSp <- length(sp)  
    if (nSp==0) deriv.sp <- 0 else deriv.sp <- deriv 

    rank.tol <- .Machine$double.eps*100 ## tolerance to use for rank deficiency

    xnames <- dimnames(x)[[2]]
    ynames <- if (is.matrix(y)) 
        rownames(y)
    else names(y)


    q <- ncol(x)
    if (length(UrS)) { ## find a stable reparameterization...
    
      grderiv <- deriv*as.numeric(scoreType%in%c("REML","ML","P-REML","P-ML"))
      rp <- gam.reparam(UrS,sp,grderiv) ## note also detects fixed penalty if present
 ## Following is for debugging only...
 #     deriv.check <- FALSE
 #     if (deriv.check&&grderiv) {
 #       eps <- 1e-4
 #       fd.grad <- rp$det1
 #       for (i in 1:length(sp)) {
 #         spp <- sp; spp[i] <- spp[i] + eps/2
 #         rp1 <- gam.reparam(UrS,spp,grderiv)
 #         spp[i] <- spp[i] - eps
 #         rp0 <- gam.reparam(UrS,spp,grderiv)
 #         fd.grad[i] <- (rp1$det-rp0$det)/eps
 #       }
 #       print(fd.grad)
 #       print(rp$det1) 
 #     }

      T <- diag(q)
      T[1:ncol(rp$Qs),1:ncol(rp$Qs)] <- rp$Qs
      T <- U1%*%T ## new params b'=T'b old params
    
      null.coef <- t(T)%*%null.coef

      x <- x%*%T   ## model matrix
   
      rS <- list()
      for (i in 1:length(UrS)) {
        rS[[i]] <- rbind(rp$rS[[i]],matrix(0,Mp,ncol(rp$rS[[i]])))
      } ## square roots of penalty matrices in current parameterization
      Eb <- Eb%*%T ## balanced penalty matrix
      rows.E <- q-Mp
      Sr <- cbind(rp$E,matrix(0,nrow(rp$E),Mp))
      St <- rbind(cbind(rp$S,matrix(0,nrow(rp$S),Mp)),matrix(0,Mp,q))
    } else { 
      T <- diag(q); 
      St <- matrix(0,q,q) 
      rSncol <- sp <- rows.E <- Eb <- Sr <- 0   
      rS <- list(0)
      rp <- list(det=0,det1 = rep(0,0),det2 = rep(0,0),fixed.penalty=FALSE)
    }
    iter <- 0;coef <- rep(0,ncol(x))
   
    conv <- FALSE
    n <- nobs <- NROW(y) ## n is just to keep codetools happy
    if (n.true <= 0) n.true <- nobs ## n.true is used in criteria in place of nobs
    nvars <- ncol(x)
    EMPTY <- nvars == 0
    if (is.null(weights)) 
        weights <- rep.int(1, nobs)
    if (is.null(offset)) 
        offset <- rep.int(0, nobs)
    variance <- family$variance
    dev.resids <- family$dev.resids
    aic <- family$aic
    linkinv <- family$linkinv
    mu.eta <- family$mu.eta
    if (!is.function(variance) || !is.function(linkinv)) 
        stop("illegal `family' argument")
    valideta <- family$valideta
    if (is.null(valideta)) 
        valideta <- function(eta) TRUE
    validmu <- family$validmu
    if (is.null(validmu)) 
       validmu <- function(mu) TRUE
    if (is.null(mustart)) {
        eval(family$initialize)
    }
    else {
        mukeep <- mustart
        eval(family$initialize)
        mustart <- mukeep
    }

    ## Added code
    if (family$family=="gaussian"&&family$link=="identity") strictly.additive <- TRUE else
      strictly.additive <- FALSE

    ## end of added code

    D1 <- D2 <- P <- P1 <- P2 <- trA <- trA1 <- trA2 <- 
        GCV<- GCV1<- GCV2<- GACV<- GACV1<- GACV2<- UBRE <-
        UBRE1<- UBRE2<- REML<- REML1<- REML2 <-NULL

    if (EMPTY) {
        eta <- rep.int(0, nobs) + offset
        if (!valideta(eta)) 
            stop("Invalid linear predictor values in empty model")
        mu <- linkinv(eta)
        if (!validmu(mu)) 
            stop("Invalid fitted means in empty model")
        dev <- sum(dev.resids(y, mu, weights))
        w <- (weights * mu.eta(eta)^2)/variance(mu)   ### BUG: incorrect for Newton
        residuals <- (y - mu)/mu.eta(eta)
        good <- rep(TRUE, length(residuals))
        boundary <- conv <- TRUE
        coef <- numeric(0)
        iter <- 0
        V <- variance(mu)
        alpha <- dev
        trA2 <- trA1 <- trA <- 0
        if (deriv) GCV2 <- GCV1<- UBRE2 <- UBRE1<-trA1 <- rep(0,nSp)
        GCV <- nobs*alpha/(nobs-gamma*trA)^2
        UBRE <- alpha/nobs - scale + 2*gamma/n*trA
        scale.est <- alpha / (nobs - trA)
    } ### end if (EMPTY)
    else {
        coefold <- NULL
        eta <- if (!is.null(etastart)) 
            etastart
        else if (!is.null(start)) 
            if (length(start) != nvars) 
                stop("Length of start should equal ", nvars, 
                  " and correspond to initial coefs for ", deparse(xnames))
            else {
                coefold <- start
                offset + as.vector(if (NCOL(x) == 1) 
                  x * start
                else x %*% start)
            }
        else family$linkfun(mustart)
        etaold <- eta
        muold <- mu <- linkinv(eta)
        if (!(validmu(mu) && valideta(eta))) 
            stop("Can't find valid starting values: please specify some")
    
        boundary <- conv <- FALSE
        rV=matrix(0,ncol(x),ncol(x))   
       
        ## need an initial `null deviance' to test for initial divergence... 
        ## null.coef <- qr.coef(qr(x),family$linkfun(mean(y)+0*y))
        ## null.coef[is.na(null.coef)] <- 0 
        null.eta <- as.numeric(x%*%null.coef + as.numeric(offset))
        old.pdev <- sum(dev.resids(y, linkinv(null.eta), weights)) + t(null.coef)%*%St%*%null.coef 
        ## ... if the deviance exceeds this then there is an immediate problem
    
        for (iter in 1:control$maxit) { ## start of main fitting iteration
            good <- weights > 0
            var.val <- variance(mu)
            varmu <- var.val[good]
            if (any(is.na(varmu))) 
                stop("NAs in V(mu)")
            if (any(varmu == 0)) 
                stop("0s in V(mu)")
            mu.eta.val <- mu.eta(eta)
            if (any(is.na(mu.eta.val[good]))) 
                stop("NAs in d(mu)/d(eta)")
            
            good <- (weights > 0) & (mu.eta.val != 0)
         
            if (all(!good)) {
                conv <- FALSE
                warning("No observations informative at iteration ", iter)
                break
            }
            mevg<-mu.eta.val[good];mug<-mu[good];yg<-y[good]
            weg<-weights[good];var.mug<-var.val[good]
            if (fisher) { ## Conventional Fisher scoring
              z <- (eta - offset)[good] + (yg - mug)/mevg
              w <- (weg * mevg^2)/var.mug
            } else { ## full Newton
              c = yg - mug
              alpha <- 1 + c*(family$dvar(mug)/var.mug + family$d2link(mug)*mevg)
              alpha[alpha==0] <- .Machine$double.eps
              z <- (eta - offset)[good] + (yg-mug)/(mevg*alpha) 
              ## ... offset subtracted as eta = X%*%beta + offset
              w <- weg*alpha*mevg^2/var.mug
            }

            ## Here a Fortran call has been replaced by pls_fit1 call
           
            if (sum(good)<ncol(x)) stop("Not enough informative observations.")
            if (control$trace) t1 <- proc.time()
            oo <- .C(C_pls_fit1,y=as.double(z),X=as.double(x[good,]),w=as.double(w),
                     E=as.double(Sr),Es=as.double(Eb),n=as.integer(sum(good)),
                     q=as.integer(ncol(x)),rE=as.integer(rows.E),eta=as.double(z),
                     penalty=as.double(1),rank.tol=as.double(rank.tol),nt=as.integer(control$nthreads))
            if (control$trace) tc <- tc + sum((proc.time()-t1)[c(1,4)])

            if (!fisher&&oo$n<0) { ## likelihood indefinite - switch to Fisher for this step
              z <- (eta - offset)[good] + (yg - mug)/mevg
              w <- (weg * mevg^2)/var.mug
              if (control$trace) t1 <- proc.time()
              oo <- .C(C_pls_fit1,y=as.double(z),X=as.double(x[good,]),w=as.double(w),
                       E=as.double(Sr),Es=as.double(Eb),n=as.integer(sum(good)),
                       q=as.integer(ncol(x)),rE=as.integer(rows.E),eta=as.double(z),
                       penalty=as.double(1),rank.tol=as.double(rank.tol),nt=as.integer(control$nthreads))
              if (control$trace) tc <- tc + sum((proc.time()-t1)[c(1,4)])
            }

            start <- oo$y[1:ncol(x)];
            penalty <- oo$penalty
            eta <- drop(x%*%start)

            if (any(!is.finite(start))) {
                conv <- FALSE
                warning("Non-finite coefficients at iteration ", 
                  iter)
                break
            }        
     
           mu <- linkinv(eta <- eta + offset)
           dev <- sum(dev.resids(y, mu, weights))
          
           if (control$trace) 
                cat("Deviance =", dev, "Iterations -", iter, 
                  "\n")
            boundary <- FALSE
            
            if (!is.finite(dev)) {
                if (is.null(coefold)) {
                  if (is.null(null.coef)) 
                  stop("no valid set of coefficients has been found:please supply starting values", 
                    call. = FALSE)
                  ## Try to find feasible coefficients from the null.coef and null.eta
                  coefold <- null.coef
                  etaold <- null.eta
                }
                warning("Step size truncated due to divergence", 
                  call. = FALSE)
                ii <- 1
                while (!is.finite(dev)) {
                  if (ii > control$maxit) 
                    stop("inner loop 1; can't correct step size")
                  ii <- ii + 1
                  start <- (start + coefold)/2
                  eta <- (eta + etaold)/2               
                  mu <- linkinv(eta)
                  dev <- sum(dev.resids(y, mu, weights))
                }
                boundary <- TRUE
                if (control$trace) 
                  cat("Step halved: new deviance =", dev, "\n")
            }
            if (!(valideta(eta) && validmu(mu))) {
                warning("Step size truncated: out of bounds", 
                  call. = FALSE)
                ii <- 1
                while (!(valideta(eta) && validmu(mu))) {
                  if (ii > control$maxit) 
                    stop("inner loop 2; can't correct step size")
                  ii <- ii + 1
                  start <- (start + coefold)/2
                  eta <- (eta + etaold)/2 
                  mu <- linkinv(eta)
                }
                boundary <- TRUE
                dev <- sum(dev.resids(y, mu, weights))
                if (control$trace) 
                  cat("Step halved: new deviance =", dev, "\n")
            }

            pdev <- dev + penalty  ## the penalized deviance 

            if (control$trace) 
                  cat("penalized deviance =", pdev, "\n")

            div.thresh <- 10*(.1+abs(old.pdev))*.Machine$double.eps^.5 
            ## ... threshold for judging divergence --- too tight and near
            ## perfect convergence can cause a failure here

            if (pdev-old.pdev>div.thresh) { ## solution diverging
             ii <- 1 ## step halving counter
             if (iter==1) { ## immediate divergence, need to shrink towards zero 
               etaold <- null.eta; coefold <- null.coef
             }
             while (pdev -old.pdev > div.thresh)  
             { ## step halve until pdev <= old.pdev
                if (ii > 200) 
                   stop("inner loop 3; can't correct step size")
                ii <- ii + 1
                start <- (start + coefold)/2 
                eta <- (eta + etaold)/2               
                mu <- linkinv(eta)
                  dev <- sum(dev.resids(y, mu, weights))
                  pdev <- dev + t(start)%*%St%*%start ## the penalized deviance
                if (control$trace) 
                  cat("Step halved: new penalized deviance =", pdev, "\n")
              }
            } 
            
            if (strictly.additive) { conv <- TRUE;coef <- start;break;}

            if (abs(pdev - old.pdev)/(0.1 + abs(pdev)) < control$epsilon) {
                if (max(abs(start-coefold))>control$epsilon*max(abs(start+coefold))/2) {
               ## if (max(abs(mu-muold))>control$epsilon*max(abs(mu+muold))/2) {
                  old.pdev <- pdev
                  coef <- coefold <- start
                  etaold <- eta 
                  muold <- mu
                } else {
                  conv <- TRUE
                  coef <- start
                  break 
                }
            }
            else {  old.pdev <- pdev
                coef <- coefold <- start
                etaold <- eta 
            }
        } ### end main loop 
       
        dev <- sum(dev.resids(y, mu, weights)) 
       
        ## Now call the derivative calculation scheme. This requires the
        ## following inputs:
        ## z and w - the pseudodata and weights
        ## X the model matrix and E where EE'=S
        ## rS the single penalty square roots
        ## sp the log smoothing parameters
        ## y and mu the data and model expected values
        ## g1,g2,g3 - the first 3 derivatives of g(mu) wrt mu
        ## V,V1,V2 - V(mu) and its first two derivatives wrt mu
        ## on output it returns the gradient and hessian for
        ## the deviance and trA 

         good <- weights > 0
         var.val <- variance(mu)
         varmu <- var.val[good]
         if (any(is.na(varmu))) stop("NAs in V(mu)")
         if (any(varmu == 0)) stop("0s in V(mu)")
         mu.eta.val <- mu.eta(eta)
         if (any(is.na(mu.eta.val[good]))) 
                stop("NAs in d(mu)/d(eta)")
   
#         if (fisher) {
#              good <- (weights > 0) & (mu.eta.val != 0)
#         } else { ## full Newton
#              c <- y - mu
#              alpha <- 1 + c*(family$dvar(mu)/var.val + family$d2link(mu)*mu.eta.val)
#              ### can't just drop obs when alpha==0, as they are informative, but
#              ### happily using an `effective zero' is stable here, and there is 
#              ### a natural effective zero, since E(alpha) = 1.
#              alpha[alpha==0] <- .Machine$double.eps 
#              good <-  (weights > 0) & (mu.eta.val != 0)
#         }

         good <- (weights > 0) & (mu.eta.val != 0)
         mevg <- mu.eta.val[good];mug <- mu[good];yg <- y[good]
         weg <- weights[good];etag <- eta[good]
         var.mug<-var.val[good]

         if (fisher) { ## Conventional Fisher scoring
              z <- (eta - offset)[good] + (yg - mug)/mevg
              w <- (weg * mevg^2)/var.mug
              alpha <- wf <- 0 ## Don't need Fisher weights separately
         } else { ## full Newton
              c <- yg - mug
              alpha <- 1 + c*(family$dvar(mug)/var.mug + family$d2link(mug)*mevg)
              ### can't just drop obs when alpha==0, as they are informative, but
              ### happily using an `effective zero' is stable here, and there is 
              ### a natural effective zero, since E(alpha) = 1.
              alpha[alpha==0] <- .Machine$double.eps 
              z <- (eta - offset)[good] + (yg-mug)/(mevg*alpha) 
              ## ... offset subtracted as eta = X%*%beta + offset
              wf <- weg*mevg^2/var.mug ## Fisher weights for EDF calculation
              w <- wf * alpha   ## Full Newton weights
         }
        
         g1 <- 1/mevg
         g2 <- family$d2link(mug)
         g3 <- family$d3link(mug)

         V <- family$variance(mug)
         V1 <- family$dvar(mug)
         V2 <- family$d2var(mug)      
        
         if (fisher) {
           g4 <- V3 <- 0
         } else {
           g4 <- family$d4link(mug)
           V3 <- family$d3var(mug)
         }

         if (TRUE) { ### TEST CODE for derivative ratio based versions of code... 
           g2 <- g2/g1;g3 <- g3/g1;g4 <- g4/g1
           V1 <- V1/V;V2 <- V2/V;V3 <- V3/V
         }

         P1 <- D1 <- array(0,nSp);P2 <- D2 <- matrix(0,nSp,nSp) # for derivs of deviance/ Pearson
         trA1 <- array(0,nSp);trA2 <- matrix(0,nSp,nSp) # for derivs of tr(A)
         rV=matrix(0,ncol(x),ncol(x));
         dum <- 1
         if (control$trace) cat("calling gdi...")

       REML <- 0 ## signals GCV/AIC used
       if (scoreType%in%c("REML","P-REML")) {REML <- 1;remlInd <- 1} else 
       if (scoreType%in%c("ML","P-ML")) {REML <- -1;remlInd <- 0} 

       if (REML==0) rSncol <- unlist(lapply(rS,ncol)) else rSncol <- unlist(lapply(UrS,ncol))
       if (control$trace) t1 <- proc.time()
       oo <- .C(C_gdi1,X=as.double(x[good,]),E=as.double(Sr),Eb = as.double(Eb), 
                rS = as.double(unlist(rS)),U1=as.double(U1),sp=as.double(exp(sp)),
                z=as.double(z),w=as.double(w),wf=as.double(wf),alpha=as.double(alpha),
                mu=as.double(mug),eta=as.double(etag),y=as.double(yg),
                p.weights=as.double(weg),g1=as.double(g1),g2=as.double(g2),
                g3=as.double(g3),g4=as.double(g4),V0=as.double(V),V1=as.double(V1),
                V2=as.double(V2),V3=as.double(V3),beta=as.double(coef),D1=as.double(D1),
                D2=as.double(D2),P=as.double(dum),P1=as.double(P1),P2=as.double(P2),
                trA=as.double(dum),trA1=as.double(trA1),trA2=as.double(trA2),
                rV=as.double(rV),rank.tol=as.double(rank.tol),
                conv.tol=as.double(control$epsilon),rank.est=as.integer(1),n=as.integer(length(z)),
                p=as.integer(ncol(x)),M=as.integer(nSp),Mp=as.integer(Mp),Enrow = as.integer(rows.E),
                rSncol=as.integer(rSncol),deriv=as.integer(deriv.sp),
                REML = as.integer(REML),fisher=as.integer(fisher),
                fixed.penalty = as.integer(rp$fixed.penalty),nthreads=as.integer(control$nthreads))      
         if (control$trace) { 
           tg <- sum((proc.time()-t1)[c(1,4)])
           cat("done!\n")
         }
 
         rV <- matrix(oo$rV,ncol(x),ncol(x)) ## rV%*%t(rV)*scale gives covariance matrix 
         
         Kmat <- matrix(0,nrow(x),ncol(x)) 
         Kmat[good,] <- oo$X                    ## rV%*%t(K)%*%(sqrt(wf)*X) = F; diag(F) is edf array 

         coef <- oo$beta;
         trA <- oo$trA;
         scale.est <- (dev+dev.extra)/(n.true-trA)
         reml.scale <- NA  

        if (scoreType%in%c("REML","ML")) { ## use Laplace (RE)ML
          
          ls <- family$ls(y,weights,n,scale)*n.true/nobs ## saturated likelihood and derivatives
          Dp <- dev + oo$conv.tol + dev.extra
          REML <- Dp/(2*scale) - ls[1] + oo$rank.tol/2 - rp$det/2 - remlInd*Mp/2*log(2*pi*scale)
          attr(REML,"Dp") <- Dp/(2*scale)
          if (deriv) {
            REML1 <- oo$D1/(2*scale) + oo$trA1/2 - rp$det1/2 
            if (deriv==2) REML2 <- (matrix(oo$D2,nSp,nSp)/scale + matrix(oo$trA2,nSp,nSp) - rp$det2)/2
            if (sum(!is.finite(REML2))) {
               stop("Non finite derivatives. Try decreasing fit tolerance! See `epsilon' in `gam.contol'")
            }
          }
          if (!scale.known&&deriv) { ## need derivatives wrt log scale, too 
            ##ls <- family$ls(y,weights,n,scale) ## saturated likelihood and derivatives
            dlr.dlphi <- -Dp/(2 *scale) - ls[2]*scale - Mp/2*remlInd
            d2lr.d2lphi <- Dp/(2*scale) - ls[3]*scale^2 - ls[2]*scale
            d2lr.dspphi <- -oo$D1/(2*scale)
            REML1 <- c(REML1,dlr.dlphi)
            if (deriv==2) {
              REML2 <- rbind(REML2,as.numeric(d2lr.dspphi))
              REML2 <- cbind(REML2,c(as.numeric(d2lr.dspphi),d2lr.d2lphi))
            }
          }
          reml.scale <- scale
        } else if (scoreType%in%c("P-REML","P-ML")) { ## scale unknown use Pearson-Laplace REML
          reml.scale <- phi <- (oo$P*(nobs-Mp)+pearson.extra)/(n.true-Mp) ## REMLish scale estimate
          ## correct derivatives, if needed...
          oo$P1 <- oo$P1*(nobs-Mp)/(n.true-Mp)
          oo$P2 <- oo$P2*(nobs-Mp)/(n.true-Mp)

          ls <- family$ls(y,weights,n,phi)*n.true/nobs ## saturated likelihood and derivatives
        
          Dp <- dev + oo$conv.tol + dev.extra
         
          K <- oo$rank.tol/2 - rp$det/2
                 
          REML <- Dp/(2*phi) - ls[1] + K - Mp/2*log(2*pi*phi)*remlInd
          attr(REML,"Dp") <- Dp/(2*phi)
          if (deriv) {
            phi1 <- oo$P1; Dp1 <- oo$D1; K1 <- oo$trA1/2 - rp$det1/2;
            REML1 <- Dp1/(2*phi) - phi1*(Dp/(2*phi^2)+Mp/(2*phi)*remlInd + ls[2]) + K1
            if (deriv==2) {
                   phi2 <- matrix(oo$P2,nSp,nSp);Dp2 <- matrix(oo$D2,nSp,nSp)
                   K2 <- matrix(oo$trA2,nSp,nSp)/2 - rp$det2/2   
                   REML2 <- 
                   Dp2/(2*phi) - (outer(Dp1,phi1)+outer(phi1,Dp1))/(2*phi^2) +
                   (Dp/phi^3 - ls[3] + Mp/(2*phi^2)*remlInd)*outer(phi1,phi1) -
                   (Dp/(2*phi^2)+ls[2]+Mp/(2*phi)*remlInd)*phi2 + K2
            }
          }
 
        } else { ## Not REML ....

           P <- oo$P
           
           delta <- nobs - gamma * trA
           delta.2 <- delta*delta           
  
           GCV <- nobs*dev/delta.2
           GACV <- dev/nobs + P * 2*gamma*trA/(delta * nobs) 

           UBRE <- dev/nobs - 2*delta*scale/nobs + scale
        
           if (deriv) {
             trA1 <- oo$trA1
           
             D1 <- oo$D1
             P1 <- oo$P1
          
             if (sum(!is.finite(D1))||sum(!is.finite(P1))||sum(!is.finite(trA1))) { 
                 stop(
               "Non-finite derivatives. Try decreasing fit tolerance! See `epsilon' in `gam.contol'")
             }
         
             delta.3 <- delta*delta.2
  
             GCV1 <- nobs*D1/delta.2 + 2*nobs*dev*trA1*gamma/delta.3
             GACV1 <- D1/nobs + 2*P/delta.2 * trA1 + 2*gamma*trA*P1/(delta*nobs)

             UBRE1 <- D1/nobs + gamma * trA1 *2*scale/nobs
             if (deriv==2) {
               trA2 <- matrix(oo$trA2,nSp,nSp) 
               D2 <- matrix(oo$D2,nSp,nSp)
               P2 <- matrix(oo$P2,nSp,nSp)
              
               if (sum(!is.finite(D2))||sum(!is.finite(P2))||sum(!is.finite(trA2))) { 
                 stop(
                 "Non-finite derivatives. Try decreasing fit tolerance! See `epsilon' in `gam.contol'")
               }
             
               GCV2 <- outer(trA1,D1)
               GCV2 <- (GCV2 + t(GCV2))*gamma*2*nobs/delta.3 +
                      6*nobs*dev*outer(trA1,trA1)*gamma*gamma/(delta.2*delta.2) + 
                      nobs*D2/delta.2 + 2*nobs*dev*gamma*trA2/delta.3  
               GACV2 <- D2/nobs + outer(trA1,trA1)*4*P/(delta.3) +
                      2 * P * trA2 / delta.2 + 2 * outer(trA1,P1)/delta.2 +
                      2 * outer(P1,trA1) *(1/(delta * nobs) + trA/(nobs*delta.2)) +
                      2 * trA * P2 /(delta * nobs) 
               GACV2 <- (GACV2 + t(GACV2))*.5
               UBRE2 <- D2/nobs +2*gamma * trA2 * scale / nobs
             } ## end if (deriv==2)
           } ## end if (deriv)
        } ## end !REML
        # end of inserted code
        if (!conv&&printWarn) 
            warning("Algorithm did not converge")
        if (printWarn&&boundary) 
            warning("Algorithm stopped at boundary value")
        eps <- 10 * .Machine$double.eps
        if (printWarn&&family$family[1] == "binomial") {
            if (any(mu > 1 - eps) || any(mu < eps)) 
                warning("fitted probabilities numerically 0 or 1 occurred")
        }
        if (printWarn&&family$family[1] == "poisson") {
            if (any(mu < eps)) 
                warning("fitted rates numerically 0 occurred")
        }
 
        residuals <- rep.int(NA, nobs)
        residuals[good] <- z - (eta - offset)[good]
          
        ## undo reparameterization....
        coef <- as.numeric(T %*% coef)
        rV <- T %*% rV

        names(coef) <- xnames 
    } ### end if (!EMPTY)
    names(residuals) <- ynames
    names(mu) <- ynames
    names(eta) <- ynames
    wt <- rep.int(0, nobs)
    if (fisher) wt[good] <- w else wt[good] <- wf  ## note that Fisher weights are returned
    names(wt) <- ynames
    names(weights) <- ynames
    names(y) <- ynames
   
    wtdmu <- if (intercept) 
        sum(weights * y)/sum(weights)
    else linkinv(offset)
    nulldev <- sum(dev.resids(y, wtdmu, weights))
    n.ok <- nobs - sum(weights == 0)
    nulldf <- n.ok - as.integer(intercept)
   
    aic.model <- aic(y, n, mu, weights, dev) # note: incomplete 2*edf needs to be added
    if (control$trace) {
      t1 <- proc.time()
      at <- sum((t1-t0)[c(1,4)])
      cat("Proportion time in C: ",(tc+tg)/at," ls:",tc/at," gdi:",tg/at,"\n")
    } 
   
    list(coefficients = coef, residuals = residuals, fitted.values = mu, 
         family = family, linear.predictors = eta, deviance = dev, 
        null.deviance = nulldev, iter = iter, weights = wt, prior.weights = weights, 
        df.null = nulldf, y = y, converged = conv,
        boundary = boundary,D1=D1,D2=D2,P=P,P1=P1,P2=P2,trA=trA,trA1=trA1,trA2=trA2,
        GCV=GCV,GCV1=GCV1,GCV2=GCV2,GACV=GACV,GACV1=GACV1,GACV2=GACV2,UBRE=UBRE,
        UBRE1=UBRE1,UBRE2=UBRE2,REML=REML,REML1=REML1,REML2=REML2,rV=rV,
        scale.est=scale.est,reml.scale= reml.scale,aic=aic.model,rank=oo$rank.est,K=Kmat)
} ## end gam.fit3


gam.fit3.post.proc <- function(X,object) {
## get edf array and covariance matrices after a gam fit. 
## X is original model matrix
  Vb <- object$rV%*%t(object$rV)*object$scale ## Bayesian cov.
  PKt <- object$rV%*%t(object$K)
  F <- PKt%*%(sqrt(object$weights)*X)
  edf <- diag(F) ## effective degrees of freedom
  edf1 <- 2*edf - rowSums(t(F)*F) ## alternative
  ## edf <- rowSums(PKt*t(sqrt(object$weights)*X))
  ## Ve <- PKt%*%t(PKt)*object$scale  ## frequentist cov
  Ve <- F%*%Vb ## not quite as stable as above, but quicker
  hat <- rowSums(object$K*object$K)
  ## get QR factor R of WX - more efficient to do this
  ## in gdi_1 really, but that means making QR of augmented 
  ## a two stage thing, so not clear cut...
  qrx <- qr(sqrt(object$weights)*X,LAPACK=TRUE)
  R <- qr.R(qrx);R[,qrx$pivot] <- R
  ##bias = as.numeric(object$coefficients - F%*%object$coefficients)
  list(Vb=Vb,Ve=Ve,edf=edf,edf1=edf1,hat=hat,F=F,R=R)
}


score.transect <- function(ii, x, y, sp, Eb,UrS=list(), 
            weights = rep(1, length(y)), start = NULL, etastart = NULL, 
            mustart = NULL, offset = rep(0, length(y)),U1,Mp,family = gaussian(), 
            control = gam.control(), intercept = TRUE,deriv=2,
            gamma=1,scale=1,printWarn=TRUE,scoreType="REML",eps=1e-7,null.coef=rep(0,ncol(x)),...) {
## plot a transect through the score for sp[ii]
  np <- 200
  if (scoreType%in%c("REML","P-REML","ML","P-ML")) reml <- TRUE else reml <- FALSE
  score <- spi <- seq(-30,30,length=np)
  for (i in 1:np) {

     sp[ii] <- spi[i]
     b<-gam.fit3(x=x, y=y, sp=sp,Eb=Eb,UrS=UrS,
      offset = offset,U1=U1,Mp=Mp,family = family,weights=weights,deriv=0,
      control=control,gamma=gamma,scale=scale,
      printWarn=FALSE,mustart=mustart,scoreType=scoreType,null.coef=null.coef,...)

      if (reml) {
        score[i] <- b$REML
      } else if (scoreType=="GACV") {
        score[i] <- b$GACV
      } else if (scoreType=="UBRE"){
        score[i] <- b$UBRE 
      } else { ## default to deviance based GCV
        score[i] <- b$GCV
      }
  }
  par(mfrow=c(2,2),mar=c(4,4,1,1))
  plot(spi,score,xlab="log(sp)",ylab=scoreType,type="l")
  plot(spi[1:(np-1)],score[2:np]-score[1:(np-1)],type="l",ylab="differences")
  plot(spi,score,ylim=c(score[1]-.1,score[1]+.1),type="l")
  plot(spi,score,ylim=c(score[np]-.1,score[np]+.1),type="l")
}

deriv.check <- function(x, y, sp, Eb,UrS=list(), 
            weights = rep(1, length(y)), start = NULL, etastart = NULL, 
            mustart = NULL, offset = rep(0, length(y)),U1,Mp,family = gaussian(), 
            control = gam.control(), intercept = TRUE,deriv=2,
            gamma=1,scale=1,printWarn=TRUE,scoreType="REML",eps=1e-7,null.coef=rep(0,ncol(x)),...)
## FD checking of derivatives: basically a debugging routine
{  if (!deriv%in%c(1,2)) stop("deriv should be 1 or 2")
   if (control$epsilon>1e-9) control$epsilon <- 1e-9 
   b<-gam.fit3(x=x, y=y, sp=sp,Eb=Eb,UrS=UrS,
      offset = offset,U1=U1,Mp=Mp,family = family,weights=weights,deriv=deriv,
      control=control,gamma=gamma,scale=scale,
      printWarn=FALSE,mustart=mustart,scoreType=scoreType,null.coef=null.coef,...)

   P0 <- b$P;fd.P1 <- P10 <- b$P1;  if (deriv==2) fd.P2 <- P2 <- b$P2 
   trA0 <- b$trA;fd.gtrA <- gtrA0 <- b$trA1 ; if (deriv==2) fd.htrA <- htrA <- b$trA2 
   dev0 <- b$deviance;fd.D1 <- D10 <- b$D1 ; if (deriv==2) fd.D2 <- D2 <- b$D2 

   if (scoreType%in%c("REML","P-REML","ML","P-ML")) reml <- TRUE else reml <- FALSE

   if (reml) {
     score0 <- b$REML;grad0 <- b$REML1; if (deriv==2) hess <- b$REML2 
   } else if (scoreType=="GACV") {
     score0 <- b$GACV;grad0 <- b$GACV1;if (deriv==2) hess <- b$GACV2 
   } else if (scoreType=="UBRE"){
     score0 <- b$UBRE;grad0 <- b$UBRE1;if (deriv==2) hess <- b$UBRE2 
   } else { ## default to deviance based GCV
     score0 <- b$GCV;grad0 <- b$GCV1;if (deriv==2) hess <- b$GCV2
   }
  
   fd.grad <- grad0
   if (deriv==2) fd.hess <- hess
   for (i in 1:length(sp)) {
     sp1 <- sp;sp1[i] <- sp[i]+eps/2
     bf<-gam.fit3(x=x, y=y, sp=sp1,Eb=Eb,UrS=UrS,
      offset = offset,U1=U1,Mp=Mp,family = family,weights=weights,deriv=deriv,
      control=control,gamma=gamma,scale=scale,
      printWarn=FALSE,mustart=mustart,scoreType=scoreType,null.coef=null.coef,...)
      
     sp1 <- sp;sp1[i] <- sp[i]-eps/2
     bb<-gam.fit3(x=x, y=y, sp=sp1, Eb=Eb,UrS=UrS,
      offset = offset,U1=U1,Mp=Mp,family = family,weights=weights,deriv=deriv,
      control=control,gamma=gamma,scale=scale,
      printWarn=FALSE,mustart=mustart,scoreType=scoreType,null.coef=null.coef,...)
      
   
      if (!reml) {
        Pb <- bb$P;Pf <- bf$P 
        P1b <- bb$P1;P1f <- bf$P1
        trAb <- bb$trA;trAf <- bf$trA
        gtrAb <- bb$trA1;gtrAf <- bf$trA1
        devb <- bb$deviance;devf <- bf$deviance
        D1b <- bb$D1;D1f <- bf$D1
      }
     

      if (reml) {
        scoreb <- bb$REML;scoref <- bf$REML;
        if (deriv==2) { gradb <- bb$REML1;gradf <- bf$REML1}
      } else if (scoreType=="GACV") {
        scoreb <- bb$GACV;scoref <- bf$GACV;
        if (deriv==2) { gradb <- bb$GACV1;gradf <- bf$GACV1}
      } else if (scoreType=="UBRE"){
        scoreb <- bb$UBRE; scoref <- bf$UBRE;
        if (deriv==2) { gradb <- bb$UBRE1;gradf <- bf$UBRE1} 
      } else { ## default to deviance based GCV
        scoreb <- bb$GCV;scoref <- bf$GCV;
        if (deriv==2) { gradb <- bb$GCV1;gradf <- bf$GCV1}
      }

      if (!reml) {
        fd.P1[i] <- (Pf-Pb)/eps
        fd.gtrA[i] <- (trAf-trAb)/eps
        fd.D1[i] <- (devf - devb)/eps
      }
      
     
      fd.grad[i] <- (scoref-scoreb)/eps
      if (deriv==2) { 
        fd.hess[,i] <- (gradf-gradb)/eps
        if (!reml) {
          fd.htrA[,i] <- (gtrAf-gtrAb)/eps
          fd.P2[,i] <- (P1f-P1b)/eps
          fd.D2[,i] <- (D1f-D1b)/eps
        } 
       
      }
   }
   
   if (!reml) {
     cat("\n Pearson Statistic... \n")
     cat("grad    ");print(P10)
     cat("fd.grad ");print(fd.P1)
     if (deriv==2) {
       fd.P2 <- .5*(fd.P2 + t(fd.P2))
       cat("hess\n");print(P2)
       cat("fd.hess\n");print(fd.P2)
     }

     cat("\n\n tr(A)... \n")
     cat("grad    ");print(gtrA0)
     cat("fd.grad ");print(fd.gtrA)
     if (deriv==2) {
       fd.htrA <- .5*(fd.htrA + t(fd.htrA))
       cat("hess\n");print(htrA)
       cat("fd.hess\n");print(fd.htrA)
     }
   

     cat("\n Deviance... \n")
     cat("grad    ");print(D10)
     cat("fd.grad ");print(fd.D1)
     if (deriv==2) {
       fd.D2 <- .5*(fd.D2 + t(fd.D2))
       cat("hess\n");print(D2)
       cat("fd.hess\n");print(fd.D2)
     }
   }
 
   cat("\n\n The objective...\n")

   cat("grad    ");print(grad0)
   cat("fd.grad ");print(fd.grad)
   if (deriv==2) {
     fd.hess <- .5*(fd.hess + t(fd.hess))
     cat("hess\n");print(hess)
     cat("fd.hess\n");print(fd.hess)
   }
   NULL
}


rt <- function(x,r1) {
## transform of x, asymptoting to values in r1
## returns derivatives wrt to x as well as transform values
## r1[i] == NA for no transform 
  x <- as.numeric(x)
  ind <- x>0 
  rho2 <- rho1 <- rho <- 0*x
  if (length(r1)==1) r1 <- x*0+r1
  h <- exp(x[ind])/(1+exp(x[ind]))
  h1 <- h*(1-h);h2 <- h1*(1-2*h)
  rho[ind] <- r1[ind]*(h-0.5)*2
  rho1[ind] <- r1[ind]*h1*2
  rho2[ind] <- r1[ind]*h2*2
  rho[!ind] <- r1[!ind]*x[!ind]/2
  rho1[!ind] <- r1[!ind]/2
  ind <- is.na(r1)
  rho[ind] <- x[ind]
  rho1[ind] <- 1
  rho2[ind] <- 0
  list(rho=rho,rho1=rho1,rho2=rho2)
}

rti <- function(r,r1) {
## inverse of rti.
  r <- as.numeric(r)
  ind <- r>0
  x <- r
  if (length(r1)==1) r1 <- x*0+r1
  r2 <- r[ind]*.5/r1[ind] + .5
  x[ind] <- log(r2/(1-r2))
  x[!ind] <- 2*r[!ind]/r1[!ind]
  ind <- is.na(r1)
  x[ind] <- r[ind]
  x
}

simplyFit <- function(lsp,X,y,Eb,UrS,L,lsp0,offset,U1,Mp,family,weights,
                   control,gamma,scale,conv.tol=1e-6,maxNstep=5,maxSstep=2,
                   maxHalf=30,printWarn=FALSE,scoreType="deviance",
                   mustart = NULL,null.coef=rep(0,ncol(X)),...)
## function with same argument list as `newton' and `bfgs' which simply fits
## the model given the supplied smoothing parameters...
{ reml <- scoreType%in%c("REML","P-REML","ML","P-ML") ## REML/ML indicator

  ## sanity check L
  if (is.null(L)) L <- diag(length(lsp)) else {
    if (!inherits(L,"matrix")) stop("L must be a matrix.")
    if (nrow(L)<ncol(L)) stop("L must have at least as many rows as columns.")
    if (nrow(L)!=length(lsp0)||ncol(L)!=length(lsp)) stop("L has inconsistent dimensions.")
  }
  if (is.null(lsp0)) lsp0 <- rep(0,ncol(L))
  ## initial fit

  b<-gam.fit3(x=X, y=y, sp=L%*%lsp+lsp0, Eb=Eb,UrS=UrS,
     offset = offset,U1=U1,Mp=Mp,family = family,weights=weights,deriv=0,
     control=control,gamma=gamma,scale=scale,
     printWarn=FALSE,mustart=mustart,scoreType=scoreType,null.coef=null.coef,...)

  if (reml) {       
          score <- b$REML
  } else if (scoreType=="GACV") {
          score <- b$GACV
  } else if (scoreType=="UBRE") {
          score <- b$UBRE
  } else score <- b$GCV

  list(score=score,lsp=lsp,lsp.full=L%*%lsp+lsp0,grad=NULL,hess=NULL,score.hist=NULL,iter=0,conv =NULL,object=b)

}


newton <- function(lsp,X,y,Eb,UrS,L,lsp0,offset,U1,Mp,family,weights,
                   control,gamma,scale,conv.tol=1e-6,maxNstep=5,maxSstep=2,
                   maxHalf=30,printWarn=FALSE,scoreType="deviance",
                   mustart = NULL,null.coef=rep(0,ncol(X)),pearson.extra,
                   dev.extra=0,n.true=-1,...)
## Newton optimizer for GAM gcv/aic optimization that can cope with an 
## indefinite Hessian! Main enhancements are: i) always perturbs the Hessian
## to +ve definite ii) step halves on step 
## failure, without obtaining derivatives until success; (iii) carries start
## values forward from one evaluation to next to speed convergence.    
## L is the matrix such that L%*%lsp + lsp0 gives the logs of the smoothing 
## parameters actually multiplying the S[[i]]'s
{  
  reml <- scoreType%in%c("REML","P-REML","ML","P-ML") ## REML/ML indicator

  ## sanity check L
  if (is.null(L)) L <- diag(length(lsp)) else {
    if (!inherits(L,"matrix")) stop("L must be a matrix.")
    if (nrow(L)<ncol(L)) stop("L must have at least as many rows as columns.")
    if (nrow(L)!=length(lsp0)||ncol(L)!=length(lsp)) stop("L has inconsistent dimensions.")
  }
  if (is.null(lsp0)) lsp0 <- rep(0,nrow(L)) 

  if (reml&&FALSE) { ## NOTE: routine set up to allow upper limits on lsp, but turned off.
    frob.X <- sqrt(sum(X*X))
    lsp.max <- rep(NA,length(lsp0))
    for (i in 1:nrow(L)) { 
      lsp.max[i] <- 16 + log(frob.X/sqrt(sum(UrS[[i]]^2))) - lsp0[i]
      if (lsp.max[i]<2) lsp.max[i] <- 2
    } 
  } else lsp.max <- NULL

  if (!is.null(lsp.max)) { ## then there are upper limits on lsp's
    lsp1.max <- coef(lm(lsp.max-lsp0~L-1)) ## get upper limits on lsp1 scale
    ind <- lsp>lsp1.max
    lsp[ind] <- lsp1.max[ind]-1 ## reset lsp's already over limit
    delta <- rti(lsp,lsp1.max) ## initial optimization parameters
  } else { ## optimization parameters are just lsp
    delta <- lsp
  }

  ## code designed to be turned on during debugging...
  check.derivs <- FALSE;sp.trace <- FALSE
  if (check.derivs) {
     deriv <- 2
     eps <- 1e-4
     deriv.check(x=X, y=y, sp=L%*%lsp+lsp0, Eb=Eb,UrS=UrS,
         offset = offset,U1=U1,Mp=Mp,family = family,weights=weights,deriv=deriv,
         control=control,gamma=gamma,scale=scale,
         printWarn=FALSE,mustart=mustart,
         scoreType=scoreType,eps=eps,null.coef=null.coef,...)
  }

#  ii <- 0
#  if (ii>0) {
#    score.transect(ii,x=X, y=y, sp=L%*%lsp+lsp0, Eb=Eb,UrS=UrS,
#         offset = offset,U1=U1,Mp=Mp,family = family,weights=weights,deriv=deriv,
#         control=control,gamma=gamma,scale=scale,
#         printWarn=FALSE,mustart=mustart,
#         scoreType=scoreType,eps=eps,null.coef=null.coef,...)
#  }
  ## ... end of debugging code 


  ## initial fit
  b<-gam.fit3(x=X, y=y, sp=L%*%lsp+lsp0,Eb=Eb,UrS=UrS,
     offset = offset,U1=U1,Mp=Mp,family = family,weights=weights,deriv=2,
     control=control,gamma=gamma,scale=scale,printWarn=FALSE,
     mustart=mustart,scoreType=scoreType,null.coef=null.coef,pearson.extra=pearson.extra,
     dev.extra=dev.extra,n.true=n.true,...)

  mustart<-b$fitted.values

  if (reml) {
     old.score <- score <- b$REML;grad <- b$REML1;hess <- b$REML2 
  } else if (scoreType=="GACV") {
    old.score <- score <- b$GACV;grad <- b$GACV1;hess <- b$GACV2 
  } else if (scoreType=="UBRE"){
    old.score <- score <- b$UBRE;grad <- b$UBRE1;hess <- b$UBRE2 
  } else { ## default to deviance based GCV
    old.score <- score <- b$GCV;grad <- b$GCV1;hess <- b$GCV2
  }
  
  grad <- t(L)%*%grad
  hess <- t(L)%*%hess%*%L

  if (!is.null(lsp.max)) { ## need to transform to delta space
    rho <- rt(delta,lsp1.max)
    nr <- length(rho$rho1)
    hess <- diag(rho$rho1,nr,nr)%*%hess%*%diag(rho$rho1,nr,nr) + diag(rho$rho2*grad)
    grad <- rho$rho1*grad
  }

  if (reml) score.scale <- abs(log(b$scale.est)) + abs(score) else 
  score.scale <- b$scale.est + abs(score)    
  uconv.ind <- abs(grad) > score.scale*conv.tol
  ## check for all converged too soon, and undo !
  if (!sum(uconv.ind)) uconv.ind <- uconv.ind | TRUE
  score.hist <- rep(NA,200)
  for (i in 1:200) {
   ## debugging code....
   if (check.derivs) {
     deriv <- 2
     eps <- 1e-4
     deriv.check(x=X, y=y, sp=L%*%lsp+lsp0, Eb=Eb,UrS=UrS,
         offset = offset,U1=U1,Mp=Mp,family = family,weights=weights,deriv=deriv,
         control=control,gamma=gamma,scale=scale,
         printWarn=FALSE,mustart=mustart,
         scoreType=scoreType,eps=eps,null.coef=null.coef,...)
    }
#    ii <- 0
#    if (ii>0) {
#    score.transect(ii,x=X, y=y, sp=L%*%lsp+lsp0, Eb=Eb,UrS=UrS,
#         offset = offset,U1=U1,Mp=Mp,family = family,weights=weights,deriv=deriv,
#         control=control,gamma=gamma,scale=scale,
#         printWarn=FALSE,mustart=mustart,
#         scoreType=scoreType,eps=eps,null.coef=null.coef,...)
#    }

    ## exclude apparently converged gradients from computation
    hess1 <- hess[uconv.ind,uconv.ind] 
    grad1 <- grad[uconv.ind]
    ## get the trial step ...
    eh <- eigen(hess1,symmetric=TRUE)
    d <- eh$values;U <- eh$vectors
    ind <- d < 0
    d[ind] <- -d[ind] ## see Gill Murray and Wright p107/8
    low.d <- max(d)*.Machine$double.eps^.7
    ind <- d < low.d
    d[ind] <- low.d 
    d <- 1/d
    
    Nstep <- 0 * grad
    Nstep[uconv.ind] <- -drop(U%*%(d*(t(U)%*%grad1))) # (modified) Newton direction
   
    Sstep <- -grad/max(abs(grad)) # steepest descent direction 
    
    ms <- max(abs(Nstep))
    if (ms>maxNstep) Nstep <- maxNstep * Nstep/ms

    ## try the step ...
    if (sp.trace) cat(lsp,"\n")

    if (!is.null(lsp.max)) { ## need to take step in delta space
      delta1 <- delta + Nstep
      lsp1 <- rt(delta1,lsp1.max)$rho ## transform to log sp space
      while (max(abs(lsp1-lsp))>maxNstep) { ## make sure step is not too long
        Nstep <- Nstep / 2 
        delta1 <- delta + Nstep
        lsp1 <- rt(delta1,lsp1.max)$rho
      } 
    } else lsp1 <- lsp + Nstep

    b<-gam.fit3(x=X, y=y, sp=L%*%lsp1+lsp0,Eb=Eb,UrS=UrS,
       offset = offset,U1=U1,Mp=Mp,family = family,weights=weights,deriv=2,
       control=control,gamma=gamma,scale=scale,printWarn=FALSE,
       mustart=mustart,scoreType=scoreType,null.coef=null.coef,
       pearson.extra=pearson.extra,dev.extra=dev.extra,n.true=n.true,...)
    
    if (reml) {
      score1 <- b$REML
    } else if (scoreType=="GACV") {
      score1 <- b$GACV
    } else if (scoreType=="UBRE") {
      score1 <- b$UBRE
    } else score1 <- b$GCV
    ## accept if improvement, else step halve
    ii <- 0 ## step halving counter
    ##sc.extra <- 1e-4*sum(grad*Nstep) ## -ve sufficient decrease 
    if (score1<score) { ## accept
      old.score <- score 
      mustart <- b$fitted.values
      lsp <- lsp1
      if (reml) {
          score <- b$REML;grad <- b$REML1;hess <- b$REML2 
      } else if (scoreType=="GACV") {
          score <- b$GACV;grad <- b$GACV1;hess <- b$GACV2
      } else if (scoreType=="UBRE") {
          score <- b$UBRE;grad <- b$UBRE1;hess <- b$UBRE2 
      } else { score <- b$GCV;grad <- b$GCV1;hess <- b$GCV2} 
      grad <- t(L)%*%grad
      hess <- t(L)%*%hess%*%L
      
      if (!is.null(lsp.max)) { ## need to transform to delta space
        delta <- delta1
        rho <- rt(delta,lsp1.max)
        nr <- length(rho$rho1)
        hess <- diag(rho$rho1,nr,nr)%*%hess%*%diag(rho$rho1,nr,nr) + diag(rho$rho2*grad)
        grad <- rho$rho1*grad
      }

    } else { ## step halving ...
      step <- Nstep ## start with the (pseudo) Newton direction
      ##sc.extra <- 1e-4*sum(grad*step) ## -ve sufficient decrease 
      while (score1>score && ii < maxHalf) {
        if (ii==3&&i<10) { ## Newton really not working - switch to SD, but keeping step length 
          s.length <- min(sum(step^2)^.5,maxSstep)
          step <- Sstep*s.length/sum(Sstep^2)^.5 ## use steepest descent direction
        } else step <- step/2
        ##if (ii>3) Slength <- Slength/2 ## keep track of SD step length
        if (!is.null(lsp.max)) { ## need to take step in delta space
          delta1 <- delta + step
          lsp1 <- rt(delta1,lsp1.max)$rho ## transform to log sp space
        } else lsp1 <- lsp + step
        b1<-gam.fit3(x=X, y=y, sp=L%*%lsp1+lsp0,Eb=Eb,UrS=UrS,
           offset = offset,U1=U1,Mp=Mp,family = family,weights=weights,deriv=0,
           control=control,gamma=gamma,scale=scale,
           printWarn=FALSE,mustart=mustart,scoreType=scoreType,
           null.coef=null.coef,pearson.extra=pearson.extra,
           dev.extra=dev.extra,n.true=n.true,...)
         
        if (reml) {       
          score1 <- b1$REML
        } else if (scoreType=="GACV") {
          score1 <- b1$GACV
        } else if (scoreType=="UBRE") {
          score1 <- b1$UBRE
        } else score1 <- b1$GCV
        ##sc.extra <- 1e-4*sum(grad*Nstep) ## -ve sufficient decrease 
        if (score1 <= score) { ## accept
          b<-gam.fit3(x=X, y=y, sp=L%*%lsp1+lsp0,Eb=Eb,UrS=UrS,
             offset = offset,U1=U1,Mp=Mp,family = family,weights=weights,deriv=2,
             control=control,gamma=gamma,scale=scale,printWarn=FALSE,
             mustart=mustart,scoreType=scoreType,null.coef=null.coef,
             pearson.extra=pearson.extra,dev.extra=dev.extra,n.true=n.true,...)
          mustart <- b$fitted.values
          old.score <- score;lsp <- lsp1
         
          if (reml) {
            score <- b$REML;grad <- b$REML1;hess <- b$REML2 
          } else if (scoreType=="GACV") {
            score <- b$GACV;grad <- b$GACV1;hess <- b$GACV2
          } else if (scoreType=="UBRE") {
            score <- b$UBRE;grad <- b$UBRE1;hess <- b$UBRE2 
          } else { score <- b$GCV;grad <- b$GCV1;hess <- b$GCV2}
          grad <- t(L)%*%grad
          hess <- t(L)%*%hess%*%L
          if (!is.null(lsp.max)) { ## need to transform to delta space
             delta <- delta1
             rho <- rt(delta,lsp1.max)
             nr <- length(rho$rho1)
             hess <- diag(rho$rho1,nr,nr)%*%hess%*%diag(rho$rho1,nr,nr) + diag(rho$rho2*grad)
             grad <- rho$rho1*grad
          }
          score1 <- score - abs(score) - 1 ## make damn sure that score1 < score
        }  # end of if (score1<= score )
        ii <- ii + 1
      } # end of step halving
    }
    ## record current score
    score.hist[i] <- score 
   
    ## test for convergence
    converged <- TRUE
    if (reml) score.scale <- abs(log(b$scale.est)) + abs(score) else
    score.scale <- abs(b$scale.est) + abs(score)
    grad2 <- diag(hess)    
    uconv.ind <- (abs(grad) > score.scale*conv.tol*.1)|(abs(grad2)>score.scale*conv.tol*.1)
    if (sum(abs(grad)>score.scale*conv.tol)) converged <- FALSE
    if (abs(old.score-score)>score.scale*conv.tol) { 
      if (converged) uconv.ind <- uconv.ind | TRUE ## otherwise can't progress
      converged <- FALSE      
    }
    if (ii==maxHalf) converged <- TRUE ## step failure
    if (converged) break
  } ## end of iteration loop
  if (ii==maxHalf) ct <- "step failed"
  else if (i==200) ct <- "iteration limit reached" 
  else ct <- "full convergence"
  list(score=score,lsp=lsp,lsp.full=L%*%lsp+lsp0,grad=grad,hess=hess,iter=i,conv =ct,score.hist = score.hist[!is.na(score.hist)],object=b)
} ## newton

bfgs0 <- function(lsp,X,y,Eb,UrS,L,lsp0,offset,U1,Mp,family,weights,
                   control,gamma,scale,conv.tol=1e-6,maxNstep=5,maxSstep=2,
                   maxHalf=30,printWarn=FALSE,scoreType="GCV",
                   mustart = NULL,null.coef=rep(0,ncol(X)),...)
## This optimizer is experimental... The main feature is to alternate infrequent 
## Newton steps with BFGS Quasi-Newton steps. In theory this should be faster 
## than Newton, because of the cost of full Hessian calculation, but
## in practice the extra steps required by QN tends to mean that the advantage
## is not realized...
## Newton optimizer for GAM gcv/aic optimization that can cope with an 
## indefinite Hessian, and alternates BFGS and Newton steps for speed reasons
## Main enhancements are: i) always peturbs the Hessian
## to +ve definite ii) step halves on step 
## failure, without obtaining derivatives until success; (iii) carries start
## values forward from one evaluation to next to speed convergence.    
## L is the matrix such that L%*%lsp + lsp0 gives the logs of the smoothing 
## parameters actually multiplying the S[[i]]'s
{ 
  reml <- scoreType%in%c("REML","P-REML","ML","P-ML") ## REML/ML indicator

  ## sanity check L
  if (is.null(L)) L <- diag(length(lsp)) else {
    if (!inherits(L,"matrix")) stop("L must be a matrix.")
    if (nrow(L)<ncol(L)) stop("L must have at least as many rows as columns.")
    if (nrow(L)!=length(lsp0)||ncol(L)!=length(lsp)) stop("L has inconsistent dimensions.")
  }
  if (is.null(lsp0)) lsp0 <- rep(0,nrow(L))
  ## initial fit

  b<-gam.fit3(x=X, y=y, sp=L%*%lsp+lsp0,Eb=Eb,UrS=UrS,
     offset = offset,U1=U1,Mp=Mp,family = family,weights=weights,deriv=2,
     control=control,gamma=gamma,scale=scale,
     printWarn=FALSE,mustart=mustart,scoreType=scoreType,null.coef=null.coef,...)

  mustart<-b$fitted.values

  QNsteps <- floor(length(UrS)/2) ## how often to Newton should depend on cost...

  if (reml) {
     score <- b$REML;grad <- b$REML1;hess <- b$REML2 
  } else if (scoreType=="GACV") {
    old.score <- score <- b$GACV;grad <- b$GACV1;hess <- b$GACV2 
  } else if (scoreType=="UBRE"){
    old.score <- score <- b$UBRE;grad <- b$UBRE1;hess <- b$UBRE2 
  } else { ## default to deviance based GCV
    old.score <- score <- b$GCV;grad <- b$GCV1;hess <- b$GCV2
  }
  
  grad <- t(L)%*%grad
  hess <- t(L)%*%hess%*%L

  if (reml)  score.scale <- abs(log(b$scale.est)) + abs(score)  
  else score.scale <- b$scale.est + abs(score)    
  uconv.ind <- abs(grad) > score.scale*conv.tol
  ## check for all converged too soon, and undo !
  if (!sum(uconv.ind)) uconv.ind <- uconv.ind | TRUE
  kk <- 0 ## counter for QN steps between Newton steps
  score.hist <- rep(NA,200)
  for (i in 1:200) {
   
    if (kk==0) { ## time to reset B
      eh <- eigen(hess,symmetric=TRUE)
      d <- eh$values;U <- eh$vectors
      ind <- d < 0
      d[ind] <- -d[ind] ## see Gill Murray and Wright p107/8
      d <- 1/d
      d[d==0] <- min(d)*.Machine$double.eps^.5
      B <- U%*%(d*t(U)) ## Newton based inverse Hessian
    }
     
    kk <- kk + 1
    if (kk > QNsteps) kk <- 0 
 
    ## get the trial step ...
    
    Nstep <- 0 * grad
    Nstep[uconv.ind] <- -drop(B[uconv.ind,uconv.ind]%*%grad[uconv.ind]) # (modified) Newton direction
    
    ms <- max(abs(Nstep))
    if (ms>maxNstep) Nstep <- maxNstep * Nstep/ms

    ## try the step ...
    sc.extra <- 1e-4*sum(grad*Nstep) ## -ve sufficient decrease 
    ii <- 0 ## step halving counter
    step <- Nstep*2
    score1 <- abs(score)*2
    while (score1>score+sc.extra && ii < maxHalf) { ## reject and step halve
      ii <- ii + 1
      step <- step/2
      sc.extra <- sc.extra/2
      lsp1 <- lsp + step
  

      if (kk!=0||ii==1) deriv <- 1 else deriv <- 0
      b1<-gam.fit3(x=X, y=y, sp=L%*%lsp1+lsp0,Eb=Eb,UrS=UrS,
          offset = offset,U1=U1,Mp=Mp,family = family,weights=weights,deriv=deriv,
          control=control,gamma=gamma,scale=scale,
          printWarn=FALSE,mustart=mustart,scoreType=scoreType,null.coef=null.coef,...)

      if (reml) {
          score1 <- b1$REML
      } else if (scoreType=="GACV") {
          score1 <- b1$GACV
      } else if (scoreType=="UBRE") {
          score1 <- b1$UBRE
      } else score1 <- b1$GCV
    } ## accepted step or step failed to lead to decrease

    if (ii < maxHalf) { ## step succeeded 
      mustart <- b1$fitted.values
      if (kk==0) { ## time for a full Newton step ...

        b<-gam.fit3(x=X, y=y, sp=L%*%lsp1+lsp0,Eb=Eb,UrS=UrS,
               offset = offset,U1=U1,Mp=Mp,family = family,weights=weights,deriv=2,
               control=control,gamma=gamma,scale=scale,
               printWarn=FALSE,mustart=mustart,scoreType=scoreType,null.coef=null.coef,...)

        mustart <- b$fitted.values
        old.score <- score;lsp <- lsp1
        if (reml) {
           score <- b$REML;grad <- b$REML1;hess <- b$REML2 
        } else if (scoreType=="GACV") {
          score <- b$GACV;grad <- b$GACV1;hess <- b$GACV2
        } else if (scoreType=="UBRE") {
          score <- b$UBRE;grad <- b$UBRE1;hess <- b$UBRE2 
        } else { score <- b$GCV;grad <- b$GCV1;hess <- b$GCV2}
        grad <- t(L)%*%grad
        hess <- t(L)%*%hess%*%L
      } else { ## just a BFGS update
        ## first derivatives only.... 

         if (ii==1) b <- b1 else  
         b<-gam.fit3(x=X, y=y, sp=L%*%lsp1+lsp0,Eb=Eb,UrS=UrS,
               offset = offset,U1=U1,Mp=Mp,family = family,weights=weights,deriv=1,
               control=control,gamma=gamma,scale=scale,
               printWarn=FALSE,mustart=mustart,scoreType=scoreType,null.coef=null.coef,...)

        mustart <- b$fitted.values
        old.score <- score;lsp <- lsp1
        old.grad <- grad
        if (reml) {
          score <- b$REML;grad <- b$REML1 
        } else if (scoreType=="GACV") {
          score <- b$GACV;grad <- b$GACV1
        } else if (scoreType=="UBRE") {
          score <- b$UBRE;grad <- b$UBRE1
        } else { score <- b$GCV;grad <- b$GCV1}
        grad <- t(L)%*%grad
        ## BFGS update of the inverse Hessian...
        yg <- grad-old.grad
        rho <- 1/sum(yg*step)
        B <- B - rho*step%*%(t(yg)%*%B)
        B <- B - rho*(B%*%yg)%*%t(step) + rho*step%*%t(step)
      } ## end of BFGS
    } ## end of successful step updating
    ## record current score
    score.hist[i] <- score

    ## test for convergence
    converged <- TRUE
    if (reml) score.scale <- abs(log(b$scale.est)) + abs(score)
    else score.scale <- b$scale.est + abs(score);    
    uconv.ind <- abs(grad) > score.scale*conv.tol
    if (sum(uconv.ind)) converged <- FALSE
    if (abs(old.score-score)>score.scale*conv.tol) { 
      if (converged) uconv.ind <- uconv.ind | TRUE ## otherwise can't progress
      converged <- FALSE      
    }
    if (ii==maxHalf) converged <- TRUE ## step failure
    if (converged) break
  } ## end of iteration loop
  if (ii==maxHalf) ct <- "step failed"
  else if (i==200) ct <- "iteration limit reached" 
  else ct <- "full convergence"
  list(score=score,lsp=lsp,lsp.full=L%*%lsp,grad=grad,hess=hess,iter=i,conv =ct,score.hist=score.hist[!is.na(score.hist)],object=b)
} ## end bfgs0




bfgs <-  function(lsp,X,y,Eb,UrS,L,lsp0,offset,U1,Mp,family,weights,
                   control,gamma,scale,conv.tol=1e-6,maxNstep=5,maxSstep=2,
                   maxHalf=30,printWarn=FALSE,scoreType="GCV",
                   mustart = NULL,null.coef=rep(0,ncol(X)),pearson.extra=0,dev.extra=0,n.true=-1,...)

## BFGS optimizer to estimate smoothing parameters of models fitted by
## gam.fit3....
##
## L is the matrix such that L%*%lsp + lsp0 gives the logs of the smoothing 
## parameters actually multiplying the S[[i]]'s. sp's do not include the 
## log scale parameter here.
##
## BFGS is based on Nocedal & Wright (2006) Numerical Optimization, Springer.
## In particular the step lengths are chosen to meet the Wolfe conditions
## using their algorithms 3.5 and 3.6. On page 143 they recommend a post step
## adjustment to the initial Hessian. I can't understand why one would do anything
## other than adjust so that the initial Hessian would give the step taken, and
## indeed the latter adjustment seems to give faster convergence than their 
## proposal, and is therefore implemented.
##
{ zoom <- function(lo,hi) {
  ## local function implementing Algorithm 3.6 of Nocedal & Wright
  ## (2006) Numerical Optimization. Relies on R scoping rules. 
  ## alpha.lo and alpha.hi are the bracketing step lengths.
  ## This routine bisection searches for a step length that meets the
  ## Wolfe conditions. lo and hi are both objects containing fields
  ## `score', `alpha', `dscore', where `dscore' is the derivative of 
  ## the score in the current step direction, `grad' and `mustart'. 
  ## `dscore' will be NULL if the gradiant has yet to be evaluated.
    for (i in 1:40) {
      trial <- list(alpha = (lo$alpha+hi$alpha)/2)
      lsp <- ilsp + step * trial$alpha
      b <- gam.fit3(x=X, y=y, sp=L%*%lsp+lsp0,Eb=Eb,UrS=UrS,
           offset = offset,U1=U1,Mp=Mp,family = family,weights=weights,deriv=0,
           control=control,gamma=gamma,scale=scale,printWarn=FALSE,
           mustart=lo$mustart,scoreType=scoreType,null.coef=null.coef,
           pearson.extra=pearson.extra,dev.extra=dev.extra,n.true=n.true,...)

      trial$mustart <- fitted(b);trial$dev <- b$dev
      if (reml) {
        trial$score <- b$REML; 
      } else if (scoreType=="GACV") {
        trial$score <- b$GACV; 
      } else if (scoreType=="UBRE"){
        trial$score <- b$UBRE; 
      } else { ## default to deviance based GCV
        trial$score <- b$GCV;
      } 
      rm(b)  
      if (trial$score>initial$score+trial$alpha*c1*initial$dscore||trial$score>=lo$score) {
        hi <- trial ## failed Wolfe 1
      } else { ## met Wolfe 1

        b <- gam.fit3(x=X, y=y, sp=L%*%lsp+lsp0,Eb=Eb,UrS=UrS,
           offset = offset,U1=U1,Mp=Mp,family = family,weights=weights,deriv=1,
           control=control,gamma=gamma,scale=scale,printWarn=FALSE,mustart=trial$mustart,
           scoreType=scoreType,null.coef=null.coef,pearson.extra=pearson.extra,dev.extra=dev.extra,n.true=n.true,...)

        if (reml) {
          trial$grad <- L%*%b$REML1;
        } else if (scoreType=="GACV") {
          trial$grad <- L%*%b$GACV1; 
        } else if (scoreType=="UBRE"){
          trial$grad <- L%*%b$UBRE1  
        } else { ## default to deviance based GCV
          trial$grad <- L%*%b$GCV1;
        } 
        trial$dev <- b$dev;rm(b);
        trial$dscore <- sum(step*trial$grad) ## directional derivative
        
        if (abs(trial$dscore) <= -c2*initial$dscore) return(trial) ## met Wolfe 2

        ## failed Wolfe 2 ...
        if (trial$dscore*(hi$alpha-lo$alpha)>=0) {
          hi <- lo }  
        lo <- trial 
      }  
    } ## end while(TRUE)
    return(NULL) ## failed
  } ## end zoom

  reml <- scoreType%in%c("REML","P-REML","ML","P-ML") ## REML/ML indicator

  ## sanity check L
  if (is.null(L)) L <- diag(length(lsp)) else {
    if (!inherits(L,"matrix")) stop("L must be a matrix.")
    if (nrow(L)<ncol(L)) stop("L must have at least as many rows as columns.")
    if (nrow(L)!=length(lsp0)||ncol(L)!=length(lsp)) stop("L has inconsistent dimensions.")
  }
  if (is.null(lsp0)) lsp0 <- rep(0,nrow(L))
 
  ## initial fit...

  ilsp <- lsp

  b <- gam.fit3(x=X, y=y, sp=L%*%ilsp+lsp0,Eb=Eb,UrS=UrS,
               offset = offset,U1=U1,Mp=Mp,family = family,weights=weights,deriv=1,
               control=control,gamma=gamma,scale=scale,printWarn=FALSE,mustart=mustart,
               scoreType=scoreType,null.coef=null.coef,
               pearson.extra=pearson.extra,dev.extra=dev.extra,n.true=n.true,...)

  initial <- list(alpha = 0,mustart=b$fitted.values)
  if (reml) {
     score <- b$REML;grad <- L%*%b$REML1;
  } else if (scoreType=="GACV") {
     score <- b$GACV;grad <- L%*%b$GACV1; 
  } else if (scoreType=="UBRE"){
     score <- b$UBRE;grad <- L%*%b$UBRE1  
  } else { ## default to deviance based GCV
     score <- b$GCV;grad <- L%*%b$GCV1;
  } 
  initial$score <- score;initial$grad <- grad;

  rm(b)

  B <- diag(length(initial$grad)) ## initial Hessian

  max.step <- 200

  c1 <- 1e-4;c2 <- .9 ## Wolfe condition constants

  score.hist <- rep(NA,max.step+1)
  score.hist[1] <- initial$score

  for (i in 1:max.step) {
   
    ## get the trial step ...

    step <- -drop(B%*%initial$grad)    
 
    ms <- max(abs(step))
    if (ms>maxNstep) { 
      step <- maxNstep * step/ms
      alpha.max <- 2
    } else alpha.max <- 2*maxNstep/ms

    initial$dscore <- sum(step*initial$grad)
    prev <- initial

    trial <- list(alpha=1)
    deriv <- 1 ## only get derivatives immediately for initial step length   
    while(TRUE) {
      lsp <- ilsp + trial$alpha*step
      b <- gam.fit3(x=X, y=y, sp=L%*%lsp+lsp0,Eb=Eb,UrS=UrS,
                    offset = offset,U1=U1,Mp=Mp,family = family,weights=weights,deriv=deriv,
                    control=control,gamma=gamma,scale=scale,printWarn=FALSE,mustart=prev$mustart,
                    scoreType=scoreType,null.coef=null.coef,
                    pearson.extra=pearson.extra,dev.extra=dev.extra,n.true=n.true,...)
      if (reml) {
        trial$score <- b$REML; 
      } else if (scoreType=="GACV") {
        trial$score <- b$GACV; 
      } else if (scoreType=="UBRE"){
        trial$score <- b$UBRE; 
      } else { ## default to deviance based GCV
        trial$score <- b$GCV;
      }  

      if (deriv>0) {
        if (reml) {
          trial$grad <- L%*%b$REML1;
        } else if (scoreType=="GACV") {
          trial$grad <- L%*%b$GACV1; 
        } else if (scoreType=="UBRE"){
          trial$grad <- L%*%b$UBRE1  
        } else { ## default to deviance based GCV
          trial$grad <- L%*%b$GCV1;
        } 
        trial$dscore <- sum(trial$grad*step)
        deriv <- 0 
      } else trial$grad <- trial$dscore <- NULL
      trial$mustart <- b$fitted.values
      trial$dev <- b$dev
      
      rm(b)

      if (trial$score>initial$score+c1*trial$alpha*initial$dscore||(deriv==0&&trial$score>=prev$score)) {
         trial <- zoom(prev,trial)
         break
      } 

      if (is.null(trial$dscore)) { ## getting gradients
        b <- gam.fit3(x=X, y=y, sp=L%*%lsp+lsp0,Eb=Eb,UrS=UrS,
                      offset = offset,U1=U1,Mp=Mp,family = family,weights=weights,deriv=1,
                      control=control,gamma=gamma,scale=scale,printWarn=FALSE,mustart=trial$mustart,
                      scoreType=scoreType,null.coef=null.coef,pearson.extra=pearson.extra,
                      dev.extra=dev.extra,n.true=n.true,...)
        if (reml) {
          trial$grad <- L%*%b$REML1;
        } else if (scoreType=="GACV") {
          trial$grad <- L%*%b$GACV1; 
        } else if (scoreType=="UBRE"){
          trial$grad <- L%*%b$UBRE1  
        } else { ## default to deviance based GCV
          trial$grad <- L%*%b$GCV1;
        } 
        trial$dscore <- sum(trial$grad*step)
        trial$dev <- b$dev
        rm(b)
      }

      if (abs(trial$dscore) <= -c2*initial$dscore) break; ## `trial' is ok.
      
      if (trial$dscore>=0) {
        trial <- zoom(trial,prev)
        break
      }
      
      prev <- trial
      if (trial$alpha == alpha.max) { trial <- NULL;break;} ## step failed
      trial <- list(alpha = min(prev$alpha + 1, alpha.max))
    } ## end of while(TRUE)

    ## Now `trial' contains a suitable step, or is NULL on failure to meet Wolfe.  
    if (is.null(trial)) { ## step failed
      lsp <- ilsp
      break ## failed to move, so nothing more can be done. 
    } else { ## update the Hessian etc...
      
      yg <- trial$grad-initial$grad
      step <- step*trial$alpha
      if (i==1) { ## initial step --- adjust Hessian as p143 of N&W
        B <- B*trial$alpha ##  sum(yg*step)/sum(yg*yg)
      }
      rho <- 1/sum(yg*step)
      B <- B - rho*step%*%(t(yg)%*%B)
      B <- B - rho*(B%*%yg)%*%t(step) + rho*step%*%t(step)

      score.hist[i+1] <- trial$score

      lsp <- ilsp <- ilsp + step 

      ## test for convergence
      converged <- TRUE
      if (reml) score.scale <- abs(log(trial$dev/nrow(X))) + abs(trial$score)
      else score.scale <- trial$dev/nrow(X) + abs(trial$score)    
      uconv.ind <- abs(trial$grad) > score.scale*conv.tol
      if (sum(uconv.ind)) converged <- FALSE
      if (abs(initial$score-trial$score)>score.scale*conv.tol) { 
        if (converged) uconv.ind <- uconv.ind | TRUE ## otherwise can't progress
        converged <- FALSE      
      }
      if (converged) break

      initial <- trial
      initial$alpha <- 0
    }  
  } ## end of iteration loop


  if (is.null(trial)) { 
    ct <- "step failed"
    lsp <- ilsp
    trial <- initial
  }
  else if (i==max.step) ct <- "iteration limit reached" 
  else ct <- "full convergence"
  ## final fit
  b <- gam.fit3(x=X, y=y, sp=L%*%lsp+lsp0,Eb=Eb,UrS=UrS,
                offset = offset,U1=U1,Mp=Mp,family = family,weights=weights,deriv=1,
                control=control,gamma=gamma,scale=scale,printWarn=FALSE,mustart=trial$mustart,
                scoreType=scoreType,null.coef=null.coef,pearson.extra=pearson.extra,dev.extra=dev.extra,n.true=n.true,...)
  if (reml) {
     score <- b$REML;grad <- L%*%b$REML1;
  } else if (scoreType=="GACV") {
     score <- b$GACV;grad <- L%*%b$GACV1; 
  } else if (scoreType=="UBRE"){
     score <- b$UBRE;grad <- L%*%b$UBRE1  
  } else { ## default to deviance based GCV
     score <- b$GCV;grad <- L%*%b$GCV1;
  } 

  ## get approximate Hessian...
  ev <- eigen(B,symmetric=TRUE)
  ind <- ev$values>max(ev$values)*.Machine$double.eps^.9
  ev$values[ind] <- 1/ev$values[ind]
  ev$values[!ind] <- 0
  B <- ev$vectors %*% (ev$values*t(ev$vectors))

  list(score=score,lsp=lsp,lsp.full=L%*%lsp+lsp0,grad=grad,hess=B,iter=i,conv =ct,
       score.hist=score.hist[!is.na(score.hist)],object=b)
} ## end of bfgs



gam2derivative <- function(lsp,args,...)
## Performs IRLS GAM fitting for smoothing parameters given in lsp 
## and returns the derivatives of the GCV or UBRE score w.r.t the 
## smoothing parameters for the model.
## args is a list containing the arguments for gam.fit3
## For use as optim() objective gradient
{ reml <- args$scoreType%in%c("REML","P-REML","ML","P-ML") ## REML/ML indicator
  if (!is.null(args$L)) {
    lsp <- args$L%*%lsp + args$lsp0
  }
  b<-gam.fit3(x=args$X, y=args$y, sp=lsp,Eb=args$Eb,UrS=args$UrS,
     offset = args$offset,U1=args$U1,Mp=args$Mp,family = args$family,weights=args$w,deriv=1,
     control=args$control,gamma=args$gamma,scale=args$scale,scoreType=args$scoreType,
     null.coef=args$null.coef,...)
  if (reml) {
          ret <- b$REML1 
  } else if (args$scoreType=="GACV") {
          ret <- b$GACV1
  } else if (args$scoreType=="UBRE") {
          ret <- b$UBRE1
  } else { ret <- b$GCV1}
  if (!is.null(args$L)) ret <- t(args$L)%*%ret
  ret
}

gam2objective <- function(lsp,args,...)
## Performs IRLS GAM fitting for smoothing parameters given in lsp 
## and returns the GCV or UBRE score for the model.
## args is a list containing the arguments for gam.fit3
## For use as optim() objective
{ reml <- args$scoreType%in%c("REML","P-REML","ML","P-ML") ## REML/ML indicator
  if (!is.null(args$L)) {
    lsp <- args$L%*%lsp + args$lsp0
  }
  b<-gam.fit3(x=args$X, y=args$y, sp=lsp,Eb=args$Eb,UrS=args$UrS,
     offset = args$offset,U1=args$U1,Mp=args$Mp,family = args$family,weights=args$w,deriv=0,
     control=args$control,gamma=args$gamma,scale=args$scale,scoreType=args$scoreType,
     null.coef=args$null.coef,...)
  if (reml) {
          ret <- b$REML 
  } else if (args$scoreType=="GACV") {
          ret <- b$GACV
  } else if (args$scoreType=="UBRE") {
          ret <- b$UBRE
  } else { ret <- b$GCV}
  attr(ret,"full.fit") <- b
  ret
}



gam4objective <- function(lsp,args,...)
## Performs IRLS GAM fitting for smoothing parameters given in lsp 
## and returns the GCV or UBRE score for the model.
## args is a list containing the arguments for gam.fit3
## For use as nlm() objective
{ reml <- args$scoreType%in%c("REML","P-REML","ML","P-ML") ## REML/ML indicator
  if (!is.null(args$L)) {
    lsp <- args$L%*%lsp + args$lsp0
  }
  b<-gam.fit3(x=args$X, y=args$y, sp=lsp, Eb=args$Eb,UrS=args$UrS,
     offset = args$offset,U1=args$U1,Mp=args$Mp,family = args$family,weights=args$w,deriv=1,
     control=args$control,gamma=args$gamma,scale=args$scale,scoreType=args$scoreType,
     null.coef=args$null.coef,...)
  
  if (reml) {
          ret <- b$REML;at <- b$REML1
  } else if (args$scoreType=="GACV") {
          ret <- b$GACV;at <- b$GACV1
  } else if (args$scoreType=="UBRE") {
          ret <- b$UBRE;at <- b$UBRE1
  } else { ret <- b$GCV;at <- b$GCV1}  

  attr(ret,"full.fit") <- b

  if (!is.null(args$L)) at <- t(args$L)%*%at

  attr(ret,"gradient") <- at
  ret
}

##
## The following fix up family objects for use with gam.fit3
##


fix.family.link<-function(fam)
# adds d2link the second derivative of the link function w.r.t. mu
# to the family supplied, as well as a 3rd derivative function 
# d3link...
# All d2link and d3link functions have been checked numerically. 
{ if (!inherits(fam,"family")) stop("fam not a family object")
  if (is.null(fam$canonical)) { ## note the canonical link - saves effort in full Newton
    if (fam$family=="gaussian") fam$canonical <- "identity" else
    if (fam$family=="poisson"||fam$family=="quasipoisson") fam$canonical <- "log" else
    if (fam$family=="binomial"||fam$family=="quasibinomial") fam$canonical <- "logit" else
    if (fam$family=="Gamma") fam$canonical <- "inverse" else
    if (fam$family=="inverse.gaussian") fam$canonical <- "1/mu^2" else
    fam$canonical <- "none"
  }
  if (!is.null(fam$d2link)&&!is.null(fam$d3link)&&!is.null(fam$d4link)) return(fam) 
  link <- fam$link
  if (length(link)>1) if (fam$family=="quasi") # then it's a power link
  { lambda <- log(fam$linkfun(exp(1))) ## the power, if > 0
    if (lambda<=0) { fam$d2link <- function(mu) -1/mu^2
      fam$d3link <- function(mu) 2/mu^3
      fam$d4link <- function(mu) -6/mu^4
    }
    else { fam$d2link <- function(mu) lambda*(lambda-1)*mu^(lambda-2)
      fam$d3link <- function(mu) (lambda-2)*(lambda-1)*lambda*mu^(lambda-3)
      fam$d4link <- function(mu) (lambda-3)*(lambda-2)*(lambda-1)*lambda*mu^(lambda-4)
    }
    return(fam)
  } else stop("unrecognized (vector?) link")

  if (link=="identity") {
    fam$d4link <- fam$d3link <- fam$d2link <- 
    function(mu) rep.int(0,length(mu))
    return(fam)
  } 
  if (link == "log") {
    fam$d2link <- function(mu) -1/mu^2
    fam$d3link <- function(mu) 2/mu^3
    fam$d4link <- function(mu) -6/mu^4
    return(fam)
  }
  if (link == "inverse") {
    fam$d2link <- function(mu) 2/mu^3
    fam$d3link <- function(mu) { mu <- mu*mu;-6/(mu*mu)}
    fam$d4link <- function(mu) { mu2 <- mu*mu;24/(mu2*mu2*mu)}
    return(fam)
  }
  if (link == "logit") {
    fam$d2link <- function(mu) 1/(1 - mu)^2 - 1/mu^2
    fam$d3link <- function(mu) 2/(1 - mu)^3 + 2/mu^3
    fam$d4link <- function(mu) 6/(1-mu)^4 - 6/mu^4
    return(fam)
  }
  if (link == "probit") {
    fam$d2link <- function(mu) { 
      eta <- fam$linkfun(mu)
      eta/fam$mu.eta(eta)^2
    }
    fam$d3link <- function(mu) {
      eta <-  fam$linkfun(mu)
      (1 + 2*eta^2)/fam$mu.eta(eta)^3
    }
    fam$d4link <- function(mu) {
       eta <-  fam$linkfun(mu)
       (7*eta + 6*eta^3)/fam$mu.eta(eta)^4
    }
    return(fam)
  }
  if (link == "cloglog") {
    fam$d2link <- function(mu) { l1m <- log(1-mu)
      -1/((1 - mu)^2*l1m) *(1+ 1/l1m)
    }
    fam$d3link <- function(mu) { l1m <- log(1-mu)
       mu3 <- (1-mu)^3
      (-2 - 3*l1m - 2*l1m^2)/mu3/l1m^3
    }
    fam$d4link <- function(mu){
      l1m <- log(1-mu)
      mu4 <- (1-mu)^4
      ( - 12 - 11 * l1m - 6 * l1m^2 - 6/l1m )/mu4  /l1m^3
    }
    return(fam)
  }
  if (link == "sqrt") {
    fam$d2link <- function(mu) -.25 * mu^-1.5
    fam$d3link <- function(mu) .375 * mu^-2.5
    fam$d4link <- function(mu) -0.9375 * mu^-3.5
    return(fam)
  }
  if (link == "cauchit") {
    fam$d2link <- function(mu) { 
     eta <- fam$linkfun(mu)
     2*pi*pi*eta*(1+eta*eta)
    }
    fam$d3link <- function(mu) { 
     eta <- fam$linkfun(mu)
     eta2 <- eta*eta
     2*pi*pi*pi*(1+3*eta2)*(1+eta2)
    }
    fam$d4link <- function(mu) { 
     eta <- fam$linkfun(mu)
     eta2 <- eta*eta
     2*pi^4*(8*eta+12*eta2*eta)*(1+eta2)
    }
    return(fam)
  }
  if (link == "1/mu^2") {
    fam$d2link <- function(mu) 6 * mu^-4
    fam$d3link <- function(mu) -24 * mu^-5
    fam$d4link <- function(mu) 120 * mu^-6
    return(fam)
  }
  if (substr(link,1,3)=="mu^") { ## it's a power link
    ## note that lambda <=0 gives log link so don't end up here
    lambda <- get("lambda",environment(fam$linkfun))
    fam$d2link <- function(mu) (lambda*(lambda-1)) * mu^{lambda-2}
    fam$d3link <- function(mu) (lambda*(lambda-1)*(lambda-2)) * mu^{lambda-3}
    fam$d4link <- function(mu) (lambda*(lambda-1)*(lambda-2)*(lambda-3)) * mu^{lambda-4}
    return(fam)
  }
  stop("link not recognised")
}


fix.family.var<-function(fam)
# adds dvar the derivative of the variance function w.r.t. mu
# to the family supplied, as well as d2var the 2nd derivative of 
# the variance function w.r.t. the mean. (All checked numerically). 
{ if (!inherits(fam,"family")) stop("fam not a family object")
  if (!is.null(fam$dvar)&&!is.null(fam$d2var)&&!is.null(fam$d3var)) return(fam) 
  family <- fam$family
  if (family=="gaussian") {
    fam$d3var <- fam$d2var <- fam$dvar <- function(mu) rep.int(0,length(mu))
    return(fam)
  } 
  if (family=="poisson"||family=="quasipoisson") {
    fam$dvar <- function(mu) rep.int(1,length(mu))
    fam$d3var <- fam$d2var <- function(mu) rep.int(0,length(mu))
    return(fam)
  } 
  if (family=="binomial"||family=="quasibinomial") {
    fam$dvar <- function(mu) 1-2*mu
    fam$d2var <- function(mu) rep.int(-2,length(mu))
    fam$d3var <- function(mu) rep.int(0,length(mu))
    return(fam)
  }
  if (family=="Gamma") {
    fam$dvar <- function(mu) 2*mu
    fam$d2var <- function(mu) rep.int(2,length(mu))
    fam$d3var <- function(mu) rep.int(0,length(mu))
    return(fam)
  }
  if (family=="quasi") {
    fam$dvar <- switch(fam$varfun,
       constant = function(mu) rep.int(0,length(mu)),
       "mu(1-mu)" = function(mu) 1-2*mu,
       mu = function(mu) rep.int(1,length(mu)),
       "mu^2" = function(mu) 2*mu,
       "mu^3" = function(mu) 3*mu^2           
    )
    if (is.null(fam$dvar)) stop("variance function not recognized for quasi")
    fam$d2var <- switch(fam$varfun,
       constant = function(mu) rep.int(0,length(mu)),
       "mu(1-mu)" = function(mu) rep.int(-2,length(mu)),
       mu = function(mu) rep.int(0,length(mu)),
       "mu^2" = function(mu) rep.int(2,length(mu)),
       "mu^3" = function(mu) 6*mu           
    )
    fam$d3var <- switch(fam$varfun,
       constant = function(mu) rep.int(0,length(mu)),
       "mu(1-mu)" = function(mu) rep.int(0,length(mu)),
       mu = function(mu) rep.int(0,length(mu)),
       "mu^2" = function(mu) rep.int(0,length(mu)),
       "mu^3" = function(mu) rep.int(6,length(mu))           
    )
    return(fam)
  }
  if (family=="inverse.gaussian") {
    fam$dvar <- function(mu) 3*mu^2
    fam$d2var <- function(mu) 6*mu
    fam$d3var <- function(mu) rep.int(6,length(mu)) 
    return(fam)
  }
  stop("family not recognised")
}


fix.family.ls<-function(fam)
# adds ls the log saturated likelihood and its derivatives
# w.r.t. the scale parameter to the family object.
{ if (!inherits(fam,"family")) stop("fam not a family object")
  if (!is.null(fam$ls)) return(fam) 
  family <- fam$family
  if (family=="gaussian") {
    fam$ls <- function(y,w,n,scale) { 
      nobs <- sum(w>0)
      c(-nobs*log(2*pi*scale)/2 + sum(log(w[w>0]))/2,-nobs/(2*scale),nobs/(2*scale*scale))
    }
    return(fam)
  } 
  if (family=="poisson") {
    fam$ls <- function(y,w,n,scale) {
      res <- rep(0,3)
      res[1] <- sum(dpois(y,y,log=TRUE)*w)
      res
    }
    return(fam)
  } 
  if (family=="binomial") {
    fam$ls <- function(y,w,n,scale) { 
      c(-binomial()$aic(y,n,y,w,0)/2,0,0)
    }
    return(fam)
  }
  if (family=="Gamma") {
    fam$ls <- function(y,w,n,scale) {
      res <- rep(0,3)
      y <- y[w>0];w <- w[w>0]
      scale <- scale/w
      k <- -lgamma(1/scale) - log(scale)/scale - 1/scale 
      res[1] <- sum(k-log(y))
      k <- (digamma(1/scale)+log(scale))/(scale*scale)
      res[2] <- sum(k/w)
      k <- (-trigamma(1/scale)/(scale) + (1-2*log(scale)-2*digamma(1/scale)))/(scale^3)
      res[3] <- sum(k/w^2) 
    #  k <- -lgamma(1/scale) - log(scale)/scale - 1/scale
    #  res[1] <- sum(w*(k-log(y)))
    #  k <- (digamma(1/scale)+log(scale))/(scale*scale)
    #  res[2] <- sum(w*k)  
    #  k <- (-trigamma(1/scale)/(scale) + (1-2*log(scale)-2*digamma(1/scale)))/(scale^3)
    #  res[3] <- sum(w*k) 
      res
    }
    return(fam)
  }
  if (family=="quasi"||family=="quasipoisson"||family=="quasibinomial") {
    ## fam$ls <- function(y,w,n,scale) rep(0,3)
    ## Uses extended quasi-likelihood form...
    fam$ls <- function(y,w,n,scale) { 
      nobs <- sum(w>0)
      c(-nobs*log(scale)/2 + sum(log(w[w>0]))/2,-nobs/(2*scale),nobs/(2*scale*scale))
    }
    return(fam)
  }
  if (family=="inverse.gaussian") {
    fam$ls <- function(y,w,n,scale) {
      nobs <- sum(w>0)
      c(-sum(log(2*pi*scale*y^3))/2 + sum(log(w[w>0]))/2,-nobs/(2*scale),nobs/(2*scale*scale))
      ## c(-sum(w*log(2*pi*scale*y^3))/2,-sum(w)/(2*scale),sum(w)/(2*scale*scale))
    }
    return(fam)
  }
  stop("family not recognised")
}

fix.family <- function(fam) {
## allows families to be patched...
   if (fam$family[1]=="gaussian") { ## sensible starting values given link...
     fam$initialize <- expression({
     n <- rep.int(1, nobs)
     if (family$link == "inverse") mustart <- y + (y==0)*sd(y)*.01 else
     if (family$link == "log") mustart <- pmax(y,.01*sd(y)) else
     mustart <- y
     })
  }
  fam
}


negbin <- function (theta = stop("'theta' must be specified"), link = "log") { 
## modified from Venables and Ripley's MASS library to work with gam.fit3,
## and to allow a range of `theta' values to be specified
## single `theta' to specify fixed value; 2 theta values (first smaller than second)
## are limits within which to search for theta; otherwise supplied values make up 
## search set.
## Note: to avoid warnings, get(".Theta")[1] is used below. Otherwise the initialization
##       call to negbin can generate warnings since get(".Theta") returns a vector
##       during initialization (only).
  linktemp <- substitute(link)
  if (!is.character(linktemp)) linktemp <- deparse(linktemp)
  if (linktemp %in% c("log", "identity", "sqrt")) stats <- make.link(linktemp)
  else if (is.character(link)) {
    stats <- make.link(link)
    linktemp <- link
  } else {
    if (inherits(link, "link-glm")) {
       stats <- link
            if (!is.null(stats$name))
                linktemp <- stats$name
        }
        else stop(linktemp, " link not available for negative binomial family; available links are \"identity\", \"log\" and \"sqrt\"")
    }
    env <- new.env(parent = .GlobalEnv)
    assign(".Theta", theta, envir = env)
    variance <- function(mu) mu + mu^2/get(".Theta")[1]
    ## dvaraince/dmu needed as well
    dvar <- function(mu) 1 + 2*mu/get(".Theta")[1]
    ## d2variance/dmu...
    d2var <- function(mu) rep(2/get(".Theta")[1],length(mu))
    d3var <- function(mu) rep(0,length(mu))
    getTheta <- function() get(".Theta")
    validmu <- function(mu) all(mu > 0)

    dev.resids <- function(y, mu, wt) { Theta <- get(".Theta")[1]
      2 * wt * (y * log(pmax(1, y)/mu) - 
        (y + Theta) * log((y + Theta)/(mu + Theta))) 
    }
    aic <- function(y, n, mu, wt, dev) {
        Theta <- get(".Theta")[1]
        term <- (y + Theta) * log(mu + Theta) - y * log(mu) +
            lgamma(y + 1) - Theta * log(Theta) + lgamma(Theta) -
            lgamma(Theta + y)
        2 * sum(term * wt)
    }
    ls <- function(y,w,n,scale) {
       Theta <- get(".Theta")[1]
       ylogy <- y;ind <- y>0;ylogy[ind] <- y[ind]*log(y[ind])
       term <- (y + Theta) * log(y + Theta) - ylogy +
            lgamma(y + 1) - Theta * log(Theta) + lgamma(Theta) -
            lgamma(Theta + y)
       c(-sum(term*w),0,0)
    }
    initialize <- expression({
        if (any(y < 0)) stop("negative values not allowed for the negative binomial family")
        n <- rep(1, nobs)
        mustart <- y + (y == 0)/6
    })

    rd <- function(mu,wt,scale) {
      Theta <- get(".Theta")[1]
      rnbinom(mu,size=Theta,mu=mu)
    }

    qf <- function(p,mu,wt,scale) {
      Theta <- get(".Theta")[1]
      qnbinom(p,size=Theta,mu=mu)
    }
 
    environment(qf) <- environment(rd) <- environment(dvar) <- environment(d2var) <- 
    environment(variance) <- environment(validmu) <- 
    environment(ls) <- environment(dev.resids) <- environment(aic) <- environment(getTheta) <- env
    famname <- paste("Negative Binomial(", format(round(theta,3)), ")", sep = "")
    structure(list(family = famname, link = linktemp, linkfun = stats$linkfun,
        linkinv = stats$linkinv, variance = variance,dvar=dvar,d2var=d2var,d3var=d3var, dev.resids = dev.resids,
        aic = aic, mu.eta = stats$mu.eta, initialize = initialize,ls=ls,
        validmu = validmu, valideta = stats$valideta,getTheta = getTheta,qf=qf,rd=rd,canonical="log"), class = "family")
} ## negbin



totalPenalty <- function(S,H,off,theta,p)
{ if (is.null(H)) St <- matrix(0,p,p)
  else { St <- H; 
    if (ncol(H)!=p||nrow(H)!=p) stop("H has wrong dimension")
  }
  theta <- exp(theta)
  m <- length(theta)
  if (m>0) for (i in 1:m) {
    k0 <- off[i]
    k1 <- k0 + nrow(S[[i]]) - 1
    St[k0:k1,k0:k1] <- St[k0:k1,k0:k1] + S[[i]] * theta[i]
  }
  St
}

totalPenaltySpace <- function(S,H,off,p)
{ ## function to obtain (orthogonal) basis for the null space and 
  ## range space of the penalty, and obtain actual null space dimension
  ## components are roughly rescaled to avoid any dominating

  Hscale <- sqrt(sum(H*H));
  if (Hscale==0) H <- NULL ## H was all zeroes anyway!

  if (is.null(H)) St <- matrix(0,p,p)
  else { St <- H/sqrt(sum(H*H)); 
    if (ncol(H)!=p||nrow(H)!=p) stop("H has wrong dimension")
  }
  m <- length(S)
  if (m>0) for (i in 1:m) {
    k0 <- off[i]
    k1 <- k0 + nrow(S[[i]]) - 1
    St[k0:k1,k0:k1] <- St[k0:k1,k0:k1] + S[[i]]/sqrt(sum(S[[i]]*S[[i]]))
  }
  es <- eigen(St,symmetric=TRUE)
  ind <- es$values>max(es$values)*.Machine$double.eps^.66
  Y <- es$vectors[,ind,drop=FALSE]  ## range space
  Z <- es$vectors[,!ind,drop=FALSE] ## null space - ncol(Z) is null space dimension
  E <- sqrt(as.numeric(es$values[ind]))*t(Y) ## E'E = St
  list(Y=Y,Z=Z,E=E)
}



mini.roots <- function(S,off,np,rank=NULL)
# function to obtain square roots, B[[i]], of S[[i]]'s having as few
# columns as possible. S[[i]]=B[[i]]%*%t(B[[i]]). np is the total number
# of parameters. S is in packed form. rank[i] is optional supplied rank 
# of S[[i]], rank[i] < 1, or rank=NULL to estimate.
{ m<-length(S)
  if (m<=0) return(list())
  B<-S
  if (is.null(rank)) rank <- rep(-1,m)
  for (i in 1:m)
  { b <- mroot(S[[i]],rank=rank[i]) 
    B[[i]] <- matrix(0,np,ncol(b))
    B[[i]][off[i]:(off[i]+nrow(b)-1),] <- b
  }
  B
}


ldTweedie <- function(y,mu=y,p=1.5,phi=1) {
## evaluates log Tweedie density for 1<=p<=2, using series summation of
## Dunn & Smyth (2005) Statistics and Computing 15:267-280.
 
  if (length(p)>1||length(phi)>1) stop("only scalar `p' and `phi' allowed.")
  if (p<1||p>2) stop("p must be in [1,2]")
  ld <- cbind(y,y,y)
  if (p == 2) { ## It's Gamma
    if (sum(y<=0)) stop("y must be strictly positive for a Gamma density")
    ld[,1] <- dgamma(y, shape = 1/phi,rate = 1/(phi * mu),log=TRUE)
    ld[,2] <- (digamma(1/phi) + log(phi) - 1 + y/mu - log(y/mu))/(phi*phi)
    ld[,3] <- -2*ld[,2]/phi + (1-trigamma(1/phi)/phi)/(phi^3)
    return(ld)
  }  

  if (length(mu)==1) mu <- rep(mu,length(y))

  if (p == 1) { ## It's Poisson like
    ## ld[,1] <- dpois(x = y/phi, lambda = mu/phi,log=TRUE)
    if (sum(!is.integer(y/phi))) stop("y must be an integer multiple of phi for Tweedie(p=1)")
    ind <- (y!=0)|(mu!=0) ## take care to deal with y log(mu) when y=mu=0
    bkt <- y*0
    bkt[ind] <- (y[ind]*log(mu[ind]/phi) - mu[ind])
    dig <- digamma(y/phi+1)
    trig <- trigamma(y/phi+1)
    ld[,1] <- bkt/phi - lgamma(y/phi+1)
    ld[,2] <- (-bkt - y + dig*y)/(phi*phi)
    ld[,3] <- (2*bkt + 3*y - 2*dig*y - trig *y*y/phi)/(phi^3)
    return(ld) 
  }

  ## .. otherwise need the full series thing....
  ## first deal with the zeros  
  
  ind <- y==0
 
  ld[ind,1] <- -mu[ind]^(2-p)/(phi*(2-p))
  ld[ind,2] <- -ld[ind,1]/phi
  ld[ind,3] <- -2*ld[ind,2]/phi

  if (sum(!ind)==0) return(ld)

  ## now the non-zeros
  y <- y[!ind];mu <- mu[!ind]
  w <- w1 <- w2 <- y*0
  oo <- .C(C_tweedious,w=as.double(w),w1=as.double(w1),w2=as.double(w2),y=as.double(y),
           phi=as.double(phi),p=as.double(p),eps=as.double(.Machine$double.eps),n=as.integer(length(y)))
  
#  check.derivs <- TRUE
#  if (check.derivs) {
#    eps <- 1e-6
#    oo1 <- .C(C_tweedious,w=as.double(w),w1=as.double(w1),w2=as.double(w2),y=as.double(y),
#           phi=as.double(phi+eps),p=as.double(p),eps=as.double(.Machine$double.eps),n=as.integer(length(y)))
#    w2.fd <- (oo1$w1-oo$w1)/eps
#    print(oo$w2);print(w2.fd)
#  }  

  theta <- mu^(1-p)
  k.theta <- mu*theta/(2-p)
  theta <- theta/(1-p)
  l.base <-  (y*theta-k.theta)/phi
  ld[!ind,1] <- l.base - log(y) + oo$w
  ld[!ind,2] <- -l.base/phi + oo$w1   
  ld[!ind,3] <- 2*l.base/(phi*phi) + oo$w2
  
  ld
}

Tweedie <- function(p=1,link=power(0)) {
## a restricted Tweedie family
  if (p<=1||p>2) stop("Only 1<p<=2 supported")
  
  linktemp <- substitute(link)
  if (!is.character(linktemp)) linktemp <- deparse(linktemp)
  okLinks <- c("log", "identity", "sqrt","inverse")
  if (linktemp %in% okLinks)
    stats <- make.link(linktemp) else 
  if (is.character(link)) {
    stats <- make.link(link)
    linktemp <- link
  } else {
    if (inherits(link, "link-glm")) {
       stats <- link
       if (!is.null(stats$name))
          linktemp <- stats$name
        } else {
            stop(gettextf("link \"%s\" not available for Tweedie family.",
                linktemp, collapse = ""),domain = NA)
        }
    }
    
    variance <- function(mu) mu^p
    dvar <- function(mu) p*mu^(p-1)
    if (p==1) d2var <- function(mu) 0*mu else
      d2var <- function(mu) p*(p-1)*mu^(p-2)
    if (p==1||p==2)  d3var <- function(mu) 0*mu else
      d3var <- function(mu) p*(p-1)*(p-2)*mu^(p-3)
    validmu <- function(mu) all(mu >= 0)

    dev.resids <- function(y, mu, wt) {
        y1 <- y + (y == 0)
        if (p == 1)
            theta <- log(y1/mu)
        else theta <- (y1^(1 - p) - mu^(1 - p))/(1 - p)
        if (p == 2)
            kappa <- log(y1/mu)
        else kappa <- (y^(2 - p) - mu^(2 - p))/(2 - p)
        2 * wt * (y * theta - kappa)
    }
    initialize <- expression({
        n <- rep(1, nobs)
        mustart <- y + 0.1 * (y == 0)
    })
    ls <-  function(y,w,n,scale) {
      power <- p
      colSums(w*ldTweedie(y,y,p=power,phi=scale))
    }

    aic <- function(y, n, mu, wt, dev) {
      power <- p
      scale <- dev/sum(wt)
      -2*sum(ldTweedie(y,mu,p=power,phi=scale)[,1]*wt) + 2
    }

    if (p==2) {
      rd <- function(mu,wt,scale) {
        rgamma(mu,shape=1/scale,scale=mu*scale)
      }   
    } else {
      rd <- function(mu,wt,scale) {
        rTweedie(mu,p=p,phi=scale)
      }
    }

    structure(list(family = paste("Tweedie(",p,")",sep=""), variance = variance, 
              dev.resids = dev.resids,aic = aic, link = linktemp, linkfun = stats$linkfun, linkinv = stats$linkinv,
        mu.eta = stats$mu.eta, initialize = initialize, validmu = validmu,
        valideta = stats$valideta,dvar=dvar,d2var=d2var,d3var=d3var,ls=ls,rd=rd,canonical="none"), class = "family")


}



rTweedie <- function(mu,p=1.5,phi=1) {
## generate Tweedie random variables, with 1<p<2, 
## adapted from rtweedie in the tweedie package
  if (p<=1||p>=2) stop("p must be in (1,2)")
  if (sum(mu<0)) stop("mean, mu, must be non negative")
  if (phi<=0) stop("scale parameter must be positive")
  
  lambda <- mu^(2-p)/((2-p)*phi)
  shape <- (2-p)/(p-1)
  scale <- phi*(p-1)*mu^(p-1)

  n.sim <- length(mu)

  ## how many Gamma r.v.s to sum up to get Tweedie
  ## 0 => none, and a zero value

  N <- rpois(length(lambda),lambda)

  ## following is a vector of N[i] copies of each gamma.scale[i]
  ## concatonated one after the other

  gs <- rep(scale,N)

  ## simulate gamma deviates to sum to get tweedie deviates

  y <- rgamma(gs*0+1,shape=shape,scale=gs)

  ## create summation index...

  lab <- rep(1:length(N),N)

  ## sum up each gamma sharing a label. 0 deviate if label does not occur
  o <- .C(C_psum,y=as.double(rep(0,n.sim)),as.double(y),as.integer(lab),as.integer(length(lab)))  
  o$y
}
