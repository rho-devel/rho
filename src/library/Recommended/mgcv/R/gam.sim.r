
## Example simulated data for gam.models (c) Simon N. Wood 2008

gamSim <- function(eg=1,n=400,dist="normal",scale=2) {

  if (eg==1||eg==7) { ## 4 term Gu  and Wahba example
    if (eg==1) cat("Gu & Wahba 4 term additive model\n")
    else  cat("Gu & Wahba 4 term additive model, correlated predictors\n")
    x0 <- runif(n, 0, 1)
    if (eg==7) x1 <- x0*.7 + runif(n, 0, .3) else
    x1 <- runif(n,0,1)
    x2 <- runif(n, 0, 1)
    if (eg==7) x3 <- x2*.9 + runif(n,0,.1) else
    x3 <- runif(n, 0, 1)
    f0 <- function(x) 2 * sin(pi * x)
    f1 <- function(x) exp(2 * x)
    f2 <- function(x) 0.2*x^11*(10*(1-x))^6+10*(10*x)^3*(1-x)^10
    f3 <- function(x) 0*x
    f <- f0(x0) + f1(x1) + f2(x2)
    if (dist=="normal") {
      e <- rnorm(n, 0, scale)
      y <- f + e
    } else if (dist=="poisson") {
      g<-exp(f*scale)
      f <- log(g) ## true linear predictor
      y<-rpois(rep(1,n),g)
    } else if (dist=="binary") {
      f <- (f-5)*scale
      g <- binomial()$linkinv(f)
      y <- rbinom(g,1,g)
    } else stop("dist not recognised")
    data <- data.frame(y=y,x0=x0,x1=x1,x2=x2,x3=x3,f=f,f0=f0(x0),f1=f1(x1),f2=f2(x2),f3=x3*0)
    return(data)
  } else if (eg==2) { ## Simple 2D smoothing example
    cat("Bivariate smoothing example\n")
    test1<-function(x,z,sx=0.3,sz=0.4)  
    { (pi**sx*sz)*(1.2*exp(-(x-0.2)^2/sx^2-(z-0.3)^2/sz^2)+
      0.8*exp(-(x-0.7)^2/sx^2-(z-0.8)^2/sz^2))
    }
    x <- runif(n);z <- runif(n);
    xs<-seq(0,1,length=40);zs<-seq(0,1,length=40)
    pr <- data.frame(x=rep(xs,40),z=rep(zs,rep(40,40)))
    truth <- matrix(test1(pr$x,pr$z),40,40)
    f <- test1(x,z)
    y <- f + rnorm(n)*scale
    data <- data.frame(y=y,x=x,z=z,f=f)
    truth <- list(x=xs,z=zs,f=truth)
    return(list(data=data,truth=truth,pr=pr))
  } else if (eg==3) { ## continuous `by' variable
    cat("Continuous `by' variable example\n")
    x1 <- runif(n, 0, 1)
    x2 <- sort(runif(n, 0, 1))
    f <-  0.2 * x2^11 * (10 * (1 - x2))^6 + 
          10 * (10 * x2)^3 * (1 - x2)^10 
    e <- rnorm(n, 0, scale)
    # A continuous `by' variable example.... 
    y <- f*x1 + e
    return(data.frame(y=y,x1=x1,x2=x2,f=f))
  } else if (eg==4) { ## factor `by' variable
    cat("Factor `by' variable example\n")
    n <- 400   
    x0 <- runif(n, 0, 1)
    x1 <- runif(n, 0, 1)
    x2 <- runif(n, 0, 1)
    f1 <- 2 * sin(pi * x2)
    f2 <-  exp(2 * x2) - 3.75887
    f3 <-  0.2 * x2^11 * (10 * (1 - x2))^6 + 
           10 * (10 * x2)^3 * (1 - x2)^10
    e <- rnorm(n, 0, scale)
    fac<-as.factor(c(rep(1,100),rep(2,100),rep(3,200)))
    fac.1<-as.numeric(fac==1);fac.2<-as.numeric(fac==2);
    fac.3<-as.numeric(fac==3)
    y<-f1*fac.1+f2*fac.2+f3*fac.3+ e 
    return(data.frame(y=y,x0=x0,x1=x1,x2=x2,fac=fac,f1=f1,f2=f2,f3=f3))
  } else if (eg==5) { ## additive + factor
    cat("Additive model + factor\n")
    x0 <- rep(1:4,50)
    x1 <- runif(n, 0, 1)
    x2 <- runif(n, 0, 1)
    x3 <- runif(n, 0, 1)
    y <- 2 * x0
    y <- y + exp(2 * x1)
    y <- y + 0.2 * x2^11 * (10 * (1 - x2))^6 + 10 * (10 * x2)^3 * (1 - x2)^10
    e <- rnorm(n, 0, scale)
    y <- y + e
    x0<-as.factor(x0)
    return(data.frame(y=y,x0=x0,x1=x1,x2=x2,x3=x3))
  } else if (eg==6) { ## Gu and Wahba + a random factor
    cat("4 term additive + random effect")
    dat <- gamSim(1,n=n,scale=0)
    fac <- rep(1:4,n/4)
    dat$f <- dat$f + fac*3
    dat$fac<-as.factor(fac)
    if (dist=="normal") {
      dat$y <- dat$f + rnorm(n)*scale 
    } else if (dist=="poisson") {
      g <- exp(dat$f*scale)
      dat$y <- rpois(rep(1,n),g)
    } else if (dist=="binary") {
      g <- (dat$f-5)*scale
      g <- binomial()$linkinv(g)
      dat$y <- rbinom(g,1,g)
    }
    return(dat)
  }
}


