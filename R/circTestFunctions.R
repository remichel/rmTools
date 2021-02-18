
##' Performs a Wallraff Test for circular data
##' @description \code{WallraffTest}
##' Performs a Wallraff Test for circular data with code from (from Pewsey, A., Neuhäuser, M., & Ruxton, G. D. (2013). Circular statistics in R. Oxford University Press.)
##' @param cdat combined vector of observations, e.g. c(Obs1_1,...Obs1_10, Obs2_1, ... Obs2_20)
##' @param ndat vector with N observations of each group, e.g. c(10,20) for one group n = 10 and another of n = 20
##' @param g number of groups
##' @author René Michel
##' @export WallraffTest
##' @name WallraffTest


WallraffTest <- function(cdat, ndat, g){

  N = length(cdat)
  ndatsum = cumsum(ndat)
  tbar = circular(0)
  distdat = 0

  for(k in 1:g){

    dist = 0
    sample = circular(0)

    if(k == 1){ low = 0 }else if(k > 1){low = ndatsum[k-1]}

    for(j in 1:ndat[k]){ sample[j] = cdat[j+low]}

    tm1 = trigonometric.moment(sample, p = 1)
    tbar[k] = tm1$mu

    for(j in 1:ndat[k]){dist[j] = pi-abs(sample[j]-tbar[k])}

    distdat = c(distdat,dist)

  }

  distdat = distdat[-1]

  #gID = c(rep(1,n1), rep(2,n2), rep(3,n3)) n1-n3 not found, error in provided code from circular statistics
  # use own piece of code here
  for(i in 1:g){
    tmp = rep(i, ndat[i])
    if(i == 1){ids = tmp}else{ids = c(ids, tmp)}
  }

  TestRes = kruskal.test(distdat,g = ids)

  return(TestRes)


}



##' Computes required stats for a Moore Test for circular data
##' @description \code{MooreRStats}
##' Computes required stats for a Moore Test for circular data with code from (from Pewsey, A., Neuhäuser, M., & Ruxton, G. D. (2013). Circular statistics in R. Oxford University Press.)
##' @param ldat1 vec 1
##' @param ldat2 vec 2
##' @author Rene Michel
##' @export MooreRStats
##' @name MooreRStats

MooreRStats = function(ldat1,ldat2){
  x = cos(ldat1)-cos(ldat2)
  y <- sin(ldat1)-sin(ldat2)

  r <- sqrt((x*x)+(y*y)); Ranks <- rank(r)

  cosphi <- x/r
  sinphi <- y/r
  return(list(cosphi,sinphi,Ranks))
}

##' Computes Moore Test statistic
##' @description \code{MooreRTestStat}
##' Computes Moore Test statistic for circular data with code from (from Pewsey, A., Neuhäuser, M., & Ruxton, G. D. (2013). Circular statistics in R. Oxford University Press.)
##' @param cosphi cosphi
##' @param sinphi sinphi
##' @param Ranks ranks
##' @author René Michel
##' @export MooreRTestStat
##' @name MooreRTestStat


MooreRTestStat = function(cosphi,sinphi,Ranks){
  n = length(cosphi)
  RbarC = (1/n)*sum(Ranks*cosphi)
  RbarS = (1/n)*sum(Ranks*sinphi)
  Rval = sqrt(((RbarC*RbarC)+(RbarS*RbarS))/n)
  return(Rval)
}

##' Computes Moore Test
##' @description \code{MooreRTestRand}
##' Computes Moore Test for circular data with code from (from Pewsey, A., Neuhäuser, M., & Ruxton, G. D. (2013). Circular statistics in R. Oxford University Press.)
##' @param sinphi sinphi
##' @param Ranks ranks
##' @param NR number of permutations
##' @author René Michel
##' @export MooreRTestRand
##' @name MooreRTestRand


MooreRTestRand = function(cosphi,sinphi,Ranks, NR){
  RObs = MooreRTestStat(cosphi, sinphi,Ranks); nxtrm = 1
  n = length(cosphi)
  for (r in 1:NR){
    cosphirand = 0; sinphirand = 0;
    for (j in 1:n){
      if (runif(1) < 0.5){cosphirand[j] = cosphi[j]; sinphirand[j] = sinphi[j]}
      else {cosphirand[j] = -cosphi[j]; sinphirand[j] = -sinphi[j]}
    }
    RRand = MooreRTestStat(cosphirand,sinphirand, Ranks)
    if(RRand >= RObs){nxtrm = nxtrm +1}
  }
  pval <- nxtrm/(NR+1)
  return(c(RObs, pval))
}


##' Computes FgVal
##' @description \code{MooreRTestRand}
##' Computes FgVal with code from (from Pewsey, A., Neuhäuser, M., & Ruxton, G. D. (2013). Circular statistics in R. Oxford University Press.)
##' @param dvals dvals
##' @param ndat vector with N observations of each group, e.g. c(10,20) for one group n = 10 and another of n = 20
##' @param g number of groups
##' @author René Michel
##' @export FgVal
##' @name FgVal


FgVal = function(dvals, ndat, g){
  N = length(dvals)
  ndatcsum = cumsum(ndat)

  sum1 = 0; sum2 = 0
  dk = 0; dbar = 0; gdbar = 0

  for(k in 1:g){
    sample = circular(0)

    if(k == 1){low = 0}else if(k > 1){low = ndatcsum[k-1]}

    for(j in 1:ndat[k]){ dk[j] = dvals[j+low]}

    dbar[k] = sum(dk)/ndat[k]
    sum2 = sum2+sum((dk-dbar[k])**2)
    gdbar = gdbar+ndat[k]*dbar[k]
  }

  gdbar = gdbar/N

  for(k in 1:g){ sum1 = sum1 + ndat[k]*(dbar[k]-gdbar)**2}
  Fg = (N-g)*sum1/((g-1)*sum2)
  return(Fg)
}

##' Computes dValues
##' @description \code{dValues}
##' Computes dValues with code from (from Pewsey, A., Neuhäuser, M., & Ruxton, G. D. (2013). Circular statistics in R. Oxford University Press.)
##' @param dvals dvals
##' @param ndat vector with N observations of each group, e.g. c(10,20) for one group n = 10 and another of n = 20
##' @param g number of groups
##' @author René Michel
##' @export dValues
##' @name dValues

dValues = function(cdat, ndat, g){
  N = length(cdat)
  ndatcsum = cumsum(ndat)
  dval = 0

  for(k in 1:g){
    sample = circular(0)

    if(k == 1){ low = 0 }else if(k > 1){ low = ndatcsum[k-1]}

    for (j in 1:ndat[k]){ sample[j] = cdat[j+low]}

    tm1 = trigonometric.moment(sample, p = 1)
    tbar = tm1$mu
    dvalk = abs(sin(sample-tbar))
    dval = c(dval, dvalk)
  }
  dval = dval[-1]
  return(dval)
}


##' Performs circular Fisher Test
##' @description \code{CircFisherTest}
##' Performs circular Fisher Test with code from (from Pewsey, A., Neuhäuser, M., & Ruxton, G. D. (2013). Circular statistics in R. Oxford University Press.)
##' @param cdat combined vector of observations, e.g. c(Obs1_1,...Obs1_10, Obs2_1, ... Obs2_20)
##' @param ndat vector with N observations of each group, e.g. c(10,20) for one group n = 10 and another of n = 20
##' @param g number of groups
##' @param parametric defaults to T
##' @param NR defaults to 0, number of permutations
##' @author René Michel
##' @export CircFisherTest
##' @name CircFisherTest
##'
##'
CircFisherTest = function(cdat, ndat, g, parametric = T, NR = 0){
  # NR = number of permutations
  # g number of groups
  # ndat = vector with N observations of each group, e.g. c(10,20) for one group n = 10 and another of n = 20
  # cdat = combined vector of observations, e.g. c(Obs1_1,...Obs1_10, Obs2_1, ... Obs2_20)

  N = sum(ndat)

  dvals = dValues(cdat, ndat, g)

  FgObs = FgVal(dvals, ndat,g)

  if(parametric){
    testRes = pf(FgObs,g-1,N-g, lower.tail = F)
    return(c(FgObs,testRes))
  }else{
    nxtrm = 1
    for(r in 1:NR){
      randdvals = sample(dvals)
      FgRand = FgVal(randdvals, ndat,g)
      if(FgRand >= FgObs){nxtrm = nxtrm+1}
    }
    pval = nxtrm/(NR+1)

    return(c(FgObs,pval))
  }
}








##' Performs SpecMeanTestBoot
##' @description \code{SpecMeanTestBoot}
##' Performs SpecMeanTestBoot with code from (from Pewsey, A., Neuhäuser, M., & Ruxton, G. D. (2013). Circular statistics in R. Oxford University Press.)
##' @author Rene Michel
##' @export SpecMeanTestBoot
##' @name SpecMeanTestBoot
##'
##'




SpecMeanTestBoot <- function(origdat, mu0, indsym, B) {

  n <- length(origdat)

  testres <- SpecMeanTestRes(origdat, indsym, mu0)

  z <- testres[[1]] ; mubc <- testres[[2]]



  shiftdat <- origdat-mubc+mu0



  if (indsym == 1) {

    refdat <- 2*mu0-shiftdat ; sampledat <- c(shiftdat, refdat)

  } else

    if (indsym == 0) { sampledat <- shiftdat }



  nxtrm <- 1

  for (b in 2:(B+1)) {

    bootdat <- sample(sampledat, size=n, replace=TRUE)

    testres <- SpecMeanTestRes(bootdat, indsym, mu0)

    z[b] <- testres[[1]]

    if (z[b] >= z[1]) { nxtrm <- nxtrm + 1 }

  }



  pval <- nxtrm/(B+1)

  return(pval)

}



##' Performs SpecMeanTestRes
##' @description \code{SpecMeanTestRes}
##' Performs SpecMeanTestBoot with code from (from Pewsey, A., Neuhäuser, M., & Ruxton, G. D. (2013). Circular statistics in R. Oxford University Press.)
##' @author Rene Michel
##' @export SpecMeanTestRes
##' @name SpecMeanTestRes
##'
##'



SpecMeanTestRes <- function(circdat, indsym, mu0) {

  n <- length(circdat)

  t10bar <- trigonometric.moment(circdat, p=1, center=FALSE)

  tbar <- atan2(t10bar$sin, t10bar$cos)

  if (tbar < 0) {tbar <- tbar + 2*pi}

  Rbar <- rho.circular(circdat) ; Rbar2 <-  Rbar*Rbar

  t2bar <- trigonometric.moment(circdat, p=2, center=TRUE)

  abar2 <- t2bar$cos ; bbar2 <- t2bar$sin

  if (indsym == 1) {bbar2 <- 0}

  div <- 2*n*Rbar2

  mubc <- tbar + (bbar2/div) ;

  if (mubc > 2*pi) {mubc <- mubc - 2*pi} else

    if (mubc < 0) {mubc <- mubc + 2*pi}

  dist <- pi-abs(pi-abs(mubc-mu0))

  tbarstderr <- sqrt((1-abar2)/div)

  z <- dist/tbarstderr

  return(list(z, mubc))

}


##' Performs RSTestBoot
##' @description \code{RSTestBoot}
##' Performs RSTestBoot with code from (from Pewsey, A., Neuhäuser, M., & Ruxton, G. D. (2013). Circular statistics in R. Oxford University Press.)
##' @author Rene Michel
##' @export RSTestBoot
##' @name RSTestBoot
##'
##'
##'
RSTestBoot <- function(origdat, B) {

  n <- length(origdat)

  absz <- RSTestStat(origdat)

  tbar <- mean(origdat) ; refcdat <- 2*tbar-origdat ; symmcdat <- c(origdat, refcdat)

  nxtrm <- 1

  for (b in 2:(B+1)) {

    bootsymmdat <- sample(symmcdat, size=n, replace=TRUE)

    absz[b] <- RSTestStat(bootsymmdat)

    if (absz[b] >= absz[1]) {nxtrm <- nxtrm+1}

  }

  pval <- nxtrm/(B+1)

  return(pval)

}

##' Performs RRSTestStat
##' @description \code{RSTestStat}
##' Performs RSTestStat with code from (from Pewsey, A., Neuhäuser, M., & Ruxton, G. D. (2013). Circular statistics in R. Oxford University Press.)
##' @author Rene Michel
##' @export RSTestStat
##' @name RSTestStat
##'
##'
##'
##'
RSTestStat <- function(circdat) {

  n <- length(circdat)

  Rbar <- rho.circular(circdat)

  t2bar <- trigonometric.moment(circdat, p=2, center=TRUE)

  bbar2 <- t2bar$sin  ; abar2 = t2bar$cos

  t3bar <- trigonometric.moment(circdat, p=3, center=TRUE)

  abar3 <- t3bar$cos

  t4bar <- trigonometric.moment(circdat, p=4, center=TRUE)

  abar4 <- t4bar$cos

  var <- ((1-abar4)/2-(2*abar2)+(2*abar2/Rbar)*(abar3+(abar2*(1-abar2)/Rbar)))/n

  absz <- abs(bbar2/sqrt(var)) ; return(absz)

}


##' Performs ConfIntLS
##' @description \code{ConfIntLS}
##' Performs ConfIntLS with code from (from Pewsey, A., Neuhäuser, M., & Ruxton, G. D. (2013). Circular statistics in R. Oxford University Press.)
##' @author Rene Michel
##' @export ConfIntLS
##' @name ConfIntLS
##'

ConfIntLS <- function(circdat, indsym, conflevel) {

  n <- length(circdat) ; tbar <- mean(circdat) ; Rbar <- rho.circular(circdat)

  t2bar <- trigonometric.moment(circdat, p=2, center=TRUE)

  t3bar <- trigonometric.moment(circdat, p=3, center=TRUE)

  t4bar <- trigonometric.moment(circdat, p=4, center=TRUE)

  abar2 <- t2bar$cos ; abar3 <- t3bar$cos

  abar4 <- t4bar$cos

  bbar2 <- t2bar$sin ; bbar3 <- t3bar$sin

  Rbar2 <-  Rbar*Rbar ; Rbar4 <- Rbar2*Rbar2

  alpha <- (100-conflevel)/100 ; qval <- qnorm(1-alpha/2)

  rhobc <- Rbar - ((1-abar2)/(4*n*Rbar)) ; rbarstderr <- sqrt((1-2*Rbar2+abar2)/(2*n))

  rhoup <- rhobc + qval*rbarstderr ; rholo <- rhobc - qval*rbarstderr

  rhores <- c(rhobc, rholo, rhoup)

  if (indsym == 1) {bbar2 <- 0 ; bbar3 <- 0 ; betab2res <- c(0,0,0)} else

    if (indsym == 0) {

      betab2bc <- bbar2 + ((bbar3/Rbar)+(bbar2/Rbar2)-(2*abar2*bbar2/Rbar4))/n

      b2bstderr <- sqrt((((1-abar4)/2)-(2*abar2)-(bbar2*bbar2)+(2*abar2/Rbar)*(abar3+(abar2*(1-abar2)/Rbar)))/n)

      betab2up <- betab2bc + qval*b2bstderr ; betab2lo <- betab2bc - qval*b2bstderr

      betab2res <- c(betab2bc, betab2lo, betab2up)

    }

  div <- 2*n*Rbar2

  mubc <- tbar + (bbar2/div) ; tbarstderr <- sqrt((1-abar2)/div)

  muup <- mubc + qval*tbarstderr ; mulo <- mubc - qval*tbarstderr

  mures <- c(mubc, mulo, muup)

  alphab2bc <- abar2 - (1-(abar3/Rbar)-((abar2*(1-abar2)+bbar2*bbar2)/Rbar2))/n

  a2bstderr <- sqrt((((1+abar4)/2)-(abar2*abar2)+(2*bbar2/Rbar)*(bbar3+(bbar2*(1-abar2)/Rbar)))/n)

  alphab2up <- alphab2bc + qval*a2bstderr ; alphab2lo <- alphab2bc - qval*a2bstderr

  alphab2res <- c(alphab2bc, alphab2lo, alphab2up)

  if (indsym == 0) { return(list(mures, rhores, betab2res, alphab2res)) } else

    if (indsym == 1) { return(list(mures, rhores, alphab2res)) }

}



##' Performs BiasCEsts
##' @description \code{BiasCEsts}
##' Performs BiasCEsts with code from (from Pewsey, A., Neuhäuser, M., & Ruxton, G. D. (2013). Circular statistics in R. Oxford University Press.)
##' @author Rene Michel
##' @export BiasCEsts
##' @name BiasCEsts
##'


BiasCEsts <- function(circdat, indsym, n) {

  t10bar <- trigonometric.moment(circdat, p=1, center=FALSE)

  tbar <- atan2(t10bar$sin, t10bar$cos)

  if (tbar < 0) {tbar <- tbar + 2*pi}

  Rbar <- rho.circular(circdat)

  t2bar <- trigonometric.moment(circdat, p=2, center=TRUE)

  t3bar <- trigonometric.moment(circdat, p=3, center=TRUE)

  abar2 <- t2bar$cos ; abar3 <- t3bar$cos

  bbar2 <- t2bar$sin ; bbar3 <- t3bar$sin

  Rbar2 <-  Rbar*Rbar ; Rbar4 <- Rbar2*Rbar2

  rhobc <- Rbar - ((1-abar2)/(4*n*Rbar))

  if (indsym == 1) {bbar2 <- 0 ; bbar3 <- 0 ; betab2bc <- 0} else

    if (indsym == 0) {

      betab2bc <- bbar2 + ((bbar3/Rbar)+(bbar2/Rbar2)-(2*abar2*bbar2/Rbar4))/n

    }

  div <- 2*n*Rbar2 ; mubc <- tbar + (bbar2/div)

  if (mubc > 2*pi) {mubc <- mubc - 2*pi} else

    if (mubc < 0) {mubc <- mubc + 2*pi}

  alphab2bc <- abar2 - (1-(abar3/Rbar)-((abar2*(1-abar2)+bbar2*bbar2)/Rbar2))/n

  return(list(mubc, rhobc, betab2bc, alphab2bc))

}

##' Performs ConfIntBoot
##' @description \code{ConfIntBoot}
##' Performs ConfIntBoot with code from (from Pewsey, A., Neuhäuser, M., & Ruxton, G. D. (2013). Circular statistics in R. Oxford University Press.)
##' @author Rene Michel
##' @export ConfIntBoot
##' @name ConfIntBoot
##'

ConfIntBoot <- function(origdat, indsym, conflevel, B) {

  alpha <- (100-conflevel)/100

  n <- length(origdat)

  ests <- BiasCEsts(origdat, indsym, n)

  muest <- ests[[1]] ; rhoest <- ests[[2]]

  betab2est <- ests[[3]] ; alphab2est <- ests[[4]]



  if (indsym == 1) {

    refdat <- 2*muest-origdat ; sampledat <- c(origdat, refdat)

  } else

    if (indsym == 0) { sampledat <- origdat }



  for (b in 2:(B+1)) {

    bootdat <- sample(sampledat, size=n, replace=TRUE)

    ests <- BiasCEsts(bootdat, indsym, n)

    muest[b] <- ests[[1]] ; rhoest[b] <- ests[[2]]

    betab2est[b] <- ests[[3]] ; alphab2est[b] <- ests[[4]]

  }



  dist <- 0

  if (indsym == 1) {

    dist <- pi-abs(pi-abs(muest-muest[1]))

    sdist <- sort(dist)

    mulo <- muest[1]-sdist[(B+1)*(1-alpha)]

    muup <- muest[1]+sdist[(B+1)*(1-alpha)]

  } else



    if (indsym == 0) {



      if (muest[1] < pi) {

        ref <- muest[1] + pi

        for (b in 1:(B+1)) {

          dist[b] <- -(pi-abs(pi-abs(muest[b]-muest[1])))

          if (muest[b] > muest[1]) {

            if (muest[b] < ref) {dist[b] <- -dist[b]}

          }

        }

      } else

        if (muest[1] >= pi) {

          ref <- muest[1] - pi

          for (b in 1:(B+1)) {

            dist[b] <- pi-abs(pi-abs(muest[b]-muest[1]))

            if (muest[b] > ref) {

              if (muest[b] < muest[1]) {dist[b] <- -dist[b]}

            }

          }

        }



      sdist <- sort(dist)

      mulo <- muest[1]+sdist[(B+1)*(alpha/2)]

      muup <- muest[1]+sdist[(B+1)*(1-alpha/2)]

      sbetab2est <- sort(betab2est)

      betab2lo <- sbetab2est[(B+1)*(alpha/2)]

      betab2up <- sbetab2est[(B+1)*(1-alpha/2)]

      betab2res <- c(betab2est[1], betab2lo, betab2up)



    }



  mures <- c(muest[1], mulo, muup)

  srhoest <- sort(rhoest)

  rholo <- srhoest[(B+1)*(alpha/2)] ; rhoup <- srhoest[(B+1)*(1-alpha/2)]

  salphab2est <- sort(alphab2est)

  alphab2lo <- salphab2est[(B+1)*(alpha/2)] ; alphab2up <- salphab2est[(B+1)*(1-alpha/2)]

  rhores <- c(rhoest[1], rholo, rhoup) ; alphab2res <- c(alphab2est[1], alphab2lo, alphab2up)

  if (indsym == 0) { return(list(mures, rhores, betab2res, alphab2res)) } else

    if (indsym == 1) { return(list(mures, rhores, alphab2res)) }

}

