
##' Performs a Wallraff Test for circular data
##' @description \code{WallraffTest}
##' Performs a Wallraff Test for circular data (from Pewsey, A., Neuhäuser, M., & Ruxton, G. D. (2013). Circular statistics in R. Oxford University Press.)
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
##' Computes required stats for a Moore Test for circular data (from Pewsey, A., Neuhäuser, M., & Ruxton, G. D. (2013). Circular statistics in R. Oxford University Press.)
##' @param ldat1 vec 1
##' @param ldat2 vec 2
##' @author René Michel
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
##' Computes Moore Test statistic for circular data (from Pewsey, A., Neuhäuser, M., & Ruxton, G. D. (2013). Circular statistics in R. Oxford University Press.)
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
##' Computes Moore Test for circular data (from Pewsey, A., Neuhäuser, M., & Ruxton, G. D. (2013). Circular statistics in R. Oxford University Press.)
##' @param cosphi cosphi
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
##' Computes FgVal (from Pewsey, A., Neuhäuser, M., & Ruxton, G. D. (2013). Circular statistics in R. Oxford University Press.)
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
##' Computes dValues (from Pewsey, A., Neuhäuser, M., & Ruxton, G. D. (2013). Circular statistics in R. Oxford University Press.)
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
##' Performs circular Fisher Test (from Pewsey, A., Neuhäuser, M., & Ruxton, G. D. (2013). Circular statistics in R. Oxford University Press.)
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
