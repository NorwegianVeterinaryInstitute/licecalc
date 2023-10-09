#' plotLok
#'
#' @description A function to plot lice development prediction
#'
#' @param loknr description
#'
#' @return A plot object
#'
#' @example
plotLok <- function(loknr, valgtSm, tid, innlestData, plotnr, hele, merd){
  if(loknr == 0){
    LnSmP <- valgtSm
  } else {
    if(as.character(loknr) %in% rownames(luse_demo_data)){
      if(tid == 0){
        LnSmP  <- log(siste_uker_demodata[as.character(loknr),1] + 1)
      } else {
        if(paste("X", tid, sep = "") %in% colnames(total_fra_demodata)){
          LnSmP <- log(total_fra_demodata[as.character(loknr), paste("X",tid, sep = "")])
        }
      }
    }
  }
  MerderM1 <- innlestData[,1]
  VektMerd <- innlestData[,2]
  ReFis <- innlestData[,3]
  LM1 <- innlestData[,4]
  LokM1 <- mean(MerderM1)
  antMerder <- length(MerderM1)
  HeleMerd <- TRUE
  if (merd == "merdvis") {
    HeleMerd <- FALSE
  }

  if(HeleMerd == TRUE){
    yCount <- countHele(MerderM1 = MerderM1, LokM1 = LokM1, VektMerd = VektMerd, LM1 = LM1, ReFis = ReFis, LnSmP = LnSmP)
    yZero <- zeroHele(MerderM1 = MerderM1, LokM1 = LokM1, VektMerd = VektMerd, LM1 = LM1, ReFis = ReFis, LnSmP = LnSmP)
  } else {
    yCount <- countMerd(MerderM1 = MerderM1, VektMerd = VektMerd, LM1 = LM1, ReFis = ReFis, LnSmP = LnSmP)
    yZero <- zeroMerd(MerderM1 = MerderM1, VektMerd = VektMerd, LM1 = LM1, ReFis = ReFis, LnSmP = LnSmP)
  }
  pCount <- exp(yCount)
  pZero <- exp(yZero)/(1+exp(yZero))

  MerderUke1T <- rep(NA, length(MerderM1))
  MUke1upT <- rep(NA,length(MerderM1))
  MUke1lowT <- rep(NA,length(MerderM1))

  for(i in 1:length(MerderM1)){
    obs <- replicate(1000, trekk(p0 = pZero[i], pC = pCount[i], Theta = thetaHele))
    MerderUke1T[i] <- mean(obs)/30
    MUke1upT[i] <- quantile(obs, 0.25)/30
    MUke1lowT[i] <- quantile(obs, 0.75)/30
  }

  if(HeleMerd == TRUE){
    yCount <- countHele(MerderM1 = MerderUke1T, LokM1 = mean(MerderUke1T), VektMerd = VektMerd, LM1 = LM1, ReFis = ReFis, LnSmP = LnSmP)
    yZero <- zeroHele(MerderM1 = MerderUke1T, LokM1 = mean(MerderUke1T), VektMerd = VektMerd, LM1 = LM1, ReFis = ReFis, LnSmP = LnSmP)
  } else {
    yCount <- countMerd(MerderM1 = MerderUke1T, VektMerd = VektMerd, LM1 = LM1, ReFis = ReFis, LnSmP = LnSmP)
    yZero <- zeroMerd(MerderM1 = MerderUke1T, VektMerd = VektMerd, LM1 = LM1, ReFis = ReFis, LnSmP = LnSmP)
  }
  pCount <- exp(yCount)
  pZero <- exp(yZero)/(1+exp(yZero))

  MerderUke2T <- rep(NA, length(MerderM1))
  MUke2upT <- rep(NA,length(MerderM1))
  MUke2lowT <- rep(NA,length(MerderM1))
  for(i in 1:length(MerderM1)){
    obs <- replicate(1000, trekk(p0 = pZero[i], pC = pCount[i], Theta = thetaHele))
    MerderUke2T[i] <- mean(obs)/30
    MUke2upT[i] <- quantile(obs, 0.25)/30
    MUke2lowT[i] <- quantile(obs, 0.75)/30
  }


  if(HeleMerd == TRUE){
    yCount <- countHele(MerderM1 = MerderUke2T, LokM1 = mean(MerderUke2T), VektMerd = VektMerd, LM1 = LM1, ReFis = ReFis, LnSmP = LnSmP)
    yZero <- zeroHele(MerderM1 = MerderUke2T, LokM1 = mean(MerderUke2T), VektMerd = VektMerd, LM1 = LM1, ReFis = ReFis, LnSmP = LnSmP)
  } else {
    yCount <- countMerd(MerderM1 = MerderUke2T, VektMerd = VektMerd, LM1 = LM1, ReFis = ReFis, LnSmP = LnSmP)
    yZero <- zeroMerd(MerderM1 = MerderUke2T, VektMerd = VektMerd, LM1 = LM1, ReFis = ReFis, LnSmP = LnSmP)
  }
  pCount <- exp(yCount)
  pZero <- exp(yZero)/(1+exp(yZero))

  MerderUke3T <- rep(NA, length(MerderM1))
  MUke3upT <- rep(NA,length(MerderM1))
  MUke3lowT <- rep(NA,length(MerderM1))
  for(i in 1:length(MerderM1)){
    obs <- replicate(1000, trekk(p0 = pZero[i], pC = pCount[i], Theta = thetaHele))
    MerderUke3T[i] <- mean(obs)/30
    MUke3upT[i] <- quantile(obs, 0.25)/30
    MUke3lowT[i] <- quantile(obs, 0.75)/30
  }

  pdat <- rbind(MerderM1, MerderUke1T, MUke1upT, MUke1lowT, MerderUke2T, MUke2upT ,MUke2lowT, MerderUke3T, MUke3upT, MUke3lowT)
  xtid <- c(0, 1, 2, 3)
  Min <- 0
  Max <- 7
  p50U1 <- 100*plogis(ParameterHunnlusSann[1] + ParameterHunnlusSann[2]*MerderUke1T)
  p75U1 <- 100*plogis(ParameterHunnlusSann[1] + ParameterHunnlusSann[2]*MUke1lowT)
  p25U1 <- 100*plogis(ParameterHunnlusSann[1] + ParameterHunnlusSann[2]*MUke1upT)
  p50U2 <- 100*plogis(ParameterHunnlusSann[1] + ParameterHunnlusSann[2]*MerderUke2T)
  p75U2 <- 100*plogis(ParameterHunnlusSann[1] + ParameterHunnlusSann[2]*MUke2lowT)
  p25U2 <- 100*plogis(ParameterHunnlusSann[1] + ParameterHunnlusSann[2]*MUke2upT)
  p50U3 <- 100*plogis(ParameterHunnlusSann[1] + ParameterHunnlusSann[2]*MerderUke3T)
  p75U3 <- 100*plogis(ParameterHunnlusSann[1] + ParameterHunnlusSann[2]*MUke3lowT)
  p25U3 <- 100*plogis(ParameterHunnlusSann[1] + ParameterHunnlusSann[2]*MUke3upT)
  par(mfrow = c(1,4), mar = c(5,5,4,1))
  if(antMerder > (plotnr - 1)*4){
    i <- (plotnr - 1)*4 + 1
    plot (xtid, pdat[c(1,2,5,8),i], type = 'l', ylim = c(Min,Max), lwd = 3, col = "red", ylab = "Lus pr fisk pr merd", xlab = "uker framover", main = paste("Merd",  i, sep = " "), axes = F, cex.lab = 1.8, cex.main = 1.8)
    axis(1, c(0,1,2,3), c(0,1,2,3))
    axis(2)
    lines(xtid[-1], pdat[c(3,6,9),i], type = 'l')
    lines(xtid[-1], pdat[c(4,7,10),i], type = 'l')

    legend(x = "topleft", c(paste("75%: ", round(p75U1[i],1), ", ", round(p75U2[i],1), ", ", round(p75U3[i],1)), paste("50%: ", round(p50U1[i],1), ", ", round(p50U2[i],1), ", ", round(p50U3[i],1)), paste("25%: ", round(p25U1[i],1), ", ", round(p25U2[i],1), ", ", round(p25U3[i],1))))
    if(antMerder > (plotnr - 1)*4 + 1){
      for(i in 2:min(antMerder, 4*plotnr)){
        plot (xtid, pdat[c(1,2,5,8),i], type = 'l', ylim = c(Min,Max), lwd = 3, col = "red", ylab = "", xlab = "uker framover", main = paste("Merd",  i, sep = " "), axes = F, cex.lab = 1.8, cex.main = 1.8)
        axis(1, c(0,1,2,3), c(0,1,2,3))
        axis(2)
        lines(xtid[-1], pdat[c(3,6,9),i], type = 'l')
        lines(xtid[-1], pdat[c(4,7,10),i], type = 'l')
        legend(x = "topleft", c(paste("75%: ", round(p75U1[i],1), ", ", round(p75U2[i],1), ", ", round(p75U3[i],1)), paste("50%: ", round(p50U1[i],1), ", ", round(p50U2[i],1), ", ", round(p50U3[i],1)), paste("25%: ", round(p25U1[i],1), ", ", round(p25U2[i],1), ", ", round(p25U3[i],1))))

      }
    }
  }
}


#' @noRd
countHele <- function(MerderM1, LokM1, VektMerd, LM1, ReFis, LnSmP){
  LogAdd <- 0.1
  co <- ParametereHeleCount[1]+(ParametereHeleCount[2]*log(MerderM1+LogAdd))+ (ParametereMerdCount[3]*(log(MerderM1+LogAdd))^2)+(ParametereHeleCount[4]*log(LokM1+LogAdd))+
    (ParametereHeleCount[5]*VektMerd) + (ParametereHeleCount[6]*LM1) +(ParametereHeleCount[7]*LnSmP)+ (ParametereHeleCount[8]*ReFis)
  return(co)
}

#' @noRd
countMerd <- function(MerderM1, VektMerd, LM1, ReFis, LnSmP){
  LogAdd <- 0.1
  co <- ParametereMerdCount[1]+(ParametereMerdCount[2]*log(MerderM1+LogAdd))+ (ParametereMerdCount[3]*(log(MerderM1+LogAdd))^2)+
    (ParametereMerdCount[4]*VektMerd) + (ParametereMerdCount[5]*LM1) +(ParametereMerdCount[6]*LnSmP)+ (ParametereMerdCount[7]*ReFis)
  return(co)
}

#' @noRd
zeroHele <- function(MerderM1, LokM1, VektMerd, LM1,  ReFis, LnSmP){
  LogAdd <- 0.1
  ze <- ParametereHeleZero[1]+(ParametereHeleZero[2]*log(MerderM1+LogAdd))+(ParametereHeleZero[3]*log(LokM1+LogAdd))+
    (ParametereHeleZero[4]*VektMerd) + (ParametereHeleZero[5]*LM1) +(ParametereHeleZero[6]*LnSmP)+ (ParametereHeleZero[7]*ReFis)
  return(ze)
}

#' @noRd
zeroMerd <- function(MerderM1, VektMerd, LM1,  ReFis, LnSmP){
  LogAdd <- 0.1
  ze <- ParametereMerdZero[1]+(ParametereMerdZero[2]*log(MerderM1+LogAdd))+
    (ParametereMerdZero[3]*VektMerd) + (ParametereMerdZero[4]*LM1) +(ParametereMerdZero[5]*LnSmP)+ (ParametereMerdZero[6]*ReFis)
  return(ze)
}


#' @noRd
trekk <- function(p0, pC, Theta){
  e0 <- rbinom(1, 1, p0)
  if(e0 == 1){
    return(0)
  } else {
    return(MASS::rnegbin(1,pC,Theta))
  }
}
