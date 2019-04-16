#' glm summary function
#' 
#' Produces Genstat like summary information for a glm
#' 
#' @m glm model
#' @dispersion numeric value for dispersion (not used)
#' 
#' @example
#' DT <- data.table(dv=c(rnorm(100,10,2),rnorm(100,20,2)),iv=as.factor(c(rep("A",100),rep("B",100))))
#' varcheck(DT,"dv","iv")
glm.sum <- function(m,dispersion=T){
  
  d.f            <- m$df.null-df.residual(m)
  deviance       <- m$null.deviance-deviance(m)
  mean.deviance  <- deviance/d.f
  deviance.ratio <- mean.deviance / (deviance(m) / df.residual(m))
  Chisq.p        <- pchisq(deviance,d.f,lower.tail=F)
  F.p            <- pf(deviance.ratio,d.f,df.residual(m),lower.tail = F)
  
  summary <- 
    rbind(Regression=data.frame(
            d.f,
            deviance,mean.deviance,
            deviance.ratio,
            Chisq.p,F.p),
          Residual=c(
            df.residual(m),
            deviance(m),
            deviance(m)/df.residual(m),
            NA,NA,NA),
          Total=c(
            m$df.null,
            m$null.deviance,
            m$null.deviance/m$df.null,
            NA,NA,NA))
  summary
}
