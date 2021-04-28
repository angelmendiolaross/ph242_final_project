# ##############
# Estimator A
# White included, MSM, no pop/pop density squared/logged
SL.glm.EstA<- function(Y, X, newX, family) {
  if(family$family=='gaussian'){
  	 fit.glm<- glm(infection_rate~population+population_density+white+asian_american+hispanic+median_hh_income+pct_unemployed+pct_college_associate+as.factor(month)+A, data=ObsData)
    pred <- predict(fit.glm, newdata=newX, type='response')
    fit<- list(object=fit.glm)
   }
  
  out <- list(pred=pred, fit=fit)
  class(out$fit) <- c('SL.glm.EstA')
  return(out)
}
#############
# Estimator B
# African_American included, MSM, no pop/pop density squared/logged
SL.glm.EstB<- function(Y, X, newX, family) {
  if(family$family=='gaussian'){
    fit.glm<- glm(infection_rate~population+population_density+african_american+asian_american+hispanic+median_hh_income+pct_unemployed+pct_college_associate+as.factor(month)+A, data=ObsData)
    pred <- predict(fit.glm, newdata=newX, type='response')
    fit<- list(object=fit.glm)

  }
  
  out <- list(pred=pred, fit=fit)
  class(out$fit) <- c('SL.glm.EstB')
  return(out)
}
#####################
# Estimator C
# White included, MSM, pop/pop density squared
SL.glm.EstC<- function(Y, X, newX, family) {
  if(family$family=='gaussian'){
    fit.glm<- glm(infection_rate~population+population_density+white+asian_american+hispanic+median_hh_income+pct_unemployed+pct_college_associate+as.factor(month)+populationsq+population_densitysq+A, data=ObsData)
    pred <- predict(fit.glm, newdata=newX, type='response')
    fit<- list(object=fit.glm)
  }
  
  out <- list(pred=pred, fit=fit)
  class(out$fit) <- c('SL.glm.EstC')
  return(out)
}

#####################
# Estimator D
# African_American included, MSM, pop/pop density squared
SL.glm.EstD<- function(Y, X, newX, family) {
  if(family$family=='gaussian'){
    fit.glm<-glm(infection_rate~population+population_density+african_american+asian_american+hispanic+median_hh_income+pct_unemployed+pct_college_associate+as.factor(month)+populationsq+population_densitysq+A, data=ObsData)
    pred <- predict(fit.glm, newdata=newX, type='response')
    fit<- list(object=fit.glm)
  }
  
  out <- list(pred=pred, fit=fit)
  class(out$fit) <- c('SL.glm.EstD')
  return(out)
}

#####################
# Estimator E
# White included, MSM, pop/pop density logged
SL.glm.EstE<- function(Y, X, newX, family) {
  if(family$family=='gaussian'){
    fit.glm<-glm(infection_rate~population+population_density+white+asian_american+hispanic+median_hh_income+pct_unemployed+pct_college_associate+as.factor(month)+log_population+log_popdensity+A, data=ObsData)
    pred <- predict(fit.glm, newdata=newX, type='response')
    fit<- list(object=fit.glm)
  }
  
  out <- list(pred=pred, fit=fit)
  class(out$fit) <- c('SL.glm.EstE')
  return(out)
}

#####################
# Estimator F
# African_American included, MSM, pop/pop density logged
SL.glm.EstF<- function(Y, X, newX, family) {
  if(family$family=='gaussian'){
    fit.glm<-glm(infection_rate~population+population_density+african_american+asian_american+hispanic+median_hh_income+pct_unemployed+pct_college_associate+as.factor(month)+log_population+log_popdensity+A, data=ObsData)
    pred <- predict(fit.glm, newdata=newX, type='response')
    fit<- list(object=fit.glm)
  }
  
  out <- list(pred=pred, fit=fit)
  class(out$fit) <- c('SL.glm.EstF')
  return(out)
}
