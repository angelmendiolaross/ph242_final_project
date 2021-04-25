# ##############
# Estimator A
# Potential Multi-colinearity (full model, MSM)
SL.glm.EstA<- function(Y, X, newX, family) {
  if(family$family=='gaussian'){
  	 fit.glm<- glm(infection_rate~population+population_density+white+african_american+asian_american+hispanic+median_hh_income+pct_unemployed+pct_college_associate+A, data=df, family=family, cluster=state)
    pred <- predict(fit.glm, newdata=newX, type='response')
    fit<- list(object=fit.glm)
    qn1_1=predict(estimator_1, txt, type="response")
    qn1_0=predict(estimator_1, control, type="response")
   }
  
  out <- list(pred=pred, fit=fit)
  class(out$fit) <- c('SL.glm.EstA')
  return(out)
}
#############
# Estimator B
# No Multi-colinearity (-white model, MSM)
SL.glm.EstB<- function(Y, X, newX, family) {
  if(family$family=='gaussian'){
    fit.glm<- glm(infection_rate~population+population_density+african_american+asian_american+hispanic+median_hh_income+pct_unemployed+pct_college_associate+A, data=df, family=family, cluster=state)
    pred <- predict(fit.glm, newdata=newX, type='response')
    fit<- list(object=fit.glm)

  }
  
  out <- list(pred=pred, fit=fit)
  class(out$fit) <- c('SL.glm.EstB')
  return(out)
}
#####################
# Estimator C
# Potential Multi-colinearity (full model, saturated SCM)
SL.glm.EstB<- function(Y, X, newX, family) {
  if(family$family=='gaussian'){
    fit.glm<- glm(infection_rate~population+population_density+white+african_american+asian_american+hispanic+median_hh_income+pct_unemployed+pct_college_associate+as.factor(A), data=df, family=family, cluster=state)
    pred <- predict(fit.glm, newdata=newX, type='response')
    fit<- list(object=fit.glm)
  }
  
  out <- list(pred=pred, fit=fit)
  class(out$fit) <- c('SL.glm.EstC')
  return(out)
}

#####################
# Estimator D
# No Multi-colinearity (-white model, saturated SCM)
SL.glm.EstB<- function(Y, X, newX, family) {
  if(family$family=='gaussian'){
    fit.glm<- glm(infection_rate~population+population_density+african_american+asian_american+hispanic+median_hh_income+pct_unemployed+pct_college_associate+as.factor(A), data=df, family=family, cluster=state)
    pred <- predict(fit.glm, newdata=newX, type='response')
    fit<- list(object=fit.glm)
  }
  
  out <- list(pred=pred, fit=fit)
  class(out$fit) <- c('SL.glm.EstD')
  return(out)
}

#####################
# Estimator E
# Potential Multi-colinearity (full model, MSM) + pop^2s
SL.glm.EstB<- function(Y, X, newX, family) {
  if(family$family=='gaussian'){
    fit.glm<- glm(infection_rate~population+populationsq+population_density+population_densitysq+white+african_american+asian_american+hispanic+median_hh_income+pct_unemployed+pct_college_associate+A, data=df, family=family, cluster=state)
    pred <- predict(fit.glm, newdata=newX, type='response')
    fit<- list(object=fit.glm)
  }
  
  out <- list(pred=pred, fit=fit)
  class(out$fit) <- c('SL.glm.EstE')
  return(out)
}

#####################
# Estimator F
# No Multi-colinearity (-white model, MSM) + pop^2s
SL.glm.EstB<- function(Y, X, newX, family) {
  if(family$family=='gaussian'){
    fit.glm<- glm(infection_rate~population+populationsq+population_density+population_densitysq+african_american+asian_american+hispanic+median_hh_income+pct_unemployed+pct_college_associate+A, data=df, family=family, cluster=state)
    pred <- predict(fit.glm, newdata=newX, type='response')
    fit<- list(object=fit.glm)
  }
  
  out <- list(pred=pred, fit=fit)
  class(out$fit) <- c('SL.glm.EstF')
  return(out)
}

#####################
# Estimator G
# POtential Multi-colinearity (full model, saturated SCM) + pop^2s
SL.glm.EstB<- function(Y, X, newX, family) {
  if(family$family=='gaussian'){
    fit.glm<- glm(infection_rate~population+populationsq+population_density+population_densitysq+white+african_american+asian_american+hispanic+median_hh_income+pct_unemployed+pct_college_associate+as.factor(A), data=df, family=family, cluster=state)
    pred <- predict(fit.glm, newdata=newX, type='response')
    fit<- list(object=fit.glm)
  }
  
  out <- list(pred=pred, fit=fit)
  class(out$fit) <- c('SL.glm.EstG')
  return(out)
}

#####################
# Estimator H
# No Multi-colinearity (-white model, saturated SCM) + pop^2s
SL.glm.EstB<- function(Y, X, newX, family) {
  if(family$family=='gaussian'){
    fit.glm<- glm(infection_rate~population+populationsq+population_density+population_densitysq+african_american+asian_american+hispanic+median_hh_income+pct_unemployed+pct_college_associate+as.factor(A), data=df, family=family, cluster=state)
    pred <- predict(fit.glm, newdata=newX, type='response')
    fit<- list(object=fit.glm)
  }
  
  out <- list(pred=pred, fit=fit)
  class(out$fit) <- c('SL.glm.EstH')
  return(out)
}

#####################
# Estimator I
# Potential Multi-colinearity (full model, MSM)
### CLUSTER ON BOTH STATE, COUNTY
SL.glm.EstA<- function(Y, X, newX, family) {
  if(family$family=='gaussian'){
    fit.glm<- glm(infection_rate~population+population_density+white+african_american+asian_american+hispanic+median_hh_income+pct_unemployed+pct_college_associate+A, data=df, family=family, cluster=c(state, county_name))
    pred <- predict(fit.glm, newdata=newX, type='response')
    fit<- list(object=fit.glm)
  }
  
  out <- list(pred=pred, fit=fit)
  class(out$fit) <- c('SL.glm.EstI')
  return(out)
}


###################
# Estimator HOLD FROM ORIGINAL
#SL.glm.EstD<- function(Y, X, newX, family, ...) {
#  if(family$family=='binomial') {
#  }
#  if(family$family=='gaussian'){
#  	fit.glm<- glm(infection_rate~   W1*W2*W5, data=X, family=family)
#    pred <- predict(fit.glm, newdata=newX, type='response')
#    fit<- list(object=fit.glm)
#  }
  
#  out <- list(pred=pred, fit=fit)
#  class(out$fit) <- c('SL.glm.EstD')
#  return(out)
#}