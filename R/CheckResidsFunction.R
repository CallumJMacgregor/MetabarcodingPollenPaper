chkres <- function(model,variable1,variable2) {
  sresid <- resid(model, type = "deviance")
  hist(sresid)
  fitted.glmm <- fitted(model, level=1)        # Extract the fitted (predicted) values
  plot(sresid ~ fitted.glmm)                   # Check for homoscedasticity
  plot(model)                                  # gives a heteroscedasticity plot #fans out, not good
  require(arm)
  binnedplot(fitted(model),resid(model))
  plot(sresid ~ variable1)
  plot(sresid ~ variable2)
}




chkres.PQL <- function(model,variable1,variable2) {
  plot(model)
  presid <- resid(model, type = "pearson")
  hist(presid)
  fitted.glmm <- fitted(model, level=1)        # Extract the fitted (predicted) values
  plot(presid ~ fitted.glmm)                   # Check for homoscedasticity
  plot(presid ~ variable1)
  plot(presid ~ variable2)
}


chkres.zi <- function(model,variable1,variable2) {
  presid <- residuals(model)
  hist(presid)
  fitted.glmm <- fitted(model, level=1)        # Extract the fitted (predicted) values
  plot(presid ~ fitted.glmm)                   # Check for homoscedasticity
  require(arm)
  binnedplot(fitted.glmm,presid)
  plot(presid ~ variable1)
  plot(presid ~ variable2)
}
