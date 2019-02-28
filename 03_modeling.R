
library(glmnet)

#DATA <- readRDS("outputs/narwhal.RDS")
#DATA <- readRDS("outputs/narwhal_Minut.RDS")
DATA <- readRDS("outputs/narwhal_TenMinut.RDS")
#DATA <- readRDS("outputs/narwhal_Diving.RDS")


# if the factor is order the model matrix uses polynomials - not what we want !
DATA$Phasesub <- DATA$Phase <- factor(DATA$Phase, ordered = F) 
levels(DATA$Phasesub) <- list("B" = c("B"),
                              "T" = c("T0", "T1", "T2", "T3", "T4", "T5"),
                              "I" = c("I0", "I1", "I2", "I3", "I4", "I5"))

# create the desired model matrixs
x <- model.matrix(~ Phase + Area + Ind + Los + Sun + ODBA + Dive + Acou.qua,
                  data = DATA)
xsub <- model.matrix(~ Phasesub + Area + Ind + Los + Sun + ODBA + Dive + Acou.qua,
                     data = DATA)



# setup
n <- dim(x)[1]
p <- dim(x)[2] - 1
train.size <- round(n/10)
train <- sample(1:n,train.size,replace=FALSE)


fitter <- function(x, y, train) {
  x.train <- x[train, ]
  y.train <- y[train]
  
  x.test <- x[-train, ]
  y.test <- y[-train]
  
  fit <- glmnet(x.train, y.train, family = "poisson", alpha = 0)
  cv <- cv.glmnet(x.train, y.train, family = "poisson", alpha = 0)
  rmse <- sqrt(mean((predict(fit, newx = x.test, s = cv$lambda.min) - y.test)^2))
  return(structure(list(fit = fit, cv = cv, rmse = rmse), class = "fitterglm"))
}

plot.fitterglm <- function(x) {
  par(mfrow=c(2,1))
  plot(x$fit, xvar = "lambda"); abline(v = log(x$cv$lambda.min))
  plot(x$cv); abline(v = log(x$cv$lambda.min))
  par(mfrow=c(1,1))
}


# Depth model ------------------------------------------------------------------
no <- which(colnames(x) %in% c("DiveTRUE","Acou.qua"))
fit.depth <- fitter(x[,-no], DATA$Depth, train)
plot(fit.depth)

fit.depth.sub <- fitter(xsub[,-no], DATA$Depth, train)
plot(fit.depth.sub)


# Click model ------------------------------------------------------------------
fit.click <- fitter(x, DATA$Click, train)
plot(fit.click)

fit.click.sub <- fitter(xsub, DATA$Click, train)
plot(fit.click.sub)


# Call model -------------------------------------------------------------------
fit.call <- fitter(x, DATA$Call, train)
plot(fit.call)

fit.call.sub <- fitter(xsub, DATA$Call, train)
plot(fit.call.sub)


# Strokerate model -------------------------------------------------------------
no <- which(colnames(x) %in% c("DiveTRUE","Acou.qua"))
fit.strokerate <- fitter(x[,-no], DATA$StrokeRate, train)
plot(fit.strokerate)

fit.strokerate.sub <- fitter(xsub[,-no], DATA$StrokeRate, train)
plot(fit.strokerate.sub)


