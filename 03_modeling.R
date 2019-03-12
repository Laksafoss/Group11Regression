
library(glmnet)
library(ggplot2)
library(gridExtra)
library(splines)
library(MASS)

#DATA <- readRDS("outputs/narwhal.RDS")
DATA <- readRDS("outputs/narwhal_Minut.RDS")
#DATA <- readRDS("outputs/narwhal_TenMinut.RDS")
#DATA <- readRDS("outputs/narwhal_Diving.RDS")

DATA <- DATA[,c("Ind", "Start", "Depth", "Seismik", "Phase", "Area", "Acou.qua", 
                "Dist.to.shore", "CallSum", "ClickBi", "ODBA", "Strokerate", 
                "Los", "Sun")]
DATA <- DATA[complete.cases(DATA),]


# if the factor is order the model matrix uses polynomials - not what we want !
DATA$Phasesub <- DATA$Phase <- factor(DATA$Phase, ordered = F) 
levels(DATA$Phasesub) <- list("B" = c("B"),
                              "T" = c("T0", "T1", "T2", "T3", "T4", "T5"),
                              "I" = c("I0", "I1", "I2", "I3", "I4", "I5"))
DATA <- DATA[,-which(colnames(DATA)=="Phase")]

# Setup for normal regression ===================================================
residualplotter <- function(fit, n, m) {
  dat <- fit$data
  index <- sample(n*m, 1)
  f <- fit$family
  plots <- lapply(seq_len(n * m), function(i) {
    if (i == index) {
      true <- ggplot2::fortify(fit)
      ggplot2::qplot(.fitted, .resid, data = true) + ggplot2::geom_smooth()
    } else {
      y <- simulate(fit)[,1]
      form <- as.formula(
        paste0("y ~ ", grep("~", fit$formula, value = T, invert = T)[2]))
      glm <- glm(form, data = cbind(y = y, dat), family = f)
      Diag <- ggplot2::fortify(glm)
      ggplot2::qplot(.fitted, .resid, data = Diag) + ggplot2::geom_smooth()
    }
  })
  print(index)
  marrangeGrob(plots, ncol = n, nrow = m)
}



# Depth model ==================================================================

# First model
fit.depth <- glm(Depth ~ Phasesub + Area + Ind + Los + Sun + ODBA+ Dist.to.shore, 
                 data = DATA, family = Gamma(link = "log"))

png("figs/GammaLogDepth.png")
residualplotter(fit.depth, 3,3)
dev.off()

# ns(ODBA, df = 3) giver ikke noget fornuftigt
# erstatte Sun med ns(Start, df = 7) giver ikke noget fornuftigt
# negative.binomial(theta) for theta = 2,5,7 giver ikke noget fornuftigt

# dummy model for testing ideas
fit.depth.dummy <- glm(Depth ~ Phasesub + Area + Ind + Los + Sun + 
                         ns(Start, df = 7) + ODBA+ Dist.to.shore, 
                       data = DATA, family = negative.binomial(7))
residualplotter(fit.depth.dummy, 2,2)

# ----------------------------------------------------------------</depth model>



# Click model ==================================================================

# First model
fit.click <- glm(ClickBi ~ Phasesub + Area + Ind + Los + Sun + ODBA + 
                   Dist.to.shore + Acou.qua, data = DATA, family = "binomial")

png("figs/ClickBiBin.png")
residualplotter(fit.click, 3,3)
dev.off()

# ns(Dist.to.shore, df = 3) forbedrer ikke smootheren i residual plottet
# ns(ODBA, df = n) ser lovende ud, har kigget på n=2,3,4. 
# fjern Sun tilføj ns(Start, df = n) forbedrer ikke smootheren for n = 6,7


# dummy model for testing ideas
fit.click.dummy <- glm(ClickBi ~ Phasesub + Area + Ind + Los + ns(Start, df = 6) + ODBA + 
                   Dist.to.shore + Acou.qua, 
                   data = DATA, family = "binomial")
residualplotter(fit.click.dummy, 3, 3)

# ----------------------------------------------------------------</click model>



# Call model ===================================================================

# First model
fit.call <- glm(CallSum ~ Phasesub + Area + Ind + Los + Sun + ODBA + Dist.to.shore + Acou.qua, 
                data = DATA, family = "poisson")

png("figs/CallSumPoisson.png")
residualplotter(fit.call, 3,3)
dev.off()



# dummy model for testing ideas
fit.call.dummy <- glm(CallSum ~ Phasesub + Area + Ind + Los + Sun + ODBA + Dist.to.shore + Acou.qua, 
                data = DATA, family = "poisson")
residualplotter(fit.call.dummy, 2, 2)

# -----------------------------------------------------------------</call model>



# Strokerate model =============================================================

# First model
fit.strokerate <- glm(Strokerate ~ Phasesub + Area + Ind + Los + Sun + ODBA + Dist.to.shore, 
                      data = DATA, family = "poisson")

png("figs/PoissonStrokerate.png")
residualplotter(fit.strokerate, 3,3)
dev.off()



# dummy model for testing ideas
fit.strokerate.dummy <- glm(Strokerate ~ Phasesub + Area + Ind + Los + Sun + ODBA + Dist.to.shore, 
                      data = DATA, family = "poisson")
residualplotter(fit.strokerate.dummy, 2,2)

# -----------------------------------------------------------</strokerate model>





## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## ##                                                                      ## ##
## ##                    R I D G E   R E G R E S S I O N                   ## ##
## ##                                                                      ## ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##



## create the desired model matrices
#x <- model.matrix(~ Phase + Area + Ind + Los + Sun + ODBA + Dive + Acou.qua,
#                  data = DATA)
#xsub <- model.matrix(~ Phasesub + Area + Ind + Los + Sun + ODBA + Dive + Acou.qua,
#                     data = DATA)



## Setup for Ridge regression ===================================================
#n <- dim(x)[1]
#p <- dim(x)[2] - 1
#train.size <- round(n/10)
#train <- sample(1:n,train.size,replace=FALSE)

#netfitter <- function(x, y, data, train) {
#  x.train <- x[train, ]
#  y.train <- y[train]
#  
#  x.test <- x[-train, ]
#  y.test <- y[-train]
#  
#  fit <- glmnet(x.train, y.train, family = "Gamma", alpha = 0)
#  cv <- cv.glmnet(x.train, y.train, family = "Gamma", alpha = 0)
#  rmse <- sqrt(mean((predict(fit, newx = x.test, s = cv$lambda.min) - y.test)^2))
#  return(structure(list(fit = fit, cv = cv, rmse = rmse), class = "netfitter"))
#}

#plot.netfitter <- function(x) {
#  par(mfrow=c(2,1))
#  plot(x$fit, xvar = "lambda"); abline(v = log(x$cv$lambda.min))
#  plot(x$cv); abline(v = log(x$cv$lambda.min))
#  par(mfrow=c(1,1))
#}



# Depth model ==================================================================

# Ridge regression
#no <- which(colnames(x) %in% c("DiveTRUE","Acou.qua"))
#fit.depth <- netfitter(x[,-no], DATA$Depth, train)
#plot(fit.depth)

#fit.depth.sub <- netfitter(xsub[,-no], DATA$Depth, train)
#plot(fit.depth.sub)


# Click model ==================================================================

# Ridge regression
#fit.click <- netfitter(x, DATA$Click, train)
#plot(fit.click)

#fit.click.sub <- netfitter(xsub, DATA$Click, train)
#plot(fit.click.sub)



# Call model ===================================================================

# Ridge regression
#fit.call <- netfitter(x, DATA$Call, train)
#plot(fit.call)

#fit.call.sub <- netfitter(xsub, DATA$Call, train)
#plot(fit.call.sub)




# Strokerate model =============================================================

# Ridge regression
#no <- which(colnames(x) %in% c("DiveTRUE","Acou.qua"))
#fit.strokerate <- netfitter(x[,-no], DATA$StrokeRate, train)
#plot(fit.strokerate)

#fit.strokerate.sub <- netfitter(xsub[,-no], DATA$StrokeRate, train)
#plot(fit.strokerate.sub)

