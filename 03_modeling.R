
#library(glmnet)
library(ggplot2)
library(gridExtra)
library(splines)
library(MASS)

#DATA <- readRDS("outputs/narwhal.RDS")
DATA <- readRDS("outputs/narwhal_Minut.RDS")
#DATA <- readRDS("outputs/narwhal_TenMinut.RDS")
#DATA <- readRDS("outputs/narwhal_Diving.RDS")
summary(DATA)

DATA <- DATA[,c("Ind", "Start", "Depth", "Seismik", "Phase", "Area", "Acou.qua", 
                "Dist.to.shore", "CallSum", "ClickBi", "ODBA", "Strokerate", 
                "Los", "Sun", "DaytimePeriodic")]
DATA <- DATA[complete.cases(DATA),]


# if the factor is order the model matrix uses polynomials - not what we want !
DATA$Phasesub <- DATA$Phase <- factor(DATA$Phase, ordered = F) 
levels(DATA$Phasesub) <- list("B" = c("B"),
                              "T" = c("T0", "T1", "T2", "T3", "T4", "T5"),
                              "I" = c("I0", "I1", "I2", "I3", "I4", "I5"))
DATA <- DATA[,-which(colnames(DATA)=="Phase")]

# Setup for normal regression ===================================================
binScale <- ggplot2::scale_fill_continuous(breaks = c(1, 10, 100, 1000), 
                                           low = "gray80", high = "black",
                                           trans = "log", guide = "none")
residualplotter <- function(fit, n, m) {
  dat <- fit$data
  index <- sample(n*m, 1)
  f <- fit$family
  plots <- lapply(seq_len(n * m), function(i) {
    if (i == index) {
      true <- ggplot2::fortify(fit)
      ggplot2::qplot(.fitted, .resid, data = true, geom = "hex") +
        stat_binhex(bins = 25) + binScale + ggplot2::geom_smooth()
    } else {
      y <- simulate(fit)[,1]
      form <- as.formula(
        paste0("y ~ ", grep("~", fit$formula, value = T, invert = T)[2]))
      glm <- glm(form, data = cbind(y = y, dat), family = f)
      Diag <- ggplot2::fortify(glm)
      ggplot2::qplot(.fitted, .resid, data = Diag, geom ="hex") + 
        stat_binhex(bins = 25) + binScale + ggplot2::geom_smooth()
    }
  })
  print(index)
  marrangeGrob(plots, ncol = n, nrow = m)
}


splineeffect <- function(fit) {
  invLink <- fit$family$linkinv
  predFrame <- expand.grid(
    Phasesub = c("B", "I", "T"),
    Area = c("IG"),
    Ind = c("Helge", "Thor"),
    Los = c(1),
    DaytimePeriodic = c(1.4),
    ODBA = seq(0.011, 0.78, length.out = 250),
    Dist.to.shore = median(DATA$Dist.to.shore))
  
  if ("Acou.qua" %in% names(fit$coefficients)) {
    predFrame <- cbind(predFrame, "Acou.qua" = 1)
  }
                          
  pred <- predict(fit, newdata = predFrame, se.fit = TRUE)
  predFrame <- cbind(predFrame, pred)
  
  qplot(ODBA, invLink(fit), data = predFrame, geom = "line", color = Phasesub) +
    geom_ribbon(aes(ymin = invLink(fit - 2 * se.fit), 
                    ymax = invLink(fit + 2 * se.fit), 
                    fill = Phasesub), alpha = 0.3) +
    facet_wrap(~Ind) +  
    ylab(names(fit$model)[1])
}

# Depth model ==================================================================

# First model
fit.depth <- glm(Depth ~ Phasesub + Area + Ind + Los + DaytimePeriodic + ODBA + Dist.to.shore, 
                 data = DATA, family = Gamma(link = "log"))

png("figs/GammaLogDepth.png")
residualplotter(fit.depth, 3,3)
dev.off()

# ns(ODBA, df = n) n=1 giver ikke noget fornuftigt, n=2 måske en ide, n=3,4,... fejler
# ns(Dist.to.shore, df =n) for n=1,2,3,4 giver ikke noget fornuftingt
# ns(DaytimePeriodic, df = n) for n=1,2,3,4 giver ikke noget fornuftigt
# negative.binomial(theta) for theta = 2,3,4 giver ikke noget fornuftigt

# dummy model for testing ideas
fit.depth.dummy <- glm(Depth ~ Phasesub + Area + Ind + Los + DaytimePeriodic + 
                         ODBA + Dist.to.shore, 
                       data = DATA, family = negative.binomial(2))
residualplotter(fit.depth.dummy, 2,2)

# ----------------------------------------------------------------</depth model>



# Click model ==================================================================

# First model
fit.click <- glm(ClickBi ~ Phasesub + Area + Ind + Los + DaytimePeriodic + 
                   ODBA + Dist.to.shore + Acou.qua, data = DATA, family = "binomial")

png("figs/ClickBiBin.png")
residualplotter(fit.click, 3,3)
dev.off()

# Second model - spline on ODBA
fit.click.sp <- glm(ClickBi ~ -1 + Phasesub + Area + Ind + Los + DaytimePeriodic + 
                   ns(ODBA, df=4) + Dist.to.shore + Acou.qua, 
                 data = DATA, family = "binomial")

png("figs/ClickBiBinSP.png")
residualplotter(fit.click.sp, 3,3)
dev.off()

png("figs/ClickBiBinSPeffect.png")
splineeffect(fit.click.sp)
dev.off()

# ns(ODBA, df = n) n=2,3,4,5 forbedrer
# ns(Dist.to.shore, df =n) for n=2,3,4,5 forbedere ikke
# ns(DaytimePeriodic, df=n) for n=2,3,4 forbedrer ikke


# dummy model for testing ideas
fit.click.dummy <- glm(ClickBi ~ Phasesub + Area + Ind + Los + ns(DaytimePeriodic, df=4) + 
                         ODBA + Dist.to.shore + Acou.qua, 
                   data = DATA, family = "binomial")
residualplotter(fit.click.dummy, 3, 3)

# ----------------------------------------------------------------</click model>


# Call model ===================================================================

# First model
fit.call <- glm(CallSum ~ Phasesub + Area + Ind + Los + DaytimePeriodic + ODBA +
                  Dist.to.shore + Acou.qua, 
                data = DATA, family = "poisson")

png("figs/CallSumPoisson.png")
residualplotter(fit.call, 3,3)
dev.off()

# ns(ODBA, df=n) for n=2,3,4,5 forbedrer ikke 
# ns(Dist.to.shore, df=n) for n=2,3,4,5 forbedrer måske for højere df ??
# ns(DaytimePeriodic, df=n) for n=2,3,4,5 forbedrer ikke
# negative.binomial(theta) for theta=2,3,5 forbedrer ikke

# dummy model for testing ideas
fit.call.dummy <- glm(CallSum ~ Phasesub + Area + Ind + Los + DaytimePeriodic + 
                        ODBA + Dist.to.shore + Acou.qua, 
                data = DATA, family = negative.binomial(2))
residualplotter(fit.call.dummy, 2, 2)

# -----------------------------------------------------------------</call model>



# Strokerate model =============================================================

# First model
fit.strokerate <- glm(Strokerate ~ Phasesub + Area + Ind + Los + DaytimePeriodic + 
                        ODBA + Dist.to.shore, 
                      data = DATA, family = "poisson")

png("figs/PoissonStrokerate.png")
residualplotter(fit.strokerate, 3,3)
dev.off()


# Second model - spline on ODBA, df = 4
fit.strokerate.sp <- glm(Strokerate ~ -1 + Phasesub + Area + Ind + Los + DaytimePeriodic + 
                        ns(ODBA, df=4)+ Dist.to.shore, 
                      data = DATA, family = "poisson")

png("figs/PoissonStrokerateSP.png")
residualplotter(fit.strokerate.sp, 3,3)
dev.off()

png("figs/PoissonStrokerateSPeffect.png")
splineeffect(fit.strokerate.sp)
dev.off()

# ns(ODBA, df=n) for n=3,4,5 virker ret godt !
# ns(Dist.to.shore, df=n) for n=2,3,4,5 ingen forbedring
# ns(DaytimePeriodic, df=n)  for n=2,3,4,5 ingen forbedring
# negative.binomial(theta) for theta=2,3,5
# Gamma(link = "log") forbedrer ikke - nogenlunde samme konklusioner


# dummy model for testing ideas
fit.strokerate.dummy <- glm(Strokerate ~ Phasesub + Area + Ind + Los + DaytimePeriodic + 
                            ODBA + Dist.to.shore, 
                      data = DATA, family = negative.binomial(2))
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

