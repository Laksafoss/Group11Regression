
library(glmnet)
library(ggplot2)
library(gridExtra)

#DATA <- readRDS("outputs/narwhal.RDS")
DATA <- readRDS("outputs/narwhal_Minut.RDS")
#DATA <- readRDS("outputs/narwhal_TenMinut.RDS")
#DATA <- readRDS("outputs/narwhal_Diving.RDS")

DATA <- DATA[complete.cases(DATA),]


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



# Setup for Ridge regression ===================================================
n <- dim(x)[1]
p <- dim(x)[2] - 1
train.size <- round(n/10)
train <- sample(1:n,train.size,replace=FALSE)

netfitter <- function(x, y, data, train) {
  x.train <- x[train, ]
  y.train <- y[train]
  
  x.test <- x[-train, ]
  y.test <- y[-train]
  
  fit <- glmnet(x.train, y.train, family = "poisson", alpha = 0)
  cv <- cv.glmnet(x.train, y.train, family = "poisson", alpha = 0)
  rmse <- sqrt(mean((predict(fit, newx = x.test, s = cv$lambda.min) - y.test)^2))
  return(structure(list(fit = fit, cv = cv, rmse = rmse), class = "netfitter"))
}

plot.netfitter <- function(x) {
  par(mfrow=c(2,1))
  plot(x$fit, xvar = "lambda"); abline(v = log(x$cv$lambda.min))
  plot(x$cv); abline(v = log(x$cv$lambda.min))
  par(mfrow=c(1,1))
}



# Setup for normal regrssion ===================================================
residualplotter <- function(fit, n, m) {
  dat <- fit$data
  index <- sample(n*m, 1)
  plots <- lapply(seq_len(n * m), function(i) {
    if (i == index) {
      true <- ggplot2::fortify(fit)
      ggplot2::qplot(.fitted, .resid, data = true) + ggplot2::geom_smooth()
    } else {
      y <- simulate(fit)[,1]
      form <- as.formula(
        paste0("y ~ ", grep("~", fit$formula, value = T, invert = T)[2]))
      glm <- glm(form, data = cbind(y = y, dat), family = "poisson")
      Diag <- ggplot2::fortify(glm)
      ggplot2::qplot(.fitted, .resid, data = Diag) + ggplot2::geom_smooth()
    }
  })
  print(index)
  marrangeGrob(plots, ncol = n, nrow = m)
}



# Depth model ==================================================================

# Ridge regression
no <- which(colnames(x) %in% c("DiveTRUE","Acou.qua"))
fit.depth <- netfitter(x[,-no], DATA$Depth, train)
plot(fit.depth)

fit.depth.sub <- netfitter(xsub[,-no], DATA$Depth, train)
plot(fit.depth.sub)




# normal regression
fit.depth <- glm(Depth ~ Phase + Area + Ind + Los + Sun + ODBA, data = DATA, family = "poisson")
fit.depth.sub <- glm(Depth ~ Phasesub + Area + Ind + Los + Sun + ODBA, data = DATA, family = "poisson")

anova(fit.depth, fit.depth.sub, test = "LRT") # Likelihood ratio test
anova(fit.depth, fit.depth.sub, test = "Cp")  # like AIC

png("figs/DepthNormalreg")
residualplotter(fit.depth, 3,3)
dev.off()
png("figs/DepthSubNormalreg")
residualplotter(fit.depth.sub, 3,3)
dev.off()


# Click model ==================================================================

# Ridge regression
fit.click <- netfitter(x, DATA$Click, train)
plot(fit.click)

fit.click.sub <- netfitter(xsub, DATA$Click, train)
plot(fit.click.sub)


# normal regression
fit.click <- glm(Click ~ Phase + Area + Ind + Los + Sun + ODBA, data = DATA, family = "poisson")
fit.click.sub <- glm(Click ~ Phasesub + Area + Ind + Los + Sun + ODBA, data = DATA, family = "poisson")

anova(fit.click, fit.click.sub, test = "LRT") # Likelihood ratio test
anova(fit.click, fit.click.sub, test = "Cp")  # like AIC

residualplotter(fit.click, 3,3)
residualplotter(fit.click.sub, 3,3)



# Call model ===================================================================

# Ridge regression
fit.call <- netfitter(x, DATA$Call, train)
plot(fit.call)

fit.call.sub <- netfitter(xsub, DATA$Call, train)
plot(fit.call.sub)


# normal regression
fit.call <- glm(Call ~ Phase + Area + Ind + Los + Sun + ODBA, data = DATA, family = "poisson")
fit.call.sub <- glm(Call ~ Phasesub + Area + Ind + Los + Sun + ODBA, data = DATA, family = "poisson")

anova(fit.call, fit.call.sub, test = "LRT") # Likelihood ratio test
anova(fit.call, fit.call.sub, test = "Cp")  # like AIC

residualplotter(fit.call, 3,3)
residualplotter(fit.call.sub, 3,3)



# Strokerate model =============================================================

# Ridge regression
no <- which(colnames(x) %in% c("DiveTRUE","Acou.qua"))
fit.strokerate <- netfitter(x[,-no], DATA$StrokeRate, train)
plot(fit.strokerate)

fit.strokerate.sub <- netfitter(xsub[,-no], DATA$StrokeRate, train)
plot(fit.strokerate.sub)


# normal regression
fit.strokerate <- glm(StrokeRate ~ Phase + Area + Ind + Los + Sun + ODBA, data = DATA, family = "poisson")
fit.strokerate.sub <- glm(StrokeRate ~ Phasesub + Area + Ind + Los + Sun + ODBA, data = DATA, family = "poisson")

anova(fit.strokerate, fit.strokerate.sub, test = "LRT") # Likelihood ratio test
anova(fit.strokerate, fit.strokerate.sub, test = "Cp")  # like AIC

residualplotter(fit.strokerate, 3,3)
residualplotter(fit.strokerate.sub, 3,3)
