
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
test <- setdiff(1:n,train)
rmse <- function(x,y) sqrt(mean( (x-y)^2 ))

# Depth model
x.train <- x[train,-which(colnames(x) %in% c("DiveTRUE","Acou.qua"))]
y.train <- DATA[train, "Depth"]
fit <- glmnet(x.train, y.train, family = "poisson", alpha = 0)
cv <- cv.glmnet(x.train, y.train, family = "poisson", alpha = 0)
plot(cv); abline(v = log(cv$lambda.min))
plot(fit, xvar = "lambda"); abline(v = log(cv$lambda.min))
which.max(fit$beta[,length(fit$lambda)])

# Click model
x.train <- x[train, ]
y.train <- DATA[train, "Click"]
fit <- glmnet(x.train, y.train, family = "poisson", alpha = 0)
cv <- cv.glmnet(x.train, y.train, family = "poisson", alpha = 0)
plot(cv); abline(v = log(cv$lambda.min))
plot(fit, xvar = "lambda"); abline(v = log(cv$lambda.min))
which.max(fit$beta[,length(fit$lambda)])


# Call model
x.train <- x[train, ]
y.train <- DATA[train, "Call"]
fit <- glmnet(x.train, y.train, family = "poisson", alpha = 0)
cv <- cv.glmnet(x.train, y.train, family = "poisson", alpha = 0)
plot(cv); abline(v = cv$lambda.min)
plot(fit, xvar = "lambda"); abline(v = cv$lambda.min)
which.max(fit$beta[,length(fit$lambda)])



# Strokerate model
x.train <- x[train,-which(colnames(x) %in% c("DiveTRUE","Acou.qua"))]
y.train <- DATA[train, "StrokeRate"]
fit <- glmnet(x.train, y.train, family = "poisson", alpha = 0)
cv <- cv.glmnet(x.train, y.train, family = "poisson", alpha = 0)
plot(cv); abline(v = log(cv$lambda.min))
plot(fit, xvar = "lambda"); abline(v = log(cv$lambda.min))
which.max(fit$beta[,length(fit$lambda)])
