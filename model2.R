library(MASS) ## For step AIC

file <- "C:/Users/james/Desktop/School/6A/STAT 371/Project/model/cbb.csv"
data <- read.csv(file)
data$win_prct <- 100*data$W/data$G

independent_vars <-c("ADJOE", "ADJDE", "BARTHAG", "EFG_O", "EFG_D", "TOR", 
                     "TORD", "ORB", "DRB", "FTR", "FTRD", "X2P_O", "X2P_D",
                     "X3P_O", "X3P_D", "ADJ_T", "WAB")

data<-data[,c('win_prct',independent_vars)]
model.data.basic<-lm(win_prct~1,data=data) # Null model
model.data.full<-lm(win_prct~.,data=data)  # Full model
# Both forward and backward selection using AIC
select.both.AIC<-stepAIC(model.data.basic,scope=formula(model.data.full),direction='both')
select.both.AIC$anova

model<-lm(win_prct ~ WAB + EFG_O + ADJOE + TOR + ORB + DRB + TORD + ADJDE + 
             EFG_D + FTRD + ADJ_T + FTR + X3P_D + X2P_O + X3P_O + BARTHAG, data)
summary(model)


diagnostics_plots <-function(model, data) {
  n <- nrow(data)
  par(mfrow =c(2,2))
  # quantile-quantile plot
  plot(model, which = 2, pch = 19)
  # studentized residual vs index
  plot(x = 1:n, y =rstudent(model),
       main = "Studentized residuals vs index", xlab ="Index",
       ylab = "Studentized residuals", pch = 19)
  # Residuals vs predicted
  plot(x =fitted.values(model), y =resid(model),
       main = "Residuals  vs predicted values", xlab = "Fitted values",
       ylab = "Residuals", pch = 19)
  #Standardized residual vs leverage
  plot(x =hatvalues(model), y =rstandard(model), xlab = "Leverage",
       ylab = "Standardized residual",
       main = "Leverage vs index plot", pch = 19)
}

diagnostics_plots(model, data = data)



# actual final model
model <- lm(win_prct ~ ADJOE + ADJDE + TOR + TORD + ORB + DRB
            + FTR + FTRD + X2P_O + X2P_D + X3P_O + X3P_D + ADJ_T + WAB, data)
summary(model)

# robust model
library(sandwich)
library(lmtest)
coeftest(model, vcov = vcovHC(model, "HC1"))
