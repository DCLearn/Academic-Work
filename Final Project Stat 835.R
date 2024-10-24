library(lmtest )
library(pROC)
library(gtools)
library(logistf)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(arm)
library(cowplot)

games=read.csv("C:\\Users\\DavetteUser\\Desktop\\KUMC Data Science\\STAT 835 Categorical\\NBA Games Data\\games.csv")
colnames(games) #load data and retrieve column names

a = data.frame(games$HOME_TEAM_WINS, #shorten the data set
      games$FG_PCT_home,
      games$AST_home,
      games$REB_home,
      games$FG_PCT_away,
      games$AST_away,
      games$REB_away)
head(a)
table(a$games.HOME_TEAM_WINS)

winnerTF= 1:(2*length(a$games.AST_away)) #make vectors for new data set to show all wins and loss in the same column 
FG=1:(2*length(a$games.HOME_TEAM_WINS))
AST=1:(2*length(a$games.HOME_TEAM_WINS))
REB=1:(2*length(a$games.HOME_TEAM_WINS))

winner=data.frame(winnerTF,FG,AST,REB) #create a new dataframe to reflect teams' wins and losses regardless of away/home
head(winner)

for (i in (1:length(a$games.HOME_TEAM_WINS))){ #loop through all the rows
  winner$FG[i] = a$games.FG_PCT_home[i] #insert home FG percentage in the same row for new dataframe
  winner$FG[i+length(a$games.HOME_TEAM_WINS)] = a$games.FG_PCT_away[i] #insert away FG percentage at the end of the dataframe
  winner$AST[i] = a$games.AST_home[i]
  winner$AST[i+length(a$games.HOME_TEAM_WINS)] = a$games.AST_away[i]
  winner$REB[i] = a$games.REB_home[i]
  winner$REB[i+length(a$games.HOME_TEAM_WINS)] = a$games.REB_away[i]
  if(a$games.HOME_TEAM_WINS[i]==0){
      winner$winnerTF[i] = a$games.HOME_TEAM_WINS[i] #home #loser
      winner$winnerTF[i+length(a$games.HOME_TEAM_WINS)] = 1 #away #winner
   }else{ # home winner #away loser
     winner$winnerTF[i] = a$games.HOME_TEAM_WINS[i] #home winner
     winner$winnerTF[i+length(a$games.HOME_TEAM_WINS)] = 0 #away loser
   }
}

winner=na.omit(winner) #omit rows with NA values

length(winner[,1])

correlation = cor(winner) #assess correlation between explanatory variables

par(mfrow = c(2, 2), mai = c(0.35,0.35,0.35,0.35), ps=7)
plot(y=jitter(winner$winnerTF,.05), x=winner$REB, ylab = "Win (yes/no)", xlab = "Rebounds", main = "Wins vs Rebounds")
plot(y=jitter(winner$winnerTF,.05), x=winner$AST, ylab = "Win (yes/no)", xlab = "Assists", main = "Wins vs Assists")
plot(y=jitter(winner$winnerTF,.05), x=winner$FG, ylab = "Win (yes/no)", xlab = "Field Goal Percentage", main = "Wins vs Field Goal Percentage")


meansandsdevs= data.frame("Mean(Losers)" = 
             c(mean(winner$REB[which(winner$winnerTF==0)]),
               mean(winner$AST[which(winner$winnerTF==0)]),
               mean(winner$FG[which(winner$winnerTF==0)])),
          "Standard Deviation (Losers)" = 
             c(sd(winner$REB[which(winner$winnerTF==0)]),
             sd(winner$AST[which(winner$winnerTF==0)]),
             sd(winner$FG[which(winner$winnerTF==0)])),                                      
          "Mean (Winners)" = 
            c(mean(winner$REB[which(winner$winnerTF==1)]),
              mean(winner$AST[which(winner$winnerTF==1)]),
              mean(winner$FG[which(winner$winnerTF==1)])),
          "Standard Deviation (Winner)" = 
            c(sd(winner$REB[which(winner$winnerTF==1)]),
              sd(winner$AST[which(winner$winnerTF==1)]),
              sd(winner$FG[which(winner$winnerTF==1)])), row.names = c("Rebounds", "Assists","Field Goal Percentage"))

null = glm(winnerTF ~ 1, data=winner, family=binomial(link="logit")) #intercept-only model

z11=glm(winnerTF ~ REB+FG+AST+REB*FG+AST*REB+FG*AST, data=na.omit(winner), family=binomial(link="logit"))

step(null,
     scope=list(lower=formula(null),upper=formula(z11)),
     direction="forward")

step(z11,direction="backward")

step(null,
     scope=list(lower=formula(null),upper=formula(z11)),
     direction="both")

step(null,
     scope=list(lower=formula(null),upper=formula(z11)),
     direction="both")

z11sum=summary(z11)
z11sum
inv.logit(z11$coefficients)
qt(p=0.025, df = 53103) #1.96

lower=z11sum$coefficients[,1]-1.96*z11sum$coefficients[,2] #prediciton intervals
upper= z11sum$coefficients[,1]+1.96*z11sum$coefficients[,2]
inv.logit(cbind(lower, upper))

z11conf=inv.logit(confint(z11))

lrtest(z11,null)

z11$deviance #residual deviance
z11$df.residual #53103 observations minus 6 explnatory variables
1-pchisq(z11$null.deviance- z11$deviance,z11$df.null-z11$df.residual) #no change in deviance . Model is no better than null model
1-pchisq(z11$deviance,z11$df.residual) # p value fo rdevainec goodness of fit test #suggests it may not fit well
anova(z11, test="Chisq")

1-pchisq(z21$null.deviance- z21$deviance,z21$df.null-z21$df.residual) #no change in deviance . Model is no better than null model
1-pchisq(z21$deviance,z21$df.residual) 

par(mfrow = c(2, 2))
plot((z11))

#plot(anova(z11))


# z11sum = summary(z11)
# z11sum$family
# z11sum$coefficients
# inv.logit(z11sum$coefficients)[,1]
# 


plot(fitted(z11)~winner$REB, ylab = "Fitted Values (Probability)", xlab = "Rebounds", main = "Fitted Values vs Rebounds") #logarithmic form
plot(fitted(z11)~winner$FG, ylab = "Fitted Values (Probability)", xlab = "Field Goal Percentage", main = "Fitted Values vs Field Goal Percentage")
plot(fitted(z11)~winner$AST, ylab = "Fitted Values (Probability)", xlab = "Assists", main = "Fitted Values vs Assists")

rocplot = roc(winnerTF~fitted(z11), data = winner) #ROC plot
plot.roc(rocplot, legacy.axes = TRUE)
abline(v=1)
abline(h=1)
auc(rocplot)

rocplot1 = roc(winnerTF~null, data = winner)
plot.roc(rocplot1, legacy.axes = TRUE)
abline(v=1)
abline(h=1)
auc(rocplot1)

par(mfrow = c(2, 2), mai = c(0.35,0.35,0.35,0.35), ps=7)
plot(y=fitted(z11), x=winner$REB, ylab = "Win (yes/no)", xlab = "Rebounds", main = "Wins vs Rebounds", col = )
plot(y=fitted(z11), x=winner$AST, ylab = "Win (yes/no)", xlab = "Assists", main = "Wins vs Assists")
plot(y=fitted(z11), x=winner$FG, ylab = "Win (yes/no)", xlab = "Field Goal Percentage", main = "Wins vs Field Goal Percentage")

ggdf= winner
ggdf$fit = fitted(z11)

fit1=ggdf %>%
  ggplot(aes(x = REB,
             y = fit,
             color = as.factor(winnerTF)))+
  scale_colour_manual(labels=c("Loss","Win"), values=c("#1846BA55","#B8000055")) +
  geom_point()+
  xlab("Rebounds") +
  ylab("Estimated Win Probability") +
  labs(color = "Win/Loss (Actual)") 

fit1

fit2=ggdf %>%
  ggplot(aes(x = FG,
             y = fit,
             color = as.factor(winnerTF)))+
  scale_colour_manual(labels=c("Loss","Win"), values=c("#1846BA55","#B8000055")) +
  geom_point()+
  xlab("Field Goal Percentage") +
  ylab("Estimated Win Probability") +
  labs(color = "Win/Loss (Actual)") 

fit3=ggdf %>%
  ggplot(aes(x = AST,
             y = fit,
             color = as.factor(winnerTF)))+
  scale_colour_manual(labels=c("Loss","Win"), values=c("#1846BA55","#B8000055")) +
  geom_point()+
  xlab("Assists") +
  ylab("Estimated Win Probability") +
  labs(color = "Win/Loss (Actual)")

plot_grid(fit1,fit2,fit3, ncol=1)

#+
 # theme_bw() +
  #theme(plot.title = element_text(color="black", size=14, face="bold", hjust=0.50),
   #     axis.title.x = element_text(color="black", size=12, face="bold"),
    #    axis.title.y = element_text(color="black", size=12, face="bold"),
     #   legend.position="top")


#fix this part

plot(sort(fitted(z11))~seq(from=20,to=80,length.out=length(predict(z11))))
lines((fitted(z11))~(winner$REB))

summary(predict(z11))
summary(seq(from=20,to=80, length.out=length(predict(z11))))
sort(predict(z11))

plot(x=winner$REB, y=winner$winnerTF, ylim = c(0, 1), type = "l", col = "blue", xlab = "x", ylab = "Probability")
lines(x, fitted(z11), col = "red")
legend("topright", legend = c("True Logistic Function", "Fitted Logistic Regression"), 
       
       col = c("blue", "red"), lty = 1)

plot(jitter(winnerTF,0.1) ~ REB, ylim=c(0,1), pch=16, ylab="Win Probability", xlab = "Rebounds", data=winner)
data.plot=data.frame(seq(from=min(winner$REB), to = max(winner$REB),length.out = 53302))
lp = predict(z11, newdata=data.plot, se.fit=TRUE)
pred.prob = exp(lp$fit)/(1 + exp(lp$fit))
LB=lp$fit - qnorm(0.975)*lp$se.fit
UB=lp$fit + qnorm(0.975)*lp$se.fit
LB.p=exp(LB)/(1 + exp(LB))
UB.p <- exp(UB)/(1 + exp(UB))
lines(seq(from=20,to=80, length.out=length(pred.prob)),pred.prob)
lines(seq(from=20,to=80, length.out=length(LB.p)), LB.p, col="red")
lines(seq(from=20,to=80, length.out=length(UB.p)), UB.p, col="blue")


#Data frame with hp in ascending order
Predicted_data=data.frame(AST=seq(min(winner$AST), max(winner$AST),len=53302))
Predicted_data$var1 = predict(z21, Predicted_data, type="response")
# Plot Predicted data and original data points
plot(winnerTF ~ AST, data=winner)
lines(Predicted_data$var1 ~ AST, Predicted_data, lwd=2, col="green")

plot(Predicted_data$REB, sort(Predicted_data$var1))

plot(x=predict(z11), y=fitted(z11))

par(mfrow = c(2, 2), pty="m")
plot((z11))

#plot 

z11_Model_Probabilities = (predict(z11, type = c("response")))

z11_Model_Indices = predict(z11,type = c("link"))

z11_Model_Predictions <- as.data.frame(winner)
z11_Model_Predictions$z11_Prob <- z11_Model_Probabilities
z11_Model_Predictions$z11_LP <- (z11_Model_Indices)
z11_Model_Predictions$TF <- as.factor(winner$winnerTF)
z11_Model_Predictions$Label <- rep("z11",nrow(z11_Model_Predictions))

colnames(z11_Model_Predictions) 


z11_Model_Predictions %>%
  ggplot(aes(x = z11_Model_Indices,
             y = z11_Prob,
             color = TF))+
  geom_line(color="black")+
  scale_colour_manual(labels=c("Loss","Win"), values=c("#1846BA55","#B8000055")) +
  geom_point(size=5)+
  xlab("Logit Values") +
  ylab("Estimated Win Probability") +
  labs(color = "Legend") +
  scale_x_continuous( limits=c(-8,8), breaks=seq(-8,8,by=1)) +
  scale_y_continuous( limits=c(0,1), breaks=seq(0,1,by=0.1),labels = scales::percent) +
  ggtitle("Estimated Probabilities of NBA Teams' Wins") +
  theme_bw() +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust=0.50),
        axis.title.x = element_text(color="black", size=12, face="bold"),
        axis.title.y = element_text(color="black", size=12, face="bold"),
        legend.position="top")


  
adf = data.frame(predict(z11, newdata=data.plot, se.fit=TRUE),
                 inv.logit(predict(z11, newdata=data.plot, se.fit=TRUE)),
                 factor(winner$winnerTF))

# Predicted_data <- data.frame(var2=seq(
#   min(winner$winnerTF), max(winnerTF),len=53302))
# 
# 
# # Fill predicted values using regression model
# Predicted_data$var1 = predict(
#   z21, Predicted_data, type="response")
# 
# # Plot Predicted data and original data points
# plot(winnerTF ~ REB, data=winner)
# lines(var1 ~ var2, Predicted_data, lwd=2, col="green")

#confusion matrix
train = winner[1:length(a$games.HOME_TEAM_WINS),]
test = winner[(length(a$games.HOME_TEAM_WINS)+1) : (2*length(a$games.HOME_TEAM_WINS)),]


confuz11 = glm(winnerTF ~ REB + FG + AST + REB*FG + AST*REB + FG*AST, data=train, family = binomial)
confuz11


z11pred1 = predict(z11, newdata=test, type = "response")
confusion.table=table(test$winnerTF,ifelse(z11pred1> 0.5, "Win", "Loss"))

sum(confusion.table[2:3])/sum(confusion.table[1:4]) #incorrect 


sum(confusion.table[c(1,4)])/sum(confusion.table[1:4]) #correct

z21=bayesglm(winnerTF ~ REB+FG+AST+REB*FG+AST*REB+FG*AST, data=na.omit(winner))
summary(z21)


#use Bayes glm to resolve the quasi-separation

confuz21 = bayesglm(winnerTF ~ REB + FG + AST + REB*FG + AST*REB + FG*AST, data=train, family = binomial)
confuz21
z21
z11

Predictz21= predict(z21, newdata=test, type="response")
confusion.tablez21=table(test$winnerTF, ifelse(Predictz21> 0.5, "1", "0"))

sum(confusion.tablez21[2:3])/sum(confusion.tablez21[1:4]) #incorrect 


sum(confusion.tablez21[c(1,4)])/sum(confusion.tablez21[1:4]) #correct

mean(ifelse(Predictz21 > 0.5, "1", "0") == test$winnerTF)
mean(ifelse(Predictz21> 0.5, "1", "0") != test$winnerTF)

summary((z21))$aic
anova(z21)
lrtest(z21)

f_bias_reduc=summary(logistf(winnerTF ~ REB+FG+AST+REB*FG+AST*REB+FG*AST, data=na.omit(winner), control=logistf.control()))
f_lr_ac=flac(winnerTF ~ REB+FG+AST+REB*FG+AST*REB+FG*AST, data=na.omit(winner), control=logistf.control())
f_inter_c=flic(winnerTF ~ REB+FG+AST+REB*FG+AST*REB+FG*AST, data=na.omit(winner), control=logistf.control())

quas= data.frame(" Firth’s Bias-Reduced Logistic Regression (FBRLR)"=f_bias_reduc$coefficients,"Firth’s Logistic Regression With Added Covariate (FLAC)"=f_lr_ac$coefficients,"Firth’s Logistic Regression With Intercept Correction (FLIC)"=f_inter_c$coefficients, "Bayesian Generalized Linear Model With Cauchy Priors (BGLM_CP)"=z21$coefficients)
pander(quas)
