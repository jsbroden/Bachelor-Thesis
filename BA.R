setwd("~/Documents/BA/R/Data")
data <- read.csv("FINALDATA.csv", header=T)
original <- read.csv("Datensatz_BA_JuliaB.csv")

data_withNA <- read.csv("Datensatz_BA_JuliaB.csv", header = T)
data_withoutNA <- read.csv("data_CompletersOnly.csv", header = T)
phq <- read.csv("phq_score.csv", header = T)
sosu <- read.csv("sosu.csv", header = T)

data <- cbind(data_withoutNA, phq, sosu)
data <- data[!is.na(data$bsss),] # 6 NA
data <- subset(data, PRE_birth <= 1996)
write.csv(data, "FINALDATA.csv", row.names = F)

#df <- read.csv("data_n722", header=T) # dataset with excluded groups

#phq
score <- table(data$PRE_score)
score


#############
# variables #
#############
#dataset with excluded groups
#df <- subset(data_RC_sosu, PRE_education != 0)
#df <- subset(df, PRE_work != 3 & PRE_work != 4 & PRE_work != 5 & PRE_work != 7)
#df <- subset(df, PRE_relation != 3)
#df <- df[!is.na(df$bsss),]
# n = 717
#write.csv(df, "data_n717", row.names = F)

# age continuous
age <- as.numeric(data[,"PRE_birth"])
max(data$PRE_birth)
age <- (2014 - age)


# age grouped
age_g <- as.numeric(data[,"PRE_birth"])
age_g <- (2014 - age_g)
age_g <- ifelse(age_g < 30, "18-29",
                   ifelse(age_g >= 30 & age_g < 40, "30-39",
                          ifelse(age_g >= 40 & age_g < 59, "40-58",
                                 ifelse(age_g >=59, "59 or over", NA))))
age_g <- factor(age_g)
min(age) #15
max(age) #84

#age_g_ordered <- factor(age_g, levels = c("15-29", "40-59", "30-39", "60 or over"))

# sex
sex <- as.factor(data[,"PRE_sex"]) 

# sosu
sosu <- as.numeric(data[, "bsss"])

# baseline depression
pre_phq <- as.numeric(data[, "PRE_score"])

# education
#edu <- subset(data_RC_sosu, PRE_education != 0) # exclude no certificate
education <- as.factor(data[, "PRE_education"])
#education <- factor(education, levels = c(2, 5, 1, 4, 3))

# work 
# ??? combine pensioner and other
#wo <- subset(data_RC_sosu, PRE_work != 3 & PRE_work != 4 & PRE_work != 5 & PRE_work != 7)
work <- as.factor(data[,"PRE_work"]) 
#work <- factor(work, levels= c(1, 6, 2))

# relation 
# ??? combine widowed and divorced
#rel <- subset(data_RC_sosu, PRE_relation != 3)
relation <- as.factor(data[,"PRE_relation"])
#relation <- factor(relation, levels= c(2, 0, 1))

# RC
RC <- as.factor(data[, "RC"])

###########################
# dichotomised categories #
###########################
# d = dichotomised
### education ###
edu_d <- as.character(education)
edu_d[edu_d %in% c(0, 1, 2)] <- "lower_edu"
edu_d[edu_d %in% c(3, 4, 5)] <- "higher_edu"
edu_d <- factor(edu_d)

t <- table(edu_d)
addmargins(t)
round(100*prop.table(t), digits=1)

RC_by_edu_d <- table(edu_d, RC)
RC_by_edu_d
RC_by_edu_d_prop <- round(100*prop.table(RC_by_edu_d, margin = 2), digits=1)
RC_by_edu_d_prop

### work ###
work_d <- as.character(work)
work_d[work_d %in% c(1,2,3,4)] <- "Employed"
work_d[work_d %in% c(5,6,7)] <- "Unemployed"
work_d <- factor(work_d)

t <- table(work_d)
addmargins(t)
round(100*prop.table(t), digits=1)

RC_by_work_d <- table(work_d, RC)
RC_by_work_d
RC_by_work_d_prop <- round(100*prop.table(RC_by_work_d, margin = 2), digits=1)
RC_by_work_d_prop

### relation ###
rel_d <- as.character(relation)
rel_d[rel_d %in% c(0, 2, 3)] <- "Single"
rel_d[rel_d %in% c(1)] <- "Married"
rel_d <- factor(rel_d)

t <- table(rel_d)
addmargins(t)
round(100*prop.table(t), digits=1)

RC_by_rel_d <- table(rel_d, RC)
RC_by_rel_d
RC_by_rel_d_prop <- round(100*prop.table(RC_by_rel_d, margin = 2), digits=1)
RC_by_rel_d_prop

### age_g ###
t <- table(age_g)
addmargins(t)
round(100*prop.table(t), digits=1)


###########
# table 1 #
###########
#table1 <- cbind(pre_phq, age, sosu, RC) 
table1_noRC <- data[(data$RC == "no"),]
table1_RC <- data[(data$RC == "yes"),]
age_noRC <- as.numeric(table1_noRC[,"PRE_birth"])
age_noRC <- (2014 - age_noRC)
age_RC <- as.numeric(table1_RC[,"PRE_birth"])
age_RC <- (2014 - age_RC)

library(psych)
describe(table1_noRC[, c("PRE_score", "bsss")])
describe(table1_RC[, c("PRE_score", "bsss")])
describe(age_noRC)
describe(age_RC)
describe(pre_phq)
describe(sosu)

#################
# describe data #
#################
library(psych)
describe(age)
describe(pre_phq)
describe(sosu)

table1 <- cbind(pre_phq, age, sosu, RC)
table1 <- data.frame(table1)
table1_RC <- table1[(table1$RC == 2),]
table1_noRC <- table1[(table1$RC == 1),]

describe(table1_noRC[, c("pre_phq", "age", "sosu")])
describe(table1_RC[, c("pre_phq", "age", "sosu")])

### RC ###
t <- table(data$RC)
t

### phq ###
summary(pre_phq)

### age ###
# 1. distribution
hist(data$PRE_birth, ylim=c(0, 200), xlab = "year of birth")
hist(age, ylim= c(0, 200), xlim = c(0, 100))
d <- density(age)
plot(d, main="", xlab="Age in years")
min(age) #18
max(age) #84
summary(age)

?plot()

# 2. relation age and outcome
#continuous
RC_by_age <- table(age, RC) # cross tabulation
RC_by_age
RC_by_age_prop <- prop.table(RC_by_age, margin = 1)
RC_by_age_prop
odds_age <- RC_by_age_prop[, "yes"]/RC_by_age_prop[, "no"] 
odds_age
logodds_age <- log(odds_age) # calculate log odds
logodds_age
plot(rownames(RC_by_age_prop), logodds_age, ylab="logodds_of_RC", xlab="Age in years") # plot the ages found in the sample against the log odds of having RC
abline(v=30, col="blue")
abline(v=60, col="blue")
abline(v=45, col="blue")
abline(v=40, col="grey")


#categorical
#age_g_ordered <- factor(age_g, levels = c("15-29", "40-59", "30-39", "60 or over"))
RC_by_age_g <- table(age_g, RC) # g = grouped
RC_by_age_g
RC_by_age_g_prop <- prop.table(RC_by_age_g, margin = 1)
RC_by_age_g_prop
odds_age_g <- RC_by_age_g_prop[, "yes"]/RC_by_age_g_prop[, "no"]
odds_age_g
logodds_age_g <- log(odds_age_g)
dotchart(logodds_age_g, xlab = "logodds_of_RC")

#überarbeitet getauschte Achsen. Aktueller Stand: noch nicht feritg
RC_by_age_g <- table(RC, age_g) # g = grouped
RC_by_age_g
RC_by_age_g_prop <- prop.table(RC_by_age_g, margin = 2)
RC_by_age_g_prop
odds_age_g <- RC_by_age_g_prop["yes" ,]/RC_by_age_g_prop["no", ]
odds_age_g
logodds_age_g <- log(odds_age_g)
dotchart(logodds_age_g, xlab = "logodds_of_RC")


RC_by_age_g_prop <- round(100*prop.table(RC_by_age_g, margin = 2), digits = 1)
RC_by_age_g_prop

# 3. correlations between predictors 
cor.test(x=age, y=sosu, method="pearson")


### sosu ###
# 1. distribution
summary(sosu)
hist(sosu, xlim=c(10, 60))
d <- density(sosu)
plot(d, main="", xlab = "BSSS_score")

#summary(sosu$bsss_e)
#hist(sosu$bsss_e)

#summary(sosu$bsss_i)
#hist(sosu$bsss_i)

#summary(sosu$bsss_s)
#hist(sosu$bsss_s)

# 2. relation sosu and outcome
RC_by_sosu <- table(sosu, RC) 
RC_by_sosu_prop <- prop.table(RC_by_sosu, margin = 1) 
odds_sosu <- RC_by_sosu_prop[, "yes"]/RC_by_sosu_prop[, "no"] 
logodds_sosu <- log(odds_sosu)
plot(rownames(RC_by_sosu_prop), logodds_sosu, ylab = "logodds_of_RC", xlab = "BSSS_score")

RC_by_sosu_s <- table(sosu$bsss_s, phq$RC) 
RC_by_sosu_s_prop <- prop.table(RC_by_sosu_s, margin = 1) 
odds_sosu_s <- RC_by_sosu_s_prop[, "yes"]/RC_by_sosu_s_prop[, "no"] 
logodds_sosu_s <- log(odds_sosu_s)
plot(rownames(RC_by_sosu_s_prop), logodds_sosu_s)


### sex ###
# 1. distribution: bimodal
# 2. relation sex and outcome
t <- table(sex)
addmargins(t)
round(100*prop.table(t), digits=2)

RC_by_sex <- table(sex, RC, exclude = NULL)
RC_by_sex
round(100*prop.table(RC_by_sex), digits=2)

RC_by_sex_prop <- prop.table(RC_by_sex, margin = 2)
RC_by_sex_prop

odds_sex <- RC_by_sex_prop[, "yes"]/RC_by_sex_prop[, "no"]
logodds_sex <- log(odds_sex)
dotchart(logodds_sex) #not useful


### education ###
# 2. relation work and outcome
t <- table(education)
addmargins(t)
round(100*prop.table(t), digits=1)

RC_by_edu <- table(education, RC)
RC_by_edu
RC_by_edu_prop <- prop.table(RC_by_edu)
RC_by_edu_prop
odds_edu <- RC_by_edu_prop[, "yes"]/RC_by_edu_prop[, "no"]
logodds_edu <- log(odds_edu)
dotchart(logodds_edu)

RC_by_edu_prop <- round(100*prop.table(RC_by_edu, margin = 2), digits=1)
RC_by_edu_prop

### work ###
# 2. relation work and outcome
t <- table(work)
addmargins(t)
round(100*prop.table(t), digits=1)

RC_by_work <- table(work, RC, exclude = NULL)
RC_by_work
round(100*prop.table(RC_by_work), digits=1)

RC_by_work_prop <- prop.table(RC_by_work)
odds_work <- RC_by_work_prop[, "yes"]/RC_by_work_prop[, "no"]
logodds_work <- log(odds_work)
dotchart(logodds_work)

RC_by_work_prop <- round(100*prop.table(RC_by_work, margin = 2), digits=1)
RC_by_work_prop

### relation ###
# 2. relation relation and outcome
t <- table(relation)
addmargins(t)
round(100*prop.table(t), digits=1)

RC_by_relation <- table(relation, RC, exclude = NULL)
RC_by_relation
round(100*prop.table(RC_by_relation), digits=1)

RC_by_rel_prop <- prop.table(RC_by_relation)
odds_rel <- RC_by_rel_prop[, "yes"]/RC_by_rel_prop[, "no"]
logodds_rel <- log(odds_rel)
dotchart(logodds_rel)

RC_by_rel_prop <- round(100*prop.table(RC_by_relation, margin = 2), digits=1)
RC_by_rel_prop

####################
# correlations old #
####################
# Produkt Moment Korrelation: sosu x pre_phq
cor(x=sosu, y=pre_phq, method="pearson") #-0.0976 -> schwacher Zusammenhang

# Koeffizient y
install.packages("Hmisc")
library(Hmisc)
# sex x age_g
rcorr.cens(sex, age_g, outx=T)

# point-biserial correlation
# pre_phq x sex
biserial.cor(pre_phq, sex, level=0) #für Männer:
# sosu x sex
# edu_d x pre_phq
# work_d x pre_phq
# rel_d x pre_phq
# sosu x edu_d
# sosu x work_d
# sosu x rel_d

# Koeffizient phi
# edu_d x sex
phicoef(edu_d, sex)
# work_d x sex
# rel_d x sex
# work_d x edu_d
# rel_d x edu_d
# rel_d x work_d

# polyseriale Korrelation

###### checking if variables are independent with Chi^2 test and Cramers V for correlation of the test
#H0: independent, H1: dependent

# Age_g x sex
age_g_by_sex <- table(age_g, sex)
age_g_by_sex

chi2 <- chisq.test(age_g_by_sex, correct = F)
c(chi2$statistic, chi2$p.value) #p=0.11 -> independent

# Edu_d x sex
edu_d_by_sex <- table(edu_d, sex)
edu_d_by_sex

chi2 <- chisq.test(edu_d_by_sex, correct = F)
c(chi2$statistic, chi2$p.value) #p=0.19 -> independent

# work_d x sex
work_d_by_sex <- table(work_d, sex)
work_d_by_sex

chi2 <- chisq.test(work_d_by_sex, correct = F)
c(chi2$statistic, chi2$p.value) #p=0.0093 -> dependent

# rel_d x sex
rel_d_by_sex <- table(rel_d, sex)
rel_d_by_sex

chi2 <- chisq.test(rel_d_by_sex, correct = F)
c(chi2$statistic, chi2$p.value) #p=8.251091e-05 -> dependent

# age_g x edu_d
age_g_by_edu_d <- table(age_g, edu_d)
age_g_by_edu_d

chi2 <- chisq.test(age_g_by_edu_d, correct = F)
c(chi2$statistic, chi2$p.value) #dependent

# age_g x rel_d
age_g_by_rel_d <- table(age_g, rel_d)
age_g_by_rel_d

chi2 <- chisq.test(age_g_by_rel_d, correct = F)
c(chi2$statistic, chi2$p.value) #dependent

# age_g x work_d
age_g_by_work_d <- table(age_g, work_d)
age_g_by_work_d

chi2 <- chisq.test(age_g_by_work_d, correct = F)
c(chi2$statistic, chi2$p.value) #dependent

# edu_d x work_d
# edu_d x rel_d
# work_d x rel_d


######
# LR #
######
logistic <- glm(RC ~ PRE_sex, data=data, family="binomial")
summary(logistic) # sex not significant

logistic <- glm(RC ~ edu_d, family="binomial")
summary(logistic) # edu not significant

logistic <- glm(RC ~ work_d, family="binomial")
summary(logistic) # work not significant

logistic <- glm(RC ~ rel_d, family="binomial")
summary(logistic) # relation not significant

logistic <- glm(RC ~ sosu, family="binomial")
summary(logistic) # sosu not significant

logistic <- glm(RC ~ age, family="binomial")
summary(logistic) # age significant

logistic <- glm(RC ~ pre_phq, family="binomial")
summary(logistic) # pre_phq significant
# age and pre_phq only significant predictors
#######
# MLR #
#######
##### Vers 1 #####
m <- glm(phq$RC ~ sex + age + education + work + relation + sosu, family=binomial (link=logit))
summary(m)

m <- glm(phq$RC ~ sex + age + work + relation, family=binomial (link=logit))
summary(m)

# calculate McFadden's r^2
null_m <- glm(phq$RC ~ 1, family=binomial (link=logit))
summary(null_m)

R2 <- 1-logLik(m)/logLik(null_m)
R2 # 2.58% bad (14% typical) -> poor predictive power 

m <- glm(phq$RC ~ sex + age_g_v3, family = binomial (link = logit))
summary(m)

m <- glm(phq$RC ~ sex + age_g_v3 + education + work + relation + sosu$bsss, family = binomial (link = logit))
summary(m)

exp(c(-0.6179, -0.5339, -0.8981))

exp(confint(m))

##### Vers 2 #####
m <- glm(RC ~ sex + age_g + education + work + relation + sosu, family = binomial (link=logit))
summary(m)
mylogit <- glm(RC ~ relation + sosu, family=binomial (link=logit))
summary(mylogit)

# https://stats.idre.ucla.edu/r/dae/logit-regression/
confint(m) # CI using profiled log-likelihood
confint.default(m) # CI using standard errors 

# calculate McFadden's r^2
null_m <- glm(RC ~ 1, family=binomial (link=logit))
summary(null_m)
R2 <- 1-logLik(m)/logLik(null_m)
R2 # 1.27% poor predictive power

#X^2
anova(m, test="Chisq") # no significant reduce of deviance -> no improve in model fit

# test for an overall effect of age (3:5), education (6:9), work (10:11) and relation (12:13)
install.packages("aod")
library(aod)

wald.test(b=coef(m), Sigma=vcov(m), Terms=3:5) # age not significant
wald.test(b=coef(m), Sigma=vcov(m), Terms=6:9) # education not significant
wald.test(b=coef(m), Sigma=vcov(m), Terms=10:11) # work not significant
wald.test(b=coef(m), Sigma=vcov(m), Terms=12:13) #relation not significant

exp(coef(m))
exp(cbind(OR= coef(m), confint(m))) # OR and 95% CI

# predicted probabilites and ggplot2
newdata1 <- with(df, data.frame(sosu = mean(sosu), relation=factor(c(2, 0, 1))))
newdata1
newdata1$relationP <- predict(mylogit, newdata = newdata1, type = "response")
newdata1
newdata2 <- with(df, data.frame(sosu = rep(seq(from = 13, to = 52, length.out = 5), 3), relation=factor(rep(c(2, 0, 1), each=5))))
newdata3 <- cbind(newdata2, predict(mylogit, newdata=newdata2, type="link", se=T))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96*se.fit))
  UL <- plogis(fit + (1.96*se.fit))
})
head(newdata3)
library(ggplot2)
ggplot(newdata3, aes(x=sosu, y=PredictedProb)) +
  geom_ribbon(aes(ymin=LL, ymax=UL, fill=relation), alpha=0.2) +
  geom_line(aes(colour=relation), size=1)

# Model fit
with(m, null.deviance - deviance) # X^2 = 12.57
with(m, df.null - df.residual) # df = 13
with(m, pchisq(null.deviance -deviance, df.null - df.residual, lower.tail=F)) # p-value = 0.48
# model as a whole fits not significantly better than an empty model
logLik(m) # likelihood ratio test (deviance residual is -2*log likelihood)
# -489.64 (df=14)

##### Vers 3 #####
# m: all predictors
m <- glm(RC ~ sex + age_g + pre_phq + edu_d + work_d + rel_d + sosu, family = binomial (link=logit))
summary(m)
4.51e-08 = 0.0000000451 < 0.0001

exp(m$coefficients)
exp(confint(m)) # CI using profiled log-likelihood
confint.default(m) # CI using standard errors

# calculate McFadden's r^2
null_m <- glm(RC ~ 1, family=binomial (link=logit))
summary(null_m)

R2 <- 1-logLik(m)/logLik(null_m)
R2 # 0.1373 (14% typical)

# m2: relation
m2 <- glm(RC ~ sex + rel_d + sosu, family = binomial (link=logit))
summary(m2)

exp(m2$coefficients)
exp(confint(m2))

#McFadden
R2 <- 1-logLik(m2)/logLik(null_m)
R2 # 0.0035 (14% typical)

# m3: pre_phq, edu, work
m3 <- glm(RC ~ sex + age + pre_phq + edu_d + work_d, family = binomial (link=logit))
summary(m3)

# m4: relation
m4 <- glm(RC ~ sex + age + rel_d + sosu, family = binomial (link=logit))
summary(m4)

##############
# age effect #
##############
age_z <- (age - 45.32)
age_z2 <- age_z^2
age_z3 <- age_z^3

hist(age_z)
d <- density(age_z)
plot(d, main="")

hist(age_z2)
d <- density(age_z2)
plot(d, main="")

hist(age_z3)
d <- density(age_z3)
plot(d, main="")

m_age_z <- glm(RC ~ age_z + age_z2 + age_z3, family = binomial (link=logit))
summary(m_age_z)

exp(m_age_z$coefficients)
exp(confint(m_age_z))

R2 <- 1-logLik(m_age_z)/logLik(null_m)
R2 #0.0106

#
m_age_g <- glm(RC ~ sex + age_g, family = binomial (link=logit))
summary(m_age_g)

#not centralized
age_2 <- age^2
age_3 <- age^3

m_age <- glm(RC ~ age + age_2 + age_3, family = binomial (link=logit))
summary(m_age)

exp(m_age$coefficients)
exp(confint(m_age))




##################
# play with data #
##################
# M1 calculate McFadden's Pseudo R^2
ll.null <- m$null.deviance/-2
ll.proposed <- m$deviance/-2
(ll.null - ll.proposed)/ll.null # R^2 = 0.1373 -> overall effect size

#calculate p-value for R^2 using Chu-square distribution
1- pchisq(2*(ll.proposed - ll.null), df=(length(m$coefficients)-1)) #p=0 -> R^2 not due to luck

# M2 calculate McFadden's Pseudo R^2
ll.null <- m2$null.deviance/-2
ll.proposed <- m2$deviance/-2
(ll.null - ll.proposed)/ll.null # R^2 = 0.0035 -> overall effect size

#calculate p-value for R^2 using Chu-square distribution
1- pchisq(2*(ll.proposed - ll.null), df=(length(m2$coefficients)-1)) #p=0.2529 -> R^2 may be due to luck

###########################
# graph predicted prob RC #
###########################
predicted.data <- data.frame(
  probability.of.RC=m$fitted.values,
  RC=RC) #df that contains probabilites of having RC along with actual RC status

predicted.data <- predicted.data[
  order(predicted.data$probability.of.RC, decreasing = F),] #sort df from low to high probabilites
predicted.data$rank <- 1:nrow(predicted.data) #add new column to df that has the rank of each sample, from low to high probability

library(ggplot2)
library(cowplot)
ggplot(data=predicted.data, aes(x=rank, y=probability.of.RC)) +
  geom_point(aes(color=RC), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of having a RC") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=20), 
        legend.title=element_text(size=20), legend.text=element_text(size=20))
ggsave("RC_probabilites.pdf") #save graph as PDF

?ggplot

#m2
predicted.data <- data.frame(
  probability.of.RC=m2$fitted.values,
  RC=RC) #df that contains probabilites of having RC along with actual RC status

predicted.data <- predicted.data[
  order(predicted.data$probability.of.RC, decreasing = F),] #sort df from low to high probabilites
predicted.data$rank <- 1:nrow(predicted.data) #add new column to df that has the rank of each sample, from low to high probability

library(ggplot2)
library(cowplot)
ggplot(data=predicted.data, aes(x=rank, y=probability.of.RC)) +
  geom_point(aes(color=RC), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of having a RC") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=20), 
        legend.title=element_text(size=20), legend.text=element_text(size=20))
ggsave("RC_probabilitesM2.pdf") #save graph as PDF


###### marital status and sosu
dev.off()
install.packages("ggplot2")
library(ggplot2)

rel <- as.character(relation)
rel[rel %in% c(0, 2, 3)] <- 2 #single
rel[rel %in% c(1)] <- 1 #married
rel <- factor(rel)

graph <- data.frame(sosu, rel, RC, sex)

mylogit <- glm(RC ~ sex + rel + sosu, data = graph, family = "binomial")
summary(mylogit)
exp(mylogit$coefficients)
exp(confint(mylogit))

newdata1 <- with(graph, data.frame(sosu=mean(sosu), rel = factor(1:2)))
newdata1
newdata1$relP <- predict(mylogit, newdata = newdata1, type = "response")
newdata1
newdata2 <- with(graph, data.frame(sosu = rep(seq(from = 13, to = 52, length.out =100)), rel=factor(rep(1:2, each=100))))
newdata3 <- cbind(newdata2, predict(m2, newdata = newdata2, type="link", se=TRUE))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 *se.fit))
  UL <- plogis(fit + (1.96*se.fit))
})

ggplot(newdata3, aes(x=sosu, y= PredictedProb)) +
  geom_ribbon(aes(ymin = LL, ymax =UL, fill = rel), alpha=0.2) +
  geom_line(aes(colour = rel), size=1)





















