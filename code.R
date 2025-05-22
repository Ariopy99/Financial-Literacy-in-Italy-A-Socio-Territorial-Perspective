## ----warnings=FALSE, message=FALSE----------------------------------------------
library(car)
library(readr)
library(MASS)
library(pscl)
library(ggplot2)


## ----warnings=FALSE, message=FALSE----------------------------------------------
ds <- read_csv("Financialliteracy.csv")
colnames(ds)[c(99:104)] <- c("Gender","Household","Age","Education","Employment","Country")
head(ds)


## ----warnings=FALSE, message=FALSE----------------------------------------------
cols_to_factor <- colnames(ds)[c(3:100,102,104:106)]
ds[cols_to_factor] <- lapply(ds[cols_to_factor], factor)


## ----warnings=FALSE, message=FALSE----------------------------------------------
require(skimr)
skim_without_charts(ds[99:106])


## ----warnings=FALSE, message=FALSE----------------------------------------------
ds$Age1 <- ds$Age


## ----warnings=FALSE, message=FALSE----------------------------------------------
hist(ds$Age , main="Istogramma",  xlab="", ylab="", col="red4")


## ----warnings=FALSE, message=FALSE----------------------------------------------
ds$Aged <- ifelse(ds$Age < 35,1,0)
ds$Aged <- ifelse(ds$Age >= 35 & ds$Age < 50,2,ds$Aged)
#ds$Aged <- ifelse(ds$Age >= 40 & ds$Age < 50,3,ds$Aged)
ds$Aged <- ifelse(ds$Age >= 50 & ds$Age < 65,3,ds$Aged)
ds$Aged <- ifelse(ds$Age >= 65,4,ds$Aged)
ds$Aged <- ordered(ds$Aged, levels= c(1:4))
table(ds$Aged)


## ----warnings=FALSE, message=FALSE----------------------------------------------
table(ds$Employment)


## ----warnings=FALSE, message=FALSE----------------------------------------------
#ds$Employment <- as.numeric(ds$Employment)
ds$Employment[ds$Employment == 4] <- 3
ds$Employment[ds$Employment == 5] <- 4
ds$Employment[ds$Employment == 6] <- 5
ds$Employment[ds$Employment == 9] <- 6
ds$Employment[ds$Employment == 10] <- 7

table(factor(ds$Employment))
ds$Employment <- factor(ds$Employment)


## ----warnings=FALSE, message=FALSE----------------------------------------------
ds$Employment1 <- ifelse(ds$Employment %in% c(1, 2), 1, 0)
table(ds$Employment1)


## ----warnings=FALSE, message=FALSE----------------------------------------------
table(ds$Education)


## ----warnings=FALSE, message=FALSE----------------------------------------------
ds[which(ds$Education == 6),c(99:106)]
ds[which(ds$Education == 7),c(99:106)]
mean(ds[which(ds$Education == 6),]$Age)
mean(ds[which(ds$Education == 7),]$Age)


## ----warnings=FALSE, message=FALSE----------------------------------------------
ds$Education[ds$Education == 6 | ds$Education == 7 ] <- 5
table(ds$Education)


## ----warnings=FALSE, message=FALSE----------------------------------------------
Education <- ds$Education
ds$Education <- as.numeric(ds$Education)
ds$Education[Education == 4| Education == 5]<-1
ds$Education[Education == 3]<-2
ds$Education[Education == 1]<-3
ds$Education <- ordered(factor(ds$Education),levels=c(1:3))
table(ds$Education)


## ----warnings=FALSE, message=FALSE----------------------------------------------
table(ds$Household)


## ----warnings=FALSE, message=FALSE----------------------------------------------
ds$Household[ds$Household == 6 | ds$Household == 5] <- 4
ds$Household <- ordered(ds$Household, levels = c(1:4))
table(ds$Household)


## ----warnings=FALSE, message=FALSE----------------------------------------------
prop.table(table(ds$Gender))


## ----warnings=FALSE, message=FALSE----------------------------------------------
table(ds$Country)


## ----warnings=FALSE, message=FALSE----------------------------------------------
table(ds$AREA5)


## ----warnings=FALSE, message=FALSE----------------------------------------------
tab<-table(Education = ds$Education, area = ds$AREA5)
tab

# Relative frequencies
prop.table(tab)

# Margin relative frequencies
prop.table(tab,margin=2)
prop.table(tab,margin=1)


## ----warnings=FALSE, message=FALSE----------------------------------------------
tab<-table(Education = ds$Education, Gender = ds$Gender)
tab
# Relative frequencies
prop.table(tab, margin=2)
mosaicplot(tab)


## ----warnings=FALSE, message=FALSE----------------------------------------------
tab<-table(Education = ds$Education, Household = ds$Household)
tab
# Relative frequencies
prop.table(tab)
prop.table(tab, margin=1)
prop.table(tab, margin=2)

mosaicplot(tab)


## ----warnings=FALSE, message=FALSE----------------------------------------------
tab<-table(Household = ds$Household, Area= ds$AREA5)
prop.table(tab, margin = 1) # Conditional frequencies by household size


## ----warnings=FALSE, message=FALSE----------------------------------------------
skim_without_charts(ds[92:98])


## ----warnings=FALSE, message=FALSE----------------------------------------------

tab_99<-data.frame(
  c(
    length(which(ds$qk3 == -99)),
    length(which(ds$qk4 == -99)),
    length(which(ds$qk5 == -99)),
    length(which(ds$qk6 == -99)),
    length(which(ds$qk7_1 == -99)),
    length(which(ds$qk7_2 == -99)),
    length(which(ds$qk7_3 == -99))
  ),
  row.names = colnames(ds[92:98])
)
colnames(tab_99) <- "N_noAnsware"
tab_99


## ----warnings=FALSE, message=FALSE----------------------------------------------
know<-ds[1]
know$qk3 <- ifelse(ds$qk3 == 3,1,0)
know$qk4 <- ifelse(ds$qk4 == 0,1,0)
know$qk5 <- ifelse(ds$qk5 == 102 ,1,0)
know$qk6 <- ifelse(ds$qk6 == 2 ,1,0)
know$qk7_1 <- ifelse(ds$qk7_1 == 1,1,0)
know$qk7_2 <- ifelse(ds$qk7_2 == 1,1,0)
know$qk7_3 <- ifelse(ds$qk7_3 == 1,1,0)

# Calculate total score (0-7) based on correct answers
know$tot <- unlist(know$qk3+know$qk4+know$qk5+know$qk6+know$qk7_1+know$qk7_2+know$qk7_3)
know$tot <- ordered(know$tot, levels = c(0:7))


## ----warnings=FALSE, message=FALSE----------------------------------------------
table(know$tot)


## ----warnings=FALSE, message=FALSE----------------------------------------------
plot(know$tot,col=c("#0077FF"))


## ----warnings=FALSE, message=FALSE----------------------------------------------
tab<-table(score = know$tot, area = ds$AREA5)
tab
barplot(tab,col=c("blue","red","yellow","green","orange","brown","pink","black"),main="Grafico frequenza voti e posizione geografica",names.arg = c("North-west","North-East", "Centre","South","Islands"))
legend("topright",                     # Posizione della legenda
       legend = c("0","1","2","3","4","5","6",7),  # Etichette della legenda (categorie)
       fill = c("blue","red","yellow","green","orange","brown","pink","black"))
#frequenze relative
prop.table(tab)
#Eta media per ciascun livello
tapply(ds$Age, know$tot, mean)


## ----warnings=FALSE, message=FALSE----------------------------------------------
table(know$tot, ds$Gender)
table(know$tot, ds$Aged)
table(know$tot, ds$Education)


## ----warnings=FALSE, message=FALSE----------------------------------------------
table(know$tot)


## ----warnings=FALSE, message=FALSE----------------------------------------------
know$tot1 <- know$tot
know$tot1[know$tot == 0 | know$tot == 1 | know$tot == 2] <- 1
know$tot1[know$tot == 3 | know$tot == 4] <- 2
know$tot1[know$tot == 5 |know$tot == 6 | know$tot == 7] <- 3
know$tot1 <- ordered(factor(know$tot1),levels=c(1:3))


## ----warnings=FALSE, message=FALSE----------------------------------------------
table(know$tot1)


## ----warnings=FALSE, message=FALSE----------------------------------------------
mod1_1<- polr(know$tot1 ~ 
                Gender + 
                Household + 
                Aged + 
                Education + 
                Employment1 + 
                AREA5,
              ds)
summary(mod1_1)


## ----warnings=FALSE, message=FALSE----------------------------------------------
Anova(mod1_1, type = "II", test.statistic = "LR")


## ----warnings=FALSE, message=FALSE----------------------------------------------
step(mod1_1)


## ----warnings=FALSE, message=FALSE----------------------------------------------
mod1_2<- polr(know$tot1 ~ Gender + Age1 + Education, ds)
summary(mod1_2)


## ----warnings=FALSE, message=FALSE----------------------------------------------
summary_table <- coef(summary(mod1_2))
pval <- pt(abs(summary_table[, "t value"]),lower.tail = FALSE,nrow(ds)-4)
summary_table <- cbind(summary_table, "p value" = round(pval,5))
summary_table


## ----warnings=FALSE, message=FALSE----------------------------------------------
library(brant)
brant(mod1_2)


## ----warnings=FALSE, message=FALSE----------------------------------------------
anova(mod1_1,mod1_2,  test = "Chisq")


## ----warnings=FALSE, message=FALSE----------------------------------------------
mod1_0 <- polr(know$tot1 ~ 1)
anova(mod1_2, mod1_0, test = "Chisq")


## ----warnings=FALSE, message=FALSE----------------------------------------------
table(ds$qf10_2)
table(ds$qf10_3)
table(ds$qf10_5)


## ----warnings=FALSE, message=FALSE----------------------------------------------
ds[which((ds$qf10_2 == -97 | ds$qf10_2 == -99) & (ds$qf10_3 == -97 | ds$qf10_3 == -99) & (ds$qf10_8 == -97 | ds$qf10_8 == -99)),c(1,59,60,65)]
ds[which((ds$qf10_3 == -97 | ds$qf10_3 == -99) & (ds$qf10_8 == -97 | ds$qf10_8 == -99)),c(59,60,65)]
ds[which((ds$qf10_2 == -97 | ds$qf10_2 == -99) & (ds$qf10_8 == -97 | ds$qf10_8 == -99)),c(59,60,65)]
ds[which((ds$qf10_3 == -97 | ds$qf10_3 == -99) & (ds$qf10_8 == -97 | ds$qf10_8 == -99)),c(59,60,65)]


## ----warnings=FALSE, message=FALSE----------------------------------------------
ds_R <- ds[!(ds$qf10_2 %in% c(-99, -97) | ds$qf10_3 %in% c(-99, -97) | ds$qf10_5 %in% c(-99, -97)| ds$qf10_7 %in% c(-99, -97) | ds$qf10_8 %in% c(-99, -97)), ]

Attitude <- ds_R[1]

Attitude$qf10_2 <- ds_R$qf10_2
Attitude$qf10_3 <- ds_R$qf10_3
Attitude$qf10_5 <- ds_R$qf10_5

Attitude$qf10_7 <- ds_R$qf10_7
Attitude$qf10_8 <- ds_R$qf10_8

Attitude$qf10_7[ds_R$qf10_7 == 1] <- 5
Attitude$qf10_7[ds_R$qf10_7 == 2] <- 4
Attitude$qf10_7[ds_R$qf10_7 == 3] <- 3
Attitude$qf10_7[ds_R$qf10_7 == 4] <- 2
Attitude$qf10_7[ds_R$qf10_7 == 5] <- 1


## ----warnings=FALSE, message=FALSE----------------------------------------------
Attitude$qf10_2 <- ordered(Attitude$qf10_2, level=c(1:5))
Attitude$qf10_3 <- ordered(Attitude$qf10_3, level=c(1:5))
Attitude$qf10_5 <- ordered(Attitude$qf10_5, level=c(1:5))
Attitude$qf10_7 <- ordered(Attitude$qf10_5, level=c(1:5))
Attitude$qf10_8 <- ordered(Attitude$qf10_5, level=c(1:5))


## ----warnings=FALSE, message=FALSE----------------------------------------------
table(Attitude$qf10_2)
table(Attitude$qf10_3)
table(Attitude$qf10_5)

table(ds_R$qf10_2)
table(ds_R$qf10_3)


## ----warnings=FALSE, message=FALSE----------------------------------------------
Attitude$score <- round(unlist((as.numeric(as.character(Attitude$qf10_2)) + as.numeric(as.character(Attitude$qf10_3)) + as.numeric(as.character(Attitude$qf10_7)) + as.numeric(as.character(Attitude$qf10_8)))/4))
Attitude$score <- ordered(Attitude$score, levels = c(1:5))
table(Attitude$score)


## ----warnings=FALSE, message=FALSE----------------------------------------------
Attitude$score2 <- Attitude$score
Attitude$score2[Attitude$score == 1 | Attitude$score == 2] <- 1
Attitude$score2[Attitude$score == 3] <- 2
Attitude$score2[Attitude$score == 4 | Attitude$score == 5] <- 3
Attitude$score2 <- ordered(Attitude$score2, levels=c(1:3))


## ----warnings=FALSE, message=FALSE----------------------------------------------
mod2_1<- polr(Attitude$score2 ~ Gender + Household + Aged + Education + Employment1  + AREA5 ,ds_R)
summary(mod2_1)


## ----warnings=FALSE, message=FALSE----------------------------------------------
library(car)
Anova(mod2_1, type = "II", test.statistic = "LR")


## ----warnings=FALSE, message=FALSE----------------------------------------------
step(mod2_1)


## ----warnings=FALSE, message=FALSE----------------------------------------------
mod2_2 <- polr(Attitude$score2 ~ Gender + Aged + AREA5, ds_R)
summary_table <- coef(summary(mod2_2))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,5))
summary_table
summary(mod2_2)


## ----warnings=FALSE, message=FALSE----------------------------------------------
library(brant)
brant(mod2_2)


## ----warnings=FALSE, message=FALSE----------------------------------------------
mod2_3 <- polr(Attitude$score2 ~ Gender + Aged, ds_R)
summary_table <- coef(summary(mod2_3))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,5))
summary_table
summary(mod2_3)


## ----warnings=FALSE, message=FALSE----------------------------------------------
library(brant)
brant(mod2_3)


## ----warnings=FALSE, message=FALSE----------------------------------------------
anova(mod2_1, mod2_2, test = "Chisq")


## ----warnings=FALSE, message=FALSE----------------------------------------------
anova(mod2_2, mod2_3, test = "Chisq")


## ----warnings=FALSE, message=FALSE----------------------------------------------
mod2_0 <- polr(Attitude$score2 ~ 1)
anova(mod2_3, mod2_0, test = "Chisq")


## ----warnings=FALSE, message=FALSE----------------------------------------------
table(Attitude$qf10_5)


## ----warnings=FALSE, message=FALSE----------------------------------------------
Attitude$risk1 <- 1
Attitude$risk1[Attitude$qf10_5==5|Attitude$qf10_5==4] <- 0
Attitude$risk1 <- factor(Attitude$risk1)
table(Attitude$risk1)


## ----warnings=FALSE, message=FALSE----------------------------------------------
mod21_1 <- glm(Attitude$risk1 ~ Gender + Household + Aged + Education + Employment1  + AREA5, family = "binomial", data = ds_R)
summary(mod21_1)


## ----warnings=FALSE, message=FALSE----------------------------------------------
Anova(mod21_1, type = "II", test.statistic = "LR")


## ----warnings=FALSE, message=FALSE----------------------------------------------
step(mod21_1)


## ----warnings=FALSE, message=FALSE----------------------------------------------
mod21_2 <- glm(Attitude$risk1 ~  Gender + Aged + Education,family = "binomial", ds_R)
summary(mod21_2)


## ----warnings=FALSE, message=FALSE----------------------------------------------
mod21_3 <- glm(Attitude$risk1 ~  Gender + Aged,family = "binomial", ds_R)
summary(mod21_3)


## ----warnings=FALSE, message=FALSE----------------------------------------------
anova(mod21_1, mod21_2, test = "Chisq")


## ----warnings=FALSE, message=FALSE----------------------------------------------
anova(mod21_2, mod21_3, test = "Chisq")


## ----warnings=FALSE, message=FALSE----------------------------------------------
mod21_0 <- glm(Attitude$risk1 ~ 1,family = "binomial", ds_R)
anova(mod21_3, mod21_0, test = "Chisq")


## ----warnings=FALSE, message=FALSE----------------------------------------------
table(ds$qf8)


## ----warnings=FALSE, message=FALSE----------------------------------------------
dsR <- ds[!(ds$qf8 == -99),]
rknow <- know[!(ds$qf8 == -99),]
dsR$know <- rknow$tot
dsR <- dsR[!(dsR$qf8 == -97),]
dsR$qf8 <- ordered(dsR$qf8, levels = c(6:1))


## ----warnings=FALSE, message=FALSE----------------------------------------------
modRet1 <- polr(qf8 ~  Gender + Household + Age1 + Education + Employment1 + AREA5 + know, data = dsR, Hess = TRUE)
summary(modRet1)


## ----warnings=FALSE, message=FALSE----------------------------------------------
step(modRet1)


## ----warnings=FALSE, message=FALSE----------------------------------------------
Anova(modRet1, type = "II", test.statistic = "LR")


## ----warnings=FALSE, message=FALSE----------------------------------------------
modRet2 <- polr(formula = qf8 ~ Age1 + Education + Employment1 + AREA5, data = dsR, Hess = TRUE)
summary(modRet2)


## ----warnings=FALSE, message=FALSE----------------------------------------------
summary_table <- coef(summary(modRet2))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,5))
summary_table


## ----warnings=FALSE, message=FALSE----------------------------------------------
summary_table <- coef(summary(modRet2))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,5))
summary_table


## ----warnings=FALSE, message=FALSE----------------------------------------------
library(brant)
brant(modRet2)


## ----warnings=FALSE, message=FALSE----------------------------------------------
anova(modRet1, modRet2, test = "Chisq")


## ----warnings=FALSE, message=FALSE----------------------------------------------
modRet0 <- polr(formula = qf8 ~ 1, data = dsR, Hess = TRUE)
anova(modRet2, modRet0, test = "Chisq")


## ----warnings=FALSE, message=FALSE----------------------------------------------
table(ds$qf9_99)


## ----warnings=FALSE, message=FALSE----------------------------------------------
dsR2 <- ds[!(ds$qf9_99==1),]


## ----warnings=FALSE, message=FALSE----------------------------------------------
# We create a new column that contain the sum of the columns related to secure retirement plans
dsR2$sum <- as.numeric(as.character(dsR2$qf9_1)) + as.numeric(as.character(dsR2$qf9_2)) + as.numeric(as.character(dsR2$qf9_3)) + as.numeric(as.character(dsR2$qf9_4)) + as.numeric(as.character(dsR2$qf9_5)) + as.numeric(as.character(dsR2$qf9_6))
# We transform the observation that have any value different from 0 in this new column to 1.
# In this way any observation that have at least one secure tool for building their
# retirement plan will be classified as 1.
# While all the other observation will remain equal to zero.
dsR2$sum[dsR2$sum != 0] <- 1


## ----warnings=FALSE, message=FALSE----------------------------------------------
dsR2$sum <- factor(dsR2$sum, levels = c(0,1))


## ----warnings=FALSE, message=FALSE----------------------------------------------
mod_qf9_1 <- glm(sum ~ Gender + Household + Aged + Education + Employment1  + AREA5, data = dsR2, family = "binomial")
summary(mod_qf9_1)


## ----warnings=FALSE, message=FALSE----------------------------------------------
step(mod_qf9_1)


## ----warnings=FALSE, message=FALSE----------------------------------------------
Anova(modRet1, type = "II", test.statistic = "LR")


## ----warnings=FALSE, message=FALSE----------------------------------------------
mod_qf9_2 <- glm(sum ~ Age1 + Education + Employment1 + AREA5, data = dsR2, family = "binomial")
summary(mod_qf9_2)


## ----warnings=FALSE, message=FALSE----------------------------------------------
# Stacked
ggplot(dsR2, aes(fill=factor(sum, levels=c(0,1)), y = after_stat(count), x=Employment1)) + 
    geom_bar(position="stack", stat="count") +
    xlab("Employment Status") +
#    legend("topleft", legend = c("Unsecure tools for retirement", "Secure tools for retirement"))
    scale_fill_discrete(labels=c('Risky', 'Safe')) +
    guides(fill=guide_legend(title="Tools for Retirement:")) +
    scale_x_discrete(labels= c("Unemployed", "Employed"))


## ----warnings=FALSE, message=FALSE----------------------------------------------
anova(mod_qf9_1, mod_qf9_2, test = "Chisq")


## ----warnings=FALSE, message=FALSE----------------------------------------------
dsPF3 <- ds[!(ds$qf3_99==1),]


## ----warnings=FALSE, message=FALSE----------------------------------------------
dsPF3$sum <- as.numeric(as.character(dsPF3$qf3_3)) + as.numeric(as.character(dsPF3$qf3_6)) + as.numeric(as.character(dsPF3$qf3_7))
dsPF3$sum[dsPF3$sum != 0] <- 1


## ----warnings=FALSE, message=FALSE----------------------------------------------
dsPF3$sum <- factor(dsPF3$sum, levels = c(0,1))


## ----warnings=FALSE, message=FALSE----------------------------------------------
mod_PF3 <- glm(sum ~ Gender + Household + Aged + Education + Employment1  + AREA5, data = dsPF3, family = "binomial")
summary(mod_PF3)


## ----warnings=FALSE, message=FALSE----------------------------------------------
step(mod_PF3)


## ----warnings=FALSE, message=FALSE----------------------------------------------
Anova(mod_PF3, type = "II", test.statistic = "LR")


## ----warnings=FALSE, message=FALSE----------------------------------------------
mod_PF3_1 <- glm(sum ~  Household + Aged + Education + Employment1 + AREA5, data = dsPF3, family = "binomial")
summary(mod_PF3_1)


## ----warnings=FALSE, message=FALSE----------------------------------------------
anova(mod_PF3, mod_PF3_1, test = "Chisq")


## ----warnings=FALSE, message=FALSE----------------------------------------------
mod_PF3_0 <- glm(sum ~  1, data = dsPF3, family = "binomial")
anova(mod_PF3_0, mod_PF3_1, test = "Chisq")


## ----warnings=FALSE, message=FALSE----------------------------------------------
dsPF4 <- ds[!(ds$qf4 == -99),]
dsPF4 <- dsPF4[!(dsPF4$qf4 == -98),]


## ----warnings=FALSE, message=FALSE----------------------------------------------
dsPF4$qf4[dsPF4$qf4 != 1] <- 0


## ----warnings=FALSE, message=FALSE----------------------------------------------
dsPF4$qf4 <- factor(dsPF4$qf4, levels = c(0,1))


## ----warnings=FALSE, message=FALSE----------------------------------------------
mod_PF4 <- glm(qf4 ~ Gender + Age1 + Education + Employment1 + AREA5 + Household, data = dsPF4, family = "binomial")
summary(mod_PF4)


## ----warnings=FALSE, message=FALSE----------------------------------------------
step(mod_PF4)


## ----warnings=FALSE, message=FALSE----------------------------------------------
Anova(mod_PF4, type = "II", test.statistic = "LR")


## ----warnings=FALSE, message=FALSE----------------------------------------------
mod_PF4_1 <- glm(qf4 ~ Age1 + Education + Employment1 + AREA5, family = "binomial", data = dsPF4)
summary(mod_PF4_1)


## ----warnings=FALSE, message=FALSE----------------------------------------------
anova(mod_PF4, mod_PF4_1, test = "Chisq")


## ----warnings=FALSE, message=FALSE----------------------------------------------
mod_PF4_0 <- glm(qf4 ~ 1, family = "binomial", data = dsPF4)
anova(mod_PF4_1, mod_PF4_0, test = "Chisq")

