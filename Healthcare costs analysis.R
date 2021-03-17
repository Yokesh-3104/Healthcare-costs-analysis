setwd("C:\\Users\\PC\\Desktop\\Healthcare_cost analysis")
HospitalCosts=read.csv("hospital_cost.csv", header=TRUE)
head(HospitalCosts)
names(HospitalCosts)
names(HospitalCosts)[1] = "AGE"
names(HospitalCosts)
##Record patient Statistics#########
summary(HospitalCosts)
summary(as.factor(HospitalCosts$AGE))
hist(HospitalCosts$AGE, main="Histogram of Age Group and their hospital visits",
     xlab="Age group", border="black", col=c("light green", "dark green"), xlim=c(0,20), ylim=c(0,350))
##Maximum Expenditure ###
ExpenseBasedOnAge = aggregate(TOTCHG ~ AGE, FUN=sum, data=HospitalCosts)
which.max(tapply(ExpenseBasedOnAge$TOTCHG, ExpenseBasedOnAge$TOTCHG, FUN=sum))
barplot(tapply(ExpenseBasedOnAge$TOTCHG, ExpenseBasedOnAge$AGE, FUN=sum))

##Diagnosis-related group that has maximum hospitalization and expenditure:
summary(as.factor(HospitalCosts$APRDRG))
DiagnosisCost = aggregate(TOTCHG ~ APRDRG, FUN = sum, data = HospitalCosts)
DiagnosisCost[which.max(DiagnosisCost$TOTCHG), ]

##Race vs Hospitalization#####
summary(as.factor(HospitalCosts$RACE))
HospitalCosts = na.omit(HospitalCosts)
summary(as.factor(HospitalCosts$RACE))
raceInfluence=lm(TOTCHG~ RACE, data=HospitalCosts)
summary(raceInfluence)
#### Anaysis using ANOVA ####
raceInfluenceAOV <- aov(TOTCHG ~ RACE, data=HospitalCosts)
raceInfluenceAOV
summary(raceInfluenceAOV)
###analyze the severity of the hospital costs 
###by age and gender for the proper allocation of resources####
summary(HospitalCosts$FEMALE)
table(hosp_cost$FEMALE)
a <- aov(TOTCHG ~ AGE+FEMALE,data=HospitalCosts)
summary(a)
b <- lm(TOTCHG ~ AGE+FEMALE,data=HospitalCosts)
summary(b)
#Since the length of stay is the crucial factor for inpatients, the agency wants to find if the length of stay can be predicted from age, gender, and race.
table(HospitalCosts$LOS)
cat <- aov(LOS ~ AGE+FEMALE+RACE,data=HospitalCosts)
summary(cat)
cat <- lm(LOS ~ AGE+FEMALE+RACE,data=HospitalCosts)
summary(cat)
#To perform a complete analysis, the agency wants to find the variable that mainly affects the hospital costs.
aov(TOTCHG ~.,data=HospitalCosts)
mod <- lm(TOTCHG ~ .,data=HospitalCosts)
summary(mod)
