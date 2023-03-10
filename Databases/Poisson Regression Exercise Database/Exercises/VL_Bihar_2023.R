# Adjust working directory to your own
setwd("L:/Public health/Education/E. Course Published/01_Current course material/MPH2223/T2 MVA/MVA2023/Datasets_csv")
# Import the dataset
library(readxl)
vl_bihar <- read_excel("VL_Bihar.xlsx")
head(vl_bihar)

# Recoding of variables 'sex' and 'age'
vl_bihar$female <- ifelse(vl_bihar$sex==1, 1, 0)

vl_bihar$agegrp[vl_bihar$age < 15] <-  1
vl_bihar$agegrp[vl_bihar$age >14 & vl_bihar$age < 35] <-  2
vl_bihar$agegrp[vl_bihar$age >34] <-  3

# Overview of study population
table(vl_bihar$female)
proportions(table(vl_bihar$female))

table(vl_bihar$agegrp)
proportions(table(vl_bihar$agegrp))

table(vl_bihar$asset_index)
proportions(table(vl_bihar$asset_index))

table(vl_bihar$sc_caste)
proportions(table(vl_bihar$sc_caste))

table(vl_bihar$bednet)
proportions(table(vl_bihar$bednet))

table(vl_bihar$riskwall)
proportions(table(vl_bihar$riskwall))

table(vl_bihar$earthfloor)
proportions(table(vl_bihar$earthfloor))

table(vl_bihar$granaries_in_hh)
proportions(table(vl_bihar$granaries_in_hh))

table(vl_bihar$neem_tree)
proportions(table(vl_bihar$neem_tree))

table(vl_bihar$bamboo_tree)
proportions(table(vl_bihar$bamboo_tree))

table(vl_bihar$banana_tree)
proportions(table(vl_bihar$banana_tree))

table(vl_bihar$rice_field)
proportions(table(vl_bihar$rice_field))

table(vl_bihar$permanent_water_body)
proportions(table(vl_bihar$permanent_water_body))

table(vl_bihar$ownbov)
proportions(table(vl_bihar$ownbov))

table(vl_bihar$owngoat)
proportions(table(vl_bihar$owngoat))

table(vl_bihar$ownpoul)
proportions(table(vl_bihar$ownpoul))

#bivariate logistic regression 
GLM.1 <- glm(case ~ bamboo_tree, family=binomial, data=vl_bihar)
summary(GLM.1)
exp(coef(GLM.1)) 
exp(confint(GLM.1)) 

GLM.2 <- glm(case ~ banana_tree, family=binomial, data=vl_bihar)
summary(GLM.2)
exp(coef(GLM.2))
exp(confint(GLM.2)) 

GLM.3 <- glm(case ~ bednet, family=binomial, data=vl_bihar)
summary(GLM.3)
exp(coef(GLM.3))
exp(confint(GLM.3)) 

GLM.4 <- glm(case ~ earthfloor, family=binomial, data=vl_bihar)
summary(GLM.4)
exp(coef(GLM.4))
exp(confint(GLM.4)) 

GLM.5 <- glm(case ~ granaries_in_hh, family=binomial, data=vl_bihar)
summary(GLM.5)
exp(coef(GLM.5)) 
exp(confint(GLM.5)) 

GLM.6 <- glm(case ~ neem_tree, family=binomial, data=vl_bihar)
summary(GLM.6)
exp(coef(GLM.6)) 
exp(confint(GLM.6)) 

GLM.7 <- glm(case ~ ownbov, family=binomial(logit), data=vl_bihar)
summary(GLM.7)
exp(coef(GLM.7)) 
exp(confint(GLM.7)) 

GLM.8 <- glm(case ~ owngoat, family=binomial, data=vl_bihar)
summary(GLM.8)
exp(coef(GLM.8)) 
exp(confint(GLM.8)) 

GLM.9 <- glm(case ~ ownpoul, family=binomial, data=vl_bihar)
summary(GLM.9)
exp(coef(GLM.9)) 
exp(confint(GLM.9)) 

GLM.10 <- glm(case ~ permanent_water_body, family=binomial, data=vl_bihar)
summary(GLM.10)
exp(coef(GLM.10))
exp(confint(GLM.10)) 

GLM.11 <- glm(case ~ rice_field, family=binomial, data=vl_bihar)
summary(GLM.11)
exp(coef(GLM.11))  
exp(confint(GLM.11)) 

GLM.12 <- glm(case ~ riskwall, family=binomial, data=vl_bihar)
summary(GLM.12)
exp(coef(GLM.12)) 
exp(confint(GLM.12)) 

GLM.13 <- glm(case ~ sc_caste, family=binomial, data=vl_bihar)
summary(GLM.13)
exp(coef(GLM.13))
exp(confint(GLM.13)) 

GLM.14 <- glm(case ~ female, family=binomial, data=vl_bihar)
summary(GLM.14)
exp(coef(GLM.14))
exp(confint(GLM.14)) 
GLM.15 <- glm(case ~ factor(agegrp), family=binomial, data=vl_bihar)
summary(GLM.15)
exp(coef(GLM.15)) 
exp(confint(GLM.15)) 

GLM.16 <- glm(case ~ factor (asset_index), family=binomial(logit), data=vl_bihar)
summary(GLM.16)
exp(coef(GLM.16)) 
exp(confint(GLM.16)) 

anova(GLM.1, test="Chisq")
anova(GLM.2, test="Chisq")
anova(GLM.3, test="Chisq")
anova(GLM.4, test="Chisq")
anova(GLM.5, test="Chisq")
anova(GLM.6, test="Chisq")
anova(GLM.7, test="Chisq")
anova(GLM.8, test="Chisq")
anova(GLM.9, test="Chisq")
anova(GLM.10, test="Chisq")
anova(GLM.11, test="Chisq")
anova(GLM.12, test="Chisq")
anova(GLM.13, test="Chisq")
anova(GLM.14, test="Chisq")
anova(GLM.15, test="Chisq")
anova(GLM.16, test="Chisq")

# Checking for confounding of association with bednet by all other variables significant at p < 0.10
GLM.18 <- glm(case ~ bednet + sc_caste, family=binomial(logit), data=vl_bihar)
summary(GLM.18)
exp(coef(GLM.18))

GLM.19 <- glm(case ~ bednet + factor(asset_index), family=binomial(logit), data=vl_bihar)
summary(GLM.19)
exp(coef(GLM.19)) 

GLM.20 <- glm(case ~ bednet + factor(agegrp), family=binomial(logit), data=vl_bihar)
summary(GLM.20)
exp(coef(GLM.20))  

GLM.21 <- glm(case ~ bednet + earthfloor, family=binomial(logit), data=vl_bihar)
summary(GLM.21)
exp(coef(GLM.21))  

GLM.22 <- glm(case ~ bednet + riskwall, family=binomial(logit), data=vl_bihar)
summary(GLM.22)
exp(coef(GLM.22)) 

GLM.23 <- glm(case ~ bednet +  bamboo_tree, family=binomial(logit), data=vl_bihar)
summary(GLM.23)
exp(coef(GLM.23)) 


# First multivariate model with bednet plus 6 varriables significant in univariate
GLM.24 <- glm(case ~ bednet + bamboo_tree + riskwall + earthfloor + sc_caste + factor(agegrp) + factor(asset_index), family=binomial(logit), data=vl_bihar)
summary(GLM.24)
exp(coef(GLM.24)) 

# Drop weakest apart from bednet, is 'riskwall'
GLM.25 <- glm(case ~ bednet + bamboo_tree +  earthfloor + sc_caste + factor(agegrp) + factor(asset_index), family=binomial(logit), data=vl_bihar)
summary(GLM.25)
exp(coef(GLM.25))  

# Difference is non-significant, OR of bednet changes from 0.81 tot 0.80, so no confounder either, can be dropped.
anova(GLM.24, GLM.25, test="Chisq")

# Drop earth floor, has a p-value of 0.18
GLM.26 <- glm(case ~ bednet + bamboo_tree + sc_caste + factor(agegrp) + factor(asset_index), family=binomial(logit), data=vl_bihar)
summary(GLM.26)
exp(coef(GLM.26)) 


# LR test is non-significant, OR of bednet changes from 0.80 to 0.79, so no confounding either. 
anova(GLM.25, GLM.26, test="Chisq")

# Weakest is now age group. I check if I can drop it but then I need to work with a subset excluding the two missing ages

GLM.27 <- glm(case ~ bednet + bamboo_tree + sc_caste + factor(asset_index), family=binomial(logit), data=vl_bihar, subset=age !='NA')
summary(GLM.27)
exp(coef(GLM.27))  # Exponentiated coefficients ("odds ratios")

# Difference is non-significant and OR of bednet hardly changes, 0.79 to 0.76, so I can drop age group.
anova(GLM.26, GLM.27, test="Chisq")

# Weakest is now assets but it seems to be significant. To make comparison I drop it but I also have to 
# run previous model again including the two records with missing ages

GLM.28 <- glm(case ~ bednet + bamboo_tree + sc_caste + factor(asset_index), family=binomial(logit), data=vl_bihar)
summary(GLM.28)
exp(coef(GLM.28))

GLM.29 <- glm(case ~ bednet + bamboo_tree + sc_caste, family=binomial(logit), data=vl_bihar)
summary(GLM.29)
exp(coef(GLM.29))  

# Difference is highly significant so I will keep assets, also because it is a confounder, OR of bednet changes from 0.76 to 0.56.
anova(GLM.28, GLM.29, test="Chisq")

# recode assets to binary for testing interactions

vl_bihar$poverty <- ifelse(vl_bihar$asset_index <=3, 1, 0)

# Testing for interaction of poverty and bednet. 
GLM.30 <- glm(case ~ bednet + poverty + bamboo_tree + sc_caste, family=binomial(logit), data=vl_bihar)
GLM.31 <- glm(case ~ bednet + poverty + bamboo_tree + sc_caste + bednet*poverty, family=binomial(logit), data=vl_bihar)
anova(GLM.31, GLM.30, test="Chisq")

# Testing for interaction between bed net and bamboo tree and bednet and sc_caste
GLM.32 <- glm(case ~ bednet + bamboo_tree + sc_caste + factor(asset_index) + bednet*bamboo_tree, family=binomial(logit), data=vl_bihar)
anova(GLM.32, GLM.28, test="Chisq")

GLM.33 <- glm(case ~ bednet  + bamboo_tree + sc_caste + factor(asset_index) + bednet*sc_caste, family=binomial(logit), data=vl_bihar)
anova(GLM.33, GLM.28, test="Chisq")

# None of the interactions were significant, so this will be my final model:
Final_Model <- glm(case ~ bednet + bamboo_tree + sc_caste + factor(asset_index), family=binomial(logit), data=vl_bihar)
summary(Final_Model)
exp(coef(Final_Model))  
exp(confint(Final_Model)) 

# The PAF measures the fraction of total disease incidence (or risk) in a population attributable to the exposure to a given risk factor.


unexposed <- subset(vl_bihar, subset=bednet == 0)
str(unexposed)

table(unexposed$case)
table(vl_bihar$case)

#(Rpop-Ru/Rpop)


