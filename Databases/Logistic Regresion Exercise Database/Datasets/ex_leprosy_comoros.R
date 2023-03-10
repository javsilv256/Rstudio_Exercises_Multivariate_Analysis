leprosy_comoros <- read.table("C:/Users/ehasker/OneDrive - ITG/epco/Epidemiology/MVA2122/Datasets_csv/leprosy_comoros.csv", header=TRUE, 
  stringsAsFactors=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
leprosy_comoros

GLM.1 <- glm(inc_case ~ offset(log(pop)) + PEP_19, family=poisson(log), data=leprosy_comoros)
summary(GLM.1)
exp(coef(GLM.1))  # Exponentiated coefficients

Confint(GLM.1, level=0.95, type="LR")
Confint(GLM.1, level=0.95, type="LR", exponentiate=TRUE)

dispp <- sum(residuals(GLM.1, type="pearson")^2)/GLM.1$df.residual
dispp

GLM.2 <- glm(inc_case ~ offset(log(pop)) + PEP_19 + dist_cat, family=poisson(log), data=leprosy_comoros)
summary(GLM.2)
exp(coef(GLM.2))  # Exponentiated coefficients

Confint(GLM.2, level=0.95, type="LR")
Confint(GLM.2, level=0.95, type="LR", exponentiate=TRUE)


anova(GLM.1, GLM.2, test="Chisq")

dispp <- sum(residuals(GLM.2, type="pearson")^2)/GLM.2$df.residual
dispp




