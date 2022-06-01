pitch = c(233,204,242,130,112,142)
sex = c(rep("female",3),rep("male", 3))

df = data.frame(sex,pitch)
model1 = lm(pitch~sex, df)
summary(model1)

mean(df[df$sex=="female",]$pitch)
"""
subtract estimate in first row from second,
get 128, which is male mean,
therefore, intercept = estimate for female category
To go from females to males,
have to go down 98.33
"""

age = c(14,23,35,48,52,67)
pitch = c(252,244,240,233,212,204)
df = data.frame(age,pitch)
model2 = lm(pitch~age,df)
summary(model2)

#subtract mean age from every value
df$age.c = df$age - mean(df$age)
model3 = lm(pitch ~ age.c, df)
summary(model3)

#residual plot
plot(fitted(model3), residuals(model3))

#random data for homoskedasticity plot
plot(rnorm(100),rnorm(100))

#histogram
hist(residuals(model3))

#qqplot
qqnorm(residuals(model3))

#absence of influential data points
dfbeta(model3)

library(lme4)
politeness = read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")
#check for missing values
which(is.na(politeness)==T)

#boxplot between politeness and pitch
boxplot(frequency~ attitude*gender,
        col=c("white","lightgray"), politeness
)

#model
model4 = lmer(frequency~attitude +
                gender +
                (1|subject) + 
                (1|scenario),
              data=politeness)
model4

#null-model
null_model = lmer(frequency~ gender+
                    (1|subject) +
                    (1|scenario), 
                    data = politeness,
                    REML = FALSE)

#full-model
model5 = lmer(frequency~attitude +
                gender +
                (1|subject) + 
                (1|scenario),
                data=politeness,
                REML = FALSE)
#anova
anova(null_model,model5)

#coefficients
coef(model5)

#random slopes model
model6 = lmer(frequency~ attitude+
                gender+
                (1+attitude|subject)+
                (1+attitude|scenario),
                data = politeness, 
                REML = FALSE)
coef(model6)

#null-model
null_model2 = lmer(frequency~ gender+
                    (1+attitude|subject)+
                    (1+attitude|scenario),
                    data = politeness,
                    REML = FALSE)
#anova2
anova(null_model2, model6)



