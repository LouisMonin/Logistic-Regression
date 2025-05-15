rm(list ~ ls())
gc()

install.packages("dplyr")
install.packages("mlbench")
install.packages("funModeling")
install.packages("Hmisc")
install.packages("ROCR")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("pander")
install.packages("reshape2")
install.packages("lazyeval")
install.packages("moments")
install.packages("entropy")

library(mlbench)
library(dplyr)
library(ggplot2)
data("PimaIndiansDiabetes2")

data("PimaIndiansDiabetes2", package = "mlbench")
PimaIndiansDiabetes2 <- na.omit(PimaIndiansDiabetes2)

set.seed(123)
training.samples <- sample(x = nrow(PimaIndiansDiabetes2),
                           size = nrow(PimaIndiansDiabetes2)*0.8)
train.data <- PimaIndiansDiabetes2[training.samples,]
test.data <- PimaIndiansDiabetes2[-training.samples,]

model <- glm( diabetes ~ glucose, data = train.data, family = binomial)
summary(model)$coef

train.data %>%
  mutate(prob = ifelse(diabetes == "pos", 1, 0)) %>%
  ggplot(aes(glucose, prob)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model",
    x = "Plasma Glucose Concentration",
    y = "Probability of being diabete-pos"
  )

newdata <- data.frame(glucose = c(20, 180))
logit <-predict(model, newdata)
logit
probabilities <- predict(model, newdata, type = "response")
probabilities
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
predicted.classes

#Ajuster un modèle avec la biais seulement
intercept_only<-glm( diabetes ~1, data = train.data, family = binomial)
intercept_only$coef
# Appliquer la méthode step en direction forward
forward <- step(intercept_only, direction='forward',
                scope=formula(model), trace=0)
#Afficher les résultats
forward$anova

#Afficher les coefficients retenus
forward$coefficients

plot(0:(nrow(forward$anova)-1),forward$anova[,"AIC"],type="b",
     xlab="# de var. introdutes",ylab="AIC",main="Sélection forward (AIC)")

model <- glm( diabetes ~ glucose + age + mass + pedigree,
              data = train.data, family = binomial)
probabilities <- predict(model, test.data, type = "reponse")
head(predicated.classes)

table(predicted.classes,test.data$diabetes)
table(test.data$diabetes, predicted.classes)

