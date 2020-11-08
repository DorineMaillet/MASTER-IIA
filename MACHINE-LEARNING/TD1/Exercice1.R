assurance <- read.csv("insurance.csv")
str(assurance)

# Définition de la normalité de notre modèle
summary(assurance$expenses)
hist(assurance$expenses)

table(assurance$region)
table(assurance$smoker)
table(assurance$sex)

# Etude de la corrélation entre les variables numériques
cor(assurance[c("age", "bmi", "children", "expenses")])

# Entrainement du modèle 
assurance_model <- lm(expenses ~ ., data = assurance)
assurance_model

# Evaluation des performances du modèle
summary(assurance_model)

#Specification du modele
assurance$age2 <- assurance$age^2

# Transformation
assurance$bmi30 <- ifelse(assurance$bmi >= 30, 1, 0)

# Interaction
expenses ~ bmi30*smoker

# Entrainement du nouveau modèle
assurance_model2 <-lm(expenses ~ age + age2 + children + bmi + sex + bmi30*smoker + region, data = assurance)
summary(assurance_model2)

# Prédiction à partir du nouveau modèle 
assurance$pred <- predict(assurance_model2, assurance)

# Corrélation entre coûts réels et coûts prédits
cor(assurance$pred, assurance$expenses)
plot(assurance$pred, assurance$expenses)
abline(a=0, b=1, col="red", lwd=3, lty=2)

# Prédiction de données de nouveaux inscrits
#1 - Homme non fumeur de 30 ans en surpoids avec 2 enfants de la région Northeast: 
predict(assurance_model2, data.frame(age=30, age2=30^2, children=2, bmi=30, sex="male", bmi30=1, smoker="no", region="northeast")) 
# 2 - Femme avec les mêmes caractéristiques que le précédent 
predict(assurance_model2, data.frame(age=30, age2=30^2, children=2, bmi=30, sex="female", bmi30=1, smoker="no", region="northeast"))
# 3 - Femme avec les mes caractéristiques que la précédente mais sans enfants
predict(assurance_model2, data.frame(age=30, age2=30^2, children=0, bmi=30, sex="female", bmi30=1, smoker="no", region="northeast"))