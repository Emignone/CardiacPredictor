#Regresion
load("C:/Users/emigi/Downloads/LogisticRegresion&StepAIC.RData")
View(NH11)

str(NH11$hypev)

NH11<-NH11[which(!is.na(NH11$hypev)),]

hyp.out <- glm(hypev~age_p+sex+sleep+bmi,
               data=NH11, family="binomial")
summary(hyp.out)

coef(summary(hyp.out))

predsAll<-predict(hyp.out, type = "response")

##BOX PLOT
boxplot(predsAll ~ NH11$hypev, col = c("green", "red"),
        ylab = "Probabilidad",
        xlab = "Tiene / No tiene hypertensión")

##DENSIDAD
plot(density(predsAll[which(NH11$hypev=="1 Yes")]), col ="dark green", main = "Funciones de densidad", ylim=c(0, 5) )
lines(density(predsAll[which(NH11$hypev=="2 No")]), col ="red", main = "Funciones de densidad")

#Predicciones:
#Por ejemplo, podemos preguntar “¿Cuánto más probable es que una mujer de 63 años tenga hipertensión en comparación con una mujer de 33 años?”.


# Crear un dataset con predictores
predDat <- with(NH11,
                expand.grid(age_p = c(33, 63),
                            sex = "2 Female",
                            bmi = mean(bmi, na.rm = TRUE),
                            sleep = mean(sleep, na.rm = TRUE)))
      # predecir

preds <- predict(hyp.out, type = "response",
                 se.fit = TRUE, interval="confidence",
                 newdata = predDat)
cbind(predDat, preds)

##El resultado indica que una mujer de 33 
##años tiene un 13% de probabilidad de haber sido diagnosticada con hipertensión, mientras que una mujer de 63 años tiene un 48%.
##6 Árboles de dec



###TEST
library(rpart)

str(cu.summary)
table(cu.summary$Reliability)

table(NH11.summary$Reliability)

#################################
data(mtcars)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(mtcars))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(mtcars)), size = smp_size)

train <- mtcars[train_ind, ]
test <- mtcars[-train_ind, ]

###########################
smp_size <- floor(0.75 * nrow(NH11))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(NH11)), size = smp_size)

train <- NH11[train_ind, ] #[filas,columnas]
test <- NH11[-train_ind, ]

hyp.out <- glm(hypev~age_p+sex+sleep+bmi,
               data=train, family="binomial")
summary(hyp.out)


preds <- predict(hyp.out, type = "response",
                 se.fit = TRUE, interval="confidence",
                 newdata = test)

#probs <-as.vector(preds$fit)

threshold<- 0.5

results<-ifelse(preds$fit < threshold,"2 No", "1 Yes")
results








