donnees_hcl <- read.csv2(file = "base_ano.txt",header = TRUE, sep="\t")
# Sélection des séjours de durée supérieure ou égale à 1 jour
donnees_hcl <- donnees_hcl[donnees_hcl$duree >= 1 & donnees_hcl$duree <= 30 ,]
# Prédicteurs du modèle 1
donnees_hcl <- subset(donnees_hcl, select = c(duree, Selected, age, diag, sexe, service))

duree_Test_RMSE <- subset(donnees_hcl[donnees_hcl$Selected == 0,], select = c(duree))
donnees_hcl$duree <- as.factor(donnees_hcl$duree)

donnees_hcl$age <- as.character(donnees_hcl$age)
donnees_hcl$age <- as.numeric(donnees_hcl$age)

TrainData <- donnees_hcl[donnees_hcl$Selected == 1,]
TestData <- donnees_hcl[donnees_hcl$Selected == 0,]

predicteurs_Train <- subset(TrainData, select = -c(duree))
duree_Train <- subset(TrainData, select = c(duree))

predicteurs_Test <- subset(TestData, select = -c(duree))
duree_Test <- subset(TestData, select = c(duree))

library(xgboost)
param <- list("objective" = "multi:softprob",
              "eta"=0.3,
              "max.depth"=5,
              "nthread" = 8,
              "num_class" = 31)

modelXgboost <- xgboost(data = as.matrix(predicteurs_Train)
                        , label = as.matrix(duree_Train)
                        , params=param
                        , nrounds = 100)
xgbtest <- xgb.DMatrix(data.matrix(predicteurs_Test), missing=NA)

xgbpred <- predict(modelXgboost, xgbtest)
probs <- t(matrix(xgbpred, nrow=31, ncol=length(xgbpred)/31))

for(i in 1:100)
{
  plot(probs[i,], type='l', xlab = "Duree de séjour", ylab = "Probabilité", lwd=2)
  abline(v=as.numeric(duree_Test$duree[i])+1, col="red", lwd = 2)
}

esperances <- vector(length = nrow(TestData)) 

for (i in 1:nrow(TestData))
{
  for(j in 1:31)
  {
    esperances[i] <- esperances[i] + (j-1)*probs[i,j]
  }
}

RMSE <- function(y, pred)
{
  sqrt(mean((y - pred)^2))
}

RMSE(as.matrix(esperances), duree_Test_RMSE)
mean(abs(as.matrix(esperances) - as.matrix(duree_Test_RMSE)))
