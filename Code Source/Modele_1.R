# Chargement de la BDD
donnees_hcl <- read.csv2(file = "base_ano.txt",header = TRUE, sep="\t")
# Sélection des séjours de durée supérieure ou égale à 1 jour
donnees_hcl <- donnees_hcl[donnees_hcl$duree >= 1,]

# Modèle 1
donnees_hcl <- subset(donnees_hcl, select = c(duree, Selected, age, diag, sexe, service))

# Transformation du facteur "age" en variable continue
donnees_hcl$age <- as.character(donnees_hcl$age)
donnees_hcl$age <- as.numeric(donnees_hcl$age)

# Déclaration de la fonction RMSE

RMSE <- function(y, pred)
{
  sqrt(mean((y - pred)^2))
}

MAE <- function(y, pred)
{
  mean(abs(as.matrix(y) - as.matrix(pred)))
} 


# Création des jeux de Train et de Test
TrainData <- donnees_hcl[donnees_hcl$Selected == 1,]
TestData <- donnees_hcl[donnees_hcl$Selected == 0,]

predicteurs_Train <- subset(TrainData, select = -c(duree))
duree_Train <- subset(TrainData, select = c(duree))

predicteurs_Test <- subset(TestData, select = -c(duree))
duree_Test <- subset(TestData, select = c(duree))

# 2ème méthode de tuning de xgboost, par recherche du RMSE minimum sur le TestSet
# eta <- seq(from = 0.1, to = 1, by = 0.1)
# depth <- seq(from = 1, to = 10, by = 1)
# 
# Erreur <-0
# eta_best <-0
# depth_best <-0
# subsample_best <-0
# Erreur_Best <- 20
# 
# for(j in 1:length(depth))
# {
#   for(i in 1:length(eta))
#   {
#     param <- list("objective" = "reg:linear",
#                   "eta"=eta[i],
#                   "max.depth"=depth[j],
#                   "nthread" = 10,
#                   "min_child_weight"=1,
#                   "colsample_bytree"=1,
#                   "subsample"=1)
#     
#     modelXgboost <- xgboost(data = as.matrix(predicteurs_Train)
#                             , label = as.matrix(duree_Train)
#                             , params=param
#                             , nrounds = 100)
#     predictionXGBoost <- predict(modelXgboost,
#                                  as.matrix(predicteurs_Test))
#     
#     Erreur <- RMSE(duree_Test, predictionXGBoost)
#     if(Erreur < Erreur_Best)
#     {
#       Erreur_Best <- Erreur
#       eta_best <- eta[i]
#       depth_best <- depth[j]
#       #???subsample_best <- subsample[k]
#     }
#   }
# }

# Construction du modèle
library(xgboost)
param <- list("objective" = "reg:linear",
              "eta"=0.2,
              "max.depth"=6,
              "nthread" = 8)

modelXgboost <- xgboost(data = as.matrix(predicteurs_Train)
                        , label = as.matrix(duree_Train)
                        , params=param
                        , nrounds = 100)
predictionXGBoost <- predict(modelXgboost,
                             as.matrix(predicteurs_Test))

RMSE(duree_Test, predictionXGBoost)
MAE(duree_Test, predictionXGBoost)

# feature importance

names <- dimnames(predicteurs_Train)[[2]]
importance_matrix <- xgb.importance(names, model = modelXgboost)
xgb.plot.importance(importance_matrix)
xgb.plot.tree(feature_names = names, model = modelXgboost, n_first_tree = 2)
