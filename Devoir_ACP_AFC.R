getwd()
# Repertoire de Travail
setwd("C:/Users/Mamadou-Korka/Desktop/MULTIVARIEE")
list.dirs()
# Importation de donnees
air_pollution=read.csv("pollution.csv")
View(air_pollution)
attach(air_pollution)
# Preprocessing
dim(air_pollution)
# voire les types de variables
str(air_pollution)
# copy de la base de donnees
data=air_pollution
# enlevons les variables qualitatives en selectionnant les valeurs numeriques
var_num=unlist(lapply(data,is.numeric))
data_num=data[var_num]
str(data_num)
df=as.matrix(data_num)
# Appliquons maintenant le ACP
library("FactoMineR")
library("factoextra")
# Le Model ACP
model_ACP=PCA(df,scale.unit=TRUE, ncp=5,graph=TRUE)
model_ACP
# Recuperer les valeurs Propres et leurs Pourcentages
model_ACP$eig
# Graph avec Pourcentage
fviz_eig(model_ACP,addlabels = TRUE)
# Model ACP sur les Variables
fviz_pca_var(model_ACP)
# Model ACP sur les individus
fviz_pca_ind(model_ACP)
# Voyons la contributions de chaque variable
model_ACP$var$contrib
# Visualisation des 5 variables sur l'axe 1
fviz_contrib(model_ACP,choice = "var", axes = 1, top=5)
# Visualisation des 5 variables sur l'axe 2
fviz_contrib(model_ACP,choice = "var", axes = 2, top=5)
# Visualisation des 23463 individus sur l'axe 1
fviz_contrib(model_ACP,choice = "ind", axes = 1, top=23)
# Visualisation de50 parmisles 23463 individus sur l'axe 2
fviz_contrib(model_ACP,choice = "ind", axes = 2, top=50)
#####################################################################

# faisons le Modele AFC
N=table(data$City,data$PM2.5.AQI.Category)
CA_N=CA(N)
fviz_ca_biplot(CA_N)
