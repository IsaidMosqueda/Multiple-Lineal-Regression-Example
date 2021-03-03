route <- "specify route"
setwd(route)
library(readr)
library(car)
library(stats)
library(corrplot)
library(MASS)
library(lmtest)
library(zoo)
library(psych) 
library(nortest)
library(glmnet)

House_data <- read_csv("Real-estate-valuation-data-set.csv")

#----------------------------Rescaling of data-------------------------------------
#We want everything to be in unities so we are goinig to reescale every variable.

X2_Age.over10 <- (House_data$`X2 house age`)/10
X3_Distance_to__the_nearest_MRK.over100 <- (House_data$`X3 distance to the nearest MRT station`)/100
X4_ConvenienceStores_nearby <- (House_data$`X4 number of convenience stores`)
x5_Latitude.over10 <- (House_data$`X5 latitude`)/10
X6_Longitude.over100 <-(House_data$`X6 longitude`)/100
Y_Price.over10000 <- (House_data$`Y house price of unit area`)/10

#New data frame
#Notice that we wont add transaction date since int will afect the LM and it actually just works as an index.
House_data_2.0 <- data.frame(Y = Y_Price.over10000, X2 = X2_Age.over10, X3 = X3_Distance_to__the_nearest_MRK.over100, 
                             X4 = X4_ConvenienceStores_nearby, X5 = x5_Latitude.over10, X6 = X6_Longitude.over100)
#----------------------------Descriptive Analysis----------------------------------

# Relacion entre las variables
#Observemos que las unicas variables que tienen una relacion es la distancia a un tren y la longitud
#la cual es una relacion inversa, de ahi en fuera no tienen ninguna relacion.
pairs(House_data_2.0, main="Diagrama de Dispersion", labels = c("Precio", "Age over", "Distance to MRK",
      "Convenience Stores", "Latitude", "Longitude"), pch= 20, bg = "black", lwd = 2, col = "deepskyblue3",
      cex.labels = 1.2)

# Correlacion entre las variables
#Como lo mencionamos en el punto anterior podemos notar que la relacion entre X3 y X6 es grande pero negativa 
Correlaciones <- cor(House_data_2.0)
corrplot(Correlaciones, method = "pie", main = "Correalciones de las variables")
Correlaciones

#Histogram of prices
#Notemos que tenemos sesgo en la izquierda y que tenemos un valor extremo en 12
#tambien cabe recalcar que despues de 6 la frecuencia de los datos es muy pequeÃ±a.
hist(Y_Price.over10000, col = "green", main = "Histograma de Precios (entre 10,000)", xlab = "Precio (entre 10,000)", ylab = "Frecuencia", freq = FALSE)
lines(density(Y_Price.over10000),col = "blue", lwd = 2)

cat("La media esta en", median(Y_Price.over10000))
cat("Esta sesgada a la izquierda.")

qqnorm(Y_Price.over10000, pch=20, col = "blue")
qqline(Y_Price.over10000, col="red", lwd= 2)
cat("El precio parece ajustarse a la distribucion normal, sin embargo tiene una inclinacion positiva y una cantidad considerable de valores significativos.")

#Histogram of house's age
hist(X2_Age.over10, col = "green", main = "Histograma de la edad de las casas (entre 10)", xlab = "Edad de las casas (entre 10)", ylab = "Frecuencia", freq = FALSE)
lines(density(X2_Age.over10),col = "blue", lwd = 2)
cat("La media esta en", median(X2_Age.over10), "sin embargo tiene una distribucion bimodal." )
cat("Esta sesgada a la izquierda.")


#Histogram of the ammount of convenience stores nearby
hist(X4_ConvenienceStores_nearby, col = "green", main = "Histograma de tiendas de convenencia cerca", xlab = "Tiendas de conveniencia cerca", ylab = "Frecuencia", freq = FALSE)
lines(density(X4_ConvenienceStores_nearby),col = "blue", lwd = 2)
cat("La media esta en", median(X4_ConvenienceStores_nearby), "con una moda de 0.")
cat("Esta  muy sesgada a la izquierda.")

#Histogram of distance to nearest RMT stations
hist(X3_Distance_to__the_nearest_MRK.over100, col = "green", main = "Histograma de distancia a las estacion de tren mas cercano (entre 100)", xlab = "Distancia a las estacion de tren mas cercano (entre 1000)", ylab = "Frecuencia", freq = FALSE)
lines(density(X3_Distance_to__the_nearest_MRK.over100),col = "blue", lwd = 2)
cat("La media esta en", median(X3_Distance_to__the_nearest_MRK.over100))
cat("Esta  muy sesgada a la izquierda.")



#----------------------------Ajustar el modelo de regresion lineal------------------- 

modelo <- lm(Y_Price.over10000 ~ X2_Age.over10 + X3_Distance_to__the_nearest_MRK.over100 + X4_ConvenienceStores_nearby + x5_Latitude.over10 + X6_Longitude.over100)
summary(modelo)

b0<-modelo$coefficients[1]
b1<-modelo$coefficients[2]
b2<-modelo$coefficients[3]
b3<-modelo$coefficients[4]
b4<-modelo$coefficients[5]
b5<-modelo$coefficients[6]
cat("Los estimadores para las betas respectivamente son:", b0, b1, b2, b3, b4, b5)
# El coeficioente de correlacion es muy bajo, este modelo no es el idoneo

#----------------------------Intervalos de confianza------------------------------------

#Intervalos de confianza 
confint(modelo,level=0.95)
#Observemos que los intervalos de B0 y B6 contienen al 0, lo que nos podria decir que estos
#datos quiza no sean muy significativos 
#----------------------------Pruebas de hipotesis------------------------------------
Y <- cbind(Y_Price.over10000)
X <- cbind(X2_Age.over10,X3_Distance_to__the_nearest_MRK.over100,X4_ConvenienceStores_nearby,x5_Latitude.over10,X6_Longitude.over100)
Betha <- rbind(b1,b2,b3,b4,b5)
lambda_0 <- 0.05
n = length(Y)

A = ((t(X)%*%X)^(1))%*%t(X)
BethaHat = A %*% Y

estadistico.de.prueba.1 =(t(BethaHat-Betha)%*%t(X)%*%X%*%(BethaHat-Betha)) / t(Y-X%*%BethaHat)%*%(Y-X%*%BethaHat)
Prueba <- estadistico.de.prueba.1 >= lambda_0^(-2/n) - 1
Prueba

if(Prueba){
  cat("No hay pruebas suficientes para rechazar H0")
} else{
  cat("Hay pruebas suficientes para rechazar H0")
}

#----------------------------Analisis de la varianza a traves de la tabla ANOVA------------

#Tabla ANOVA
anova(modelo)
#El valor F de todas las varaiables es significativo pero el de X6 no.

#----------------------------Medidas de bondad de ajuste del modelo------------

#Para esto obtendremos los valores de R^2 y R^2 ajustada, y veremos que tan explicativo es el modelo.
R.square <- summary(modelo)[8]
R.square

R.square.adjusted <- summary(modelo)[9]
R.square.adjusted
#Vemos que no es muy explicativo es modelo ya que solo esplica un poco mas del 50%

#----------------------------Intervalos de confianza para la respuesta media------------------------------------
nuevos_datos <- data.frame(X2 = X2_Age.over10, X3 = X3_Distance_to__the_nearest_MRK.over100, 
                           X4 = X4_ConvenienceStores_nearby,  X5 = x5_Latitude.over10, X6 = X6_Longitude.over100)

ic <- predict(modelo, nuevos_datos, interval = "confidence")
#View(ic)

#----------------------------Intervalos de prediccion para una nueva observacion------------------------------------

ip <- predict(modelo, nuevos_datos, interval = "prediction")
#View(ip)

#----------------------------Multicolinealidad y Factores de inlfacion de la variancia-------------------------

vif(modelo)
vif(modelo)>10

#No existe multicolinealidad entre las variables pero veamos si podemos mejorar el modelo apartir de optimizar la verosimilitud 

#----------------------------Analisis de residuales--------------------------------------------------
residuos <- rstandard(modelo)
valores_ajustados <- fitted(modelo)
par(mfrow=c(2,2))
plot(valores_ajustados, residuos, pch=20, col="blue", 
     main = "Residuos vs Y ajustados")
abline(0,0, col="red", lwd=2, lty=2)
abline(2,0, col="green", lwd=2, lty=2)
abline(-2,0, col="green", lwd=2, lty=2)

qqnorm(residuos, col= "deepskyblue3", pch= 20)
qqline(residuos, col = "red")

plot(residuos, col= "orange", pch= 20, main="residuos vs tiempo")
abline(0,0, col="red", lwd=2, lty=2)
abline(2,0, col="green", lwd=2, lty=2)
abline(-2,0, col="green", lwd=2, lty=2)

acf(residuos, main= "Serie de los residuos", col="red")
par(mfrow=c(1,1))
#Podemos notar que en la grafica Q-Qplot tenemos colas ligeras y por esto no tenemos valores extremos
#viendo Residuos vs Tiempo tambien podemos ver esto pero en la serie de residuos no se pueden notar 

#Pruebas formales

#Homocedasticidad
bptest(modelo)
ncvTest(modelo)

#Correlacion
dwtest(modelo)
bgtest(modelo)

#Normalidad
ad.test(residuos)
shapiro.test(residuos)
#Atravez de las pruebas podemos ver que el modelo cumple la homocedasticidad y no existe correlacion entre las variables
#pero los residuos no cumplen distribuirse normal, por lo que tendremos que usar transformaciones 

#----------------------------Influencia de los datos--------------------------------------
outlierTest(modelo)

dist_cook <- cooks.distance(modelo)
head(dist_cook)
influenceIndexPlot(modelo, vars = "Cook", id.cex = 0.7, id.n = 3, col="red")
#Ningun valor es influyente ya que ninguna distancia de cook sobrepasa el 1

hat_valores <- hatvalues(modelo)
head(hat_valores)
influenceIndexPlot(modelo, vars = "hat", id.cex = 0.7, id.n = 3, col="red")

#Quitamos los outliers
House_data_3 <- House_data_2.0[-c( 271, 313, 114, 221),]

Y_new <- House_data_3$Y
X2_new <- House_data_3$X2
X3_new <- House_data_3$X3
X4_new <- House_data_3$X4
X5_new <- House_data_3$X5
X6_new <- House_data_3$X6

modelo2 <- lm(Y_new ~ X2_new + X3_new + X4_new + X5_new + X6_new)
summary(modelo2)
summary(modelo)
#El modelo mejora en cuanto a la R2 primero era de 57.12% y ahora de 64.75%

#Veremos si cambia algo respecto al anaisis de residuales
residuos2 <- rstandard(modelo2)

#Homocedasticidad
bptest(modelo2)
ncvTest(modelo2)

#Correlacion
dwtest(modelo2)
bgtest(modelo2)

#Normalidad
ad.test(residuos2)
shapiro.test(residuos2)
#Seguimos sin lograr tener normalidad.

# Ahora vemos la diferencia entre los valores estimados de nuestros parametros
cat("las direfencias para B0, B1, B2, B3, B4 y B5 son:", b0-modelo2$coefficients[1],
    b1-modelo2$coefficients[2], b2-modelo2$coefficients[3], b3-modelo2$coefficients[4],
    b4-modelo2$coefficients[5], b5-modelo2$coefficients[6], "respectivamente.")

modelo$coefficients
modelo2$coefficients

#El cambio mas significativo aparece en B0, B4 y B5

#----------------------------Transformaciones-----------------------------------------------
  #Utilizaremos la transformacion de box cox 
  boxcox(modelo2)
  
  y_transf <-log(Y_new)
  
  modelo_boxcox <- lm(y_transf ~ X2_new + X3_new + X4_new + X5_new +X6_new)
  summary(modelo_boxcox)
  
  #Analisemos los residuales
residuos_BC <- rstandard(modelo_boxcox)

#Normalidad
ad.test(residuos_BC)
shapiro.test(residuos_BC)

#Como no cumple el supuesto de normalidad, probaremos con otra transformación

#Utilizaremos la transformacion Raiz de y
X2op <- X2_new^5
X3op <- X3_new^5
X4op <- X4_new^5
X5op <- X5_new^5
X6op <- X6_new^5

modelo_op <- lm(y_transf ~ X2op + X3op + X4op + X5op +X6op)
summary(modelo_op)

#Analisemos los residuales
residuos_op <- rstandard(modelo_op)
#Normalidad
ad.test(residuos_op)
shapiro.test(residuos_op)

#Notemos que ahora si cumple todos los supuestos, sin embargo disminuye R^2 a 0.58.
#Procederemos inmediatamente a hacer el análisis de los valores influyentes, y ver como mejora R^2.

#----------------------------Influencia de los datos--------------------------------------
outlierTest(modelo_op)

dist_cook_2.0 <- cooks.distance(modelo_op)
head(dist_cook_2.0)
influenceIndexPlot(modelo_op, vars = "Cook", id.cex = 0.7, id.n = 3, col="red")
#Ningun valor es influyente ya que ninguna distancia de cook sobrepasa el 1

hat_valores_2.0 <- hatvalues(modelo_op)
head(hat_valores_2.0)
influenceIndexPlot(modelo_op, vars = "hat", id.cex = 0.7, id.n = 3, col="red")

#Quitamos los outliers
House_data_4.0 <- House_data_3[-c( 148),]

#Definimos las variables on la transformación encontrada previamente
Y_def <- log(House_data_4.0$Y)
X2_def <- House_data_4.0$X2^5
X3_def <- House_data_4.0$X3^5
X4_def <- House_data_4.0$X4^5
X5_def <- House_data_4.0$X5^5
X6_def <- House_data_4.0$X6^5

modelo_def <- lm(Y_def ~ X2_def + X3_def + X4_def + X5_def + X6_def)
summary(modelo_def)

#Notese eque R^2 incremento ligeramente, sin embargo ahora es mas cercana a R2 ajustada
#Dado que ahora el modelo cuenta, con todos los supuestos, volveremos a realizar su analisis. 

#Diferencia del vector de bethas nuevo y el original
diff_bethas <- modelo_def$coefficients - modelo$coefficients
diff_bethas
#----------------------------Intervalos de confianza 2.0------------------------------------

#Intervalos de confianza 
confint(modelo_def,level=0.95)

#----------------------------Pruebas de hipotesis 2.0------------------------------------
Y_2.0 <- cbind(Y_def)
X_2.0 <- cbind(X2_def,X3_def,X4_def,X5_def,X6_def)

b0_def<-modelo_def$coefficients[1]
b1_def<-modelo_def$coefficients[2]
b2_def<-modelo_def$coefficients[3]
b3_def<-modelo_def$coefficients[4]
b4_def<-modelo_def$coefficients[5]
b5_def<-modelo_def$coefficients[6]

Betha_def <- rbind(b1_def,b2_def,b3_def,b4_def,b5_def)
lambda_0_def <- 0.05
n_def = length(Y_2.0)

A_def = ((t(X_2.0)%*%X_2.0)^(1))%*%t(X_2.0)
BethaHat_def = A_def %*% Y_2.0

estadistico.de.prueba.1_2.0 =(t(BethaHat_def-Betha_def)%*%t(X_2.0)%*%X_2.0%*%(BethaHat_def-Betha_def)) / t(Y_2.0-X_2.0%*%BethaHat_def)%*%(Y_2.0-X_2.0%*%BethaHat_def)
Prueba_2.0 <- estadistico.de.prueba.1_2.0 >= lambda_0_def^(-2/n_def) - 1
Prueba_2.0

if(Prueba_2.0){
  cat("No hay pruebas suficientes para rechazar H0")
} else{
  cat("Hay pruebas suficientes para rechazar H0")
}

#----------------------------Analisis de la varianza a traves de la tabla ANOVA 2.0------------

#Tabla ANOVA
anova(modelo_def)

#----------------------------Medidas de bondad de ajuste del modelo 2.0---------------------

#Para esto obtendremos los valores de R^2 y R^2 ajustada, y veremos que tan explicativo es el modelo.
R.square_2.0 <- summary(modelo_def)[8]
R.square_2.0

R.square.adjusted_2.0 <- summary(modelo_def)[9]
R.square.adjusted_2.0

#----------------------------Intervalos de confianza para la respuesta media 2.0------------------------------------
nuevos_datos_2.0 <- data.frame(X2def = X2_def, X3def = X3_def, X4def = X4_def,  X5def = X5_def, X6def = X6_def)

ic_2.0 <- predict(modelo_def, nuevos_datos_2.0, interval = "confidence")

#----------------------------Intervalos de prediccion para una nueva observacion 2.0------------------------------------

ip_2.0 <- predict(modelo_def, nuevos_datos_2.0, interval = "prediction")

#----------------------------Multicolinealidad y Factores de inlfacion de la variancia 2.0-------------------------

vif(modelo_def)
vif(modelo_def)>10

#No existe multicolinealidad entre las variables pero veamos si podemos mejorar el modelo apartir de optimizar la verosimilitud 

#----------------------------Analisis de residuales 2.0-------------------------------------------------------------
residuos_2.0 <- rstandard(modelo_def)
valores_ajustados_2.0 <- fitted(modelo_def)
par(mfrow=c(2,2))
plot(valores_ajustados_2.0, residuos_2.0, pch=20, col="blue", 
     main = "Residuos vs Y ajustados")
abline(0,0, col="red", lwd=2, lty=2)
abline(2,0, col="green", lwd=2, lty=2)
abline(-2,0, col="green", lwd=2, lty=2)

qqnorm(residuos_2.0, col= "deepskyblue3", pch= 20)
qqline(residuos_2.0, col = "red")

plot(residuos_2.0, col= "orange", pch= 20, main="residuos vs tiempo")
abline(0,0, col="red", lwd=2, lty=2)
abline(2,0, col="green", lwd=2, lty=2)
abline(-2,0, col="green", lwd=2, lty=2)

acf(residuos_2.0, main= "Serie de los residuos", col="red")
par(mfrow=c(1,1))

#Pruebas formales

#Homocedasticidad
bptest(modelo_def)
ncvTest(modelo_def)
#Notemos que a pesar de que no pasa la prueba de BP, si pasa ncv y gráficamente podemos notar que 
#estos no son precisamente aleatorios.

#Correlacion
dwtest(modelo_def)
bgtest(modelo_def)

#Normalidad
ad.test(residuos_2.0)
shapiro.test(residuos_2.0)