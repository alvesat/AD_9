#######[AD-UFPE-2019] LISTA 9 ###########################################################
####### ANTONIO FERNANDES ###############################################################

###### EXERCICIO 4.1 ######################################################################

options(scipen=999)

###### 4.1 a) ######################################################################

  # lendo banco worldrecall

worldrecall <- read.delim("~/Dados/Listas/AD_9/AD_9/worldrecall.txt")

  # modelo linear

Linear <- lm(prop ~ time, data = worldrecall)

  # resumo do modelo

summary(Linear)

  # ajuste do modelo

plot(worldrecall$time, worldrecall$prop)
abline(lm(prop ~ time, data = worldrecall), col = 'red')

plot(Linear, which=2, col=c("red")) # Residuals vs Fitted Plot

plot(Linear, which=1, col=c("blue")) # Q-Q plot


    ##### modelo com vi em log ####

  # colocando vi em log

worldrecall$timeln <- log(worldrecall$time)

  # modelo linear

Linear_ln <- lm(prop ~ timeln, data = worldrecall)

  # resumo do modelo

summary(Linear_ln)

  # ajuste do modelo

plot(worldrecall$timeln, worldrecall$prop)
abline(lm(prop ~ timeln, data = worldrecall))

plot(Linear_ln, which=2, col=c("red")) # Residuals vs Fitted Plot

plot(Linear_ln, which=1, col=c("blue")) # Q-Q plot]

###### 4.1 b) ######################################################################

  # lendo banco shortleaf

shortleaf <- read.delim("~/Dados/Listas/AD_9/AD_9/shortleaf.txt")

  # modelo linear

Linear <- lm(Vol ~ Diam, data = shortleaf)

  # resumo do modelo

summary(Linear)

  # ajuste do modelo

plot(shortleaf$Diam, shortleaf$Vol)
abline(lm(Vol ~ Diam, data = shortleaf), col = 'red')

plot(Linear, which=2, col=c("red")) # Residuals vs Fitted Plot

plot(Linear, which=1, col=c("blue")) # Q-Q plot

  # histrograma das variáveis

hist(shortleaf$Diam)
hist(shortleaf$Vol)

  
