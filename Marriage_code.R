library(readr)
library(arm)
data <- read.csv("microdata_salari.csv", sep=",")

##1
pdf("C:/Users/lory0/Desktop/Università/Statistica aziendale/Rplot.pdf")
par(mfrow = c(1,2))
hist(data$WAGE,  freq = F, xlab = "Salario", ylab = "Densità", ylim = c(0,0.12), xlim = c(0,45), col = "mediumturquoise", main = "Distribuzione")
lines(density(data$WAGE), col = "red")
box()
hist(log(data$WAGE),  freq = F, xlab = "Salario", ylab = "Densità",  ylim = c(0,1), col = "mediumturquoise", main = "Distribuzione logaritmica")
lines(density(log(data$WAGE)), col = "red")
box()


summary(data$WAGE)
dev.off()

##2
pdf("C:/Users/lory0/Desktop/Università/Statistica aziendale/Rplot01.pdf")
par(mfrow = c(2,2))
data$OCCUPAZIONE <- as.factor(data$OCCUPATION)
levels(data$OCCUPAZIONE) <- c("Manager", "Addetti alle vendite" , "Impiegati", "Operai" , "Professionisti" , "Altro")
plot(data$OCCUPAZIONE, data$WAGE, xlab = "Occupazione", ylab = "Salario (in dollari americani)")

data$SETTORE <- ifelse(data$SECTOR == 0, "Altro", ifelse(data$SECTOR == 1, "Manifatturiero", "Costruzioni"))
data$SETTORE <- as.factor(data$SETTORE)
plot(data$SETTORE, data$WAGE, xlab = "Settore Economico", ylab = "Salario (in dollari americani)")

data$SINDACATO <-data$UNION 
data$SINDACATO <- as.factor(data$SINDACATO)
levels(data$SINDACATO) <- c("Non iscritto", "Iscritto")
plot(data$SINDACATO, data$WAGE, xlab = "Iscrizione ad un sindacato", ylab = "Salario (in dollari americani)", )


mean(data$WAGE[data$SINDACATO=="Non iscritto"])
mean(data$WAGE[data$SINDACATO=="Iscritto"])


data$ISTRUZIONE <- ifelse(data$EDUCATION >= 0 & data$EDUCATION < 12, "Istruzione base", ifelse(data$EDUCATION >= 12 & data$EDUCATION <= 15, "Primo livello", "Secondo livello"))

data$ISTRUZIONE <- as.factor(data$ISTRUZIONE)
plot(data$ISTRUZIONE, data$WAGE, xlab = "Livello d'istruzione", ylab = "Salario (in dollari americani)")

dev.off()

pdf("C:/Users/lory0/Desktop/Università/Statistica aziendale/Rplot02.pdf")

par(mfrow = c(2,2))
data$DONNA <- data$SEX
data$DONNA <- as.factor(data$DONNA)
levels(data$DONNA) <- c("Uomo", "Donna")
plot(data$DONNA, data$WAGE, xlab = "Sesso", ylab = "Salario (in dollari americani)")

data$MATRIMONIO <- as.factor(data$MARR)
levels(data$MATRIMONIO) <- c("Non sposato", "Sposato")
plot(data$MATRIMONIO, data$WAGE, xlab = "Stato civile", ylab = "Salario (in dollari americani)")

data$ETNIA <- as.factor(data$RACE)
levels(data$ETNIA) <- c("Altrimenti", "Ispanica", "Caucasica")
data$ETNIA <- relevel(as.factor(data$ETNIA), "Caucasica")
plot(data$ETNIA, data$WAGE, xlab = "Etnia", ylab = "Salario (in dollari americani)")

data$SUD <- as.factor(data$SOUTH)
levels(data$SUD) <- c("Altrimenti", "Sud")
plot(data$SUD, data$WAGE, xlab = "Residenza", ylab = "Salario (in dollari americani)")

dev.off()

data$ESPERIENZA <- data$EXPERIENCE

pdf("C:/Users/lory0/Desktop/Università/Statistica aziendale/Rplot03.pdf")

par(mfrow=c(1,2))
plot(data$AGE, data$WAGE, main = "Salario in funzione dell'età", xlab = "Età", ylab = "Salario (in dollari americani)", cex.lab = 1.2, pch = 19, col = ifelse(data$DONNA == "Uomo", "blue", "red"))
legend("topright", paste(c("donne", "uomini")), col=c("red", "blue"), pch = 16, cex = 0.9)

hist(data$AGE, xlab = "Età" , ylab = "Densità", xlim = c(15,65), freq = F, col = "mediumaquamarine", main = "Istogramma dell'età")
box()

dev.off()
pdf("C:/Users/lory0/Desktop/Università/Statistica aziendale/Rplot04.pdf")
par(mfrow=c(1,2))

plot(data$ESPERIENZA, data$WAGE, main = "Salario in funzione dell'esperienza",xlab = "Anni di esperienza lavorativa", ylab = "Salario (in dollari americani)", cex.lab = 1.2, pch = 19, col = ifelse(data$DONNA == "Uomo", "blue", "red"))
legend("topright", paste(c("donne", "uomini")), col=c("red", "blue"), pch = 16, cex = 0.9)


hist(data$ESPERIENZA, xlab = "Esperienza" , ylab = "Densità", xlim = c(0,55), freq = F, col = "mediumaquamarine", main = "Istogramma dell'esperienza")
box()
dev.off()





#3
mean(data$WAGE[data$SEX==0])
mean(data$WAGE[data$SEX==1])

mean(data$WAGE[data$ETNIA=="Altrimenti" | data$ETNIA=="Ispanica"])
mean(data$WAGE[data$ETNIA=="Caucasica"])

mean(data$WAGE[data$OCCUPAZIONE=="Manager"])
mean(data$WAGE[data$OCCUPAZIONE=="Addetti alle vendite"])
mean(data$WAGE[data$OCCUPAZIONE=="Impiegati"])
mean(data$WAGE[data$OCCUPAZIONE=="Operai"])
mean(data$WAGE[data$OCCUPAZIONE=="Professionisti"])
mean(data$WAGE[data$OCCUPAZIONE=="Altro"])


#4

ricod <- function(vec){
  ifelse(vec == 1, 0 , 1) 
}
data$DONNA <- as.numeric(data$DONNA)
data$DONNA <- ricod(data$DONNA)

data$MATRIMONIO <- as.numeric(data$MATRIMONIO)
data$MATRIMONIO <- ricod(data$MATRIMONIO)

data$SUD <- as.numeric(data$SUD)
data$SUD <- ricod(data$SUD)

data$SINDACATO <- as.numeric(data$SINDACATO)
data$SINDACATO <- ricod(data$SINDACATO)

#data$SUD <- ifelse(data$SUD == "Altrimenti", 0 , 1)

#data$SINDACATO <- ifelse(data$SINDACATO == "Non iscritto", 0 , 1)

modello1 <- lm(log(WAGE)~DONNA,data = data)
display(modello1)

exp(0.23)-1
exp(2.17)

modello2 <- lm(log(WAGE)~MATRIMONIO,data = data)
display(modello2)


exp(0.15)-1

##5

modello3 <- lm(log(WAGE)~OCCUPAZIONE + SINDACATO + ISTRUZIONE + DONNA + MATRIMONIO + ETNIA + SUD + rescale(ESPERIENZA) + DONNA:MATRIMONIO, data = data)
display(modello3,3)

exp(2.077)

##5.1

uomo_sposato = coef(modello3)[1] + coef(modello3)[15] *((20-mean(data$ESPERIENZA))/(2*sd(data$ESPERIENZA)))+ coef(modello3)[7] + coef(modello3)[11] + coef(modello3)[14]

previsione_uomo_sposato <- exp(uomo_sposato)

uomo_single = coef(modello3)[1] + coef(modello3)[15] *((20-mean(data$ESPERIENZA))/(2*sd(data$ESPERIENZA)))+ coef(modello3)[7] + coef(modello3)[14]

previsione_uomo_single <- exp(uomo_single)

donna_sposata = coef(modello3)[1] + coef(modello3)[15] *((20-mean(data$ESPERIENZA))/(2*sd(data$ESPERIENZA)))+ coef(modello3)[7] + coef(modello3)[11] + coef(modello3)[14] + coef(modello3)[10] + coef(modello3)[16]

previsione_donna_sposata <- exp(donna_sposata)

donna_single = coef(modello3)[1] + coef(modello3)[15] *((20-mean(data$ESPERIENZA))/(2*sd(data$ESPERIENZA)))+ coef(modello3)[7] + coef(modello3)[14] + coef(modello3)[10]

previsione_donna_single <- exp(donna_single)

##5.2

pdf("C:/Users/lory0/Desktop/Università/Statistica aziendale/Rplot05.pdf")

par(mfrow = c(1,2))
##uomini
plot( range(data$ESPERIENZA),range(data$WAGE) , xlab="Esperienza", ylab="Salario orario", pch=20, type="n", main="Uomini", ylim = c(5,11))
curve(exp((cbind(1,0,0,0,0,0,0,0,0,0,0,0,0,0,rescale(x),0) %*% coef(modello3))), add = T , col = "black") ##non sposati
curve(exp((cbind(1,0,0,0,0,0,0,0,0,0,1,0,0,0,rescale(x),0) %*% coef(modello3))), add = T , col = "red") ##sposati
legend("topleft", paste(c("Non sposati", "Sposati")), col=c("black", "red"), lty = 1)

##donne
plot(  range(data$ESPERIENZA),range(data$WAGE) , xlab="Esperienza", ylab="Salario orario", pch=20, type="n", main="Donne", ylim = c(5,11))
curve(exp((cbind(1,0,0,0,0,0,0,0,0,1,0,0,0,0,rescale(x),0) %*% coef(modello3))), add = T , col = "black") ##non sposati
curve(exp((cbind(1,0,0,0,0,0,0,0,0,1,1,0,0,0,rescale(x),1) %*% coef(modello3))), add = T , col = "red") ##sposati
legend("topleft", paste(c("Non sposati", "Sposati")), col=c("black", "red"), lty = 1)

dev.off()

##DIAGNOSTICA
pdf("C:/Users/lory0/Desktop/Università/Statistica aziendale/Rplot06.pdf")
par(mfrow=c(2,2))
plot(modello3$fitted.values, modello3$model[, 1], xlab = "Valori Stimati", 
     ylab = "Salario orario", cex = 0.6) +	lines(modello3$fitted.values, modello3$fitted.values , xlim = c(1,3), col = "red")

plot(modello3$fitted.values, modello3$residuals, xlab = "Valori Stimati", ylab = "Residui", cex = 0.6, xlim = c(1,3))

abline(h = 0, col = "red") 
abline(h=-sigma.hat(modello3), lty=2)
abline(h=+sigma.hat(modello3), lty=2)

qqnorm(modello3$residuals, cex = 0.6, main = "", xlab = "Quantili", ylab = "Residui")

qqline(modello3$residuals, col = "red")

hist(modello3$residuals, freq = FALSE, col = "gray", main = "", xlab = "Residui",ylab = "Densita'", ylim=c(0, 1)) 

lines(density(modello3$residuals))

curve(dnorm(x, 0, sigma.hat(modello3)), -3 * sigma.hat(modello3), 4 * sigma.hat(modello3), add = TRUE, col="red")

box()
dev.off()

sd(data$ESPERIENZA) * 2

