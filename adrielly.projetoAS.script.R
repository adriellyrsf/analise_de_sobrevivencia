#Pré-analise Projeto

dados <- read.csv2("C:/Users/Adrielly/Downloads/dados.infarto.csv")

library(survival)
library(questionr)
library(KMsurv)
library(ggplot2)
library(survminer)
library(ggExtra)
library(ggfortify)
library(gtsummary)

#Estimativas - Geral
tempo <- dados$fim - dados$inicio
status <- dados$status
ekm= survfit(Surv(tempo, status)~1)

summary(ekm)

plot(ekm, ylab="S(t) Estimada", xlab="Tempo (dias)", col = c("red") , conf.int=F)


#Genero
ekmg= survfit(Surv(tempo, status)~dados$sexo)

summary(ekmg)

plot(ekmg, ylab="S(t) Estimada", xlab="Tempo (dias)", col = c("red", "green"), conf.int=F)
legend(1,0.3,lty=c(2,1),c("Feminino", "Masculino"), col = c("red", "green"), lwd=1, bty="n")


#Idade
ekmidade= survfit(Surv(tempo, status)~dados$idade)

summary(ekmidade)

plot(ekmidade, ylab="S(t) Estimada", xlab="Tempo (dias)", col = c("red", "green", "blue"), conf.int=F)
legend(1,0.5,lty=c(3,2,1),c("Menos que 40 anos", "Entre 40 e 60", "Mais que 60 anos"), col = c("red", "green","blue"), lwd=1, bty="n")
 

#Hospital - Volume
ekmvol= survfit(Surv(tempo, status)~dados$volume)

summary(ekmvol)

plot(ekmvol, ylab="S(t) Estimada", xlab="Tempo (dias)", col = c("red", "blue"), conf.int=F)
legend(1,0.5,lty=c(2,1),c("Menor que 25", "Maior ou igual a 25"), col = c("red","blue"), lwd=1, bty="n")


#Hospital - Natureza
ekmnat= survfit(Surv(tempo, status)~dados$natureza)

summary(ekmnat)

plot(ekmnat, ylab="S(t) Estimada", xlab="Tempo (dias)",col = c("red", "green", "blue", "yellow"), conf.int=F)
legend(1,0.5,lty=c(4,3,2,1),c("Municipal", "Contratado", "Estadual", "Federal e Universitário"), col = c("red", "green","blue", "yellow"), lwd=1, bty="n")


#Hospital - Leitos de UTI
ekmluti= survfit(Surv(tempo, status)~dados$luti)

summary(ekmluti)

plot(ekmluti, ylab="S(t) Estimada", xlab="Tempo (dias)", col = c("red", "green", "blue") , conf.int=F)
legend(1,0.5,lty=c(3,2,1),c("n = nenhum", "1a24= de 1 a 25", "25+= mais que 25"), col = c("red", "green","blue"), lwd=1, bty="n")

#Genero + Idade
ekmgi= survfit(Surv(tempo, status)~dados$sexo+dados$idade)

summary(ekmgi)

plot(ekmgi, ylab="S(t) Estimada", xlab="Tempo (dias)", col = c("red", "green", "blue", "yellow", "brown", "orange"), conf.int=F)
legend(1,0.8,lty=c(6,5,4,3,2,1),c("Feminino -40", "Masculino -40", "Feminino 40a60", "Masculino 40a60", "Feminino 60+", "Masculino 60+" ), col = c("red", "green", "blue", "yellow", "brown", "orange"), lwd=1, bty="n")


##########Testes de LogRank - rho = 0 #################################################
#Genero
survdiff(Surv(tempo,status)~dados$sexo,rho=0)

#Idade

survdiff(Surv(tempo,status)~dados$idade,rho=0)

#Natureza

survdiff(Surv(tempo,status)~dados$natureza,rho=0)

#Volume

survdiff(Surv(tempo,status)~dados$volume,rho=0)

#Leitos

survdiff(Surv(tempo,status)~dados$luti,rho=0)


##########Teste de Wilcoxon - rho = 1  #################################################

#Genero
survdiff(Surv(tempo,status)~dados$sexo, rho=1)

#Idade

survdiff(Surv(tempo,status)~dados$idade,rho=1)

#Natureza

survdiff(Surv(tempo,status)~dados$natureza,rho=1)

#Volume

survdiff(Surv(tempo,status)~dados$volume,rho=1)

#Leitos

survdiff(Surv(tempo,status)~dados$luti,rho=1)


#####Função Taxa de falha: ########################################

# Calcular a taxa de falha
taxa_falha <- 1 - ekm$surv

plot(ekm$time, taxa_falha, type = "s", 
     xlab = "Tempo (em dias)", ylab = "Taxa de Falha", ylim = c(0, 1), col = "red")

####### Tabelas de Frequencia ###################################

# Gráfico de barras para a frequência de cada covariável

#Genero
ggplot(dados, aes(x = sexo, fill = sexo)) +
  geom_bar() +
  scale_fill_manual(values = c("M" = "skyblue", "F" = "red")) +
  labs(x = "Gênero", y = "Frequência") +  # Nomes dos eixos x e y
  theme_minimal()

#Natureza
ggplot(dados, aes(x = natureza, fill = natureza)) +
  geom_bar() +
  scale_fill_manual(values = c("PM" = "skyblue", "C" = "red",  "PE" = "yellow", "PFU" = "green")) +
  labs(x = "Natureza Juridica", y = "Frequência") +  # Nomes dos eixos x e y
  theme_minimal()

#Volume
ggplot(dados, aes(x = volume, fill = volume)) +
  geom_bar() +
  scale_fill_manual(values = c("vp" = "skyblue", "vg" = "red")) +
  labs(x = "Volume", y = "Frequência") +  # Nomes dos eixos x e y
  theme_minimal()

#Luti
ggplot(dados, aes(x = luti, fill = luti)) +
  geom_bar() +
  scale_fill_manual(values = c("n" = "skyblue", "1a24" = "red", "25+"= "green")) +
  labs(x = "Leitos de UTI", y = "Frequência") +  # Nomes dos eixos x e y
  theme_minimal()

#Status
ggplot(dados, aes(x = factor(status), fill = factor(status))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "red"), 
        name = "Status",  # Renomeia a legenda
       labels = c("0" = "0", "1" = "1")) +  # Etiquetas personalizadas
       labs(x = "Status", y = "Frequência") +  # Nomes dos eixos x e y
  theme_minimal()

#Idade
ggplot(dados, aes(x = idade, fill = idade)) +
  geom_bar() +
  scale_fill_manual(values = c("-40" = "skyblue", "40a60" = "red", "60+"= "green")) +
  labs(x = "Idades Categorizadas", y = "Frequência") +  # Nomes dos eixos x e y
  theme_minimal()


#Modelo de Cox #######################################################################

#Seleção dos modelos

#Difinindo as covariaveis
sexo = as.factor(dados$sexo)
idade = as.factor(dados$idade)
volume = as.factor(dados$volume )
natureza = as.factor(dados$natureza)
luti = as.factor(dados$luti)
ini= dados$inicio
fim= dados$fim


#Modelo 1
modelo1=coxph(Surv(ini,fim,status) ~ sexo + idade + natureza + volume + luti, x = T, method = "breslow")
summary(modelo1)

-2*modelo1$loglik[2]
modelo1$loglik

#Modelo 2
modelo2=coxph(Surv(ini,fim,status)~sexo + idade + luti + natureza, x = T, method = "breslow")
summary(modelo2)

-2*modelo2$loglik[2]
modelo2$loglik

#Modelo 3
modelo3=coxph(Surv(ini,fim,status)~sexo + idade + natureza, x = T, method = "breslow")
summary(modelo3)

-2*modelo3$loglik[2]
modelo3$loglik

#Modelo 4
modelo4=coxph(Surv(ini,fim,status)~sexo + idade + luti, x = T, method = "breslow")
summary(modelo4)

-2*modelo4$loglik[2]
modelo4$loglik

modelo5= coxph(Surv(ini,fim,status)~sexo + idade, x=T, method = "breslow")
summary(modelo5)

modelo5$loglik
-2*modelo5$loglik[2]

#P-valor para as comparações dos testes
#modelo 1x2
p_value1 <- 1 - pchisq(0.473, 1); p_value1

#modelo 2x3
p_value2 <- 1 - pchisq(24.647, 1); p_value2

#modelo 2x4
p_value3 <- 1 - pchisq(31.117, 1); p_value3

#Veridicando Suposições


test.ph <- cox.zph(modelo2);test.ph

testph<-cox.zph(modelo2)
ggcoxzph(testph)

#Graficos log vs tempo
par(mfrow=c(2,2))
fit<-coxph(Surv(tempo[sexo=="F"],status[sexo=="F"]) ~ 1, data=dados, x = T, method="breslow")
ss<- survfit(fit)
s0<-round(ss$surv,digits=5)
H0<- -log(s0)
plot(ss$time,log(H0), xlab="Tempos",ylim=range(c(-5,1)),ylab = expression(log(Lambda[0]* (t))),bty="n",type="s", col = "red")
fit<-coxph(Surv(tempo[sexo=="M"],status[sexo=="M"]) ~ 1, data=dados, x = T, method="breslow")
ss<- survfit(fit)
s0<-round(ss$surv,digits=5)
H0<- -log(s0)
lines(ss$time,log(H0),type="s",lty=4, , col = "green")
title("GENERO")

fit_40m <- coxph(Surv(tempo[idade=="-40"], status[idade=="-40"]) ~ 1, data=dados, x = T, method = "breslow")
ss_40m <- survfit(fit_40m)
s0_40m <- round(ss_40m$surv, digits = 5)
H0_40m <- -log(s0_40m)
plot(ss_40m$time, log(H0_40m), xlab = "Tempos", ylim = range(c(-5, 1)), ylab = expression(log(Lambda[0]* (t))), 
     bty = "n", type = "s", col = "blue")

# Gráfico para o grupo 40 a 60 anos (idade == "40a60")
fit_40a60 <- coxph(Surv(tempo[idade=="40a60"], status[idade=="40a60"]) ~ 1, data=dados, x = T, method = "breslow")
ss_40a60 <- survfit(fit_40a60)
s0_40a60 <- round(ss_40a60$surv, digits = 5)
H0_40a60 <- -log(s0_40a60)
lines(ss_40a60$time, log(H0_40a60), type = "s", lty = 4, col = "red")

# Gráfico para o grupo 60+ anos (idade == "60+")
fit_60p <- coxph(Surv(tempo[idade=="60+"], status[idade=="60+"]) ~ 1, data=dados, x = T, method = "breslow")
ss_60p <- survfit(fit_60p)
s0_60p <- round(ss_60p$surv, digits = 5)
H0_60p <- -log(s0_60p)
lines(ss_60p$time, log(H0_60p), type = "s", lty = 2, col = "green")

# Título do gráfico
title("IDADE")


# Para o grupo natureza == "PM"
fit_pm <- coxph(Surv(tempo[natureza=="PM"], status[natureza=="PM"]) ~ 1, data=dados, x = T, method = "breslow")
ss_pm <- survfit(fit_pm)
s0_pm <- round(ss_pm$surv, digits = 5)
H0_pm <- -log(s0_pm)
plot(ss_pm$time, log(H0_pm), xlab = "Tempos", ylim = range(c(-5, 1)), 
     ylab = expression(log(Lambda[0]* (t))), bty = "n", type = "s", 
     col = "blue")  # Cor azul para "PM"

# Para o grupo natureza == "C"
fit_c <- coxph(Surv(tempo[natureza=="C"], status[natureza=="C"]) ~ 1, data=dados, x = T, method = "breslow")
ss_c <- survfit(fit_c)
s0_c <- round(ss_c$surv, digits = 5)
H0_c <- -log(s0_c)
lines(ss_c$time, log(H0_c), type = "s", lty = 2, col = "red")  # Cor vermelha para "C"

# Para o grupo natureza == "PE"
fit_pe <- coxph(Surv(tempo[natureza=="PE"], status[natureza=="PE"]) ~ 1, data=dados, x = T, method = "breslow")
ss_pe <- survfit(fit_pe)
s0_pe <- round(ss_pe$surv, digits = 5)
H0_pe <- -log(s0_pe)
lines(ss_pe$time, log(H0_pe), type = "s", lty = 3, col = "green")  # Cor verde para "PE"

# Para o grupo natureza == "PFU"
fit_pfu <- coxph(Surv(tempo[natureza=="PFU"], status[natureza=="PFU"]) ~ 1, data=dados, x = T, method = "breslow")
ss_pfu <- survfit(fit_pfu)
s0_pfu <- round(ss_pfu$surv, digits = 5)
H0_pfu <- -log(s0_pfu)
lines(ss_pfu$time, log(H0_pfu), type = "s", lty = 4, col = "purple")  # Cor roxa para "PFU"
title("NATUREZA")

# Para o grupo luti == "n"
fit_n <- coxph(Surv(tempo[luti=="n"], status[luti=="n"]) ~ 1, data=dados, x = T, method = "breslow")
ss_n <- survfit(fit_n)
s0_n <- round(ss_n$surv, digits = 5)
H0_n <- -log(s0_n)
plot(ss_n$time, log(H0_n), xlab = "Tempos", ylim = range(c(-5, 1)), 
     ylab = expression(log(Lambda[0]* (t))), bty = "n", type = "s", 
     col = "blue")  # Cor azul para "n"

# Para o grupo luti == "1a24"
fit_1a25 <- coxph(Surv(tempo[luti=="1a24"], status[luti=="1a24"]) ~ 1, data=dados, x = T, method = "breslow")
ss_1a25 <- survfit(fit_1a25)
s0_1a25 <- round(ss_1a25$surv, digits = 5)
H0_1a25 <- -log(s0_1a25)
lines(ss_1a25$time, log(H0_1a25), type = "s", lty = 2, col = "red")  # Cor vermelha para "1a25"

# Para o grupo luti == "25+"
fit_25p <- coxph(Surv(tempo[luti=="25+"], status[luti=="25+"]) ~ 1, data=dados, x = T, method = "breslow")
ss_25p <- survfit(fit_25p)
s0_25p <- round(ss_25p$surv, digits = 5)
H0_25p <- -log(s0_25p)
lines(ss_25p$time, log(H0_25p), type = "s", lty = 3, col = "green")  # Cor verde para "25+"
# Título do gráfico
title("LUTI")

