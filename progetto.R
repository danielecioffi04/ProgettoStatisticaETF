setwd(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), "FILE CSV"))
record_etf<-read.table("etf.csv", header = TRUE, sep=";")
record_indici<-read.table("indici.csv", header = TRUE, sep=";")
record_vettori<-read.table("vettori.csv", header = TRUE, sep=";")
record_guerra<-read.table("guerra.csv", header = TRUE, sep=";")
record_dazi<-read.table("dazi.csv", header = TRUE, sep=";")
record_covid<-read.table("covid.csv", header = TRUE, sep=";")
record_predizione<-read.table("predizione.csv", header = TRUE, sep=";")

record_etf <- record_etf[-46:-96,]
record_guerra <- record_guerra[-42:-53,]
record_guerra <- record_guerra[,-3:-13]
record_dazi <- record_dazi[,-3:-8]

#----------------------------------------------------------------------------------------------------------------------------------------------------------
# Quali sono state le performance degli ETF negli ultimi 10 anni?
#Istogrammi rendimenti indici e etf con fasce di guadagno, confronto dei boxplot, istogramma indici di rischio, istogramma rendimento complessivo su 10 anni

#ISTOGRAMMA + BOXPLOT Rendimento cumulativo
hist_data <- hist(record_etf$RENDIMENTO.CUMULATIVO*100, xlab="Percentuale rendimento cumulativo 10y ETF", main="Rendimento cumulativo ETF", ylab="Frequenza assoluta", breaks=3, ylim=c(0,35))
frequenze <- hist_data$counts
mids <- hist_data$mids
text(x = mids, y = frequenze, labels = frequenze, pos = 3, cex = 0.8, col = "black")
print("Dati istogramma rendimento cumulativo ETF:")
print(hist(record_etf$RENDIMENTO.CUMULATIVO*100, plot=FALSE))

boxplot(record_etf$RENDIMENTO.CUMULATIVO*100, main="Rendimento cumulativo 10y ETF")
x<-mean(record_etf$RENDIMENTO.CUMULATIVO*100)
abline(h=x, col = "blue", lwd=3)
print("Dati boxplot rendimento cumulativo ETF:")
print(boxplot(record_etf$RENDIMENTO.CUMULATIVO*100, plot=FALSE))
summary(record_etf$RENDIMENTO.CUMULATIVO*100)


#ISTOGRAMMA + BOXPLOT Rendimento medio annuale degli ETF
hist_data <- hist(record_etf$RENDIMENTO.MEDIO..ANNUALE.*100, xlab="Percentuale rendimento medio annuale su 10y ETF", main="Rendimento medio annuale ETF", ylim = c(0,35), ylab="Frequenza assoluta", breaks=3)
frequenze <- hist_data$counts
mids <- hist_data$mids
text(x = mids, y = frequenze, labels = frequenze, pos = 3, cex = 0.8, col = "black")
print("Dati istogramma rendimento medio annuale ETF:")
print(hist(record_etf$RENDIMENTO.MEDIO..ANNUALE.*100, breaks=3, plot=FALSE))

boxplot(record_etf$RENDIMENTO.MEDIO..ANNUALE.*100, main="Rendimento medio annuale su 10y ETF")
y<-mean(record_etf$RENDIMENTO.MEDIO..ANNUALE.*100)
print(y)
abline(h=y, col = "blue", lwd=3)
print("Dati boxplot rendimento medio annuale ETF:")
print(boxplot(record_etf$RENDIMENTO.MEDIO..ANNUALE.*100, plot=FALSE))
summary(record_etf$RENDIMENTO.MEDIO..ANNUALE.*100)

#ISTOGRAMMA + BOXPLOT Rendimento medio annuale INDICI
hist_data <- hist(record_indici$RENDIMENTO.MEDIO.ANNUALE*100, xlab="Percentuale rendimento medio annuale su 10y INDICI", ylab="Frequenza", main="Rendimento medio annuale INDICI", ylim=c(0,35), breaks=2)
frequenze <- hist_data$counts
mids <- hist_data$mids
text(x = mids, y = frequenze, labels = frequenze, pos = 3, cex = 0.8, col = "black")
print("Dati istogramma rendimento medio annuale INDICI:")
print(hist(record_indici$RENDIMENTO.MEDIO.ANNUALE*100, plot=FALSE))

boxplot(record_indici$RENDIMENTO.MEDIO.ANNUALE*100, main="Rendimento medio annuale su 10y INDICI")
y<-mean(record_indici$RENDIMENTO.MEDIO.ANNUALE*100)
abline(h=y, col = "blue", lwd=3)
print("Dati boxplot rendimento medio annuale INDICI:")
print(boxplot(record_indici$RENDIMENTO.MEDIO.ANNUALE*100, plot=FALSE))
summary(record_indici$RENDIMENTO.MEDIO.ANNUALE*100)

#ISTOGRAMMA + BOXPLOT Rendimento cumulativo INDICI
hist_data <- hist(record_indici$RENDIMENTO.CUMULATIVO*100, xlab="Percentuale rendimento cumulativo 10y INDICI", ylab="Frequenza", main="Rendimento cumulativo INDICI", ylim=c(0,40), breaks=3)
frequenze <- hist_data$counts
mids <- hist_data$mids
text(x = mids, y = frequenze, labels = frequenze, pos = 3, cex = 0.8, col = "black")
print("Dati istogramma rendimento cumulativo INDICI:")
print(hist(record_indici$RENDIMENTO.CUMULATIVO*100, plot=FALSE))

boxplot(record_indici$RENDIMENTO.CUMULATIVO*100)
y <- mean(record_indici$RENDIMENTO.CUMULATIVO*100)
abline(h=y, col = "blue", lwd=3)
print("Dati boxplot rendimento cumulativo INDICI:")
print(boxplot(record_indici$RENDIMENTO.CUMULATIVO*100, plot=FALSE))
summary(record_indici$RENDIMENTO.CUMULATIVO*100)

#ISTOGRAMMA + BOXPLOT Scarto ETF-Indice - Riguardare classi, provare a usare i range di replicazione scritti sulle note (-0.25,0.25 ecc)
hist_data <- hist(record_etf$SCARTO.ANNUALE.MEDIO*100, xlab="Percentuale scarto medio annuale su 10y ETF", ylab = "Frequenza", main="Scarto annuale medio", breaks=4, ylim=c(0,25))
frequenze <- hist_data$counts
mids <- hist_data$mids
text(x = mids, y = frequenze, labels = frequenze, pos = 3, cex = 0.8, col = "black")
hist(record_etf$SCARTO.ANNUALE.MEDIO*100, plot=FALSE)

boxplot(record_etf$SCARTO.ANNUALE.MEDIO*100, main="Scarto annuale medio su 10y ETF")
y <- mean(record_etf$SCARTO.ANNUALE.MEDIO*100)
abline(h=y, col = "blue", lwd=3)
boxplot(record_etf$SCARTO.ANNUALE.MEDIO*100, plot=FALSE)
summary(record_etf$SCARTO.ANNUALE.MEDIO*100)

#ISTOGRAMMA + BOXPLOT degli indici di rischio degli ETF - Sistemare le classi da 1 a 7
hist(record_etf$X.1, xlab="Indici di rischio", ylab="Frequenza assoluta", main="Indici di rischio", ylim=c(0,25),breaks = 5)
hist(record_etf$X.1, plot=FALSE)
boxplot(record_etf$X.1)
boxplot(record_etf$X.1, plot=FALSE)

#CONFRONTO BOXPLOT
boxplot(record_etf$RENDIMENTO.CUMULATIVO*100, record_indici$RENDIMENTO.CUMULATIVO*100, main="Rendimenti cumulativi a confronto")
boxplot(record_etf$RENDIMENTO.MEDIO..ANNUALE.*100, record_indici$RENDIMENTO.MEDIO.ANNUALE*100, main="Rendimenti medi annuali a confronto")

#----------------------------------------------------------------------------------------------------------------------------------------------------------
#Gli ETF sono costosi? - Istogramma fasce di TER

#ISTOGRAMMA dei TER
hist_data <- hist(record_etf$X*100, xlab="TER (%)", ylab="Frequenza assoluta", main="Costi di gestione (TER)",breaks = 4, ylim=c(0,32))
frequenze <- hist_data$counts
mids <- hist_data$mids
text(x = mids, y = frequenze, labels = frequenze, pos = 3, cex = 0.8, col = "black")

boxplot(record_etf$X*100)
y <- mean(record_etf$X*100)
abline(h=y, col = "blue", lwd=3)
summary(record_etf$X*100)

#BOXPLOT dimensione
boxplot(record_etf$DIMENSIONE.DEL.FONDO..mld., main="Dimensione del fondo (mld)")
summary(record_etf$DIMENSIONE.DEL.FONDO..mld.)

#Intervallo di confidenza per media TER
t.test(record_etf$X*100, conf.level = 0.95)

#BARPLOT tipo di replica
# Installa e carica ggplot2 se non lo hai già fatto
install.packages("ggplot2")
library(ggplot2)

# Inserisci i dati in un dataframe
data <- data.frame(
  Tipo_di_Replica = c(
    "sintetica", "a campionamento", "a campionamento", "fisica totale", "a campionamento",
    "fisica totale", "sintetica", "fisica totale", "fisica totale", "fisica totale",
    "fisica totale", "a campionamento", "fisica totale", "sintetica", "fisica totale",
    "a campionamento", "fisica totale", "sintetica", "fisica totale", "fisica totale",
    "sintetica", "sintetica", "fisica totale", "fisica totale", "fisica totale",
    "sintetica", "sintetica", "a campionamento", "sintetica", "sintetica", "fisica totale",
    "fisica totale", "fisica totale", "sintetica", "sintetica", "fisica totale",
    "fisica totale", "sintetica", "fisica totale", "fisica totale", "sintetica",
    "fisica totale", "fisica totale", "sintetica", "fisica totale"
  )
)

# Conta le frequenze assolute
frequenze <- table(data$Tipo_di_Replica)
frequenze_df <- as.data.frame(frequenze)

# Crea il grafico a barre senza colore con ggplot2 e aggiungi le etichette delle frequenze
ggplot(frequenze_df, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "grey") +
  geom_text(aes(label = Freq), vjust = -0.5, size = 3.5) +
  labs(title = "Frequenze dei Tipi di Replica", x = "Tipo di Replica", y = "Frequenza") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), # Rimuove la griglia principale
    panel.grid.minor = element_blank()  # Rimuove la griglia minore
  )

#BOXPLOT indici di rischio

# Inserisci i dati in un dataframe
data <- data.frame(
  Indice_di_Rischio = c(
    5, 3, 2, 4, 2, 5, 5, 4, 4, 5, 5, 2, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 3, 5, 5, 5, 3, 2, 4, 4, 3, 5, 5, 3, 5, 5, 4, 4, 4, 4
  )
)

# Conta le frequenze assolute
frequenze <- table(data$Indice_di_Rischio)
frequenze_df <- as.data.frame(frequenze)

# Crea il grafico a barre senza colore con ggplot2 e aggiungi le etichette delle frequenze
ggplot(frequenze_df, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "grey") +
  geom_text(aes(label = Freq), vjust = -0.5, size = 3.5) +
  labs(title = "Frequenze dell'Indice di Rischio", x = "Indice di Rischio", y = "Frequenza") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), # Rimuove la griglia principale
    panel.grid.minor = element_blank()  # Rimuove la griglia minore
  )

#----------------------------------------------------------------------------------------------------------------------------------------------------------
#Quanto rende in media un etf su 10 anni? (intervallo di confidenza per media con varianza incognita del rendimento cumulato ETF) 

#INTERVALLO DI CONFIDENZA 0.99 del rendimento cumulato con varianza incognita
t.test(record_etf$RENDIMENTO.CUMULATIVO*100, conf.level = 0.95)

#----------------------------------------------------------------------------------------------------------------------------------------------------------
#Gli etf raggiungono il loro obiettivo? (Verifica di ipotesi H_0: u1-u2 < -0.30 H_1: u1-u2>=-0.30 a varianza incognita, intervallo di confidenza su proporzione “quante etf raggiungono l’obiettivo (>=0 scarto con indice)”)

#TEST H_0: u1-u2 < -0.25 H_1: u1-u2 >= -0.25 (CONFRONTO TRA POPOLAZIONI)
t.test(record_vettori$RENDIMENTO.ANNUALE.MEDIO.ETF*100, record_vettori$RENDIMENTO.ANNUALE.MEDIO.INDICE*100, "greater", -0.30, TRUE, FALSE)

#INTERVALLO DI CONFIDENZA PER PROPORZIONE degli etf con scarto >=0
prop.test(20,45,conf.level=0.95)

#----------------------------------------------------------------------------------------------------------------------------------------------------------
#Se dovesse comprare una ETF a caso quale sarebbe la probabilità che quest’ultima abbia replicato (o battuto) l’indice? (verifica del modello e intervallo di predizione)

print(shapiro.test(record_etf$SCARTO.ANNUALE.MEDIO))
qqnorm(record_etf$SCARTO.ANNUALE.MEDIO)
qqline(record_etf$SCARTO.ANNUALE.MEDIO)
shapiro.test(record_etf$SCARTO.ANNUALE.MEDIO)

# Dati forniti
dati <- c(0.0090, -0.0075, -0.0021, -0.0015, -0.0015, 0.0004, -0.0046, 0.0008, -0.0009, 0.0005, 
          -0.0010, -0.0262, 0.0023, -0.0049, 0.0003, -0.0014, -0.0066, 0.0036, 0.0025, -0.0042, 
          -0.0050, -0.0176, 0.0046, 0.0007, -0.0003, -0.0070, -0.0046, -0.0021, -0.0005, 0.0041, 
          0.0055, -0.0027, -0.0010, 0.0005, 0.0008, -0.0005, -0.0024, 0.0046, -0.0013, 0.0057, 
          0.0048, 0.0006, -0.0044, 0.0005, 0.0044)

# Calcolare la media e la deviazione standard
media <- mean(dati)
deviazione_standard <- sd(dati)

# Numero di osservazioni
n <- length(dati)

# Livello di confidenza (es. 95%)
confidenza <- 0.95
alpha <- 1 - confidenza

# Valore critico della distribuzione t di Student
t_valore <- qt(1 - alpha / 2, df = n - 1)

# Calcolare l'intervallo di predizione
lower_bound <- media - t_valore * deviazione_standard * sqrt(1 + 1 / n)*100
upper_bound <- media + t_valore * deviazione_standard * sqrt(1 + 1 / n)*100

# Stampare l'intervallo di predizione
cat("Intervallo di predizione al 95%:(", lower_bound,",", upper_bound, ")\n")


#----------------------------------------------------------------------------------------------------------------------------------------------------------
#Simone riconosce che il campione non è normale, come sostenuto dalla verifica del modello precedentemente fatta allora decide di crearsi un portafoglio di 40 etf e vuole sapere la probabailità che almeno 30 etf replichino l'indice.

#Abbiamo un campione numeroso quindi possiamo stimare puntualmente la proporzione della popolazione, sfruttando l'intervallo di confidenza della proporzione ci poniamo in un
#caso favorevole (55%) e con un test d'ipotesi vogliamo vedere se possiamo usare questo valore prima di andare a calcolare la probabilità
prop.test(34,45,conf.level=0.95) #ricordarsi di scrivere da qualche parte la verifica delle condizioni (np > 5, n(1-p)>5)
prop.test(34,45, 0.80, "two.sided", conf.level = 0.99, correct = FALSE) #Scegliamo di usare 80% come proporzione perchè più alto della stima puntuale. Perchè non scegliere la stima puntuale?
#Perchè siamo convinti che la proporzione sia più alta di quella puntuale alla luce delle slide precedenti (vedi domanda "ETF raggiungono il loro obiettivo")

probabilita <- pbinom(30, 40, 0.8, lower.tail = FALSE)
print(probabilita)
cat("P(X ≥ 30):", probabilita*100,"%")

#----------------------------------------------------------------------------------------------------------------------------------------------------------
#Confronto con portafoglio Francesco
#Test Rendimento medio annuale vs Francesco (2,39%)
t.test(record_etf$RENDIMENTO.MEDIO..ANNUALE.*100, NULL, "greater", 2.39, FALSE, FALSE, conf.level = 0.95)

#Test Rendimento medio cumulato vs Francesco (27,20%)
t.test(record_etf$RENDIMENTO.CUMULATIVO*100, NULL, "greater", 27.20, FALSE, FALSE, conf.level = 0.95)

#Test TER vs Francesco (1,76)
t.test(record_etf$X*100, NULL, "less", 1.76, FALSE, FALSE, conf.level = 0.95)

#Test SCARTO vs Francesco (-2,91)
t.test(record_etf$SCARTO.ANNUALE.MEDIO*100, NULL, "greater", -2.91, FALSE, FALSE, conf.level = 0.95)

#----------------------------------------------------------------------------------------------------------------------------------------------------------
#MODELLO 1
#Regressione lineare semplice - TER su scarto
install.packages("car") 
install.packages("nnet")
library(car)
library(nnet)

rData <- data.frame(record_etf$X*100, record_etf$SCARTO.ANNUALE.MEDIO*100)
colnames(rData) <- c("TER","Scarto")
rData <- rData[-25,]
#rData <- rData[-13,]
rData <- rData[-12,]

plot(TER~ Scarto, data=rData)
ter_vs_scarto = lm(TER~ Scarto, data=rData)
abline(ter_vs_scarto, col = "blue", lwd = 2)
summary(ter_vs_scarto)
rstandard(ter_vs_scarto)
plot(rstandard(ter_vs_scarto), main="Residui standardizzati", ylab="", xlab="")
shapiro.test(rstandard(ter_vs_scarto))

qqnorm(rstandard(ter_vs_scarto), xlab="", ylab="")
qqline(rstandard(ter_vs_scarto))
#----------------------------------------------------------------------------------------------------------------------------------------------------------
#Regressione multipla (modello carino)
rData <- data.frame(record_etf$X*100, record_etf$SCARTO.ANNUALE.MEDIO*100, record_etf$RENDIMENTO.MEDIO..ANNUALE.*100, record_etf$TIPO.DI.REPLICA, record_etf$INDICE, record_etf$RENDIMENTO.CUMULATIVO*100, record_etf$DIMENSIONE.DEL.FONDO..mld., record_etf$DOMICILIO, record_etf$X.1, record_etf$FAMIGLIA.DI.INDICI)
colnames(rData) <- c("TER","Scarto", "Rendimento_annuale", "Tipo_di_replica", "indice", "Rendimento_cumulativo", "Dimensione", "Domicilio", "Rischio", "Famiglia")
rData <- rData[-28,]
rData <- rData[-25,]
rData <- rData[-19,]
#rData <- rData[-12,]

pairs(rData)
reg = lm(TER~Scarto + Dimensione + Tipo_di_replica*Dimensione, data=rData)
summary(reg)
rstandard(reg)
plot(rstandard(reg), main="Residui standardizzati", ylab="", xlab="")
shapiro.test(rstandard(reg))
qqnorm(rstandard(reg), xlab="", ylab="")
qqline(rstandard(reg))

predict(reg,data.frame(Scarto=0.05,Dimensione=2, Tipo_di_replica="fisica totale"),interval="confidence")
#----------------------------------------------------------------------------------------------------------------------------------------------------------
#Regressione multipla
rData <- data.frame(record_etf$X*100, record_etf$SCARTO.ANNUALE.MEDIO*100, record_etf$X.1)
colnames(rData) <- c("TER","Scarto", "Rischio")
rData <- rData[-43,]
rData <- rData[-25,]
rData <- rData[-12,]


pairs(rData)
reg = lm(TER~Rischio + Scarto, data=rData)
summary(reg)
rstandard(reg)
plot(rstandard(reg), main="Residui standardizzati", xlab="", ylab="")
shapiro.test(rstandard(reg))
qqnorm(rstandard(reg), xlab="", ylab="")
qqline(rstandard(reg))

#----------------------------------------------------------------------------------------------------------------------------------------------------------
#Regressione rendimento etf - rendimento Indice
rData <- data.frame(record_vettori$RENDIMENTO.ANNUALE.MEDIO.ETF*100, record_vettori$RENDIMENTO.ANNUALE.MEDIO.INDICE*100)
rData <- rData[-26,] #abbiamo tolto 2 outlier
rData <- rData[-6,]
colnames(rData) <- c("Rendimento_etf", "Rendimento_indice")

plot(rData)
reg2 = lm(Rendimento_etf~ Rendimento_indice, data=rData)
abline(reg2, col = "blue", lwd = 2)
summary(reg2)
shapiro.test(rstandard(reg2))

rstandard(reg2)
plot(rstandard(reg2), main="Residui standardizzati", xlab="", ylab="")

qqnorm(rstandard(reg), xlab="", ylab="")
qqline(rstandard(reg))

#----------------------------------------------------------------------------------------------------------------------------------------------------------
#Incisione degli eventi sui rendimenti degli indici - TEST

#Dazi
prop.test(35,41, 0.27561, "two.sided", conf.level = 0.99, correct = FALSE)

#Covid
prop.test(10,41, 0.27561, "two.sided", conf.level = 0.99, correct = FALSE)

#Guerra
prop.test(38,41, 0.27561, "two.sided", conf.level = 0.99, correct = FALSE)


test <- c(13.5,19.8,21.1,22.4,22.5,25.5,26.5)
boxplot(test)

#----------------------------------------------------------------------------------------------------------------------------------------------------------
#Test d'indipendenza
Var1 <- c(0.0080, 0.0257, 0.0255, 0.0967, 0.0619, 0.0012, 0.0779, 0.0781, 0.0768, 
          0.0573, 0.0534, 0.0518, 0.0376, 0.0274, 0.0353, 0.0146, 0.0137, 0.0184,
          -0.0006, 0.0748, 0.0493, 0.0685, 0.0756, 0.0705, 0.0834, 0.1150, 0.0431, 
          0.1341, 0.1492, 0.0968, 0.0978, 0.2237, 0.2233, 0.0988, 0.0774, 0.1550, 
          0.1535, 0.0974, 0.2245, 0.1285, 0.1274, 0.1290, 0.0738, 0.0406, 0.1214)

Var2 <- c(0.0100, 0.0278, 0.0269, 0.0991, 0.0616, 0.0273, 0.0724, 0.0724, 0.0722,
          0.0582, 0.0528, 0.0522, 0.0441, 0.0301, 0.0365, 0.0160, 0.0147, 0.0189,
          0.0069, 0.0758, 0.0563, 0.0730, 0.0712, 0.0696, 0.0827, 0.1326, 0.0446, 
          0.1251, 0.1487, 0.0963, 0.0969, 0.2283, 0.2283, 0.0984, 0.0822, 0.1501,
          0.1489, 0.1018, 0.2250, 0.1249, 0.1249, 0.1249, 0.0732, 0.0383, 0.1257)

# Numero di intervalli usando il metodo di Sturges
num_breaks <- function(x) {
  return(ceiling(log2(length(x)) + 1))
}

breaks_var1 <- seq(min(Var1), max(Var1), length.out = 3)
breaks_var2 <- seq(min(Var2), max(Var2), length.out = 3)

# Creazione delle categorie
Var1_cat <- cut(Var1, breaks = breaks_var1, include.lowest = TRUE)
Var2_cat <- cut(Var2, breaks = breaks_var2, include.lowest = TRUE)

# Creazione della tabella di contingenza
contingency_table <- table(Var1_cat, Var2_cat)

# Stampa della tabella di contingenza
print(contingency_table)

# Esecuzione del test chi-quadro
chi_sq_test <- chisq.test(contingency_table)
print(chi_sq_test)




