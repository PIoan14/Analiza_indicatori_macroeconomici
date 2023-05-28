
date_pib <- read.csv("valori_PIB.csv", header= TRUE, sep = ',')

data_f_pib <- data.frame(date_pib$An, date_pib$Val_PIB)

attach(data_f_pib)
View(data_f_pib)

plot(date_pib.Val_PIB, main = "Evolutia PIB in Romania", type = 'b', col = 'blue')

index <- which.min(date_pib.Val_PIB)

date_pib.An[index]

library(moments)
skewness(date_pib.Val_PIB)


date_RS <- read.csv("rata_somajului.csv", header = TRUE, sep=',')

data_f_RS <- data.frame(date_RS$Ani, date_RS$Valoare, date_RS$Numar)
attach(data_f_RS)

plot(date_RS.Valoare, main = "Evolutia Ratei Somajului", col = 'blue', type = 'l')

max(date_RS.Valoare)

date_RS.Ani[which.max(date_RS.Valoare)]

library(tidyverse)

mod <- lm(date_RS.Valoare~date_RS.Numar , data = date_RS)
mod
summary(mod)

plot(date_RS.Valoare, date_RS.Numar, type = 'l', col = 'black')


date_Ri <- read.csv("rata_inflatiei.csv", header = TRUE, sep = ',')

data_f_ri <- data.frame(date_Ri$ANUL, date_Ri$RATA.INFLATIEI..)

attach(data_f_ri)

View(data_f_ri)

plot(date_Ri.RATA.INFLATIEI.., main = "Evolutia ratei inflatiei", col = 'black', type = 'b')

max(date_Ri.RATA.INFLATIEI..)/date_Ri.RATA.INFLATIEI..[length(date_Ri.RATA.INFLATIEI..)]
max(date_Ri.RATA.INFLATIEI..)/date_Ri.RATA.INFLATIEI..[1]


date_Ri.ANUL[which.max(date_Ri.RATA.INFLATIEI..)]


date_DB <- read.csv("Deficit_Bugetar.csv", header = TRUE, sep = ',')

data_f_DB <- data.frame(date_DB$An, date_DB$X.din.PIB)

attach(data_f_DB)

plot(date_DB.X.din.PIB, main = "Evolutia deficitului bugetar", col = 'black', type = 'l')

max(date_DB.X.din.PIB)

date_DB.An[which.max(date_DB.X.din.PIB)] 
secMax <-0

for( i  in 1 : length(date_DB.X.din.PIB))
{
  if (secMax < date_DB.X.din.PIB[i] & date_DB.X.din.PIB[i] < max(date_DB.X.din.PIB))
  {
    secMax<-date_DB.X.din.PIB[i]
    secMax
  }
}
  
secMax


date_DB.An[match(secMax, date_DB.X.din.PIB)]
#--------------------------------------------------------------------------------
# Pentru corelatie

View(data_f_pib)
View(data_f_RS)
View(data_f_ri)
View(data_f_DB)

ani <- date_DB.An
DB <- date_DB.X.din.PIB
date_totale<-cbind(ani, DB)
#View(date_totale)

Rata_inflatiei<- date_Ri[17:34,3]
Rata_inflatiei

date_totale <- cbind(date_totale, Rata_inflatiei)
#View(date_totale)

Rata_somajului <- date_RS[14:31,2]
Rata_somajului

date_totale <- cbind(date_totale, Rata_somajului)

Valoare_PIB <- date_pib[17:34,2]
Valoare_PIB

date_totale <- cbind(date_totale, Valoare_PIB)

View(date_totale)
date_totale <- data.frame(date_totale)
library(corrplot)


matr_corelatie<-cor(date_totale[,-1])

corrplot(matr_corelatie, method = 'circle')