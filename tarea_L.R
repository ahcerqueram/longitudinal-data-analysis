install.packages("ggthemr")
library(ggthemr)

install.packages("nlme")
install.packages("reshape")
install.packages("ez")
install.packages("psych")
install.packages("ggplot2")


library(psych)
library(ez)
library(reshape)
library(ggthemes)
library(skimr)
library(DataExplorer)
library(ggpubr)
library(univariateML)
library(GGally)
library(doParallel)
library(rstatix)
library(ggthemes)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(magrittr)
library(tidyr)
library(stringr)
library(printr)
library(PASWR)
library(GGally)
library(esquisse)
library(MVN)
library(readxl)
library(lme4)
library(nlme)
library(MASS)
library(goftest)
library(lmerTest)
library(lme4)
library(nlme)



file.choose()
tarea<-"C:\\Users\\mejia\\Desktop\\estadistica\\medidas longitudinales\\TAREA_1\\Six_Cities_Pollution.txt"
city<-read.table(tarea, header = TRUE, sep="\t")
View(city)
?read.table

women <- read_excel("C:/Users/mejia/Desktop/estadistica/medidas longitudinales/TAREA_1/women.xlsx")
View(women)
summary(women)


######## grafico de perfiles
ggplot(women_2, aes(x = tiempo, y = puntuacion)) + 
  geom_point(aes(color = ID)) +
  geom_path(aes(color = ID, group = ID))




#######ggplot   GRAFICO DE PERFILES
library(rstatix)
library(ggthemes)
women_2<- women %>% 
  gather(key= "tiempo",value = "puntuacion",T1,T2,T3) %>% 
  convert_as_factor(ID,tiempo)

women_2
view(women_2)


### otro grafico de perfiles
ggplot(women_2,aes(x=tiempo,y= puntuacion, group= ID))+
  geom_line(aes(color=ID))+
  theme_solarized_2()


##### promedios con puntos

ggplot(women_2,aes(x=tiempo,y= puntuacion, group= ID))+
  geom_line(aes(color=ID))+
  stat_summary(aes(group = tiempo), geom = "point", fun.y = mean)+
  theme_solarized_2()

##########      promedios CON PUTNOS GRANDES

ggplot(women_2,mapping=aes(x=tiempo,y= puntuacion, group= ID))+
  geom_line(aes(color=ID))+
  stat_summary(aes(group = tiempo,color = paste("mean")), 
               geom = "point", fun = mean,size=3)+
  theme_solarized_2()


###### promedios con lienas

ggplot(women_2,mapping=aes(x=tiempo,y= puntuacion, group= ID))+
  geom_line(aes(color=ID))+
  stat_summary(aes(group = tiempo,color = paste("mean")), 
               geom = "point", fun = mean,size=3)+
  theme_solarized_2()

gg.Gline <- gg.base + geom_line(aes(color = gender, group = id))
#gg.Gline + geom_point()
gg.Gline + stat_summary(aes(group = gender, color = paste("mean", gender)),
                        geom = "line", fun = mean, size = 3)


##########

colnames(women)
women<-as.factor(women$ID)
test_score<-melt(women,id="ID" ,measured= c("T1","T2","T3"))
test_score
colnames(women)
ggplot(test_scores,aes(x=))
library(ggplot2)
ggplot(women, aes(type, dv, colour=ID)) +
  geom_point(size = 2.5) +
  geom_line(aes(group = snum), size = 1) +
  theme_minimal()

###### numero 1 densidades
p1<-ggplot(women, aes(x=T1)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="black")+
  geom_density(alpha=.2, fill="red")+
  theme_fivethirtyeight()
p1

#### numero 2

p2<-ggplot(women, aes(x=T2)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="black")+
  geom_density(alpha=.2, fill="blue")+
  theme_fivethirtyeight()
p2

######## grafico de densidades
ggplot(women, aes(x = T2)) +
  geom_density(fill = "steelblue") + 
  labs(title = "VARIABLE T2")

### numero 33

p3<-ggplot(women, aes(x=T3)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="black")+
  geom_density(alpha=.2, fill="deepskyblue")+
  theme_fivethirtyeight()
p3


#############################  'uniones   




library(ggthemr)
library(ggpubr)
install.packages(" ggpubr")
install.packages(" ggtherm")
ggarrange(p1,p2,p3)

ggplot(women, aes(x=T3)) + geom_density(col="green") + 
  geom_histogram(aes(y=..density..), colour="black", fill=NA)



ggplot(women, aes(x = T3)) +
  geom_density(fill = "deepskyblue") + 
  labs(title = "VARIABLE T3")


ggplot(women, aes(x = T3)) +
  geom_density() +
  scale_y_continuous("Densidad") +
  scale_x_continuous("Edad") +
  labs(title = "Histograma de densidad 2",
       subtitle = "Forma de la distribuci?n de la variable edad")



##########EDA DE GRAFICOS

EDA(women$T3)
?EDA

##########3 CORRELACION

round(cor(women[,3:5]),2)

plot_correlation(
  data = datos,
  type = "continuous",
  title = "Matriz de correlaci?n variables continuas",
  theme_config = list(legend.position = "none",
                      plot.title = element_text(size = 16, face = "bold"),
                      axis.title = element_blank(),
                      axis.text.x = element_text(angle = -45, hjust = +0.1)
  )
)


########### matriz de varianzas y covarianzas


rico <- women%>%
  select(T1,T2,T3) 

beto
View(women)
mat_cov<-cov(beto)
mat_cov
print(xtable(mat_cov))


##########  grafico de dispersion

 install.packages("gclus")
library(gclus)
data <- mtcars[c(1, 3, 5, 6)] # Variables num?ricas
# cpairs(data) # Alternativtoa a pairs()

corr <- abs(cor(beto)) # Correlaci?n en valor absoluto
corr
colors <- dmat.color(corr)
order <- order.single(corr)

cpairs(beto, order, panel.colors = colors, gap = 0.5,
       main = "Dispersion para Variables ordenadas  por correlaci?n")




 ######

boxplot(women$T1,women$T2,women$T3,col = rainbow(ncol(trees)))

g<-mean(women$T1,women$T2,women$T3)
g
mean(women$T2)
g2<-summarise(g,media=round(mean(susc),1))


g<-group_by(women$T1)
g2<-summarise(g,media=round(mean(susc),1))

##############TEST MULTIVARIADO NORMAL

mvnormtest::mshapiro.test(t(beto))

install.packages("mvnormtest")
library(mvnormtest)
library(MVN)
beto
mardiaTest(women)
womenroystonTest(beto)


######### TEST SHAPIRO WILK ANOVA

shapiro.test(women$T1)
shapiro.test(women$T2)
shapiro.test(women$T3)

install.packages("gsl")
library(gsl)
result <- mvn(data = beto, univariateTest = "SW", desc = TRUE)
result


############## manova 1
str(women)
outcome<- cbind(women$T1,women$T2,women$T3)
outcome


modelo_1<-manova(outcome~ ID, data=women)

modelo_1
summary(modelo_1,intercept=TRUE)
summary(modelo_1,intercept=TRUE,test="Hotelling")
summary(modelo_1,intercept=TRUE,test="Wilks")
summary(modelo_1,intercept=TRUE,test="Roy")

########## MANOVA en 2
colnames(women)
str(women)
y<-as.matrix(cbind(women$T1,women$T2,women$T3))

m<-manova(y~women$ID)
summary(m,test = "Pillai")
summary(m,test = "Wilks")
summary(m,test = "Hotelling-Lawley")
summary(m,test = "Roy")

#### manova 3
install.packages("goftest")
library(nlme)
library(MASS)
library(goftest)

 women<-as.data.frame(women)
women$ID<-as.factor(women$ID)
t<-as.matrix(cbind(women$T1,women$T2,women$T3))
k<-as.vector(women$ID)
m<-manova(t~women$ID)
summary(m,test = "Pillai")
summary(m,test = "Wilks")

####### MANOVA 5   ########### este da resultado

y<-matrix(c(women$T1,women$T2,women$T3),nrow = 11,ncol=3)
y
library(mvShapiroTest)
mvShapiro.Test(y)

library(car)
modelo<-lm(y~1)
anova1<-Anova(modelo,type="III")
anova1
tiempo<-factor(c("T1","T2","T3"))
anova2 <- Anova(modelo,idata=data.frame(tiempo), idesign=~tiempo, type="III"
                ,icontrasts = "contr.poly")

anova2

summary(anova2,univariate = F)


dim(women)

womenView(women)

############ TEST MULTIVARIADO MANOVA

Y<-matrix(c(peso$P1,peso$P2,peso$P3,peso$P4,peso$P5),nrow=33, ncol=5);Y
library(mvShapiroTest)
mvShapiro.Test(Y)

library(dplyr)
library(tidyverse)
beto <- women%>%
  select(T1,T2,T3) 
beto
install.packages("mvShapiroTest")
library(mvShapiroTest)
mvShapiro.Test(beto)

############## MATRIZ DE CORRELACION

cor(women[,3:5],method = "pearson")





####################################################### ejericcio 2

library(readxl)
Pollution <- read_excel("C:/Users/mejia/Desktop/estadistica/medidas longitudinales/TAREA_1/Six_Cities_Pollution.xlsx")
View(Pollution)
summary(Pollution)

library(dplyr)

######### TABLA DE FRECUENCIAS


library(dplyr)
plotdata <- Pollution %>%
  count(Sujeto)
plotdata


tico <- Pollution  %>%
  select(Sujeto,Age) %>% 
  filter(Sujeto<3)

tico

############################# grafico de barras

p <- ggplot(Pollution, aes(Age, Sujeto)) +
  geom_bar(stat="identity", fill="steelblue") + theme_minimal()
p

########################### espaguetis con muestra

babies<- Pollution%>%
  select(Sujeto,Age,Log_fev) %>% 
  filter(Sujeto<20)


gg.bas <- ggplot(babies, aes(x = Age, y = Log_fev))
gg.bas + stat_summary(aes(group = babies$Sujeto, color = babies$Sujeto),
                       geom = "line", fun.y = mean, size = 3)




########## grafico espagueti de perfiles totles


gg.base <- ggplot(Pollution, aes(x = Age, y = Log_fev)+ 
                    theme_solarized_2()
                  )
gg.base + stat_summary(aes(group = Pollution$Sujeto, color = Pollution$Sujeto),
                       geom = "line", fun.y = mean, size = 3)

####################################### BOX PLOT 


ggplot(Pollution, aes(x=Age, y=Log_fev, col=factor(Age),group=factor(Age))) +
  geom_boxplot(aes(x=Age, y=Log_fev),fill="gray",alpha=0.5)


###################################




library(dplyr)
plotdata <- Pollution %>%
  count(Sujeto)
plotdata

pl <- ggplot(Pollution, aes(x=Age)) + 
pl + geom_histogram()

pl2 <- pl + geom_histogram(binwidth = 0.1, col='black', fill='green', alpha=0.4)
pl2

?ggplot

ggplot(Pollution) + 
  geom_histogram(aes(x = Age), fill = 'steelblue') + 
  xlab("Carat") + 
  ylab("Frecuencia") + 
  ggtitle("Distribuci?n de la variable Carat") +
  theme_minimal()

ggplot(Pollution) + 
  geom_histogram(binwidth = 0.1, aes(x = Age), fill = 'steelblue') + 
  xlab("Carat") + 
  ylab("Frecuencia") + 
  ggtitle("Distribuci?n de la variable Carat") +
  theme_minimal()

ggplot(data = Pollution, aes(x = Sujeto)) + 
  geom_bar(color = 'darkslategray', fill = 'steelblue') + 
  xlab("N?mero de sujetos") + 
  ylab("Cantidades") + 
  ggtitle("Numero de datos de cada sujeto")+
  theme_solarized()


ggplot(diamonds) + 
  geom_density(aes(x = Age), fill = 'steelblue') + 
  xlab("Carat") + 
  ylab("Frecuencia") + 
  ggtitle("Distribuci?n de la variable Carat (Densidad)") +
  theme_minimal()


library(ggplot2)
library(ggthemr)

ggplot(Pollution) + 
  geom_density(aes(x = Age), fill = 'steelblue') + 
  xlab("Carat") + 
  ylab("Frecuencia") + 
  ggtitle("Distribuci?n de la variable Carat (Densidad)") +
  theme_minimal()


ggplot(Pollution) + 
  geom_histogram(bins = 200, aes(x = Age), fill = 'black') + 
  xlab("Age") + 
  ylab("Frecuencia") + 
  ggtitle("Distribuci?n de la variable Edad") +
  theme_solarized_2()

######## grafico de perfiles

install.packages("lcsm")
library(lcsm)

library(lme4)
#> Loading required package: Matrix
library(ggplot2)
data("sleepstudy")
View(sleepstudy)
ids <- sample(levels(sleepstudy$Subject), size = 8, replace = FALSE)
sleepstudy <- dplyr::filter(sleepstudy, Subject %in% ids)

##### perfiles
ggplot(sleepstudy, aes(x = Days, y = Reaction)) +
  geom_line(aes(color = Subject)) +
  geom_smooth(method = "lm")


peo <- sample(levels(Pollution$Sujeto), size = 200, replace = FALSE)
Pollution <- dplyr::filter(Pollution, Sujeto %in% peo)

ggplot(Pollution, aes(x = Age, y = Log_fev)) +
  geom_line(aes(color = Sujeto)) +
  geom_smooth(method = "lm")
################################## intento 2



library(ggplot2)

ggplot(data = Pollution, aes(x = Sujeto)) + 
  geom_bar(color = 'darkslategray', fill = 'steelblue') + 
  xlab("N?mero de sujetos") + 
  ylab("Cantidades") + 
  ggtitle("Numero de datos de cada sujeto")+
  theme_solarized()


ggplot(Pollution, aes(Age, Log_fev, color=Sujeto) ) + 
  geom_point() + 
  labs(colour="sujetos")



?ggplot

geom_point(aes(colour = factor(cyl)))

library(lme4)
#> Loading required package: Matrix
library(ggplot2)
data("sleepstudy")






#########################

# ejemplo

library(readxl)
spruce <- read_excel("C:/Users/Andres/Desktop/estadistica/medidas longitudinales/TAREA_1/SPRUCE1.csv.xlsx")
View(spruce)
##################### ejemplor de lmmm


library(lme4)

head(sleepstudy)
View(sleepstudy)

library(ggplot2)

ggplot(data = sleepstudy, aes(x = Days, y = Reaction, color = Subject)) +
  geom_point() +
  theme_bw() +
  facet_wrap(~ Subject) + 
  theme(legend.position = "none")
colnames(Pollution)


cor(Pollution)

ladies<- Pollution%>%
  select(Sujeto,Age,Log_fev) %>% 
  filter(Sujeto<10)

####### prueba con edad GRAFICOS DE DISPERSION

ggplot(data = ladies, aes(x = Age, y = Log_fev, color = Sujeto)) +
  geom_point() +
  theme_solarized() +
  facet_wrap(~ Sujeto) + 
  theme(legend.position = "none")

#######  prueba con altura

queen<- Pollution%>%
  select(Sujeto,Height,Log_fev) %>% 
  filter(Sujeto<10)
queen

ggplot(data = queen, aes(x = Height, y = Log_fev, color = Sujeto)) +
  geom_point() +
  theme_solarized() +
  facet_wrap(~ Sujeto) + 
  theme(legend.position = "none")



############################################################# MODELO LMM

install.packages("lmerTest")
library(lmerTest)
resu<-suppressMessages(lmer(Log_fev~ Height+Age+(1|Sujeto),data=Pollution))
summary(resu)

resu_2<-suppressMessages(lmer(Log_fev~ Height+Age+Init_Height+Init_Age+(1|Sujeto),data=Pollution))
summary(resu_2)

broom.mixed::glance(resu)
broom.mixed::tidy(resu)
AIC(resu_2)
AIC(resu)
anova(resu_2)


resuresubroom.mixed::tidy(resu)


resu#############################modelo_2


install.packages("resubroom.mixed")
library(broom.mixed)
library(resubroom.mixed)

women_2<-women_2as.factor
ggplot(women_2, aes(x =tiempo, y = puntuacion, color = ID)) +
  geom_point()+
  theme_bw()+
  geom_path()




##### analisis de residuales

require(lme4)
qqnorm(residuals(resu))
colnames(Pollution)

############## GRAFICO DE PERfILES
View(Pollution)
ggplot(Pollution, aes(x = Age, y = Log_fev)) + 
  geom_point(aes(color = Sujeto)) +
  geom_path(aes(color = Sujeto, group = Sujeto))+
  theme_solarized()
