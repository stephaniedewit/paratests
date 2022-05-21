#Bar charts
library(tidyverse)
install.packages("palmerpenguins")
library(palmerpenguins)
View(penguins)
summary(penguins)
saveRDS(penguins,"lesson4/penguins.rds")
penguins_from_server<-readRDS("lesson4/penguins.rds")

penguins_from_server%>%
ggplot()+ 
geom_bar(aes(x=species),fill="magenta",colour="black")+
theme_minimal()+
labs(title="Penguins per Species in the Palmerpenguins data")
#\/Niet in aantal maar proportie, geeft dezelfde verhouding tussen bars
penguins_from_server%>%
ggplot()+
geom_bar(aes(x=species,y=..prop..,group=1),fill="magenta",colour="black")+
labs(title="Proportions of penguins per Species")
#Zelfde als (handmatig)
penguins%>%count(species)%>%mutate(proportion=n/sum(n))%>% #n is de uitkomst van count()!
ggplot()+
geom_col(aes(x=species,y=proportion,fill=species))+
labs(title="Also proportions of Penguins per Species")

#Opdracht 4.1
library(dslabs)
View(stars)
saveRDS(stars,"lesson4/stars.rds") #File in folder
stars_from_server<-readRDS("lesson4/stars.rds") #Object in Environment
class(stars_from_server) #= data.frame

ggplot(data=stars_from_server)+
geom_bar(aes(x=type,fill=type),colour="black")+
theme_minimal()+
labs(title="Number of stars per type",x="Type",y="Aantal")

penguins%>%count(species,sex)%>%  
ggplot(aes(x=species,y=n,fill=sex))+ 
geom_col()
#Geom_col = geom_bar want count()
penguins%>%count(species,sex)%>%  
ggplot()+ 
geom_col(aes(x=species,y=n,fill=sex))
#Zelfde als (want count) (nu aparte staven)
penguins%>% 
ggplot()+ 
geom_bar(aes(x=species,fill=sex),position=position_dodge())

flipper_summary<-penguins%>%group_by(species)%>%summarize(mean_flipper=mean(flipper_length_mm,na.rm=TRUE),stdev=sd(flipper_length_mm,na.rm=TRUE))
ggplot(data=flipper_summary,aes(x=species,y=mean_flipper,fill=species))+ 
geom_bar(stat="identity",)+ 
geom_errorbar(aes(ymin=mean_flipper-stdev,ymax=mean_flipper+stdev),width=.2)+ 
labs(title="Figure X. Average flipper length of penguins", 
subtitle="Errorbars depict 1 standard deviation",
x="Penguin species", 
y="Average flipper length (mm)")+
theme(text=element_text(size=16))

#Unpaired two sample t test
#1 get and prepair data
rabbit_ears<-tibble(rabbitnr=seq(1,20),colour=c(rep("beige",10),rep("brown",10)),earlength=c(17.5,13.0,18.6,19.4,20.9,18.9,22.8,20.3,22.1,20.4,23.0,24.1,26.1,25.2,27.8,28.5,26.5,21.5,20.2,18.8))
colour_rabbits_summary<-rabbit_ears%>%group_by(colour)%>%summarize(mean_ear=mean(earlength),stdev=sd(earlength))
  ## A tibble: 2 × 3
  #colour mean_ear stdev
  #<chr>     <dbl> <dbl>
  #  1 beige      19.4  2.76
  #2 brown      24.2  3.25

#2 plot bar chart
ggplot(data=colour_rabbits_summary,aes(x=colour,y=mean_ear,fill=colour))+ 
geom_col()+ 
geom_errorbar(aes(ymin=mean_ear-stdev,ymax=mean_ear+stdev),width=0.2)+ 
labs(title="Average ear length of rabbits per colour",subtitle="Errorbars depict 1 standard deviation",x="Rabbit colour",y="Mean ear length")

#3 check normality
ggplot(data=rabbit_ears,aes(x=earlength))+
geom_histogram()+
labs(title="Normality check earlength",x="Earlength",y="Count")+
facet_wrap(~colour) 

#Shapiro-Wilk
rabbit_ears_colomns<-pivot_wider(data=rabbit_ears,names_from="colour",values_from="earlength")
rabbit_ears_colomns%>%map(shapiro.test)%>%map_dbl("p.value")
#of
brown_ears<-rabbit_ears[rabbit_ears$colour=="brown",]
beige_ears<-rabbit_ears[rabbit_ears$colour=="beige",]
brown_beige<-tibble(brown_ears$earlength,beige_ears$earlength)
brown_beige%>%map(shapiro.test)%>%map_dbl("p.value")
  #$beige
  #p-value = 0.2251
  #$brown
  #p-value = 0.7852

#4 check variances met Levene's test
library(car)
?leveneTest() #arg1=afh. v. kolom van tibble, arg2=onafh. v. kolom als factor
leveneTest(rabbit_ears$earlength,factor(rabbit_ears$colour),center=mean)
  #Levene's Test for Homogeneity of Variance (center = mean)
  #Pr(>F)
  #0.3653

#5 perform t test
#Indien data is Tidy: formula=kolom met waarden~kolom met groepen
t.test(formula=rabbit_ears$earlength~rabbit_ears$colour,paired=FALSE,var.equal=TRUE)
t.test(formula=rabbit_ears$earlength~rabbit_ears$colour,paired=FALSE,var.equal=TRUE)$p.value%>%round(.,3)
  #[1] 0.002

#Indien data is niet Tidy: kolom groep1,kolom groep2
brown_ears<-rabbit_ears[rabbit_ears$colour=="brown",3]
beige_ears<-rabbit_ears[rabbit_ears$colour=="beige",3]
t.test(brown_ears,beige_ears,paired=FALSE,var.equal=TRUE)

#Opdracht 4.3
cat_tails #tibble van chr x dbl
View(cat_tails)
summary(cat_tails)
#a
cat_tails$breed<-factor(cat_tails$breed)
cat_tails #tibble van fct x dbl
#b
tail_length_summary<-cat_tails%>%group_by(breed)%>%summarize(gemiddelde_length=mean(tail_length),stdev_length=sd(tail_length))
ggplot(data=tail_length_summary,aes(x=breed,y=gemiddelde_length))+
geom_bar(stat="identity",fill="lightgreen")+
geom_errorbar(aes(ymin=gemiddelde_length-stdev_length,ymax=gemiddelde_length+stdev_length),width=0.2)+
labs(title="Gemiddelde staartlengte van Siamesen en Abysinnian katten",subtitle="Foutbalken met +- 1 stdev",x="Soort",y="Staartlengte")
#c
#Waarom werkt het volgende niet?:
#cat_tails_untidy<-cat_tails%>%pivot_wider(names_from="breed",values_from="tail_length")
#cat_tails_untidy%>%map(shapiro.test)

siamese<-cat_tails[cat_tails$breed=="Siamese",]
abyssinian<-cat_tails[cat_tails$breed=="Abyssinian",]
siamese_abyssinian<-tibble(siamese$tail_length,abyssinian$tail_length)
siamese_abyssinian%>%map(shapiro.test)%>%map_dbl("p.value")
  #Beide groepen normaalverdeeld

#check: cat_tails is tibble en variabele breed is factor
leveneTest(cat_tails$tail_length,cat_tails$breed,center=mean)
  #Geen gelijke variantie tussen groepen
#d
t.test_cat_tails<-t.test(formula=cat_tails$tail_length~cat_tails$breed,paired=FALSE,var.equal=FALSE,alternative="less") #Abyssinian is 1e level en heeft een kleiner gemiddelde
t.test_cat_tails$p.value%>%round(.,3)
  #p<<<0.05, verschil in staartlengte is sig. verschillend

#Paired two sample t test
#1 data staat in tekstbestand --> laden naar tibble
hematocrit_paired.tbl<-read_csv2("lesson4/hematocrit_paired")
hematocrit_paired.tbl
#2 tibble bestand tidy maken
hematocrit_paired_tidy<-pivot_longer(data=hematocrit_paired.tbl,cols=c("voor","na"),names_to="Conditie",values_to="Hematocriet")
#3 bar chart maken
hematocrit_summary<-hematocrit_paired_tidy%>%group_by(Conditie)%>%summarize(gemiddelde_hema=mean(Hematocriet),stdev_hema=sd(Hematocriet))
ggplot(data=hematocrit_summary,aes(x=Conditie,y=gemiddelde_hema))+
geom_bar(stat="identity",fill="red")+
geom_errorbar(aes(ymin=gemiddelde_hema-stdev_hema,ymax=gemiddelde_hema+stdev_hema),width=0.2)+
labs(title="Gemiddelde hematocrietwaarden(%) van atleten voor en na training op berg",subtitle="Foutbalken met +- 1 stdev",x="Conditie",y="Gemiddelde hematocriet (%)")
theme(text=element_text(size=14))
#4 normality check
##Beschouw paired two-sample als one-sample
##Verschil berekenen per testelement (en vergelijken met 0)
(hematocrit_paired$na-hematocrit_paired$voor)%>%shapiro.test() #=$voor-$na
  #0,29, normal distribution
#5 paired t test
t.test(hematocrit_paired$voor,hematocrit_paired$na,paired=TRUE)
  #p-value = 0,23, geen stat. sig. verschil
t.test(formula=hematocrit_paired_tidy$Hematocriet~hematocrit_paired_tidy$Conditie,paired=TRUE)
  #p-value = 0,23
##I.p.v. paired: Is het gemiddelde verschil onder 4 wel of niet gelijk aan 0? (one-sample)
t.test((hematocrit_paired$na-hematocrit_paired$voor),mu=0)
  #p-value = 0,23

#Plotting significance
t.test_rabbit<-t.test(formula=rabbit_ears$earlength~rabbit_ears$colour,paired=FALSE,var.equal=TRUE)$p.value%>%round(.,3)
install.packages("ggsignif")
library(ggsignif)
ggplot(data=colour_rabbits_summary,aes(x=colour,y=mean_ear,fill=colour))+ 
geom_bar(stat="identity")+ 
geom_errorbar(aes(ymin=mean_ear-stdev,ymax=mean_ear+stdev),width=.2)+ 
labs(title="Average ear length of rabbits per colour",subtitle="Errorbars depict 1 standard deviation",x="Rabbit colour",y="Mean ear length")+
ylim(0,35)+
geom_signif(comparisons=list(c("beige","brown")),y_position=30,tip_length=0,vjust=0.2,annotation=c("*"))+
annotate("text",x=1.5,y=35,label="two sample t-test p=0.002")

#ANOVA
#1 Een dataframe maken van build-in dataset
saveRDS(PlantGrowth,"lesson4/PlantGrowth.rds")
#Klik op PlantGrowth.rds in Files om een R object PlantGrowth te laden (zonder readRDS() dus)!
#2 Het dataframe is al Tidy
#3 Maak een gem.+sd tibble voor een bar chart
PlantGrowth_summary<-PlantGrowth%>%group_by(group)%>%summarize(mean_weight=mean(weight),stdev_weight=sd(weight))
PlantGrowth_summary #Tibble met group = factor!
#4 Maak een bar chart met foutbalken
ggplot(data=PlantGrowth_summary,aes(x=group,y=mean_weight,fill=group))+
geom_bar(stat="identity")+
geom_errorbar(aes(ymin=mean_weight-stdev_weight,ymax=mean_weight+stdev_weight),width=.2)+
labs(title="Gemiddelde plantgewicht o.i.v. behandeling1, behandeling2 en geen behandeling",x="Conditie",y="Gewicht (grams)")
#5 Controleer of groepen normaalverdeeld zijn
ctrl<-PlantGrowth[PlantGrowth$group=="ctrl",]
trt1<-PlantGrowth[PlantGrowth$group=="trt1",]
trt2<-PlantGrowth[PlantGrowth$group=="trt2",]
  #A
  PlantGrowth_groups<-data.frame(ctrl$weight,trt1$weight,trt2$weight)
  PlantGrowth_groups%>%map(shapiro.test)%>%map_dbl("p.value")
#of
  #B
  ctrl$weight%>%shapiro.test() #p-value = >0,05
  trt1$weight%>%shapiro.test() #p-value = >0,05
  trt2$weight%>%shapiro.test() #p-value = >0,05
  #Alle groepen zijn normaalverdeeld
#6 ANOVA T-test op Tidy PlantGrowth
summary.aov(aov(formula=PlantGrowth$weight~PlantGrowth$group))
  #p-value = <0,05, stat. sig. verschil tussen condities
#7 ANOVA aanvullen met Post Hoc tests (Welke condities veroorzaken verschil?)
#Alle onderlinge 2S-T-tests tegelijk uitvoeren met:
pairwise.t.test(PlantGrowth$weight,PlantGrowth$group,p.adj="none")

#Opdracht 4.4
#1 Tibble maken potato tekstbestand in folder
potato.tbl<-read_delim("lesson4/potato",delim=":",locale=locale(decimal_mark=","),col_types=cols(group1=col_number(),group2=col_number(),group3=col_number(),group4=col_number()))
#2 Tibble Tidy maken
potato_tidy<-pivot_longer(data=potato.tbl,cols=c("group1","group2","group3","group4"),names_to="Groep",values_to="Natrium")
#3 Boxplot maken (data=Tidy,y=value_variable,geom_boxplot,geen foutbalken)
ggplot(data=potato_tidy,aes(x=Groep,y=Natrium,fill=Groep))+
geom_boxplot()+
labs(title="Natrium in chips gemeten door 4 groepen",x="Studentengroep",y="Gewichtspercentage natrium")
#4 Shapiro-Wilk
#Je wilt niet-Tidy data hier! Generen of...
#Gebruik de niet-Tidy versie waarmee je begon waarin elke groep eigen kolom heeft!
potato.tbl%>%map(shapiro.test)%>%map_dbl("p.value")%>%round(digits=3) #Alle >0.05 = normaalverdeeld
#5 ANOVA 
#Op Tidy tibble potato_tidy
potato_tidy_aov<-summary.aov(aov(formula=potato_tidy$Natrium~potato_tidy$Groep))
#Alleen p-value latenzien op andere manier dan met map. 
##De summary is een list van één element: een ANOVA data.frame.
###Selecteer data.frame binnen een list met [[]], selecteer binnen een data.frame 
####een kolom met $, selecteer je 'dingetje' met [].
potato_tidy_aov[[1]]$'Pr(>F)'[1]
  #P-value = >0,05, geen stat. sig. verschil tussen gemeten natrium per groep

#One sample T-test`
#1 Tibble aanmaken
m_data<-tibble(mercury=c(30.6, 30, 30.1, 31, 29.8, 29.1, 31.1, 29.5, 30.5, 29.6, 31.6, 
                         30, 29.8, 30, 28.9, 29.3, 28.3, 29.2, 30.2, 30, 31.9, 29.4, 29.6, 
                         30.2, 29.5, 29.5, 29.4, 29.2, 30, 30))
#2 Histogram maken
ggplot(data=m_data,aes(x=mercury))+
geom_histogram(binwidth=0.2)+
labs(title="Distribution of mercury measures of a material with 29% mercury",x="Mercury percentage",y="Count")

#3 Shapiro-Wilk
shapiro.test(m_data$mercury)

#One sample T-test
t.test(m_data$mercury,mu=29)

#Line graphs
library(dslabs)
View(gapminder)
str(gapminder) #Data.frame met $country = factor
gapminder[gapminder$country=="Netherlands",]%>%
ggplot(aes(x=year,y=life_expectancy))+
geom_line()+
geom_point()

gapminder%>%group_by(year,continent)%>%mutate(life_exp=mean(life_expectancy,na.rm=TRUE))%>%
#group_by(year,continent) omdat we per continent voor elk jaar de gemiddelde life_exp willen berekenen om uit te zetten in de grafiek!
ggplot(aes(x=year,y=life_exp))+
geom_line(aes(colour=continent))

#CA_perday is summary met gem. en stdev. per dag
CA_perday%>%
ggplot(aes(x=day,y=mean_CA))+
geom_line(colour="purple")+
geom_point(colour="purple")+
geom_errorbar(aes(ymin=mean_CA-stdev_CA,ymax=mean_CA+stdev_CA),width=0.2,colour="purple")+
labs(title="Ca2+ levels in the duck pond",subtitle="20-25 november 2021",x="days after incident",y="Ca2+ (mg/L)")+
theme_classic(base_size=12)+
theme(axis.text.y=element_text(size=12),axis.text.x=element_text(size=12),axis.title.x=element_text(vjust=-1))

#Opdracht 4.7
hand_trembling.tbl<-read_table("lesson4/hand_trembling")
#Je kan ook rechtstreeks de waarde invoeren in tibble()!

tidy_hand_trembling.tbl<-pivot_longer(data=hand_trembling.tbl,cols=c("meting1","meting2","meting3"),names_to="Measurement",values_to="Trembling_frequency_hz")
str(tidy_hand_trembling.tbl)

#Opdracht 4.8
hand_trembling_summary<-tidy_hand_trembling.tbl%>%group_by(conc_nM)%>%summarize(mean_freq=mean(Trembling_frequency_hz,na.rm=TRUE),stdev_freq=sd(Trembling_frequency_hz,na.rm=TRUE))

ggplot(data=hand_trembling_summary,aes(x=conc_nM,y=mean_freq))+
geom_line(colour="gold")+
geom_point(colour="gold")+
geom_errorbar(aes(ymin=mean_freq-stdev_freq,ymax=mean_freq+stdev_freq),width=5,colour="black")+
labs(title="Average hand trembling (Hz) after a certain TBA dose (nM)",x="TBA dosis (nM)",y="Hand trembling (Hz)")+
theme_classic(base_size=12)+
theme(axis.text.y=element_text(size=12),axis.text.x=element_text(size=12),axis.title.x=element_text(vjust=-1))

#Box plots
airquality
class(airquality) #data.frame
airqua.tbl<-as_tibble(airquality) #data.frame-->tibble
#$Month bestaat uit data type int. Omzetten naar factor om te groeperen per maand.
ggplot(data=airqua.tbl,aes(x=as.factor(Month),y=Ozone,fill=as.factor(Month)))+
geom_boxplot()+
labs(title="Wind per month",x="Month",y="Wind")

#Opdracht 4.9
summary(airqua.tbl) 
#of
length(airqua.tbl$Ozone[is.na(airqua.tbl$Ozone)])

ggplot(data=airqua.tbl,aes(x=as.factor(Month),y=Ozone))+
geom_boxplot(aes(fill=as.factor(Month)),outlier.colour="red",outlier.shape=8)+
labs(title="Wind per month",x="Month",y="Wind")

ggplot(data=airqua.tbl,aes(x=as.factor(Month),y=Ozone))+
geom_boxplot(aes(fill=as.factor(Month)),outlier.colour="red",outlier.shape=8,show.legend=FALSE)+ #Geen leganda nodig
labs(title="Wind per month",x="Month",y="Wind")+
coord_flip() #Box plots in breedte

ggplot(data=airqua.tbl,aes(x=as.factor(Month),y=Ozone))+
geom_boxplot(aes(fill=as.factor(Month)),outlier.colour="red",outlier.shape=8,show.legend=FALSE)+
scale_x_discrete(labels=c("May","June","July","August","September"))+
theme_classic(base_size=12)+
theme(axis.text.y=element_text(size=12,angle=0,vjust=0),axis.text.x=element_text(size=12,angle=0,vjust=0),axis.title.x=element_text(vjust=-2))+
labs(title="Mean and spreading of ozone concentration at Roosevelt Island",x="Month",y="Ozone (ppb)")

#Opdracht 4.10
ggplot(data=iris,aes(x=Species,y=Sepal.Length,fill=Species))+
geom_boxplot(outlier.colour="red",outlier.shape=8,show.legend=FALSE)+
theme_classic(base_size=12)+
theme(axis.text.y=element_text(size=12),axis.text.x=element_text(size=12),axis.title.x=element_text(vjust=-2))+
labs(title="Mean and spreading of sepal length for three iris species",x="Specie",y="Sepal length")

#Outliers
white_rabbit_ears=tibble(ear_length=c(17.5, 13.0, 18.6, 19.4, 20.9, 18.9, 22.8, 20.3, 62.1, 20.4, 23.0,
                                       24.1, 26.1, 25.2, 27.8, 28.5, 76.5, 21.5, 20.2, 18.8))
str(white_rabbit_ears)

ggplot(data =white_rabbit_ears,aes(y=ear_length)) +
geom_boxplot(outlier.colour="red",outlier.shape=8,show.legend=FALSE)+
labs(title="Mean and spreading of white rabbits ear length",x="White rabbits",y="Ear length (cm)")+
theme_classic()+
theme(text=element_text(size=12))

Q1<-quantile(white_rabbit_ears$ear_length,.25)
Q3<-quantile(white_rabbit_ears$ear_length,.75)
IQR<-IQR(white_rabbit_ears$ear_length)

Tlower=Q1-(1.5*IQR)
Tupper=Q3+(1.5*IQR)

white_rabbit_ears$ear_length[white_rabbit_ears$ear_length<Tlower]<-NA
white_rabbit_ears$ear_length[white_rabbit_ears$ear_length>Tupper]<-NA

ggplot(data=white_rabbit_ears,aes(y=ear_length)) +
geom_boxplot(outlier.colour="red",outlier.shape=8,show.legend=FALSE)+
labs(title="Mean and spreading of white rabbits ear length",x="White rabbits",y="Ear length (cm)")+
theme_classic()+
theme(text=element_text(size=12))

#Correlation: is er een verband tussen variabelen i.p.v. verschil-vraag
library(palmerpenguins)
head(penguins)
#Scatter plot voor twee continue, numerieke variabelen
ggplot(data = penguins, aes(x = body_mass_g, y = flipper_length_mm)) +
  geom_point()+
  labs(title = "Relation between flipper length and body mass",
       subtitle = " Penguins, Palmer Station LTER",
       y = "Flipper length (mm)",
       x = "Body mass (g)") +
  theme_minimal()
#Pearson gebruiken indien verband lineair lijkt
cor.test(penguins$body_mass_g,penguins$flipper_length_mm,method="pearson")
  #p-value = <0.05, r^2 = 0.87 --> er is een sig. verband

cor_coefficient<-round(cor.test(penguins$body_mass_g, penguins$flipper_length_mm, method=c("pearson"))$estimate,1)
ggplot(data = penguins, aes(x = body_mass_g, y = flipper_length_mm)) +
  geom_point(aes(color=species,shape=species),size=1,alpha=0.8)+
  labs(title = "Relation between flipper length and body mass",
       subtitle = " Penguins, Palmer Station LTER",
       y = "Flipper length (mm)",
       x = "Body mass (g)") +
  theme_minimal()+
  annotate("text",x=3500,y=230,size=4,label=paste("pearson's r = ",cor_coefficient))

#Opdracht 4.11
library(dslabs)
View(temp_carbon)
?temp_carbon #carbon_emissions in millions of metric tons of carbon, land_anomaly in degrees Celsius relative to the 20th century mean temperature.

ggplot(data = temp_carbon, aes(x = carbon_emissions, y = temp_anomaly)) +
  geom_point(aes(y=ocean_anomaly),color="blue")+
  geom_point(aes(y=land_anomaly),color="red")+
  labs(title = "Relation between carbon emission \nand temperature over oceans and on land",
       y = "Temperature relative to the \n20th century mean (oC)",
       x = "Carbon emission in millions \nof metric tons") +
  theme_minimal()

#Opdracht 4.12
#Als je je data tidy maakt, hoef je land en ocean niet als aparte points toe te voegen
tidy_temp_carbon<-pivot_longer(data=temp_carbon,cols=c("land_anomaly","ocean_anomaly"),names_to="Anomaly",values_to="Temperature")

ggplot(data = tidy_temp_carbon, aes(x = carbon_emissions, y = Temperature)) +
geom_point(aes(color=Anomaly),size=1,alpha=0.8)+
labs(title = "Relation between carbon emission \nand temperature over oceans and on land",
y = "Temperature relative to the \n20th century mean (oC)",
x = "Carbon emission in millions \nof metric tons") +
theme_minimal()

#Regression
install.packages("ggpubr")
library(ggpubr)
library(ggplot2)
set.seed(123)
df <- data.frame(x = c(1:10))
df$y <- 2 + 3 * df$x + rnorm(10, sd = 0.8)
View(df)
?geom_point()

ggplot(data = df, aes(x = x, y = y)) +
  geom_smooth(method = "lm", se=FALSE, color="blue", formula = y ~ x) +
  geom_point()+
  stat_cor(label.y = 60, size = 5,digits = 3,
           aes(label = paste(..rr.label.., sep = "~`,`~"))) +
  stat_regline_equation(label.y = 55, size = 5)+
  theme_classic(base_size = 18) + # text size and overall theme
  labs(title = "Calibration line for X",
       x = "X (units)",
       y = "Y (units)")