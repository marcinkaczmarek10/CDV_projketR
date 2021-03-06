library(dplyr)
library(ggplot2)
ceny <- read.csv (file.choose(), 
                    header =T , 
                    sep=";")
ceny
dim(ceny)
ceny$Wartosc <- gsub(",",".", ceny$Wartosc)

 
num = sapply(ceny, is.numeric)
num

names(ceny)

ceny2 <- ceny[,c(2,4,6,7)]

ceny2 %>%
  group_by(Nazwa, Rodzaj) 

ceny2$Wartosc <- as.numeric(ceny2$Wartosc)
is.numeric(ceny2$Wartosc)
is.na(ceny2)
ceny2$Wartosc[which(is.na(ceny2$Wartosc))] <- 0


#宺ednia arytmetyczna 
srednia_lekarz <- ceny2 %>% 
  filter(Rodzaj == "wizyta u lekarza specjalisty")%>%
  group_by(Nazwa) %>%
  summarize(srednia= mean(Wartosc))

srednia_bulka <- ceny2 %>% 
  filter(Rodzaj == "bu艂ka pszenna - za 50g")%>%
  group_by(Nazwa) %>%
  summarize(srednia= mean(Wartosc))

srednia_kielbasa <- ceny2 %>% 
  filter(Rodzaj == "kie艂basa w臋dzona - za 1kg")%>%
  group_by(Nazwa) %>%
  summarize(srednia= mean(Wartosc))

srednia_mleko3 <- ceny2 %>% 
  filter(Rodzaj == "mleko krowie spo偶ywcze o zawarto艣ci t艂uszczu 3-3,5%, sterylizowane - za 1l")%>%
  group_by(Nazwa) %>%
  summarize(srednia= mean(Wartosc))

srednia_mleko2 <- ceny2 %>% 
  filter(Rodzaj == "mleko krowie spo偶ywcze o zawarto艣ci t艂uszczu 2-2,5% - za 1l")%>%
  group_by(Nazwa) %>%
  summarize(srednia= mean(Wartosc))

srednia_koszulka <- ceny2 %>% 
  filter(Rodzaj == "podkoszulek m臋ski bawe艂niany, bez r臋kawa")%>%
  group_by(Nazwa) %>%
  summarize(srednia= mean(Wartosc))

srednia_buty_meskie <- ceny2 %>% 
  filter(Rodzaj == "p贸艂buty m臋skie sk贸rzane na podeszwie niesk贸rzanej - za 1par臋")%>%
  group_by(Nazwa) %>%
  summarize(srednia= mean(Wartosc))

srednia_buty_damskie <- ceny2 %>% 
  filter(Rodzaj == "p贸艂buty damskie sk贸rzane na podeszwie niesk贸rzanej - za 1par臋")%>%
  group_by(Nazwa) %>%
  summarize(srednia= mean(Wartosc))
  

srednia_olej <- ceny2 %>% 
  filter(Rodzaj == "olej nap臋dowy - za 1l")%>%
  group_by(Nazwa) %>%
  summarize(srednia= mean(Wartosc))

srednie <- data.frame(srednia_bulka, srednia_buty_meskie, srednia_buty_damskie,srednia_kielbasa, srednia_koszulka, srednia_lekarz, srednia_mleko2, srednia_mleko3, srednia_olej) 
ggplot(data=srednie, (aes(x=Nazwa, y=srednia)) +
         geom_col()

       #mediana dla wizyty u lekrza specjalisty
ceny2 %>% 
  filter(Rodzaj == "wizyta u lekarza specjalisty")%>%
  group_by(Nazwa) %>%
  summarize(mediana= median(Wartosc))  
#odchylenie standardowe dla wizyty u lekrza specjalisty
ceny2 %>% 
  filter(Rodzaj == "wizyta u lekarza specjalisty")%>%
  group_by(Nazwa) %>%
  summarize(odchylenie= sd(Wartosc))

      
#analiza zmian ceny wizytu u lekarza specjalisty w przedziale 2006-2019 w Polsce oraz por體nanie do wybranych wojew骴ztw

polska <- ceny2[ceny2$Nazwa == "POLSKA",]
polska_lekarz <- polska[polska$Rodzaj=="wizyta u lekarza specjalisty",]
ggplot(data=polska_lekarz, 
       (aes(x=Rok, 
            y=Wartosc)))+
  geom_point(size=2)

median(polska_lekarz$Wartosc)
sd(polska_lekarz$Wartosc)
range(polska_lekarz$Wartosc)
mean(polska_lekarz$Wartosc)
library(moments)
skewness(polska_lekarz$Wartosc)
kurtosis(polska_lekarz$Wartosc)
quantile(polska_lekarz$Wartosc)


mazowsze <- ceny2[ceny2$Nazwa == "MAZOWIECKIE",]
mazowsze_lekarz <- mazowsze[mazowsze$Rodzaj=="wizyta u lekarza specjalisty",]
ggplot(data=mazowsze_lekarz, 
       (aes(x=Rok, 
            y=Wartosc)))+
  geom_point(size=2)

cor(polska_lekarz$Wartosc, mazowsze_lekarz$Wartosc)
mod <-lm(mazowsze_lekarz$Wartosc ~ polska_lekarz$Wartosc)
summary(mod)

podkarpackie <- ceny2[ceny2$Nazwa == "PODKARPACKIE",]
podkarpackie_lekarz <- podkarpackie[podkarpackie$Rodzaj=="wizyta u lekarza specjalisty",]
ggplot(data=podkarpackie_lekarz, 
       (aes(x=Rok, 
            y=Wartosc)))+
  geom_point(size=2)

cor(polska_lekarz$Wartosc,podkarpackie_lekarz$Wartosc)

#por體nanie zmian cen produkt體 w polsce w przedziale czasowym 2006-1019

polska_bulka <- polska[polska$Rodzaj=="bu艂ka pszenna - za 50g",]
ggplot(data=polska_bulka, 
       (aes(x=Rok, 
            y=Wartosc)))+
  geom_point(size=2)

polska_kurczak <- polska[polska$Rodzaj=="kurcz臋ta patroszone - za 1kg",]
ggplot(data=polska_kurczak, 
       (aes(x=Rok, 
            y=Wartosc)))+
  geom_point(size=2)

polska_mleko3 <- polska[polska$Rodzaj=="mleko krowie spo偶ywcze o zawarto艣ci t艂uszczu 3-3,5%, sterylizowane - za 1l",]
ggplot(data=polska_mleko3, 
       (aes(x=Rok, 
            y=Wartosc)))+
  geom_point(size=2)

polska_mleko2 <- polska[polska$Rodzaj=="mleko krowie spo偶ywcze o zawarto艣ci t艂uszczu 2-2,5% - za 1l",]
ggplot(data=polska_mleko2, 
       (aes(x=Rok, 
            y=Wartosc)))+
  geom_point(size=2)

polska_podkoszulek <- polska[polska$Rodzaj=="podkoszulek m臋ski bawe艂niany, bez r臋kawa",]
ggplot(data=polska_podkoszulek, 
       (aes(x=Rok, 
            y=Wartosc)))+
  geom_point(size=2)

polska_buty_meskie <- polska[polska$Rodzaj=="p贸艂buty m臋skie sk贸rzane na podeszwie niesk贸rzanej - za 1par臋",]
ggplot(data=polska_buty_meskie, 
       (aes(x=Rok, 
            y=Wartosc)))+
  geom_point(size=2)

polska_buty_damskie <- polska[polska$Rodzaj=="p贸艂buty damskie sk贸rzane na podeszwie niesk贸rzanej - za 1par�",]
ggplot(data=polska_buty_damskie, 
       (aes(x=Rok, 
            y=Wartosc)))+
  geom_point(size=2)

polska_olej_napedowy <- polska[polska$Rodzaj=="olej nap臋dowy - za 1l",]
ggplot(data=polska_olej_napedowy, 
       (aes(x=Rok, 
            y=Wartosc)))+
  geom_point(size=2)

  #Mo縠my zaobserwowa� tendencj� do wzrostu w przedziale czasowym, dla niekt髍ych produkt體 ceny zachowuj� czasow� stabilizacj� lub spadki.

#analiza dla roku 2006

ceny_2006 <- ceny2[ceny2$Rok == "2006",]

polska_bulka2006 <- ceny_2006[ceny_2006$Rodzaj=="bu艂ka pszenna - za 50g",]
ggplot(data=polska_bulka2006, 
       (aes(x=Nazwa, 
            y=Wartosc)))+
  geom_point(size=2)

polska_kurczak2 <- ceny_2006[ceny_2006$Rodzaj=="kurcz臋ta patroszone - za 1kg",]
ggplot(data=polska_kurczak2, 
       (aes(x=Nazwa, 
            y=Wartosc)))+
  geom_point(size=2)

polska_mleko3_2 <- ceny_2006[ceny_2006$Rodzaj=="mleko krowie spo偶ywcze o zawarto艣ci t艂uszczu 3-3,5%, sterylizowane - za 1l",]
ggplot(data=polska_mleko3_2, 
       (aes(x=Nazwa, 
            y=Wartosc)))+
  geom_point(size=2)

polska_mleko2_2 <- ceny_2006[ceny_2006$Rodzaj=="mleko krowie spo偶ywcze o zawarto艣ci t艂uszczu 2-2,5% - za 1l",]
ggplot(data=polska_mleko2, 
       (aes(x=Nazwa, 
            y=Wartosc)))+
  geom_point(size=2)

polska_podkoszulek2 <- ceny_2006[ceny_2006$Rodzaj=="podkoszulek m臋ski bawe艂niany, bez r臋kawa",]
ggplot(data=polska_podkoszulek2, 
       (aes(x=Nazwa, 
            y=Wartosc)))+
  geom_point(size=2)

polska_buty_meskie2 <- ceny_2006[ceny_2006$Rodzaj=="p贸艂buty m臋skie sk贸rzane na podeszwie niesk贸rzanej - za 1par臋",]
ggplot(data=polska_buty_meskie2, 
       (aes(x=Nazwa, 
            y=Wartosc)))+
  geom_point(size=2)

polska_buty_damskie2 <- ceny_2006[ceny_2006$Rodzaj=="p贸艂buty damskie sk贸rzane na podeszwie niesk贸rzanej - za 1par�",]
ggplot(data=polska_buty_damskie2, 
       (aes(x=Nazwa, 
            y=Wartosc)))+
  geom_point(size=2)

#Na podstawie danych mo縠my wywnioskowa�, 縠 縴cie w polsce by硂 najta駍ze w roku 2006 w wojew骴ztwach: Opolskim i Podkarpackim.