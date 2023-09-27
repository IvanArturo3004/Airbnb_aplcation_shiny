##### ANALISIS 3 ELIMINANDO POR CRITERIO DE COOK #####

#setwd("/Users/takem/Documents/modelos no parametricos/proyecto_final")
library(arrow)
library(MASS)
library(pins)
library(readr)
library(skimr)
library(tidyverse)
library(tidytext)
library(magrittr)
library(readr)
library(dplyr)
library(stringr)
library(car)
library(wesanderson)
library(nortest)
lista<-read_csv("listings_1.csv")
head(lista)
# Nombre de las colonias 

nom_col<- c( "Miguel Hidalgo",        
             "Cuajimalpa de Morelos", 
             "Cuauhtémoc",            
             "Venustiano Carranza",   
             "La Magdalena Contreras",
             "Benito Juárez",         
             "Álvaro Obregón",        
             "Iztacalco",      
             "Tlalpan",    
             "Gustavo A. Madero   ",  
             "Xochimilco",
             "Iztapalapa",
             "Azcapotzalco ",
             "Tláhuac"               
             ,"Milpa Alta",
             "Coyoacán") 


#lista %>%view


lista$price<-gsub('[^0-9.]','',lista$price)
lista$price<-as.numeric(lista$price)
# de aqui obsevern el complete rate para cada variable (al mandar a llamar a skim)
# y digan tipo como estan completos en tal porcentaje podemos eliminar los na de tal y tal
lista %>%skimr::skim()
lista <-lista %>% drop_na(bedrooms)
lista <-lista %>% drop_na(bathrooms_text)
lista <-lista %>% drop_na(beds)
lista <-lista %>% drop_na(host_is_superhost)
# para cada analisis primario vamos atrabajar sobre el precio por persona
lista$precio_persona <-lista$price/lista$accommodates

hist(lista$price, breaks=1000)
boxplot(lista$price, xlab="precio_airbnb")
boxplot.stats(lista$price)
# como se nota que existe una gran dispersion en los datos, vamos realizar un analisis por cada delegacion
ggplot(data = lista, mapping=aes(x = neighbourhood_cleansed, y = price, color=neighbourhood_cleansed)) +
  geom_boxplot() +
  geom_jitter(width = 0.1) +
  theme_bw() + theme(legend.position = "none")
# Eliminando outliers por delegacion
# observemos que la media de los precios cambia considerablemente segun la delegacion
# aunque inicialmente agrupamos por precio, el analisis de atipicos se va hacer
# por precio/persona pues no sería valio comparar sin tomar en cuenta el tamaño del hospedaje y
# por consiguiente la cantidad de individuos
lista %>% group_by(neighbourhood_cleansed) %>% summarise_at(vars(price),list(name=mean),na.rm=TRUE)

# separando la base por delegación
lista_2<-lista
inmuebles_Azcapotzalco <-lista_2[lista_2$neighbourhood_cleansed=="Azcapotzalco",]
inmuebles_Benito_Juárez<-lista_2[lista_2$neighbourhood_cleansed=="Benito Juárez",]
inmuebles_Coyoacán <-lista_2[lista_2$neighbourhood_cleansed=="Coyoacán",]
inmuebles_Cuajimalpa_de_Morelos<-lista_2[lista_2$neighbourhood_cleansed=="Cuajimalpa de Morelos",]
inmuebles_Cuauhtémoc  <-lista_2[lista_2$neighbourhood_cleansed=="Cuauhtémoc",]
inmuebles_GAM<-lista_2[lista_2$neighbourhood_cleansed=="Gustavo A. Madero",]
inmuebles_Iztacalco<-lista_2[lista_2$neighbourhood_cleansed=="Iztacalco",]
inmuebles_Iztapalapa<-lista_2[lista_2$neighbourhood_cleansed=="Iztapalapa",]
inmuebles_MC<-lista_2[lista_2$neighbourhood_cleansed=="La Magdalena Contreras",]
inmuebles_MH<-lista_2[lista_2$neighbourhood_cleansed=="Miguel Hidalgo",]
inmuebles_Milpa_Alta<-lista_2[lista_2$neighbourhood_cleansed=="Milpa Alta",]
inmuebles_Tlalpan<-lista_2[lista_2$neighbourhood_cleansed=="Tlalpan",]
inmuebles_Tláhuac<-lista_2[lista_2$neighbourhood_cleansed=="Tláhuac",]
inmuebles_Venustiano_Carranza<-lista_2[lista_2$neighbourhood_cleansed=="Venustiano Carranza",]
inmuebles_Xochimilco<-lista_2[lista_2$neighbourhood_cleansed=="Xochimilco",]
inmuebles_Álvaro_Obregón<-lista_2[lista_2$neighbourhood_cleansed=="Álvaro Obregón",]


# Analisis Azcapotzalco
hist(inmuebles_Azcapotzalco$precio_persona,main ="Precios Azcapotzalco",xlab="Precio")
boxplot(inmuebles_Azcapotzalco$precio_persona, xlab="Precio airbnb Azcapotzalco")
boxplot.stats(inmuebles_Azcapotzalco$precio_persona)
# con ayuda de boxplot.stats el parametro $ out nos dara los outliers para cada caso, solo bastara filtrar los datos, se usa un
# coeficiente 3 para los valores atipicos para que estos sean claramente atipicos y no solo candidatos
out_Azcapotzalco<-boxplot.stats(inmuebles_Azcapotzalco$precio_persona)$out
inmuebles_Azcapotzalco_filtro <-inmuebles_Azcapotzalco[!grepl(paste(out_Azcapotzalco,collapse='|'),inmuebles_Azcapotzalco$precio_persona),]

# Analisis Benito Juarez
hist(inmuebles_Benito_Juárez$precio_persona,main ="Precios Benito_Juárez",xlab="Precio")
boxplot(inmuebles_Benito_Juárez$precio_persona, xlab="Precio airbnb Benito Juarez")
boxplot.stats(inmuebles_Benito_Juárez$precio_persona)
out_Benito_Juárez<-boxplot.stats(inmuebles_Benito_Juárez$precio_persona)$out
inmuebles_Benito_Juárez_filtro <-inmuebles_Benito_Juárez[!grepl(paste(out_Benito_Juárez,collapse='|'),inmuebles_Benito_Juárez$precio_persona),]


# Analisis Coyoacan
hist(inmuebles_Coyoacán$precio_persona,main ="Precios Coyoacán",xlab="Precio")
boxplot(inmuebles_Coyoacán$precio_persona, xlab="Precio airbnb Coyoacán")
boxplot.stats(inmuebles_Coyoacán$precio_persona)
out_Coyoacán<-boxplot.stats(inmuebles_Coyoacán$precio_persona)$out
inmuebles_Coyoacán_filtro <-inmuebles_Coyoacán[!grepl(paste(out_Coyoacán,collapse='|'),inmuebles_Coyoacán$precio_persona),]

# Analisis Cuajimalpa
hist(inmuebles_Cuajimalpa_de_Morelos$precio_persona,main ="Precios Cuajimalpa",xlab="Precio")
boxplot(inmuebles_Cuajimalpa_de_Morelos$precio_persona, xlab="Precio airbnb Cuajimalpa")
boxplot.stats(inmuebles_Cuajimalpa_de_Morelos$precio_persona)
out_Cuajimalpa_de_Morelos<-boxplot.stats(inmuebles_Cuajimalpa_de_Morelos$precio_persona)$out
inmuebles_Cuajimalpa_de_Morelos_filtro <-inmuebles_Cuajimalpa_de_Morelos[!grepl(paste(out_Cuajimalpa_de_Morelos,collapse='|'),inmuebles_Cuajimalpa_de_Morelos$precio_persona),]

# Analisis Cuauhemoc
hist(inmuebles_Cuauhtémoc$precio_persona,main ="Precios Cuauhtémoc",xlab="Precio")
boxplot(inmuebles_Cuauhtémoc$precio_persona, xlab="Precio airbnb Cuauhtémoc")
boxplot.stats(inmuebles_Cuauhtémoc$precio_persona)
out_Cuauhtémoc<-boxplot.stats(inmuebles_Cuauhtémoc$precio_persona)$out
inmuebles_Cuauhtémoc_filtro <-inmuebles_Cuauhtémoc[!grepl(paste(out_Cuauhtémoc,collapse='|'),inmuebles_Cuauhtémoc$precio_persona),]


# Analisis GAM
hist(inmuebles_GAM$precio_persona,main ="Precios GAM",xlab="Precio")
boxplot(inmuebles_GAM$precio_persona, xlab="Precio airbnb GAM")
boxplot.stats(inmuebles_GAM$precio_persona)
out_GAM<-boxplot.stats(inmuebles_GAM$precio_persona)$out
inmuebles_GAM_filtro <-inmuebles_GAM[!grepl(paste(out_GAM,collapse='|'),inmuebles_GAM$precio_persona),]


# Analisis Iztacalco
hist(inmuebles_Iztacalco$precio_persona,main ="Precios Iztacalco",xlab="Precio")
boxplot(inmuebles_Iztacalco$precio_persona, xlab="Precio airbnb Iztacalco")
boxplot.stats(inmuebles_Iztacalco$precio_persona)
out_Iztacalco<-boxplot.stats(inmuebles_Iztacalco$precio_persona)$out
inmuebles_Iztacalco_filtro <-inmuebles_Iztacalco[!grepl(paste(out_Iztacalco,collapse='|'),inmuebles_Iztacalco$precio_persona),]


# Analisis Iztapalapa
hist(inmuebles_Iztapalapa$precio_persona,main ="Precios Iztapalapa",xlab="Precio")
boxplot(inmuebles_Iztapalapa$precio_persona, xlab="Precio airbnb Iztapalapa")
boxplot.stats(inmuebles_Iztapalapa$precio_persona)
out_Iztapalapa<-boxplot.stats(inmuebles_Iztapalapa$precio_persona)$out
inmuebles_Iztapalapa_filtro <-inmuebles_Iztapalapa[!grepl(paste(out_Iztapalapa,collapse='|'),inmuebles_Iztapalapa$precio_persona),]


# Analisis Magdalena Contreras
hist(inmuebles_MC$precio_persona,main ="Precios Magdalena Contreras",xlab="Precio")
boxplot(inmuebles_MC$precio_persona, xlab="Precio airbnb Magdalena Contreras")
boxplot.stats(inmuebles_MC$precio_persona)
out_MC<-boxplot.stats(inmuebles_MC$precio_persona)$out
inmuebles_MC_filtro <-inmuebles_MC[!grepl(paste(out_MC,collapse='|'),inmuebles_MC$precio_persona),]


# Analisis Miguel Hidalgo
hist(inmuebles_MH$precio_persona,main ="Precios Miguel Hidalgo",xlab="Precio")
boxplot(inmuebles_MH$precio_persona, xlab="Precio airbnb Miguel Hidalgo")
boxplot.stats(inmuebles_MH$precio_persona)
out_MH<-boxplot.stats(inmuebles_MH$precio_persona)$out
inmuebles_MH_filtro <-inmuebles_MH[!grepl(paste(out_MH,collapse='|'),inmuebles_MH$precio_persona),]

# Analisis Milpa_Alta
hist(inmuebles_Milpa_Alta$precio_persona,main ="Precios Milpa_Alta",xlab="Precio")
boxplot(inmuebles_Milpa_Alta$precio_persona, xlab="Precio airbnb Milpa_Alta")
boxplot.stats(inmuebles_Milpa_Alta$precio_persona)
out_Milpa_Alta<-boxplot.stats(inmuebles_Milpa_Alta$precio_persona)$out
inmuebles_Milpa_Alta_filtro <-inmuebles_Milpa_Alta[!grepl(paste(out_Milpa_Alta,collapse='|'),inmuebles_Milpa_Alta$precio_persona),]

# Analisis Tlalpan
hist(inmuebles_Tlalpan$precio_persona,main ="Precios Tlalpan",xlab="Precio")
boxplot(inmuebles_Tlalpan$precio_persona, xlab="Precio airbnb Tlalpan")
boxplot.stats(inmuebles_Tlalpan$precio_persona)
out_Tlalpan<-boxplot.stats(inmuebles_Tlalpan$precio_persona)$out
inmuebles_Tlalpan_filtro <-inmuebles_Tlalpan[!grepl(paste(out_Tlalpan,collapse='|'),inmuebles_Tlalpan$precio_persona),]

# Analisis Tláhuac
hist(inmuebles_Tláhuac$precio_persona,main ="Precios Tláhuac",xlab="Precio")
boxplot(inmuebles_Tláhuac$precio_persona, xlab="Precio airbnb Tláhuac")
boxplot.stats(inmuebles_Tláhuac$precio_persona)
out_Tláhuac<-boxplot.stats(inmuebles_Tláhuac$precio_persona)$out
inmuebles_Tláhuac_filtro <-inmuebles_Tláhuac[!grepl(paste(out_Tláhuac,collapse='|'),inmuebles_Tláhuac$precio_persona),]

# Analisis Venustiano_Carranza
hist(inmuebles_Venustiano_Carranza$precio_persona,main ="Precios Venustiano_Carranza",xlab="Precio")
boxplot(inmuebles_Venustiano_Carranza$precio_persona, xlab="Precio airbnb Venustiano_Carranza")
boxplot.stats(inmuebles_Venustiano_Carranza$precio_persona)
out_Venustiano_Carranza<-boxplot.stats(inmuebles_Venustiano_Carranza$precio_persona)$out
inmuebles_Venustiano_Carranza_filtro <-inmuebles_Venustiano_Carranza[!grepl(paste(out_Venustiano_Carranza,collapse='|'),inmuebles_Venustiano_Carranza$precio_persona),]

# Analisis Xochimilco
hist(inmuebles_Xochimilco$precio_persona,main ="Precios Xochimilco",xlab="Precio")
boxplot(inmuebles_Xochimilco$precio_persona, xlab="Precio airbnb Xochimilco")
boxplot.stats(inmuebles_Xochimilco$pprecio_persona)
out_Xochimilco<-boxplot.stats(inmuebles_Xochimilco$precio_persona)$out
inmuebles_Xochimilco_filtro <-inmuebles_Xochimilco[!grepl(paste(out_Xochimilco,collapse='|'),inmuebles_Xochimilco$precio_persona),]

# Analisis Álvaro_Obregón
hist(inmuebles_Álvaro_Obregón$precio_persona,main ="Precios Álvaro_Obregón",xlab="Precio")
boxplot(inmuebles_Álvaro_Obregón$precio_persona, xlab="Precio airbnb Álvaro_Obregón")
boxplot.stats(inmuebles_Álvaro_Obregón$precio_persona)
out_Álvaro_Obregón<-boxplot.stats(inmuebles_Álvaro_Obregón$precio_persona)$out
inmuebles_Álvaro_Obregón_filtro <-inmuebles_Álvaro_Obregón[!grepl(paste(out_Álvaro_Obregón,collapse='|'),inmuebles_Álvaro_Obregón$precio_persona),]

# Ahora juntamos en un mismo dataframe una vez eliminado los atípicos por delegacion
lista_filtrada_2<-rbind(inmuebles_Álvaro_Obregón_filtro,inmuebles_Azcapotzalco_filtro,inmuebles_Benito_Juárez_filtro,inmuebles_Coyoacán_filtro,inmuebles_Cuajimalpa_de_Morelos_filtro,inmuebles_Cuauhtémoc_filtro,inmuebles_GAM_filtro,inmuebles_Iztacalco_filtro,inmuebles_Iztapalapa_filtro,inmuebles_MC_filtro,inmuebles_MH_filtro,inmuebles_Milpa_Alta_filtro,inmuebles_Tláhuac_filtro,inmuebles_Tlalpan_filtro,inmuebles_Venustiano_Carranza_filtro,inmuebles_Xochimilco_filtro)
hist(lista_filtrada_2$price, breaks=10000)
boxplot(lista_filtrada_2$price, xlab="precio_airbnb")
boxplot.stats(lista_filtrada_2$price)
hist(log(lista_filtrada_2$price))

ggplot(data = lista_filtrada_2, mapping=aes(x = neighbourhood_cleansed, y = price, color=neighbourhood_cleansed)) +
  geom_boxplot() +
  geom_jitter(width = 0.1) +
  theme_bw() + theme(legend.position = "none")
# por un lado, a pesar de hacer un analisis por delegacion, la dispersion y outliers siguen presentes
# por lo que hace un analisis por cada tipo de cuarto, pues de esto depende el tamaño del hospedaje
# y la privacidad del mismo
# vemos que la media de precios pareciera no haber cambiado tanto
lista_filtrada_2 %>% group_by(neighbourhood_cleansed) %>% summarise_at(vars(price),list(name=mean),na.rm=TRUE)
table(lista_filtrada_2$room_type) # para ver la cantidad de hospedajes por cada tipo de cuarto

lista_3<-lista_filtrada_2
# dividimos por el tipo de hospedaje
entire <-lista_3[lista_3$room_type=="Entire home/apt",]
hotel_room<-lista_3[lista_3$room_type=="Hotel room",]
private_room<-lista_3[lista_3$room_type=="Private room",]
shared_room<-lista_3[lista_3$room_type=="Shared room",]

# Analisis entire
hist(entire$precio_persona,main ="Precios entire",xlab="Precio")
boxplot(entire$precio_persona, xlab="Precio airbnb entire")
boxplot.stats(entire$precio_persona)
out_entire<-boxplot.stats(entire$precio_persona)$out
entire_filtro <-entire[!grepl(paste(out_entire,collapse='|'),entire$precio_persona),]


# Analisis hotel_room
hist(hotel_room$precio_persona,main ="Precios hotel_room",xlab="Precio")
boxplot(hotel_room$precio_persona, xlab="Precio airbnb hotel_room")
boxplot.stats(hotel_room$precio_persona)
out_hotel_room<-boxplot.stats(hotel_room$precio_persona)$out
hotel_filtro <-hotel_room[!grepl(paste(out_hotel_room,collapse='|'),hotel_room$precio_persona),]

# Analisis private_room
hist(private_room$precio_persona,main ="Precios private_room",xlab="Precio")
boxplot(private_room$precio_persona, xlab="Precio airbnb private_room")
boxplot.stats(private_room$precio_persona)
out_private_room<-boxplot.stats(private_room$precio_persona)$out
private_room_filtro <-private_room[!grepl(paste(out_private_room,collapse='|'),private_room$precio_persona),]

# Analisis shared_room
hist(shared_room$precio_persona,main ="Precios shared_room",xlab="Precio")
boxplot(shared_room$precio_persona, xlab="Precio airbnb shared_room")
boxplot.stats(shared_room$precio_persona)
out_shared_room<-boxplot.stats(shared_room$precio_persona)$out
shared_room_filtro <-shared_room[!grepl(paste(out_shared_room,collapse='|'),shared_room$precio_persona),]

lista_filtrada_3<-rbind(entire_filtro,hotel_filtro,private_room_filtro,shared_room_filtro)
hist(lista_filtrada_3$price, breaks=100)
boxplot(lista_filtrada_3$price, xlab="precio_airbnb")
boxplot.stats(lista_filtrada_3$price)
hist(log(lista_filtrada_3$price))
ggplot(data = lista_filtrada_3, mapping=aes(x = room_type, y = price, color=room_type)) +
  geom_boxplot() +
  geom_jitter(width = 0.1) +
  theme_bw() + theme(legend.position = "none")
# Una vez tratados los datos, observamos que 16 delegaciones al ser usadas como variable cualitativa
# generaria una gran cantidad de variables dummy (15) por lo tanto se agruparan segun su media de precios
# en rangos de precio o localidad de precio bajo, medio y alto, esto a fin de intentar un mejor tratamiento de los
# datos
# se agruparan en zonas tipo A,B,C donde A corresponde a las delegaciones de precios mas altos, B las de precio medio
# y las de precio mas bajo 
tipo_A <-c("Cuajimalpa de Morelos","Cuauhtémoc","Miguel Hidalgo")
tipo_B <-c("Benito Juárez","Coyoacán","Iztacalco","La Magdalena Contreras","Milpa Alta","Tlalpan","Xochimilco","Álvaro Obregón")
tipo_C <-c("Azcapotzalco","Gustavo A. Madero","Iztapalapa","Tláhuac","Venustiano Carranza")
for (i in 1:length(lista_filtrada_3$neighbourhood_cleansed)) {
  if (lista_filtrada_3$neighbourhood_cleansed[i] %in% tipo_A) {
    lista_filtrada_3$zona_tipo[i] <- "A"
  }else if(lista_filtrada_3$neighbourhood_cleansed[i] %in% tipo_B){
    lista_filtrada_3$zona_tipo[i] <- "B"
  }
  else if(lista_filtrada_3$neighbourhood_cleansed[i] %in% tipo_C){
    lista_filtrada_3$zona_tipo[i] <- "C"
  }
}
# las medias para cada tipo de zona son las siguientes
lista_filtrada_3 %>% group_by(zona_tipo) %>% summarise_at(vars(price),list(name=mean),na.rm=TRUE)

ggplot(data = lista_filtrada_3, mapping=aes(x = zona_tipo, y = price, color=zona_tipo)) +
  geom_boxplot() +
  geom_jitter(width = 0.1) +
  theme_bw() + theme(legend.position = "none")
# por ultimo para este analisis inicial, se trataran los valores atipicos para cada
# primero se segmenta por el tipo de zona
alto <-lista_filtrada_3[lista_filtrada_3$zona_tipo=="A",]
medio<-lista_filtrada_3[lista_filtrada_3$zona_tipo=="B",]
bajo<-lista_filtrada_3[lista_filtrada_3$zona_tipo=="C",]

# Analisis  alto
hist(alto$precio_persona,main ="Precios zona A",xlab="Precio")
boxplot(alto$precio_persona, xlab="Precio airbnb zona A")
boxplot.stats(alto$precio_persona)
out_alto<-boxplot.stats(alto$precio_persona)$out
alto_filtro <-alto[!grepl(paste(out_alto,collapse='|'),alto$precio_persona),]

# Analisis  medio
hist(medio$precio_persona,main ="Precios zona B",xlab="Precio")
boxplot(medio$precio_persona, xlab="Precio airbnb zona B")
boxplot.stats(medio$precio_persona)
out_medio<-boxplot.stats(medio$precio_persona)$out
medio_filtro <-medio[!grepl(paste(out_medio,collapse='|'),medio$precio_persona),]

# Analisis  bajo
hist(bajo$precio_persona,main ="Precios zona C",xlab="Precio")
boxplot(bajo$precio_persona, xlab="Precio airbnb zona C")
boxplot.stats(bajo$precio_persona)
out_bajo<-boxplot.stats(bajo$precio_persona)$out
bajo_filtro <-bajo[!grepl(paste(out_bajo,collapse='|'),bajo$precio_persona),]

lista_filtrada_4<-rbind(alto_filtro,medio_filtro,bajo_filtro)
hist(lista_filtrada_4$price, breaks=100)
boxplot(lista_filtrada_4$price, xlab="precio_airbnb")
boxplot.stats(lista_filtrada_4$price)
hist(lista_filtrada_4$price)
hist(log(lista_filtrada_4$price),breaks=10)

# segun el documento (uno que les voy a pasar) segun varios trabajos es importante
# la antiguedad al considerar el precio, por lo que se tomara la antiguedad (en años)
# para cada host
lista_filtrada_4$antiguedad <-as.Date("2023-06-01")-lista_filtrada_4$host_since
lista_filtrada_4$antiguedad <-as.numeric(lista_filtrada_4$antiguedad)/365


# En esta parte segmentamos las variables que podrían ser de interes pues el dataset original
# contiene muchas variables que no reflejan información sobresaliente explican por que
# variables como url o imagen no se integran

datos_filtrados <-lista_filtrada_4[c('host_is_superhost','host_identity_verified','instant_bookable','neighbourhood_cleansed','zona_tipo','room_type','price','antiguedad','accommodates','bathrooms_text','beds','bedrooms','amenities','number_of_reviews','review_scores_rating')]
# de aca superhost, id verified e instant booking viene en la pagina de airbnb que te permite
# cobrar mas
#datos_filtrados %>%view
#datos_filtrados %>%skimr::skim()
datos_filtrados$bathrooms_text <- (gsub("[^0-9.]", "", datos_filtrados$bathrooms_text))
datos_filtrados$bathrooms_text<-as.numeric(datos_filtrados$bathrooms_text)
#datos_filtrados <-datos_filtrados %>% drop_na(bathrooms_text)
library(stringr)
# se podríá tomar en cuenta el numero de amenidades pues la renta del inmueble deberia cubrir o amortizar el desgaste de dichas amenidades
for (i in 1:length(datos_filtrados$amenities)){
  datos_filtrados$amenities[i] <-stringr::str_replace_all(gsub("[^0-9a-zA-Z]", "",datos_filtrados$amenities[i]),"((?<=[a-z])[A-Z]|[A-Z](?=[a-z]))",  " \\1")
  datos_filtrados$amenities[i]<-str_count(datos_filtrados$amenities[i], " ")
}
datos_filtrados$amenities<-as.numeric(datos_filtrados$amenities)
# se infiere que el score rating es 0 para los na
datos_filtrados$review_scores_rating[is.na(datos_filtrados$review_scores_rating)] <- 0
datos_filtrados <-datos_filtrados %>% drop_na(bathrooms_text)

datos_filtrados$host_is_superhost <-as.factor(datos_filtrados$host_is_superhost)
datos_filtrados$host_identity_verified <-as.factor(datos_filtrados$host_identity_verified)
datos_filtrados$instant_bookable <-as.factor(datos_filtrados$instant_bookable)
datos_filtrados$zona_tipo <-as.factor(datos_filtrados$zona_tipo)
datos_filtrados$room_type <-as.factor(datos_filtrados$room_type)
model1_T <-lm(price ~ host_identity_verified+host_is_superhost+instant_bookable
            +zona_tipo+room_type+antiguedad+accommodates+bathrooms_text+
              beds+bedrooms+amenities+number_of_reviews+review_scores_rating,data=datos_filtrados)

model1<- model1_T
summary(model1)$adj.r.squared




cooksDistance = cooks.distance(model1_T)
numOutliersCD = sum(cooksDistance > 4 / length(cooksDistance))
datos_filtrados_2<-datos_filtrados
datos_filtrados_2$cooksDistance <- as.numeric(cooks.distance(model1_T))

criterio_4_over_n<-datos_filtrados_2$cooksDistance[datos_filtrados_2$cooksDistance>4/length(datos_filtrados_2$cooksDistance)]
datos_filtrados_2_no_out <-datos_filtrados_2[!grepl(paste(criterio_4_over_n,collapse='|'),datos_filtrados_2$cooksDistance),]
# una vez filtrados, se calcula el modelo de nuevo con la base actualizada
model1_cooks <-lm(price ~ host_identity_verified+host_is_superhost+instant_bookable
                  +zona_tipo+room_type+antiguedad+accommodates+bathrooms_text+
                    beds+bedrooms+amenities+number_of_reviews+review_scores_rating,data=datos_filtrados_2_no_out)
summary(model1_cooks)$adj.r.squared
# 0.6301603
# Revisamos de nuevo el supuesto de normalidad
qqPlot(model1_cooks$residuals, 
       ylab = "", 
       xlab = "")
mtext("Residuals", side = 2, line = 2.6, cex = 1)
mtext("Normal Quantiles", side = 1, line = 2.6, cex = 1)
#prueba estadística

ad.test(pnorm(rstandard(model1_cooks),mean=0,sd=1))
# como no se cumple le hacemos su boxplot
bc<-boxcox(model1_cooks) 
lamda<-bc$x[which.max(bc$y)]
lamda
price_T<-((datos_filtrados_2_no_out$price^lamda)-1)/lamda
model1_cooks_T <-lm(price_T ~ host_identity_verified+host_is_superhost+instant_bookable
                    +zona_tipo+room_type+antiguedad+accommodates+bathrooms_text+
                      beds+bedrooms+amenities+number_of_reviews+review_scores_rating,data=datos_filtrados_2_no_out)
summary(model1_cooks_T)$adj.r.squared
# 0.6824103

# REVISANDO LA NORMALIDAD
# Graficamente
qqPlot(model1_cooks_T$residuals, 
       ylab = "", 
       xlab = "")
mtext("Residuals", side = 2, line = 2.6, cex = 1)
mtext("Cuantiles dist normal", side = 1, line = 2.6, cex = 1) # se cumple nomralidad en errores
#prueba estadística
ad.test(pnorm(rstandard(model1_cooks_T),mean=0,sd=1))
# revisar si la media de los residuales es cercana a 0
mean(model1_cooks_T$residuals)

# Homocedasticidad
# graficamente, ambas graficas son para ver la varianza constante
resVarianceDF = data.frame(residuals = model1_cooks_T$residuals, fittedValues = model1_cooks_T$fitted.values)
ggplot(resVarianceDF, aes(x = model1_cooks_T$fitted.values, y = model1_cooks_T$residuals)) + 
  geom_point(alpha = 0.4) + 
  labs(x = 'Residuos', y = 'Ajustados') +
  theme_minimal() +
  theme(text = element_text(size = 13), 
        axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 13))

plot(model1_cooks_T,3) # varianza constante
plot(rstudent(model1_cooks_T)) # varianza conste
# Prueba estadistica
ncvTest(model1_cooks_T) # se cumple varianza constante

# Multicolinealidad
# hacemos la prueba vif pero como hay varios predictores cuantitativos no se da la colinealidad
vif(model1_cooks_T)

# Linealidad
plot(model1_cooks_T,1) 
library(ggplot2)
library(gridExtra)
plot1 <- ggplot(data = datos_filtrados_2_no_out, aes(antiguedad,model1_cooks_T$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot2 <- ggplot(data = datos_filtrados_2_no_out, aes(accommodates,model1_cooks_T$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot3 <- ggplot(data = datos_filtrados_2_no_out, aes(bathrooms_text,model1_cooks_T$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot4 <- ggplot(data = datos_filtrados_2_no_out, aes(beds,model1_cooks_T$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
grid.arrange(plot1, plot2, plot3, plot4)
# explicar porque pareciera que nos e cumple la linealidad con los baños en resumen es por la observacion
# extrema, fuera de esto se cumple la linealidad
plot1 <- ggplot(data = datos_filtrados_2_no_out, aes(bedrooms,model1_cooks_T$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot2 <- ggplot(data = datos_filtrados_2_no_out, aes(amenities,model1_cooks_T$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot3 <- ggplot(data = datos_filtrados_2_no_out, aes(number_of_reviews,model1_cooks_T$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot4 <- ggplot(data = datos_filtrados_2_no_out, aes(review_scores_rating,model1_cooks_T$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
grid.arrange(plot1, plot2, plot3, plot4)
# igual aca es explicar que para bedrooms no pareciera cumplirse por la observacion extrema, fuera de esto
# se cumpliria, al igual que para las demas variables



#PASO3: ANÁLISIS DE PUNTOS INFLUYENTES
#Distancia de Cook
plot(model1_cooks_T, 4)

#Puntos palanca para betas
plot(dfbetas(model1_cooks_T)[,1],type="h", ylab="Beta0")
plot(dfbetas(model1_cooks_T)[,2],type="h", ylab="Beta1")
plot(dfbetas(model1_cooks_T)[,3],type="h", ylab="Beta2")
plot(dfbetas(model1_cooks_T)[,4],type="h", ylab="Beta3")
plot(dfbetas(model1_cooks_T)[,5],type="h", ylab="Beta4")
plot(dfbetas(model1_cooks_T)[,6],type="h", ylab="Beta5")
plot(dfbetas(model1_cooks_T)[,7],type="h", ylab="Beta6")
plot(dfbetas(model1_cooks_T)[,8],type="h", ylab="Beta7")
plot(dfbetas(model1_cooks_T)[,9],type="h", ylab="Beta8")
plot(dfbetas(model1_cooks_T)[,10],type="h", ylab="Beta9")
plot(dfbetas(model1_cooks_T)[,11],type="h", ylab="Beta10")
plot(dfbetas(model1_cooks_T)[,12],type="h", ylab="Beta11")
plot(dfbetas(model1_cooks_T)[,13],type="h", ylab="Beta12")
plot(dfbetas(model1_cooks_T)[,14],type="h", ylab="Beta13")
plot(dfbetas(model1_cooks_T)[,15],type="h", ylab="Beta14")
plot(dfbetas(model1_cooks_T)[,16],type="h", ylab="Beta15")
plot(dfbetas(model1_cooks_T)[,17],type="h", ylab="Beta16")
#Puntos palanca para los datos ajustados
plot(dffits(model1_cooks_T),type="h")

# se denota laobservacion 9692 como influyente para el calculo de betas y valores ajustados
datos_filtrados_3<-datos_filtrados_2_no_out[-9692,]
model2 <-lm(price ~ host_identity_verified+host_is_superhost+instant_bookable
                  +zona_tipo+room_type+antiguedad+accommodates+bathrooms_text+
                    beds+bedrooms+amenities+number_of_reviews+review_scores_rating,data=datos_filtrados_3)
summary(model2)$adj.r.squared
# 0.62907
ad.test(pnorm(rstandard(model2),mean=0,sd=1))
bc<-boxcox(model2) 
lamda<-bc$x[which.max(bc$y)]
lamda
price_T<-((datos_filtrados_3$price^lamda)-1)/lamda
model2_T <-lm(price_T ~ host_identity_verified+host_is_superhost+instant_bookable
                    +zona_tipo+room_type+antiguedad+accommodates+bathrooms_text+
                      beds+bedrooms+amenities+number_of_reviews+review_scores_rating,data=datos_filtrados_3)
summary(model2_T)$adj.r.squared
# 0.6824131

# Analisis de supuestos
# REVISANDO LA NORMALIDAD
# Graficamente
qqPlot(model2_T$residuals, 
       ylab = "", 
       xlab = "")
mtext("Residuals", side = 2, line = 2.6, cex = 1)
mtext("Cuantiles dist normal", side = 1, line = 2.6, cex = 1) # se cumple nomralidad en errores
#prueba estadística
ad.test(pnorm(rstandard(model2_T),mean=0,sd=1))
# revisar si la media de los residuales es cercana a 0
mean(model2_T$residuals)

# Homocedasticidad
# graficamente, ambas graficas son para ver la varianza constante
resVarianceDF = data.frame(residuals = model2_T$residuals, fittedValues = model2_T$fitted.values)
ggplot(resVarianceDF, aes(x = model2_T$fitted.values, y = model2_T$residuals)) + 
  geom_point(alpha = 0.4) + 
  labs(x = 'Residuos', y = 'Ajustados') +
  theme_minimal() +
  theme(text = element_text(size = 13), 
        axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 13))

plot(model2_T,3) # varianza constante
plot(rstudent(model2_T)) # varianza conste
# Prueba estadistica
ncvTest(model2_T)# se cumple varianza constante
# Multicolinealidad
# hacemos la prueba vif pero como hay varios predictores cuantitativos no se da la colinealidad
vif(model2_T)

# Linealidad
plot(model2_T,1)
plot1 <- ggplot(data = datos_filtrados_3, aes(antiguedad,model2_T$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot2 <- ggplot(data = datos_filtrados_3, aes(accommodates,model2_T$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot3 <- ggplot(data = datos_filtrados_3, aes(bathrooms_text,model2_T$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot4 <- ggplot(data = datos_filtrados_3, aes(beds,model2_T$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
grid.arrange(plot1, plot2, plot3, plot4)
# explicar porque pareciera que nos e cumple la linealidad con los baños en resumen es por la observacion
# extrema, fuera de esto se cumple la linealidad
plot1 <- ggplot(data = datos_filtrados_3, aes(bedrooms,model2_T$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot2 <- ggplot(data = datos_filtrados_3, aes(amenities,model2_T$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot3 <- ggplot(data = datos_filtrados_3, aes(number_of_reviews,model2_T$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot4 <- ggplot(data = datos_filtrados_3, aes(review_scores_rating,model2_T$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
grid.arrange(plot1, plot2, plot3, plot4)
# vemos que se cumple ahora si la linealidad para todas las variables al retirar dicho punto influyente


#ANÁLISIS DE PUNTOS INFLUYENTES
#Distancia de Cook
plot(model2_T, 4) # aunque parecieran existir, no son suficiente en cuanto a distancia de cook
# como para ser sobresalientes 
plot(model2_T, 5) # se muestra qui que los puntos palancas ya no generan algun cambio en la estimacion 

# observemos lo siguiente
#EL MODELO model2_T ES NUESTRO MODELO FINAL:
#CUMPLE CON:
#1)Normalidad
#2)Linealidad
#3)Homocedasticidad
#4)No tiene multicolinelaidad
#5)No hay observaciones influyentes
summary(model2_T) # nota para churros de aca toma las betas

nom_col<- c( "Miguel Hidalgo",        
             "Cuajimalpa de Morelos", 
             "Cuauhtémoc",            
             "Venustiano Carranza",   
             "La Magdalena Contreras",
             "Benito Juárez",         
             "Álvaro Obregón",        
             "Iztacalco",      
             "Tlalpan",    
             "Gustavo A. Madero   ",  
             "Xochimilco",
             "Iztapalapa",
             "Azcapotzalco ",
             "Tláhuac"               
             ,"Milpa Alta",
             "Coyoacán") 

coeficientes <- model2_T[1]$coefficients
