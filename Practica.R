#Punto 1 y 2
install.packages("dplyr")
library(dplyr)

listings <- read.csv("listings.csv")

listing_corto=listings[, c("name", "neighbourhood", "price", "bedrooms", "beds", "accommodates", "minimum_nights", "maximum_nights", "number_of_reviews", "host_identity_verified")]

listing_num=listings[, c("price", "bedrooms", "beds", "accommodates", "minimum_nights", "maximum_nights", "number_of_reviews", "host_identity_verified")]

listing_corto$price_2 = as.numeric(gsub("\\$", "", listing_corto$price))

listing_corto <- mutate_if(listing_corto, is.numeric, ~replace(., is.na(.), 0))


#En este punto tenemos los dos listings y una columna de más donde tenemos el precio si el signo
#de dolar.

#Instruccion que mira cuantos NA(Valores no validos en cada columna y cuantos hay en cada)
apply(is.na(listing_corto), 2, sum)

#Instrucción que dice cuantos NA(Valores no validos hay en todo el df)
#sum(is.na(listing_corto))


calculos=function(x) 
{
  N=as.numeric(table(!is.na(x))["TRUE"])
  NAs=length(x)-N
  
  return(c(N=N,NAs=NAs,media=mean(x),Varianza=var(x),
           Q1=quantile(x,0.25),Q3=quantile(x,0.75),mediana=median(x),
           Max=max(x),Min=min(x)))
}
knitr::kable(rawdata,digits = 2,aling="c",
             caption="Valores originales de la encuesta del alumnado de Mates III")

aux=apply(listing_num,calculos)

knitr::kable(t(round(aux,2)),digits = 2,aling="c",caption="Estadísticos básicos toda la muestra")



library(knitr)
rawd <- read.csv("listings.csv")
rawdata=rawd[, c("name", "neighbourhood", "price", "bedrooms", "beds", "accommodates", "minimum_nights", "maximum_nights", "number_of_reviews", "host_identity_verified")]
knitr::kable(rawdata,digits = 2,aling="c",caption="Valores originales del listings de Malta Febrero 2021")

