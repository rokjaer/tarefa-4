
pacman::p_load(terra, spData)
install.packages("devtools")
utils::remove.packages('geobr')
devtools::install_github("ipeaGIT/geobr", subdir = "r-package")

install.packages("sf")
library(dplyr)
library(geobr)
library(ggplot2)
library(sf)
library(terra)
library(spData)


path="C:\\Users\\rosal\\OneDrive\\Documentos\\Mestrado PUC-Rio\\Verão\\Ciências de Dados\\Exercícios"
my_rast<-rast(file.path(path,"brasil_coverage_2020.tif"))
plot(my_rast)


mun = read_municipality(year =2020)
santa_catarina = mun %>% 
  filter(abbrev_state == "SC") 

# crop, mask e extract
cr = crop(my_rast, santa_catarina)
ms = mask(cr, vect(santa_catarina))
ex = extract(ms, vect(santa_catarina))

plot(cr)
plot(ms)

#somando a cobertura total por município de SC
cobertura <- ex %>%
  group_by(ID) %>%
  summarise(cobertura = n())

#pegando somente a cobertura vegetal
cobertura_vegetal <- ex %>%
  group_by(ID) %>%
  filter(brasil_coverage_2020 %in% c(1,3,4,5,49)) %>%
  summarise(cobertura_v = n())

#juntando as coberturas em um df
cobertura_santa_catarina <- merge(cobertura, cobertura_vegetal, by=c("ID"), all.x=TRUE)

#dividindo cobertura vegetal pela total
cobertura_santa_catarina <- cobertura_santa_catarina %>%
  mutate(p_v = cobertura_v/cobertura)

#criando coluna ID no df original e juntando
santa_catarina <- santa_catarina %>%
  mutate(ID = c(1:295), .after = code_muni) %>%
  left_join(cobertura_santa_catarina, by = "ID")

#plotando mapa de Santa Catarina
plot_santa_catarina_cobertura <- santa_catarina %>%
  ggplot() +
  geom_sf(aes(fill = p_v), alpha = 5, col = "white") +
  scale_fill_viridis_c(name = "Porcentagem") +
  labs(title = "Porcentagem de Cobertura Vegetal", subtitle = "Estado de Santa Catarina")
plot_santa_catarina_cobertura

#cirando df somente com municipios e cobertura vegetal
sc_vegetal <- santa_catarina %>%
  subset(select = c(name_muni, p_v))

