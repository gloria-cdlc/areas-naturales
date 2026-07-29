#indicando mi cuenta de github
library(usethis)
use_git_config(user.name = "gloria-cdlc", user.email = "melissacdlc@gmail.com")
library(usethis)
create_github_token()
library(gitcreds)
gitcreds::gitcreds_set()
#
#

#### Actividade #####

#Tydiverse
####Import -> tydi -> Transformar datos -> Modelar / Visualizar####
install.packages("readxl")
install.packages("tidyverse")
install.packages("writexl")
install.packages("here")

####Carregando pacotes####
library(tidyverse)
library(readxl)
library(openxlsx)
library(here)

 
####Importando solo datos####
uso_solo_rio <- read_excel(here("DATA", "Classes de Uso do solo e Cobertura Vegetal - RJ.xlsx"),
                           sheet = "Dados")
aps_rio <- read.xlsx(here("DATA", "aps_rio.xlsx"))

####Exploracao inicial dos dados####
glimpse(uso_solo_rio)
glimpse(aps_rio)

####Dados de uso do solo no Rio de Janeiro####
#organizando os dados de acordo com uso
uso_solo_categorias <- relocate(uso_solo_rio, Reflorestamento, .after='Afloramento Rochoso')
#Mover bairros rio de janeiro a la columna final
uso_solo_categorias_relocado <- relocate(uso_solo_categorias, `Bairros Rio de Janeiro`, .after = Praia)


#Lo mismo de antes per o de otra forma, usando %>%
uso_solo_categorias_relocado_tidy <- uso_solo_rio %>% 
  relocate(Reflorestamento, .after = 'Afloramento Rochoso') %>% 
  relocate('Bairros Rio de Janeiro', .after = Praia)

#Sumar para crear una única columna de Areas naturais
#Transformando os dados do uso do solo
estimativa_uso_do_solo <- uso_solo_categorias %>% 
  mutate(Reflorestamento_1 = Reflorestamento) %>%
  mutate(Reflorestamento=rep(NA))
#Para ver eso:
View(estimativa_uso_do_solo)

#Se puede usar 'pick' o 'across'
estimativa_uso_do_solo <- uso_solo_categorias %>%
  mutate(Vegetacao_natural = rowSums(across('Floresta Ombrófila Densa' :Brejo))
         , .before ='Floresta Ombrófila Densa') %>%  
  mutate(Antropismos = rowSums(across('Área Urbana' :Reflorestamento)), before = 'Área Urbana') %>%
  mutate(Corpos_dagua_continental = rowSums(across(`Corpo d'água continental` :Praia)), before = `Corpo d'água continental`) %>%
  mutate(Uso_do_solo = rowSums(across(any_of(c("Vegetacao_natural", "Antropismos", "Corpos_dagua_continental"))))) %>%
  relocate(Uso_do_solo, .before = Vegetacao_natural)




####filtrar Bairros RJ ####
uso_do_solo_Bairros_RJ <- estimativa_uso_do_solo %>%
  filter (`Bairros Rio de Janeiro`!= "Lapa") %>%
  select(`Bairros Rio de Janeiro`, Uso_do_solo, Vegetacao_natural, Antropismos, Corpos_dagua_continental)

porcentagens_uso_do_solo_Bairros_RJ <- uso_do_solo_Bairros_RJ %>%
  mutate(Vegetacao_natural_porcentagem = (Vegetacao_natural/Uso_do_solo)*100) %>%
  mutate(Antropismos_porcentagem = (Antropismos/Uso_do_solo)*100) %>%
  mutate(Corpos_dagua_continental_porcentagem = (Corpos_dagua_continental/Uso_do_solo)*100) %>%
  mutate(across(where(is.numeric), round, 2)) %>% #Función para reducir decimales
  select(`Bairros Rio de Janeiro`, Vegetacao_natural_porcentagem: Corpos_dagua_continental_porcentagem)#Seleccionar solo las columnas de interés
View(porcentagens_uso_do_solo_Bairros_RJ)

writexl::write_xlsx(porcentagens_uso_do_solo_Bairros_RJ,"Bairros Rio de Janeiro X Uso do solo - RJ.xlsx")






