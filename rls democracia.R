install.packages("janitor")
install.packages("readxl")
install.packages("naniar")
install.packages("freqdist")
install.packages("berryfunctions")
install.packages("ggstatsplot")
install.packages("berryFunctions")

#Lendo os pacotes necessários
library(janitor)
library(readxl)
library(naniar)
library(dplyr)
library(freqdist)
library(ggplot2)
library(ggstatsplot)
library(berryFunctions)

rsl_democr <- read_excel("C:/Users/netoi/Downloads/RSL_Retrocesso (2).xlsx")


area <-rsl_democr %>% 
  group_by(categoria_journal) %>% 
  count() # contagem de artigos por area da revista

area$perc<-area$n/10*100 # % de artigos por área

area$perc<- round(area$perc, 1) # uma casa decimal (facila a visualização)

ggplot(area, aes(x = reorder(categoria_journal, perc), y = perc)) + geom_bar(stat = "identity" , alpha = .3, fill = "#8E7CC3") +
  coord_flip() +
  labs(title = "Distribuição de artigos pela categoria do journal", 
       subtitle = "Retrocesso Democrático (n = 10)",
       caption = "Fonte: elaboração própria",
       x = "" , y = "%") +
  theme_bw(base_size = 10) +
  geom_text(aes(label = perc), vjust = 0, hjust = -.1) +
  scale_y_continuous(limits = c(0, 100)) # Gráfico com a distribuição de artigos por área da revista


freqdist(rsl_democr$amostra) # distribuição de freq da variável estratégia de comparação (amostra)

amostra_democracia <-rsl_democr %>% 
  group_by(amostra) %>% 
  count() # contagem de artigos por estratégia de comparação

amostra_democracia$perc<-amostra_democracia$n/10*100 # criação do % em relação ao total de artigos revisados
amostra_democracia$perc<-round(amostra_democracia$perc, 1) # redução da quantidade de decimais (para facilitar a visualização)

amostra_democracia <- amostra_democracia %>% 
  mutate(amostra_rec = case_when(
    amostra == "1" ~ "Estudo de caso",
    amostra == "2" ~ "Comparado",
    amostra == "3" ~ "Large n")) # Recodificação das categorias para o gráfico ficar mais informativo (de acordo com o codebook)

ggplot(amostra_democracia, aes(x = reorder(amostra_rec, perc), y = perc)) + geom_bar(stat = "identity" , alpha = .3, fill = "#8E7CC3") +
  coord_flip() +
  labs(title = "Distribuição de artigos por estratégia de comparação", 
       subtitle = "Retrocesso democrático (n = 10)",
       caption = "Fonte: elaboração própria",
       x = "" , y = "%") +
  theme_bw(base_size = 10) +
  geom_text(aes(label = perc), vjust = 0, hjust = -.1) +
  scale_y_continuous(limits = c(0, 100)) # Gráfico com o % de cada categoria (estratégia de comparação)


# por ênfase em técnicas de pesquisa

freqdist(rsl_democr$metodo) # frequencia da variável

tecnicas_democracia <-rsl_democr %>% 
  group_by(metodo) %>% 
  count() # contagem de artigos por ênfase em técnicas de pesquisa

tecnicas_democracia$perc<-tecnicas_democracia$n/10*100 # criação do %
tecnicas_democracia$perc<-round(tecnicas_democracia$perc, 1) # redução das casas decimais

tecnicas_democracia <- tecnicas_democracia %>% 
  mutate(metodo_rec = case_when(
    metodo == "1" ~ "Qualitativa",
    metodo == "2" ~ "Quantitativa")) # recoficação para fins de visualização

ggplot(tecnicas_democracia, aes(x = reorder(metodo_rec, perc), y = perc)) + geom_bar(stat = "identity" , alpha = .3, fill = "#8E7CC3") +
  coord_flip() +
  labs(title = "Distribuição de artigos por ênfase em técnicas de pesquisa", 
       subtitle = "Retrocesso democrático (n = 10)",
       caption = "Fonte: elaboração própria",
       x = "" , y = "%") +
  theme_bw(base_size = 10) +
  geom_text(aes(label = perc), vjust = 0, hjust = -.1) +
  scale_y_continuous(limits = c(0, 100)) # Gráfico com os % por enfase de técnica de pesquisa


freqdist(rsl_democr$fonte_dados) # frequencia da variável

fontedados <-rsl_democr %>% 
  group_by(fonte_dados) %>% 
  count() # contagem de artigos por recorte (tipo de segmento)

fontedados$perc<-fontedados$n/10*100 # criação do $
fontedados$perc<-round(fontedados$perc, 1) # redução das casas decimais

fontedados <- fontedados %>% 
  mutate(fontedados_rec = case_when(
    fonte_dados == "1" ~ "Objetivo",
    fonte_dados == "2" ~ "Subjetivo",
    fonte_dados == "3" ~ "Ambos")) # Recodificação para fins de visualização

ggplot(fontedados, aes(x = reorder(fontedados_rec, perc), y = perc)) + geom_bar(stat = "identity" , alpha = .3, fill = "#8E7CC3") +
  coord_flip() +
  labs(title = "Distribuição de artigos por tipo de coleta", 
       subtitle = "Regressão Democrática (n = 10)",
       caption = "Fonte: elaboração própria",
       x = "" , y = "%") +
  theme_bw(base_size = 10) +
  geom_text(aes(label = perc), vjust = 0, hjust = -.1) +
  scale_y_continuous(limits = c(0, 100)) # Gráfico com o % por tipo de segmento


