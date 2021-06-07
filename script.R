library(tidyverse)
library(lubridate)
library(jsonlite)


# Calculate Incidência nacional com base nos dados mais recentes

data_nacional <- read_csv('https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data.csv') %>%
  mutate(data = dmy(data)) %>%
  select(data, confirmados)

data_atual <- data_nacional %>%
  select(data) %>%
  filter(data == max(data)) %>%
  pull()

data_14_dias <- data_atual - 14

novos_casos_14_dias <- data_nacional %>% filter(data == data_atual | data == data_14_dias)

novos_casos_14_dias <- novos_casos_14_dias$confirmados[2] - novos_casos_14_dias$confirmados[1]

pop_pt <- 10196707

incidencia_nacional <- (novos_casos_14_dias / pop_pt) * 100000




# Get incidêncial local para todos os concelhos

incidencia_concelhos <- read_csv('https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data_concelhos_new.csv') %>%
  mutate(data = dmy(data))

data_atual_concelhos <- incidencia_concelhos %>%
  select(data) %>%
  filter(data == max(data)) %>%
  slice(1) %>%
  pull()

incidencia_concelhos <- incidencia_concelhos %>%
  filter(data == data_atual_concelhos) %>%
  select(distrito, concelho, incidencia) %>%
  rename('value' = 'incidencia',
         'label' = 'concelho',
         'group' = 'distrito')



# Export Everything to JSON

data <-
  list(
    dados_nacionais = list(
      updated = data_atual,
      incidencia_nacional = incidencia_nacional
  ),
    dados_concelho = list(
      updated = data_atual_concelhos,
      incidencia_concelhos = incidencia_concelhos
    )
)

data <- data %>% toJSON(pretty = FALSE, auto_unbox = TRUE, na = "null")

write(data, "incidencia_covid.json")




