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
  filter(data == data_atual_concelhos)


metadata <- read_delim('concelhos-metadata.csv', delim = ';') %>%
  select(dicofre, pre, pre2, pre3, designacao)

distritos_ok <- read_csv('distritos.csv')

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

incidencia_concelhos <- incidencia_concelhos %>%
  left_join(metadata) %>%
  select(designacao, distrito, incidencia, pre, pre2, pre3) %>%
  rename('label' = 'designacao',
         'group' = 'distrito') %>%
  mutate(value = label) %>%
  left_join(distritos_ok) %>%
  select(-group) %>%
  rename('group' = 'distrito') %>%
  arrange(group)



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




