# Bibliotecas necessárias
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(ggmap)
library(maps)
library(RColorBrewer)
library(lubridate)
library(tidyverse)
library(lubridate)
library(forcats)
library(scales)


# Importar o CSV
df <- read_delim("C:\\Users\\nickg\\OneDrive\\detratan\\datatran2024.csv", delim = ";",
                 locale = locale(encoding = "Latin1"))
#Pré-Vizualização

##Lendo as primeias dez linhas
head(df,10)

## nome das colunas
colnames(df)


## Extraindo as imnformações
glimpse(df)

## Extraindo a descrição
summary(df)

## Contando valores não nulos para cada coluna
sapply(df, function(x) sum(!is.na(x)))
## Contando valores nulos por coluna
sapply(df, function(x) sum(is.na(x)))

## Contando valores únicos para cada coluna
sapply(df, function(x) n_distinct(x))

## Contando valores duplicados por coluna
sapply(df, function(x) sum(duplicated(x)))

# TRATAMENTO
df_corrigido <- df

## Corrigindo problemas de digitação
colunas_para_corrigir <- c("dia_semana", "uf", "municipio", "causa_acidente", 
                           "tipo_acidente", "classificacao_acidente", "fase_dia", 
                           "sentido_via", "condicao_metereologica", "tipo_pista", 
                           "tracado_via", "uso_solo")

df_corrigido <- df_corrigido %>%
  mutate(across(all_of(colunas_para_corrigir), ~str_trim(str_to_lower(.x)))) %>%
  mutate(across(all_of(colunas_para_corrigir), ~str_replace_all(.x, "[-_]", " ")))

#específicOs
df_corrigido$dia_semana <- str_replace_all(df_corrigido$dia_semana, 
                                           c("segunda feira" = "segunda-feira",
                                             "terca feira" = "terça-feira",
                                             "quarta feira" = "quarta-feira",
                                             "quinta feira" = "quinta-feira",
                                             "sexta feira" = "sexta-feira"))

df_corrigido$condicao_metereologica <- str_replace_all(df_corrigido$condicao_metereologica, 
                                                       c("céu claro" = "claro",
                                                         "sol" = "claro",
                                                         "chuva leve" = "chuva",
                                                         "chuva forte" = "chuva"))


# =========================
# GRAFICOS
# =========================

# Acidentes ao longo do tempo
df_corrigido$data <- dmy(df_corrigido$data_inversa)

df_corrigido %>%
  count(data) %>%
  ggplot(aes(x = data, y = n)) +
  geom_line(color = "#FFC300", size = 1) +
  labs(
    title = "Evolução Diária dos Acidentes",
    x = "Data",
    y = "Quantidade de Acidentes"
  ) +
  theme_minimal()

# Dias da semana
df_corrigido %>%
  count(dia_semana) %>%
  mutate(dia_semana = fct_relevel(dia_semana, 
                                  "segunda-feira", "terça-feira", "quarta-feira", 
                                  "quinta-feira", "sexta-feira", "sábado", "domingo")) %>%
  ggplot(aes(x = dia_semana, y = n)) +
  geom_col(fill = "#FFDD00", color = "black") +
  labs(title = "Acidentes por Dia da Semana", x = "Dia", y = "Quantidade") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Quantidade de Acidentes por Horário
df_corrigido %>%
  count(horario) %>%
  ggplot(aes(x = horario, y = n)) +
  geom_line(color = "yellow", size = 1) +
  geom_point(color = "#FF8C00", size = 2) +
  labs(
    title = "Quantidade de Acidentes por Horário",
    x = "Hora do dia",
    y = "Quantidade de Acidentes"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Estados mais perigosos
df_corrigido %>%
  count(uf, sort = TRUE) %>%
  ggplot(aes(x = reorder(uf, n), y = n)) +
  geom_col(fill = "#FF8C00", color = "black") +
  coord_flip() +
  labs(title = "Quantidade de Acidentes por Estado (UF)", x = "Estado", y = "Quantidade de Acidentes") +
  theme_minimal()

#Top 3 BR's com mais Acidentes
df_corrigido %>%
  count(br) %>%
  top_n(3) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = reorder(br, -n), y = n, fill = br)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.3, color = "black", fontface = "bold") +
  labs(
    title = "Top 3 BR's com mais Acidentes",
    x = "BR",
    y = "Quantidade de Acidentes"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none")

# Boxplot de KM dos acidentes
df_corrigido$km <- as.numeric(df_corrigido$km)

df_corrigido %>%
  filter(!is.na(km) & km < quantile(km, 0.99, na.rm = TRUE)) %>%
  ggplot(aes(y = km)) +
  geom_boxplot(fill = "#FFDA44", color = "black") +
  labs(title = "Distribuição de KM nos Acidentes", y = "KM") +
  theme_minimal()

# Top 10 municípios
df_corrigido %>%
  count(municipio, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(municipio, n), y = n)) +
  geom_col(fill = "#FFD700", color = "black") +
  coord_flip() +
  labs(title = "Top 10 Municípios com Mais Acidentes", x = "Município", y = "Quantidade") +
  theme_minimal()

# Causas dos acidentes
df_corrigido %>%
  count(causa_acidente, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(causa_acidente, n), y = n)) +
  geom_col(fill = "#FF8C00", color = "black") +
  coord_flip() +
  labs(title = "Top 10 Causas de Acidente", x = "Causa", y = "Quantidade") +
  theme_minimal()

# Tipo de acidente
df_corrigido %>%
  count(tipo_acidente, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(tipo_acidente, n), y = n)) +
  geom_col(fill = "#FF8C00", color = "black") +
  coord_flip() +
  labs(title = "Top 10 Tipos de Acidente", x = "Tipo", y = "Quantidade") +
  theme_minimal()


# Classificação do acidente
df_class <- df_corrigido %>% count(classificacao_acidente) %>% mutate(perc = round(100 * n / sum(n), 1))

ggplot(df_class, aes(x = 2, y = n, fill = classificacao_acidente)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  xlim(0.5, 2.5) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlOrBr"))(nrow(df_class))) +
  geom_text(aes(label = paste0(perc, "%")), position = position_stack(vjust = 0.5)) +
  theme_void() +
  labs(title = "Classificação dos Acidentes")

# Período dos acidentes
df_corrigido %>%
  count(fase_dia, sort = TRUE) %>%
  mutate(
    perc = n / sum(n),
    labels = scales::percent(perc)
  ) %>%
  ggplot(aes(x = reorder(fase_dia, n), y = n)) +
  geom_col(fill = "#FF8C00", color = "black") +
  geom_text(
    aes(label = labels),
    vjust = -0.5,
    color = "black",
    fontface = "bold"
  ) +
  labs(title = "Acidentes por Período do Dia", x = "Período", y = "Quantidade de Acidentes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Sentido da via
df_sentido_via <- df_corrigido %>%
  count(sentido_via) %>%
  mutate(percentage = n / sum(n) * 100)

#
df_condicao_metereologica <- df_corrigido %>%
  count(condicao_metereologica) %>%
  mutate(percentage = n / sum(n) * 100)

num_niveis <- length(unique(df_condicao_metereologica$condicao_metereologica))
cores <- colorRampPalette(c("#FF8C00", "#e0a96d", "#d1b28d"))(num_niveis)

ggplot(df_condicao_metereologica, aes(x = reorder(condicao_metereologica, -percentage), y = percentage, fill = condicao_metereologica)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5, color = "black") +
  scale_fill_manual(values = cores) +
  labs(title = "Condição Meteorológica", x = "Condição Meteorológica", y = "Porcentagem") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Tipo de pista

df_tipo_pista <- df_corrigido %>%
  count(tipo_pista) %>%
  mutate(percentage = n / sum(n) * 100)

num_niveis <- length(unique(df_tipo_pista$tipo_pista))
cores <- colorRampPalette(c("#FF8C00", "#e0a96d", "#d1b28d"))(num_niveis)

ggplot(df_tipo_pista, aes(x = reorder(tipo_pista, -percentage), y = percentage, fill = tipo_pista)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5, color = "black") +
  scale_fill_manual(values = cores) +
  labs(title = "Tipo de Pista", x = "Tipo de Pista", y = "Porcentagem") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Via
df_tracado_via <- df_corrigido %>%
  count(tracado_via) %>%
  mutate(percentage = n / sum(n) * 100)

num_niveis <- length(unique(df_tracado_via$tracado_via))
cores <- colorRampPalette(c("#FF8C00", "#e0a96d", "#d1b28d"))(num_niveis)

ggplot(df_tracado_via, aes(x = reorder(tracado_via, -percentage), y = percentage, fill = tracado_via)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5, color = "black") +
  scale_fill_manual(values = cores) +
  labs(title = "Traçado da Via", x = "Traçado da Via", y = "Porcentagem") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Uso do Solo
df_uso_solo <- df_corrigido %>%
  count(uso_solo) %>%
  mutate(percentage = n / sum(n) * 100)

num_niveis <- length(unique(df_uso_solo$uso_solo))
cores <- colorRampPalette(c("#FF8C00", "#e0a96d", "#d1b28d"))(num_niveis)

ggplot(df_uso_solo, aes(x = reorder(uso_solo, -percentage), y = percentage, fill = uso_solo)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5, color = "black") +
  scale_fill_manual(values = cores) +
  labs(title = "Uso do Solo", x = "Uso do Solo", y = "Porcentagem") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Distribuição do Número de Pessoas Envolvidas
df_pessoas <- df_corrigido %>%
  count(pessoas)
ggplot(df_pessoas, aes(x = pessoas, y = n)) +
  geom_bar(stat = "identity", fill = "gray", color = "black") +
  labs(title = "Distribuição do Número de Pessoas Envolvidas", x = "Número de Pessoas", y = "Contagem") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

#Acidentados

# Calcular a média da variável 'pessoas'
media_pessoas <- mean(df_corrigido$pessoas, na.rm = TRUE)

# Contar as ocorrências de cada valor de 'pessoas' para cada variável categórica
df_feridos_graves <- df_corrigido %>%
  count(feridos_graves, pessoas) %>%
  mutate(percentage = n / sum(n) * 100,
         relacao_media = ifelse(pessoas > media_pessoas, "Acima da Média", "Abaixo da Média"))

df_ilesos <- df_corrigido %>%
  count(ilesos, pessoas) %>%
  mutate(percentage = n / sum(n) * 100,
         relacao_media = ifelse(pessoas > media_pessoas, "Acima da Média", "Abaixo da Média"))

df_ignorados <- df_corrigido %>%
  count(ignorados, pessoas) %>%
  mutate(percentage = n / sum(n) * 100,
         relacao_media = ifelse(pessoas > media_pessoas, "Acima da Média", "Abaixo da Média"))

df_feridos <- df_corrigido %>%
  count(feridos, pessoas) %>%
  mutate(percentage = n / sum(n) * 100,
         relacao_media = ifelse(pessoas > media_pessoas, "Acima da Média", "Abaixo da Média"))

# Combinar os data frames em um único
df_combined <- bind_rows(
  df_feridos_graves %>% mutate(variable = "Feridos Graves"),
  df_ilesos %>% mutate(variable = "Ilesos"),
  df_ignorados %>% mutate(variable = "Ignorados"),
  df_feridos %>% mutate(variable = "Feridos")
)

# Definir rótulos personalizados para cada faceta
rotulos_facetas <- c(
  "Feridos Graves" = "Gravemente Feridos",
  "Ilesos" = "Sem Ferimentos",
  "Ignorados" = "Dados Ignorados",
  "Feridos" = "Feridos"
)

# Gerar gráfico
ggplot(df_combined, aes(x = as.factor(pessoas), y = percentage, fill = relacao_media)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~variable, scales = "free_y", labeller = labeller(variable = rotulos_facetas)) +
  labs(title = "Distribuição das Variáveis em Relação à Média de Pessoas",
       x = "Número de Pessoas",
       y = "Porcentagem",
       fill = "Relação com a Média") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none",
        strip.text = element_text(face = "bold", size = 12))

# Quantidade de veículos em acidentes
df_corrigido %>%
  count(veiculos) %>%
  filter(!is.na(veiculos)) %>%
  ggplot(aes(x = as.factor(veiculos), y = n)) +
  geom_col(fill = "#FF8C00", color = "black") +
  labs(title = "Quantidade de Veículos Envolvidos nos Acidentes", x = "Número de Veículos", y = "Quantidade de Acidentes") +
  theme_minimal()


