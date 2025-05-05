source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #


# Análise 1
## Evolução dos índices de pobreza
##BANCOS

dados <- read_csv("indices_pobreza_consolidado.csv")
dados_pobreza <- dados %>%
  mutate(
    data = ymd(paste0(periodo, "01")),
    ano = year(data),
    mes = month(data),
    pobreza_milhoes = pobreza / 1000000,
    extrema_pobreza_milhoes = extrema_pobreza / 1000000,
    total_milhoes = total / 1000000
  ) %>%
  arrange(data)

dados_anual <- dados_pobreza %>%
  mutate(ano = year(ymd(paste0(periodo, "01")))) %>%
  group_by(ano) %>%
  summarise(
    pobreza = mean(porcentagem_pobreza, na.rm = TRUE),
    extrema_pobreza = mean(porcentagem_extrema_pobreza, na.rm = TRUE),
    vulnerabilidade = mean(porcentagem_vulnerabilidade, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = -ano, names_to = "tipo", values_to = "porcentagem")


b2<- dados_pobreza %>%
  mutate(
    data = ymd(paste0(periodo, "01")),  
    ano = year(data)                     
  ) %>%
  select(
    data,
    ano,
    familias_pobreza,
    familias_extrema_pobreza,
    familias_vulnerabilidade
  ) %>%
  arrange(data)

b2 <- dados_pobreza %>%
  mutate(
    data = ymd(paste0(periodo, "01")),
    ano = year(data),
    Pobres = populacao_estimada * (porcentagem_pobreza/100),
    `Extrema Pobreza` = populacao_estimada * (porcentagem_extrema_pobreza/100),
    Vulneráveis = populacao_estimada * (porcentagem_vulnerabilidade/100)
  ) %>%
  group_by(ano) %>%
  summarise(
    Pobres = mean(Pobres),
    `Extrema Pobreza` = mean(`Extrema Pobreza`),
    Vulneráveis = mean(Vulneráveis)
  ) %>%
  pivot_longer(cols = -ano, names_to = "Categoria", values_to = "Indivíduos")

b3 <- dados_pobreza %>%
  mutate(
    data = ymd(paste0(periodo, "01")),
    total_populacao = total,
    total_indigenas = indigenas_pobreza + indigenas_extrema_pobreza + indigenas_vulnerabilidade,
    proporcao_pobreza_geral = pobreza / total_populacao,
    proporcao_pobreza_indigena = indigenas_pobreza / total_indigenas,
    diferenca_absoluta = proporcao_pobreza_indigena - proporcao_pobreza_geral,
    diferenca_relativa = diferenca_absoluta / proporcao_pobreza_geral - 1
  ) %>%
  select(
    data,
    periodo,
    total_populacao,
    total_indigenas,
    pobreza,
    indigenas_pobreza,
    proporcao_pobreza_geral,
    proporcao_pobreza_indigena,
    diferenca_absoluta,
    diferenca_relativa
  ) %>%
  arrange(data)

b3_longo <- b3 %>%
  select(data, proporcao_pobreza_geral, proporcao_pobreza_indigena) %>%
  pivot_longer(cols = -data, 
               names_to = "tipo_pobreza", 
               values_to = "proporcao") %>%
  mutate(tipo_pobreza = case_when(
    tipo_pobreza == "proporcao_pobreza_geral" ~ "Geral",
    tipo_pobreza == "proporcao_pobreza_indigena" ~ "Indígena"
  ))


estatisticas_pobreza <- dados_pobreza %>%
  summarise(
    Média = mean(porcentagem_pobreza),
    Mediana = median(porcentagem_pobreza),
    Desvio_Padrão = sd(porcentagem_pobreza),
    Mínimo = min(porcentagem_pobreza),
    Máximo = max(porcentagem_pobreza),
    Amplitude = max(porcentagem_pobreza) - min(porcentagem_pobreza)
  )

print(estatisticas_pobreza)

library(tidyverse)
library(ggplot2)
library(scales)


#------------------------------------------


ggplot(dados_pobreza, aes(x = ymd(paste0(periodo, "01")), y = porcentagem_pobreza)) +
  geom_line(color = "#003366", linewidth = 1.2) +
  geom_smooth(method = "loess", color = "#FF6600") +
  labs(title = "Evolução da Taxa de Pobreza no Brasil (2012-2022)",
       subtitle = "Porcentagem da população em situação de pobreza",
       x = "Ano",
       y = "Taxa de Pobreza (%)") +
  scale_y_continuous(labels = percent_format(accuracy = 1), 
                     limits = c(0, NA)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_estat() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, face = "bold"))


#análise 1.2

ggplot(dados_pobreza, aes(x = mes, y = porcentagem_pobreza)) +
  geom_boxplot(fill = "#008091", alpha = 0.7) +
  labs(title = "Distribuição da Pobreza por Mês",
       x = "Mês",
       y = "Taxa de Pobreza (%)") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_estat()

ggplot(dados_pobreza, aes(x = ymd(paste0(periodo, "01")), y = porcentagem_extrema_pobreza)) +
  geom_line(color = "#A11D21", linewidth = 1.2) +  
  geom_smooth(method = "loess", color = "#666666", se = FALSE) +  
  labs(title = "EVOLUÇÃO DA EXTREMA POBREZA NO BRASIL (2012-2022)",
       subtitle = "Percentual da população em situação de extrema pobreza",
       x = "Ano",
       y = "Taxa de Extrema Pobreza (%)",
       caption = "Fonte: Banco de dados de indicadores sociais") +
  scale_y_continuous(labels = percent_format(accuracy = 1), 
                     limits = c(0, NA),
                     breaks = seq(0, 0.25, by = 0.05)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_estat()

                                          
#análise 1.3

ggplot(dados_pobreza, aes(x = ymd(paste0(periodo, "01")), y = porcentagem_vulnerabilidade)) +
  geom_line(color = "#008091", linewidth = 1.2) +
  geom_smooth(method = "loess", color = "#003366", se = FALSE, linewidth = 1.5) +
  labs(title = "EVOLUÇÃO DA VULNERABILIDADE SOCIAL NO BRASIL",
       subtitle = "Percentual da população em situação de vulnerabilidade",
       x = "Ano",
       y = "Taxa de Vulnerabilidade",
       caption = "Fonte: Banco de dados de indicadores sociais") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     limits = c(0, NA),
                     breaks = seq(0, 0.5, by = 0.05)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_estat()


#Análise 1.1 refeita no padrão

ggplot(dados_anual, aes(x = ano, y = porcentagem, color = tipo)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_colour_manual(name = "Indicador", labels = c("Extrema Pobreza", "Pobreza", "Vulnerabilidade")) +
  labs(x = "Ano", y = "Porcentagem da População") +
  theme_estat() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(2012, 2022, by = 1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))

#Análise 1.2 refeita no padrão

ggplot(dados_pobreza, aes(x = ymd(paste0(periodo, "01")), y = porcentagem_extrema_pobreza)) +
  geom_line(color = "#A11D21", linewidth = 1.2) +
  geom_point(colour = "#008091", size = 2) +
  labs(title = "Evolução da Taxa de Extrema Pobreza no Brasil",
       x = "Ano",
       y = "Taxa de Extrema Pobreza (%)") +
  theme_estat()

#Análise 1.3 refeita no padrão

ggplot(dados_pobreza, aes(x = ymd(paste0(periodo, "01")), y = porcentagem_vulnerabilidade)) +
  geom_line(color = "#008091", linewidth = 1.2) +
  geom_point(colour = "#663333", size = 2) +
  labs(title = "Evolução de Vulnerabilidade no Brasil",
       x = "Ano",
       y = "Taxa de Vulnerabilidade (%)") +
  theme_estat()


#Análise 2.1 Distribuição do número de famílias em situação de pobreza

ggplot(dados_pobreza, aes(x = ymd(paste0(periodo, "01")), y = familias_pobreza)) +
  geom_line(color = "#008091", linewidth = 1) +
  geom_point(colour = "#A11D21", size = 2) +
  labs(title = "Evolução do número de famílias em pobreza",
       x = "Ano",
       y = "Número de famílias") +
  theme_estat()


#Análise 2.2 Distribuição do número de famílias em situação de estrema pobreza

ggplot(dados_pobreza, aes(x = ymd(paste0(periodo, "01")), y = familias_extrema_pobreza)) +
  geom_line(color = "#003366", linewidth = 1) +
  geom_point(colour = "#FF6600", size = 2) +
  labs(title = "Evolução do número de famílias em extrema pobreza",
       x = "Ano",
       y = "Número de famílias") +
  theme_estat()


#Análise 2.3 Distribuição do número de famílias em situação de vulnerabilidade

ggplot(dados_pobreza, aes(x = ymd(paste0(periodo, "01")), y = familias_vulnerabilidade)) +
  geom_line(color = "#041835", linewidth = 1) +
  geom_point(colour = "#A11D21", size = 2) +
  labs(title = "Evolução do número de famílias em vulnerabilidade",
       x = "Ano",
       y = "Número de famílias") +
  theme_estat()


#Análise 3

ggplot(dados_pobreza, aes(x = data, y = indigenas_pobreza)) +
  geom_line(color = "#041835", linewidth = 1) +
  geom_point(colour = "#A11D21", size = 2) +
  labs(title = "Evolução do número de indígenas em situação de pobreza",
       x = "Ano",
       y = "Número de famílias") +
  theme_estat()



##Correção Final

##Análise 1 

ggplot(dados_anual, aes(x = ano, y = porcentagem, color = tipo)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_colour_manual(name = "Indicador", labels = c("Extrema Pobreza", "Pobreza", "Vulnerabilidade")) +
  labs(x = "Ano", y = "Porcentagem da População") +
  theme_estat() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(2012, 2022, by = 1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))

##Análise 2

ggplot(b2, aes(x = factor(ano), y = Indivíduos/1e6, fill = Categoria)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(name = "Famílias") +
  labs(x = "Ano", y = "Número de Indivíduos (milhões)") +
  theme_estat() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = label_number(suffix = "M")) +
  geom_text(aes(label = ifelse(Indivíduos/1e6 > 10, round(Indivíduos/1e6, 1), "")),
            position = position_stack(vjust = 0.5),
            size = 3, color = "#666666", fontface = "bold")

##Quadro Análise 2 ##por algum motivo a tabela não roda no código, ele diz apresentar erro 

#Análise 3

ggplot(b3, aes(x = proporcao_pobreza_geral, y = proporcao_pobreza_indigena)) +
  geom_jitter(colour = "#A11D21", size = 3) +
  labs(
    x = "Pobreza na população geral (proporção)",
    y = "Pobreza entre indígenas"
  ) +
  geom_smooth(method = "lm", se = FALSE, COLOR = "blue")+
  theme_estat()