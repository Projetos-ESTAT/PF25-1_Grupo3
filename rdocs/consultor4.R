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


library(ggplot2)
library(scales)

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
