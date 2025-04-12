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

## Análise 1.1 entre pobreza e extrema pobreza
ggplot(dados_pobreza, aes(x = data)) +
  geom_line(aes(y = porcentagem_pobreza, color = "Pobreza"), size = 1.2) +
  geom_line(aes(y = porcentagem_extrema_pobreza, color = "Extrema Pobreza"), size = 1.2) +
  scale_color_manual(values = c("Pobreza" = "#008091", "Extrema Pobreza" = "#663333")) +
  labs(title = "Taxas de Pobreza e Extrema Pobreza no Brasil (2012-2022)",
       subtitle = "Como porcentagem da população total",
       x = "Ano",
       y = "Porcentagem da população",
       color = "Indicador") +
  theme_estat() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = percent_format(accuracy = 1))

## Análise 1.2 análise envolvendo a situação de Vulnerabilidade

ggplot(dados_pobreza, aes(x = data)) +
  geom_col(aes(y = porcentagem_vulnerabilidade, fill = "Vulnerabilidade"), 
           width = 20) +
  geom_col(aes(y = porcentagem_extrema_pobreza, fill = "Extrema Pobreza"), 
           width = 20) +
  geom_col(aes(y = porcentagem_pobreza, fill = "Pobreza"), 
           width = 20) +
  scale_fill_manual(values = c("Pobreza" = "#003366", 
                               "Extrema Pobreza" = "#A11D21", 
                               "Vulnerabilidade" = "#CC9900"),
                    breaks = c("Pobreza", "Extrema Pobreza", "Vulnerabilidade")) +
  labs(title = "Composição da Pobreza e Vulnerabilidade no Brasil",
       subtitle = "Como porcentagem da população total",
       x = "Ano",
       y = "Porcentagem da população",
       fill = "Indicador") +
  theme_estat() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = percent_format(accuracy = 1))

## Análise 1.3 comparação anual da pobreza

dados_anual <- dados_pobreza %>%
  group_by(ano) %>%
  summarise(
    media_pobreza = mean(porcentagem_pobreza),
    media_extrema_pobreza = mean(porcentagem_extrema_pobreza),
    media_vulnerabilidade = mean(porcentagem_vulnerabilidade)
  ) %>%
  pivot_longer(cols = -ano, names_to = "indicador", values_to = "valor")


ggplot(dados_anual, aes(x = factor(ano), y = valor, fill = indicador)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("media_pobreza" = "#008091", 
                               "media_extrema_pobreza" = "#CC9900",
                               "media_vulnerabilidade" = "#663333"),
                    labels = c("Pobreza", "Extrema Pobreza", "Vulnerabilidade")) +
  labs(title = "Média Anual das Taxas de Pobreza, Extrema Pobreza e Vulnerabilidade",
       subtitle = "Como porcentagem da população total",
       x = "Ano",
       y = "Porcentagem da população",
       fill = "Indicador") +
  theme_estat() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1))

# Análise 2
##  Tipos de pobreza nas famílias
dados_familias <- dados_pobreza %>%
  mutate(
    data = ymd(paste0(periodo, "01")),
    total_familias = familias_pobreza + familias_extrema_pobreza + familias_vulnerabilidade,
    prop_pobreza = familias_pobreza / total_familias,
    prop_extrema = familias_extrema_pobreza / total_familias,
    prop_vulneravel = familias_vulnerabilidade / total_familias
  ) %>%
  select(data, periodo, starts_with("familias_"), total_familias, starts_with("prop_")) %>%
  pivot_longer(cols = starts_with("prop_"), 
               names_to = "tipo_pobreza", 
               values_to = "proporcao") %>%
  mutate(tipo_pobreza = case_when(
    tipo_pobreza == "prop_pobreza" ~ "Pobreza",
    tipo_pobreza == "prop_extrema" ~ "Extrema Pobreza",
    tipo_pobreza == "prop_vulneravel" ~ "Vulnerabilidade"
  ))


##Análise 2.1 evolução temporal

ggplot(dados_familias, aes(x = data, y = proporcao, fill = tipo_pobreza)) +
  geom_area(alpha = 0.8) +
  scale_fill_manual(values = c("Pobreza" = "#041835", 
                               "Extrema Pobreza" = "#663333",
                               "Vulnerabilidade" = "#CC9900")) +
  labs(title = "Distribuição dos Tipos de Pobreza nas Famílias Brasileiras (2012-2022)",
       subtitle = "Evolução temporal das proporções",
       x = "Ano",
       y = "Proporção de famílias",
       fill = "Tipo de Pobreza") +
  theme_estat() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = percent_format(accuracy = 1))

# Análise 3
## Acompanhando se o grupo indígena segue a tendência geral

dados_comparativos <- dados_pobreza %>%
  mutate(
    data = ymd(paste0(periodo, "01")),
    
    taxa_geral_pobreza = pobreza / populacao_estimada,
    taxa_indigena_pobreza = indigenas_pobreza / (indigenas_pobreza + indigenas_extrema_pobreza + indigenas_vulnerabilidade),
    
    diferenca_absoluta = taxa_indigena_pobreza - taxa_geral_pobreza,
    
    razao_taxas = taxa_indigena_pobreza / taxa_geral_pobreza
  )

##Análise 3.1 Evolução comparativa das taxas

ggplot(dados_comparativos, aes(x = data)) +
  geom_line(aes(y = taxa_geral_pobreza, color = "População Geral"), linewidth = 1.2) +
  geom_line(aes(y = taxa_indigena_pobreza, color = "População Indígena"), linewidth = 1.2) +
  scale_color_manual(values = c("População Geral" = "#A11D21", "População Indígena" = "#003366")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
  labs(title = "Comparação: Taxas de Pobreza Geral vs. Indígena (2012-2022)",
       subtitle = "Proporção da população em situação de pobreza",
       x = "Ano",
       y = "Taxa de Pobreza",
       color = "Grupo") +
  theme_estat() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")


##Análise 3.2 correlação entre as tendências

correlacao <- cor.test(dados_comparativos$taxa_geral_pobreza, 
                       dados_comparativos$taxa_indigena_pobreza,
                       method = "pearson")

tibble(
  `Correlação` = round(correlacao$estimate, 3),
  `Valor-p` = ifelse(correlacao$p.value < 0.001, "< 0.001", round(correlacao$p.value, 3))
)





Gráfico1 <- ggplot(dados_pobreza, aes(x = data)) +
  geom_line(aes(y = porcentagem_pobreza, color = "Pobreza"), size = 1.2) +
  geom_line(aes(y = porcentagem_extrema_pobreza, color = "Extrema Pobreza"), size = 1.2) +
  scale_color_manual(values = c("Pobreza" = "#008091", "Extrema Pobreza" = "#663333")) +
  labs(title = "Taxas de Pobreza e Extrema Pobreza no Brasil (2012-2022)",
       subtitle = "Como porcentagem da população total",
       x = "Ano",
       y = "Porcentagem da população",
       color = "Indicador") +
  theme_estat() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = percent_format(accuracy = 1))



#análise 1 

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
