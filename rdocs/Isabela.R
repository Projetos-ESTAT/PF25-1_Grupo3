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



#Análise 1: Avaliar a evolução dos grupos em situação de pobreza
## variáveis: indigenas_pobreza x quilombolas_pobreza x ciganos_pobreza
mr1= indices_pobreza_consolidado %>%
  print_quadro_resumo(var_name = indigenas_pobreza)
mr2=indices_pobreza_consolidado %>%
  print_quadro_resumo(var_name = quilombolas_pobreza)
mr3= indices_pobreza_consolidado %>%
  print_quadro_resumo(var_name = ciganos_pobreza)

dados_ano= indices_pobreza_consolidado %>%
  mutate(ano= substr(referencia, 4, 7)) %>%
  group_by(ano) %>%
  summarise(indigenas_pobreza= sum(indigenas_pobreza, na.rm = TRUE),
            quilombolas_pobreza= sum(quilombolas_pobreza, na.rm = TRUE),
            ciganos_pobreza= sum(ciganos_pobreza, na.rm = TRUE))

dados_longos <- dados_ano %>%
  pivot_longer(cols = c(indigenas_pobreza, quilombolas_pobreza, ciganos_pobreza),
               names_to = "Grupos",
               values_to = "quantidade") %>%
  mutate(Grupos = recode(Grupos,
                         "indigenas_pobreza" = "Indígenas",
                         "quilombolas_pobreza" = "Quilombolas",
                         "ciganos_pobreza" = "Ciganos"))

ggplot(dados_longos) +
  aes(x = ano, y = quantidade, group = Grupos, colour = Grupos) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Ano", 
       y = "Quantidade de indivíduos", 
       title = "Evolução anual da pobreza por grupo") +
  theme_estat() 
