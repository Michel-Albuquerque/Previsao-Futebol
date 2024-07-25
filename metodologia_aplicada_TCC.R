# Exemplo da metodologia utilizada no trabalho
# Abaixo o script com análises da variáveis, modificações na base e aplicações
# dos modelos para a previsão dos jogos.

# A base utilizada como referência utiliza média 4 para as partidas anteriores,
# que foi que obteve os melhores resultados





# bibliotecas
library(tidyverse)
library(caret)

library(ISLR)
library(kernlab)
library(epiDisplay)
library(mlr)

setwd("C:/Users/Michel/Documents/Michel/ESTATISTICA/TCC")
# write.csv2(dados,"dados_modelo20172018media4_virgula.csv",dec = ",",row.names = FALSE)


# Importando base

dados = read_csv("dados_modelo20172018media4.csv")

# estrutura dos dados
str(dados)






##### Análise Descritiva

# Variável alvo para fator
dados$resultado = factor(dados$resultado)

# resumo dos dados
summary(dados)
dim(dados)


# variável alvo
tab1(dados$resultado)

# Criando o objeto com as proporções
dados_alvo <- dados |>
  count(resultado) |>
  arrange(desc(resultado)) |>
  mutate(prop = round(n*100/sum(n), 1),
         rot.ypos = cumsum(prop) - 0.5*prop)

valores = c("Visitante","Mandante","Empate")
barra_alvo =
  ggplot(data = dados_alvo,mapping = (aes(fill= valores,
                                          x = reorder(resultado,-n),
                                          y = n)))+
  geom_bar(stat = "identity")+
  labs(title = "Gráfico da variável Resultado",
       subtitle = "Quantidade de valores para cada classe",
       x = "Resultado",
       y = "Frequência")+
  geom_text(mapping = aes(label = n),
            vjust=-0.5)+
  theme_classic()+
  #scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))
  scale_fill_manual("legend", values = c("Empate" = "#999999", "Mandante" = "#E69F00",
                                         "Visitante" = "#56B4E9"))+
  guides(fill="none")



# Adicionando os rótulos que irão aparecer no gráfico
dados_alvo$rotulo <- paste0(dados_alvo$resultado, "\n",
                            round(dados_alvo$prop), "%")


# Criando um gráfico de setores com rótulos
pizza_alvo = ggplot(data = dados_alvo, 
                    mapping = aes(x = "",
                                  y = prop, 
                                  fill = resultado)) +
  geom_bar(width = 1, 
           stat = "identity", 
           color = "black") +
  geom_text(mapping = aes(y = rot.ypos,
                          label = rotulo),
            color = "black") +
  coord_polar("y", 
              start = 0, 
              direction = -1) +
  theme_void() +
  theme(legend.position = "FALSE") +
  labs(title = "Porcentagem das Classes")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))


library(gridExtra)

grafico_alvo = grid.arrange(ncol = 2,
                            barra_alvo,
                            pizza_alvo)















#View(dados)

## Obtendo uma primeira imagem das variáveis mais importantes
# segundo o modelo randomForest
library(randomForest)



# Importancia com permutacao (mais confiÃ¡vel)
rf <- randomForest(resultado~.,data = dados, importance=T,)
#imp <- importance(rf, type=1, scale = F) # permutation importances

varImpPlot(rf,type = 1,scale = F)










# AnÃ¡lise ExploratÃ³ria dos dados

nomes_var = colnames(dados)
nomes_var[2:length(nomes_var)]


# funcao para obter o Boxplot das variáveis preditoras x Resultado
box_plot1 <- function(data, x, y) {
  ggplot(data, aes({{x}}, {{y}})) +
    geom_boxplot()
}
box_plot1(dados,resultado,Home_Score)


library(gridExtra)

# -c(Home_Score,Home_Gls,Home_Ast,Home_PK,Home_PKatt,Home_Sh,
# Home_CrdY,Home_CrdR,Home_Tkl,Home_Int_x,Home_Blocks,Home_Succ_Take_Ons,
# Away_Gls,Away_CrdY,Away_CrdR,Away_Int_x)

# [1] "Home_Score":
box_plot1(dados,resultado,Home_Score) # pouco relevante

box_plot1(dados,resultado,Away_Score)



# "Away_Score"
box_plot1(dados,resultado,Away_Score) # alguma relevancia em relacao aos visitantes

# [3] "Home_Gls"
box_plot1(dados,resultado,Home_Gls)   # Repetido

# "Home_Ast"
box_plot1(dados,resultado,Home_Ast)   # pouco relevante
box_plot1(dados,resultado,Away_Ast)


# [5] "Home_PK"       Penaltis para o Mandante
box_plot1(dados,resultado,Home_PK)    # testar
box_plot1(dados,resultado,Away_PK)


# "Home_PKatt"        Penaltis convertidos para o mandante
box_plot1(dados,resultado,Home_PKatt) # sem relevancia
box_plot1(dados,resultado,Away_PKatt)


# [7] "Home_Sh"       Numero de chutes
box_plot1(dados,resultado,Home_Sh)    # pouco relevante
box_plot1(dados,resultado,Away_Sh)


# "Home_SoT"          Numero de chutes no gol
box_plot1(dados,resultado,Home_SoT)   # pouco relevante
box_plot1(dados,resultado,Away_SoT)


# [9] "Home_CrdY"     Cartoes amarelos
box_plot1(dados,resultado,Home_CrdY)  # sem relevancia
box_plot1(dados,resultado,Away_CrdY)  # sem relevancia


#### BoxPlot das variáveis

# "Home_CrdR"         Cartoes vermelhos
box_plot1(dados,resultado,Home_CrdR)  # sem relevancia
box_plot1(dados,resultado,Away_CrdR)
# [11] "Home_Touches"

# "Home_Tkl"          Divididas
box_plot1(dados,resultado,Home_Tkl)  # sem relevancia
box_plot1(dados,resultado,Away_Tkl)

#  "Home_Int_x"   Interceptacoes
box_plot1(dados,resultado,Home_Int_x) # sem relevancia
box_plot1(dados,resultado,Away_Int_x)

# "Home_Blocks"       Bloqueios
box_plot1(dados,resultado,Home_Blocks) # sem relevancia
box_plot1(dados,resultado,Away_Blocks)

#  "Home_SCA_SCA"
box_plot1(dados,resultado,Home_SCA_SCA) # pouca relevancia
box_plot1(dados,resultado,Away_SCA_SCA)

# "Home_GCA_SCA"
box_plot1(dados,resultado,Home_GCA_SCA) # pouca relevancia
box_plot1(dados,resultado,Away_GCA_SCA)

#  "Home_Cmp_Passes"      Passes completos
box_plot1(dados,resultado,Home_Cmp_Passes) # pouca relevancia

# "Home_Att_Passes"           Passes
box_plot1(dados,resultado,Home_Att_Passes) # Pouca relevancia
box_plot1(dados,resultado,Away_Att_Passes)

#  "Home_Cmp_percent_Passes"
box_plot1(dados,resultado,Home_Cmp_percent_Passes) # Pouca relevancia
box_plot1(dados,resultado,Away_Cmp_percent_Passes)

# "Home_PrgP_Passes"        Passes para frente
box_plot1(dados,resultado,Home_PrgP_Passes) # pouca relevancia
box_plot1(dados,resultado,Away_PrgP_Passes)

#  "Home_Carries_Carries"    Bolas carregadas
box_plot1(dados,resultado,Home_Carries_Carries) # Pouca relevancia
box_plot1(dados,resultado,Away_Carries_Carries)

# "Home_PrgC_Carries"            Bolas carregadas para frente
box_plot1(dados,resultado,Home_PrgC_Carries) # Pouca Relevancia
box_plot1(dados,resultado,Away_PrgC_Carries)

#  "Home_Att_Take_Ons"       Tentativas de drible
box_plot1(dados,resultado,Home_Att_Take_Ons) # Pouca Relevancia
box_plot1(dados,resultado,Away_Att_Take_Ons)

# "Home_Succ_Take_Ons"           dribles efetuados com sucesso
box_plot1(dados,resultado,Home_Succ_Take_Ons) # Sem relevancia
box_plot1(dados,resultado,Away_Succ_Take_Ons)

#  "Away_Gls"
box_plot1(dados,resultado,Away_Score)         # relevante
box_plot1(dados,resultado,Away_Gls)           # repetido

# "Away_Ast"

#  "Away_PK"
box_plot1(dados,resultado,Away_Ast)        # pouco relevante

# "Away_PKatt"
box_plot1(dados,resultado,Away_PKatt)     # testar

#  "Away_Sh"
box_plot1(dados,resultado,Away_Sh)        # pouco relevante

# "Away_SoT"
box_plot1(dados,resultado,Away_SoT)       # POUCO RELEVANTE

#  "Away_CrdY"
box_plot1(dados,resultado,Away_CrdY)     # IRRELEVANTE

# "Away_CrdR"
box_plot1(dados,resultado,Away_CrdR)    # iRRELEVANTE

#  "Away_Touches"
box_plot1(dados,resultado,Away_Touches) # pouco relevante
# "Away_Tkl"


#  "Away_Int_x"
box_plot1(dados,resultado,Away_Int_x)  # irrelevante


# "Away_Blocks"
box_plot1(dados,resultado,Away_Blocks) # irrelevante

#  "Away_SCA_SCA"
box_plot1(dados,resultado,Away_SCA_SCA) # pouco relevante

# "Away_GCA_SCA"
box_plot1(dados,resultado,Away_GCA_SCA) # pouco relevante


#  "Away_Cmp_Passes"
box_plot1(dados,resultado,Away_Cmp_Passes) # pouco relevante

# "Away_Att_Passes"
box_plot1(dados,resultado,Away_Att_Passes) # pouco relevante

#  "Away_Cmp_percent_Passes"
box_plot1(dados,resultado,Away_Cmp_percent_Passes) # irrelevante

# "Away_PrgP_Passes"
box_plot1(dados,resultado,Away_PrgP_Passes) # pouco relevante

#  "Away_Carries_Carries"
box_plot1(dados,resultado,Away_Carries_Carries) # pouco relevante

# "Away_PrgC_Carries"
box_plot1(dados,resultado,Away_PrgC_Carries) # pouco relevante

#  "Away_Att_Take_Ons"
box_plot1(dados,resultado,Away_Att_Take_Ons) # irrelevante

# "Away_Succ_Take_Ons"
box_plot1(dados,resultado,Away_Succ_Take_Ons) # irrelevante

#  "Home_Final_Third"
box_plot1(dados,resultado,Home_Final_Third) # pouco relevante

# "Away_Final_Third"
box_plot1(dados,resultado,Away_Final_Third) # pouco relevante

#  "Home_Touches_Touches"
box_plot1(dados,resultado,Home_Touches_Touches) # pouco relevante

# "Home_Def_Pen_Touches"
box_plot1(dados,resultado,Home_Def_Pen_Touches) # irrelevante
box_plot1(dados,resultado,Away_Def_Pen_Touches)

# "Home_Def_3rd_Touches"
box_plot1(dados,resultado,Home_Def_3rd_Touches) # irrelevante
box_plot1(dados,resultado,Away_Def_3rd_Touches)

# "Home_Mid_3rd_Touches"
box_plot1(dados,resultado,Home_Mid_3rd_Touches) # pouco relevante
box_plot1(dados,resultado,Away_Mid_3rd_Touches)

#  "Home_Att_3rd_Touches"
box_plot1(dados,resultado,Home_Att_3rd_Touches) # pouco relevante
box_plot1(dados,resultado,Away_Att_3rd_Touches)

# "Away_Touches_Touches"
box_plot1(dados,resultado,Away_Touches_Touches) # pouco relevante

# "Away_Def_Pen_Touches"
box_plot1(dados,resultado,Away_Def_Pen_Touches) # testar

# "Away_Def_3rd_Touches"
box_plot1(dados,resultado,Away_Def_3rd_Touches) # irrelevante

# "Away_Mid_3rd_Touches"
box_plot1(dados,resultado,Away_Mid_3rd_Touches) # relevante

# "Away_Att_3rd_Touches"
box_plot1(dados,resultado,Away_Att_3rd_Touches) # pouco relevante

# "Home_TklW_Tackles"
box_plot1(dados,resultado,Home_TklW_Tackles) # irrelevante

# "Home_Def_3rd_Tackles"
box_plot1(dados,resultado,Home_Def_3rd_Tackles) # irrelevante
box_plot1(dados,resultado,Away_Def_3rd_Tackles)
# [61] "Home_Mid_3rd_Tackles"
box_plot1(dados,resultado,Home_Mid_3rd_Tackles) # testar
box_plot1(dados,resultado,Away_Mid_3rd_Tackles)

# "Home_Att_3rd_Tackles"
box_plot1(dados,resultado,Home_Att_3rd_Tackles) # irrelevante
box_plot1(dados,resultado,Away_Att_3rd_Tackles)
# [63] "Home_Tkl_Challenges"
box_plot1(dados,resultado,Home_Tkl_Challenges) # irrelevante

# "Home_Att_Challenges"
box_plot1(dados,resultado,Home_Att_Challenges) # irrelevante

# [65] "Home_Tkl_percent_Challenges"
box_plot1(dados,resultado,Home_Tkl_percent_Challenges) # irrelevante

# "Home_Lost_Challenges"
box_plot1(dados,resultado,Home_Lost_Challenges) # irrelevante

# [67] "Home_Blocks_Blocks"
box_plot1(dados,resultado,Home_Blocks_Blocks) # irrelevante

# "Home_Sh_Blocks"
box_plot1(dados,resultado,Home_Sh_Blocks) # pouco relevante

# [69] "Home_Pass_Blocks"
box_plot1(dados,resultado,Home_Pass_Blocks) # irrelevante
# "Home_Int_y"
box_plot1(dados,resultado,Home_Int_y) # irrelevante

# [71] "Home_Tkl_Int"
box_plot1(dados,resultado,Home_Tkl_Int) # irrelevante

# "Home_Clr"
box_plot1(dados,resultado,Home_Clr) # pouco relevante

# [73] "Away_TklW_Tackles"
box_plot1(dados,resultado,Away_TklW_Tackles) # irrelevante


# "Away_Def_3rd_Tackles"
box_plot1(dados,resultado,Away_Def_3rd_Tackles) # irrelevante

# [75] "Away_Mid_3rd_Tackles"
box_plot1(dados,resultado,Away_Mid_3rd_Tackles) # irrelevante

# "Away_Att_3rd_Tackles"
box_plot1(dados,resultado,Away_Att_3rd_Tackles) # testar
# [77] "Away_Tkl_Challenges"
box_plot1(dados,resultado,Away_Tkl_Challenges) # testar

# "Away_Att_Challenges"
box_plot1(dados,resultado,Away_Att_Challenges) # irrelevante

# [79] "Away_Tkl_percent_Challenges"
box_plot1(dados,resultado,Away_Tkl_percent_Challenges) # testar

# "Away_Lost_Challenges"
box_plot1(dados,resultado,Away_Lost_Challenges) # testar

# [81] "Away_Blocks_Blocks"
box_plot1(dados,resultado,Away_Blocks_Blocks) # pouco relevante
box_plot1(dados,resultado,Home_Blocks_Blocks)
# "Away_Sh_Blocks"
box_plot1(dados,resultado,Away_Sh_Blocks) # pouco relevante
box_plot1(dados,resultado,Home_Sh_Blocks)
# [83] "Away_Pass_Blocks"
box_plot1(dados,resultado,Away_Pass_Blocks) # irrelevante

# "Away_Int_y"
box_plot1(dados,resultado,Away_Int_y) # irrelevante

# [85] "Away_Tkl_Int"
box_plot1(dados,resultado,Away_Tkl_Int)

# "Away_Clr"
box_plot1(dados,resultado,Away_Clr) # pouco relevante
box_plot1(dados,resultado,Home_Clr)
# [87] "Home_Recov"
box_plot1(dados,resultado,Home_Recov) # irrelevante

# "Away_Recov"
box_plot1(dados,resultado,Away_Recov) # irrelevante

# [89] "Away_Pontuacao_3"
box_plot1(dados,resultado,Home_Pontuacao_3)
box_plot1(dados,resultado,Away_Pontuacao_3) # relevante

# "Home_Pontuacao_3"
box_plot1(dados,resultado,Home_Pontuacao_3) # pouco relevante

# [91] "Home_Pontuacao_Geral"
box_plot1(dados,resultado,Home_Pontuacao_Geral) # pouco relevante

# "Away_Pontuacao_Geral"
box_plot1(dados,resultado,Away_Pontuacao_Geral) # pouco relevante

# [93] "Home_media_merc"
box_plot1(dados,resultado,Home_media_merc) # relevante


# "Home_valor_merc"
box_plot1(dados,resultado,Home_valor_merc) # relevante

# [95] "Away_media_merc"
box_plot1(dados,resultado,Away_media_merc) # relevante

# "Away_valor_merc"
box_plot1(dados,resultado,Away_valor_merc) # relevante





### Ressaltando alguns resultados importantes
# boa correlacao com resultado
a = ggplot(data = dados,mapping = aes(x = resultado,
                                      y = Home_media_merc))+
  geom_boxplot()+
  labs(title = "Boxplot",
       subtitle = "Resultado x Valor médio do time Mandante",
       x = "Resultado",
       y = "Valor (em milhões de euros)")


b = ggplot(data = dados,mapping = aes(x = resultado,
                                      y = Away_valor_merc))+
  geom_boxplot()+
  labs(title = "Boxplot",
       subtitle = "Resultado x Valor total do time Visitante",
       x = "Resultado",
       y = "Valor (em milhões de euros)")

c = ggplot(data = dados,mapping = aes(x = resultado,
                                      y = dados$Home_Cmp_Passes))+
  geom_boxplot()+
  labs(title = "Boxplot",
       subtitle = "Resultado x Passes Completos do Mandante",
       x = "Resultado",
       y = "Número de Passes Completos")

d = ggplot(data = dados,mapping = aes(x = resultado,
                                      y = dados$Home_SoT))+
  geom_boxplot()+
  labs(title = "Boxplot",
       subtitle = "Resultado x Finalizações no Alvo do Mandante",
       x = "Resultado",
       y = "Número de Finalizações no Alvo")

grid.arrange(ncol = 2,nrow = 2,a,b,c,d)


# ma correlacao com resultado

a = ggplot(data = dados,mapping = aes(x = resultado,
                                      y = Home_Pass_Blocks))+
  geom_boxplot()+
  labs(title = "Boxplot",
       subtitle = "Resultado x Bloqueios de Passe do Mandante",
       x = "Resultado",
       y = "Passes Bloqueados")


b = ggplot(data = dados,mapping = aes(x = resultado,
                                      y = dados$Away_CrdY))+
  geom_boxplot()+
  labs(title = "Boxplot",
       subtitle = "Resultado x Cartões amarelos do Visitante",
       x = "Resultado",
       y = "Cartões Amarelos")



grid.arrange(ncol = 2,a,b)
library(gridExtra)

# 
# # Excluindo variÃ¡veis repetidas
# Isso porque a base foi uma junção de 5 bases,
# onde em alguns casos algumas variáveis apareciam mais de uma vez




dados = dados |> 
  dplyr::select( -c(Home_Gls,Home_PK,Home_PKatt,Home_Sh,
                    Home_CrdY,Home_CrdR,Home_Tkl,Home_Int_x,Home_Blocks,Home_Succ_Take_Ons,
                    Away_Gls,Away_CrdY,Away_CrdR,Away_Int_x,Away_Blocks,Away_Cmp_percent_Passes,
                    Away_Att_Take_Ons,Away_Succ_Take_Ons,Home_Def_Pen_Touches,
                    Home_Def_3rd_Touches,Away_Def_3rd_Touches,Home_TklW_Tackles,
                    Home_Def_3rd_Tackles,Home_Att_3rd_Tackles,Home_Tkl_Challenges,
                    Home_Att_Challenges,Home_Tkl_percent_Challenges,Home_Lost_Challenges,
                    Home_Blocks_Blocks,Home_Pass_Blocks,Home_Int_y,Home_Tkl_Int,
                    Away_TklW_Tackles,Away_Def_3rd_Tackles,Away_Mid_3rd_Tackles,
                    Away_Att_Challenges,Away_Pass_Blocks,Away_Int_y,Away_Tkl_Int,
                    Home_Recov,Away_Recov,Home_Touches,
                    Away_Touches,Home_Int_x,Away_Int_x,
                    Home_Blocks_Blocks,Away_Blocks_Blocks
                    
  ))


####### teste rfe - seleção de variáveis


library("dplyr")
#install.packages("faux")
library("faux")
#install.packages("DataExplorer")
library("DataExplorer")
library("caret")
library("randomForest")



# Definindo o control para utilizar na função rfe
# Utilizando novamente o randomForest
control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 3, # number of repeats
                      number = 10) # number of folds


# Dividindo as bases em treino e teste
x <- dados %>%
  dplyr::select(-resultado) |> 
  as.data.frame()

# Target variable
y <- dados$resultado

# Training: 80%; Test: 20%
set.seed(2021)


x_train <- x[ 41:570, ]
x_test  <- x[571:760, ]

y_train <- y[ 41:570]
y_test  <- y[571:760]

dim(dados)
dim(x_train)
length(y_train)

# Run RFE
result_rfe1 <- rfe(x = x_train, 
                   y = y_train, 
                   sizes = c(20:70),
                   rfeControl = control)



# Print the results
result_rfe1

#row.names(varImp(result_rfe1))[1:10]

# Print the selected features
predictors(result_rfe1)

# Print the results visually
ggplot(data = result_rfe1, metric = "Accuracy") + theme_bw()+
  labs(x = "Número de Variáveis",
       y = "Acurácia",
       title = "Resultado do RFE")
ggplot(data = result_rfe1, metric = "Kappa") + theme_bw()

# OBS: as variáveis indicadas pelo RFE foram as que obtiveram os melhores
# resultados nas previsões







# Retirando as 4 rodadas iniciais dos dois campeonatos que formam a base,
# já que a aplicação da média das 4 partidas anteriores faz com que esses dados
# se tornem ruins para o modelo.


 
# # media 4 
dados40 = dados[41:380,]
dados41 = dados[421:760,]

dados45 = rbind(dados40,dados41)

dados = dados45
 dim(dados)










# Separando a base em treino e teste



dados_treino = dados[1:490,]        #4           # as 4 primeiras rodadas DE CADA CAMPEONATO devem ser descartadas por conta da media ser feita com valores anteriores
dados_teste = dados[491:680,]

dim(dados_treino)
dim(dados_teste)





# Nova análise dos dados
# Nesse momento é feita a escolha entre manter ou retirar a classe
# Empate da variável resultado.

# Portanto, caso desejado, retirar dos comentários as próximas linhas para obter uma
# base sem a classe empate



# dim(dados)
# 
# tab1(dados_treino$resultado,main = "Variável alvo nos dados de treino" )
# tab1(dados_teste$resultado,main="Variável alvo nos dados de teste")
# #View(dados_treino)
# 
# 
# # passando resultado para character pois factor segura as 
# # categorias originais
# dados_treino$resultado = as.character(dados_treino$resultado)
# dados_teste$resultado = as.character(dados_teste$resultado)
# 
# # retirando empates
# dados_treino = dados_treino |> filter(resultado %in% c("Mandante","Visitante"))
# dados_teste = dados_teste |> filter(resultado %in% c("Mandante","Visitante"))
# 
# # retornando para fator agora so com duas categorias
# dados_treino$resultado = factor(dados_treino$resultado)
# dados_teste$resultado = factor(dados_teste$resultado)
# 
# tab1(dados_treino$resultado,main = "Variável alvo nos dados de treino" )
# tab1(dados_teste$resultado,main="Variável alvo nos dados de teste")
# 
# 
# # Análise descritiva sem empates
# 
# 
# # Criando o objeto com as proporções
# dados_alvo <- dados |>
#   count(resultado) |>
#   arrange(desc(resultado)) |>
#   mutate(prop = round(n*100/sum(n), 1),
#          rot.ypos = cumsum(prop) - 0.5*prop)
# 
# valores = c("Visitante","Mandante","Empate")
# barra_alvo =
#   ggplot(data = dados_alvo,mapping = (aes(fill= valores,
#                                           x = reorder(resultado,-n),
#                                           y = n)))+
#   geom_bar(stat = "identity")+
#   labs(title = "Gráfico da variável Resultado",
#        subtitle = "Com Empate",
#        x = "Resultado",
#        y = "Frequência")+
#   geom_text(mapping = aes(label = n),
#             vjust=-0.5)+
#   theme_classic()+
#   #scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))
#   scale_fill_manual("legend", values = c("Empate" = "#999999", "Mandante" = "#E69F00",
#                                          "Visitante" = "#56B4E9"))+
#   guides(fill="none")
# 
# 
# dados_alvo1 <- dados1 |>
#   count(resultado) |>
#   arrange(desc(resultado)) |>
#   mutate(prop = round(n*100/sum(n), 1),
#          rot.ypos = cumsum(prop) - 0.5*prop)
# 
# valores1 = c("Visitante","Mandante")
# 
# c = ggplot(data = dados_alvo1,mapping = (aes(fill= valores1,
#                                              x = reorder(resultado,-n),
#                                              y = n)))+
#   geom_bar(stat = "identity")+
#   labs(title = "Gráfico da variável Resultado",
#        subtitle = "Sem Empate",
#        x = "Resultado",
#        y = "Frequência")+
#   geom_text(mapping = aes(label = n),
#             vjust=-0.5)+
#   theme_classic()+
#   #scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))
#   scale_fill_manual("legend", values = c( "Mandante" = "#E69F00",
#                                           "Visitante" = "#56B4E9"))+
#   guides(fill="none")
# 
# 
# 
# 
# dados1 = dados
# 
# 
# dados1$resultado = as.character(dados1$resultado)
# 
# 
# # retirando empates
# dados1 = dados1 |> filter(resultado %in% c("Mandante","Visitante"))
# 
# 
# # retornando para fator agora so com duas categorias
# dados1$resultado = factor(dados1$resultado)
# 
# 
# tab1(dados1$resultado,main = "Variável alvo nos dados de treino" )
# 
# 
# 
# library(gridExtra)
# grid.arrange(barra_alvo,c,ncol = 2)
# 
# 
# 
# # dimensoes finais
# dim(dados_treino)
# dim(dados_teste)
# 
# table(dados_treino$resultado)
# 






# padronizando os dados

padr = preProcess(dados_treino,
                  method = c("center","scale"))

dados2_treino = predict(padr,dados_treino)
#View(dados2_treino)


# padronizando os dados de teste
dados2_teste = predict(padr,dados_teste)
head(dados2_teste)


dim(dados2_teste)





# verificando a vari?ncia
dados2_treinot = dados2_treino |> 
  dplyr::select(-c(resultado))

nearZeroVar(dados2_treinot,saveMetrics = T)
# sem vari?veis com baixa vari?ncia



# correla??o
dados_treino_cor = dados2_treinot
correlacao = cor(dados_treino_cor)
#View(correlacao)
#View(correlacao)
#install.packages("corrgram")
library(corrgram)
library(corrplot)
corrplot(correlacao,method = "color")
corrgram(dados2_treinot)


# Encontrando as variáveis com alta correlacao entre sí
findCorrelation(correlacao,cutoff = 0.75,verbose = T,names = T)

highcor = findCorrelation(correlacao,cutoff = 0.75,names = T)
highcor

#names(correlacao[,28])

##### Não vao ser retiradas as variáveis correlacionadas pois os modelos apresentaram piora significativa quando feitos desse modo

# # eliminando vari?veis com alta correlac?o com outras
# dados2_treino = dados2_treino |> 
#   dplyr::select(-c(highcor))
# 
# dim(dados_treino)
# dim(dados2_treino)
# 
# # retirando vari?veis dos dados de teste
# dados2_teste = dados2_teste |> 
#   dplyr::select(-c(highcor))
# dim(dados2_teste)
# 
# 
# # Verificando se h? vari?veis com dependencia linear
# dados2_treinot = dados2_treino |> 
#   dplyr::select(-c(resultado))


findLinearCombos(dados2_treinot)

#?findLinearCombos
# sem vari?veis com dependencia linear


# verificando a proporcao das caracter?sticas da 
# vari?vel alvo


prop.table(table(dados2_treino$resultado))



# As linhas abaixo fazem o redimensionamento da base com 3 classes, já que há mais 
# observações para a classe mandante do que das outras.
# Porém, como os resultados dos modelos foram ruins tanto com 3 classes quanto
# com a base sem empates, então essa parte está colocada abaixo
# apenas como observação e como aprendizado, já que o redimensionamento
# com 3 classes não é tão simples.


# # balanceando a variÃ¡vel alvo
# 
# 
# dados1 = dados2_treino |> 
#   filter(resultado %in% c("2","1"))
# tab1(dados1$resultado)
# dados1$resultado = factor(dados1$resultado,
#                           levels = c(2,1))
# tab1(dados1$resultado)
# set.seed(100)
# 
# up_treino = upSample(x = dados1[,-1],
#                      y = dados1$resultado,yname = "resultado")
# 
# dados2 = dados2_treino |> 
#   filter(resultado %in% c("2","0"))
# tab1(dados2$resultado)
# dados2$resultado = factor(dados2$resultado,
#                           levels = c(2,0))
# tab1(dados2$resultado)
# set.seed(100)
# 
# up_treino2 = upSample(x = dados2[,-1],
#                       y = dados2$resultado,yname = "resultado")
# 
# tab1(up_treino2$resultado)
# up_treino2 = up_treino2 |> 
#   filter(resultado == "0")
# 
# dim(up_treino2)
# dado_total = rbind(up_treino,up_treino2)
# dim(dado_total)
# tab1(dado_total$resultado, main = "VariÃ¡vel alvo nos dados de treino depois")
# 
# 





##### Analisando vitórias de visitantes e empates




table(dados$resultado)








######################### Modelos ######################

# Fase de treinamento dos modelos com a base de treino, além do teste com
# a base de teste

# Modelos utilizados:  - Regressão Logística
#                      - SVM
#                      - Redes Neurais
#                      - XGBoost





######################## Regressão Logística
ctrl = trainControl(method = "repeatedcv",number = 10,
                    repeats = 3,
                    preProcOptions = list(thresh = 0.8))


grid_logistic1 = expand.grid(loss = "L1",
                             epsilon = c(0.001,0.01,0.1,1),
                             cost = seq(0.1,1.5,0.1))
set.seed(1000)
modelo_rl = caret::train(resultado~Away_valor_merc +            Away_media_merc  +           Home_valor_merc    +         Home_media_merc   +         
                           Home_Att_3rd_Touches   +     Home_Mid_3rd_Touches   +     Home_Cmp_percent_Passes   +  Away_Clr   +                
                           Away_Mid_3rd_Touches   +     Away_Tkl_percent_Challenges +Home_Carries_Carries     +   Home_SCA_SCA    +           
                           Home_SoT             +       Away_Cmp_Passes    +         Home_Cmp_Passes      +       Home_PrgC_Carries +         
                           Away_Touches_Touches   +     Away_SCA_SCA       +         Away_Att_Passes       +      Away_Carries_Carries  +     
                           Home_Touches_Touches    +    Away_Final_Third   +         Away_PrgP_Passes        +    Away_Sh_Blocks   +          
                           Away_Lost_Challenges    +    Home_PrgP_Passes      +      Away_Def_Pen_Touches    +    Away_GCA_SCA     +          
                           Home_Att_Take_Ons     +      Home_Score   +               Home_Clr    +                Home_Att_Passes   +         
                           Home_Sh_Blocks
                         ,
                         data = dados2_treino,
                         method = "regLogistic",
                         trControl = ctrl,
                         #preProcess = "pca",      # PCA foi testado, porém sem sucesso
                         tuneGrid = grid_logistic1
                         
)

# Melhores Hiperparâmetros
modelo_rl$bestTune

# Previsão
previsao = predict(modelo_rl,dados2_teste)





# Matriz de Confusão

confusionMatrix(data = previsao,
                reference = dados2_teste$resultado)






################################# SVM

set.seed(1000)
grid_svm = expand.grid(degree = 1,
                       scale = seq(0.008,0.012,0.001),
                       C = seq(0.1,1,0.1))

modelo_svm = caret::train(resultado~Away_valor_merc +            Away_media_merc  +           Home_valor_merc    +         Home_media_merc   +         
                            Home_Att_3rd_Touches   +     Home_Mid_3rd_Touches   +     Home_Cmp_percent_Passes   +  Away_Clr   +                
                            Away_Mid_3rd_Touches   +     Away_Tkl_percent_Challenges +Home_Carries_Carries     +   Home_SCA_SCA    +           
                            Home_SoT             +       Away_Cmp_Passes    +         Home_Cmp_Passes      +       Home_PrgC_Carries +         
                            Away_Touches_Touches   +     Away_SCA_SCA       +         Away_Att_Passes       +      Away_Carries_Carries  +     
                            Home_Touches_Touches    +    Away_Final_Third   +         Away_PrgP_Passes        +    Away_Sh_Blocks   +          
                            Away_Lost_Challenges    +    Home_PrgP_Passes      +      Away_Def_Pen_Touches    +    Away_GCA_SCA     +          
                            Home_Att_Take_Ons     +      Home_Score   +               Home_Clr    +                Home_Att_Passes   +         
                            Home_Sh_Blocks,
                          data = dados2_treino,
                          method = "svmPoly",
                          trControl = ctrl,
                          tuneGrid = grid_svm
)

# Resultados
modelo_svm$results

# Melhores Hiperparametros
modelo_svm$bestTune

#varImp(modelo_svm)
previsao = predict(modelo_svm,dados2_teste)



# Matriz de Confusão

confusionMatrix(data = previsao,
                reference = dados2_teste$resultado)





################# Redes Neurais

# Obtendo o número máximo de camadas
maxSize <- max(grid_avnnet$size)

# Obtendo o número máximo de parâmetros
numWts <- 1*(maxSize * (length(dados2_treino) + 1) + maxSize + 1)


set.seed(1000)
grid_nnet = expand.grid(size = 1,
                          decay = seq(0.04,0.50,0.02)
                          #bag = FALSE
)

modelo_nnet = caret::train(resultado~Away_valor_merc +            Away_media_merc  +           Home_valor_merc    +         Home_media_merc   +         
                            Home_Att_3rd_Touches   +     Home_Mid_3rd_Touches   +     Home_Cmp_percent_Passes   +  Away_Clr   +                
                            Away_Mid_3rd_Touches   +     Away_Tkl_percent_Challenges +Home_Carries_Carries     +   Home_SCA_SCA    +           
                            Home_SoT             +       Away_Cmp_Passes    +         Home_Cmp_Passes      +       Home_PrgC_Carries +         
                            Away_Touches_Touches   +     Away_SCA_SCA       +         Away_Att_Passes       +      Away_Carries_Carries  +     
                            Home_Touches_Touches    +    Away_Final_Third   +         Away_PrgP_Passes        +    Away_Sh_Blocks   +          
                            Away_Lost_Challenges    +    Home_PrgP_Passes      +      Away_Def_Pen_Touches    +    Away_GCA_SCA     +          
                            Home_Att_Take_Ons     +      Home_Score   +               Home_Clr    +                Home_Att_Passes   +         
                            Home_Sh_Blocks ,
                          data = dados2_treino,
                          method = "nnet",
                          trControl = ctrl,
                          #tuneLenght = 5,
                          tuneGrid = grid_nnet,
                          MaxNWts = numWts,
                          maxit = 1000,
                          trace = FALSE
                          
) 


# Gráfico
plot(modelo_nnet)

# Resultados
modelo_nnet$results

# Melhores hiperparametros
modelo_nnet$bestTune



previsao = predict(modelo_svm,dados2_teste)

#warnings()


# resultado

confusionMatrix(data = previsao,
                reference = dados2_teste$resultado)







############################ XGBoost



ctrl = trainControl(method = "repeatedcv",number = 10,
                    repeats = 3,
                    verboseIter = FALSE,
                    allowParallel = T)



gbmGrid =expand.grid(
  nrounds = 1000,
  eta = c(0.01, 0.1, 0.3),
  max_depth = c(2, 3, 5,6, 10),
  gamma = c(0,5),
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = c(0.5,1)
)
set.seed(1000)

# Modelo
gbmFit2 <- caret::train( resultado~ Away_valor_merc +            Away_media_merc  +           Home_valor_merc    +         Home_media_merc   +         
                           Home_Att_3rd_Touches   +     Home_Mid_3rd_Touches   +     Home_Cmp_percent_Passes   +  Away_Clr   +                
                           Away_Mid_3rd_Touches   +     Away_Tkl_percent_Challenges +Home_Carries_Carries     +   Home_SCA_SCA    +           
                           Home_SoT             +       Away_Cmp_Passes    +         Home_Cmp_Passes      +       Home_PrgC_Carries +         
                           Away_Touches_Touches   +     Away_SCA_SCA       +         Away_Att_Passes       +      Away_Carries_Carries  +     
                           Home_Touches_Touches    +    Away_Final_Third   +         Away_PrgP_Passes        +    Away_Sh_Blocks   +          
                           Away_Lost_Challenges    +    Home_PrgP_Passes      +      Away_Def_Pen_Touches    +    Away_GCA_SCA     +          
                           Home_Att_Take_Ons     +      Home_Score   +               Home_Clr    +                Home_Att_Passes   +         
                           Home_Sh_Blocks   
                         ,
                         data = dados2_treino,
                         method = "xgbTree",
                         trControl = ctrl,
                         #verboseIter = FALSE,
                         ## Now specify the exact models
                         ## to evaluate:
                         #allowParallel = TRUE
                         tuneGrid = gbmGrid
)

# Melhores hiperparametros
gbmFit2$bestTune

# Previsão
previsao = predict(gbmFit2,dados2_teste)


# Matriz de Confusão
confusionMatrix(data = previsao,
                reference = dados2_teste$resultado)







############### Conclusão

# Melhor modelo com 3 classes: Redes Neurais com 61,59% de acurácia
# Melhor modelo com 2 classes: Redes Neurais com 72,61% de acurácia









# Observação: exemplo para gerar as probabilidades geradas pelos
# modelos para cada classe da variável resultado

previsao_prob = data.frame(probs = predict(gbmFit2,
                                           dados2_teste,
                                           type = "prob"))
# Resultado real
previsao_prob$resultado = dados2_teste$resultado

# Previsao
previsao_prob$previsao = previsao

# Probabilidades para cada classe
previsao_prob$probs.Empate= round(previsao_prob$probs.Empate,digits = 4)
previsao_prob$probs.Mandante = round(previsao_prob$probs.Mandante,digits = 4)
previsao_prob$probs.Visitante = round(previsao_prob$probs.Visitante,digits = 4)
View(previsao_prob)







