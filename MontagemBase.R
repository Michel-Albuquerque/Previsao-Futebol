# Projeto TCC:  previsÃ£o no futebol

# Titulo: Aprendizado de MÃ¡quinas na previsÃ£o de resultados de jogos de futebol

# Resumo: Diante da facilidade atual em se obter dados de eventos esportivos e ao
# sucesso de casas de apostas esportivas no brasil, o objetivo deste trabalho 
# Ã© encontrar um modelo que seja capaz de prever os resultados de partidas de futebol.


# A ideia do trabalho é prever os resultados dos jogos do campeonato inglês de futebol, utilizando
# estatísticas de cada uma das partidas dos campeonatos,
# tentando verificar qual quantidade de dados ideal para se realizar a previsão (se utilizar
# 1, 2, 3 ou mais campeonatos), verificar quais as variáveis mais relevantes e depois obter
# o modelo que melhor faça as previsões.





# O seguinte script apresenta um exemplo do que foi utilizado para obter as bases
# O resultado será uma base utilizando para cada uma observação, para cada uma das
# variáveis, a média das últimas 3 partidas.



#importando bibliotecas

library(tidyverse)
library(worldfootballR) # pacote para a retirada dos dados dos jogos

ls("package:worldfootballR")

# Obtendo os endereços das páginas dos jogos onde estão as informações de cada um dos jogos
# do campeonato ingles 2018/2019
# Site: Fbref
a = fb_match_urls(country = "ENG", gender = "M", season_end_year = "2019", tier = "1st")
a

# Resumo
fb_match_summary(match_url = a)

# Exemplo de aplicação
b = fb_advanced_match_stats("https://fbref.com/en/matches/b1b5e590/Sheffield-United-Aston-Villa-December-14-2019-Premier-League",
                            team_or_player = "team",
                            stat_type = c("passing_types"))


# Retirada dos dados
# Contém informações para ambos os times dos confrontos,
# através de 5 bases com estatísticas específicas das partidas

resumo18 = fb_advanced_match_stats(a,
                                   team_or_player = "team",
                                   stat_type = c("summary"))
#write.csv(resumo18, "resumo18.csv", row.names=FALSE)

passes18 = fb_advanced_match_stats(a,
                                   team_or_player = "team",
                                   stat_type = c("passing"))
#write.csv(passes18, "passes18.csv", row.names=FALSE)

defesa18 = fb_advanced_match_stats(a,
                                   team_or_player = "team",
                                   stat_type = c("defense"))
#write.csv(defesa18, "defesa18.csv", row.names=FALSE)

posse18 = fb_advanced_match_stats(a,
                                  team_or_player = "team",
                                  stat_type = c("possession"))
#write.csv(posse18, "posse18.csv", row.names=FALSE)

misc18 = fb_advanced_match_stats(a,
                                 team_or_player = "team",
                                 stat_type = c("misc"))
#write.csv(misc18, "misc18.csv",row.names = FALSE)

#goleiro17 = fb_advanced_match_stats(a,
# team_or_player = "team",
# stat_type = c("keeper"))                    ############## Os dados dos goleiros não foram utilizados
                                                          # pois estavam incompletos
#write.csv(goleiro17, "goleiro17.csv", row.names=FALSE)




library(tidyverse)




resumo17 = read_csv("resumo18.csv")


#write.csv(passes17, "passes17.csv", row.names=FALSE)
passes17 = read_csv("passes18.csv")

#write.csv(defesa17, "defesa17.csv", row.names=FALSE)
defesa17 = read_csv("defesa18.csv")

#write.csv(posse17, "posse17.csv", row.names=FALSE)
posse17 = read_csv("posse18.csv")


#write(misc17, "misc17.csv",row.names = FALSE)
misc17= read_csv("misc18.csv")


#write.csv(goleiro17, "goleiro17.csv", row.names=FALSE)
#goleiro17 = read_csv("goleiro17.csv")



#View(resumo17)





# Realizando modificações nas bases, tentando deixa-las com um mesmo padrão,
# como por exemplo alterar os nomes de variáveis,
# além de excluir variáveis sem importância para o trabalho

# Separando os dados em 3 bases: - Padrao: dados gerais do confronto
                               # - Home: dados do time mandante
                               # - Away: dados do time visitante
resumo17_padrao = resumo17 |> 
  dplyr::select(Home_Team:Away_Red_Cards)

resumo17_home = resumo17 |> 
  dplyr::select(Home_Team,Away_Team,Team:Succ_Take_Ons) |> 
  dplyr::filter(Home_Away == "Home")


resumo17_away = resumo17 |> 
  dplyr::select(Home_Team,Away_Team,Team:Succ_Take_Ons) |> 
  filter(Home_Away == "Away")

head(resumo17_home)
head(resumo17_away)


resumo17_home = resumo17_home |> 
  rename_with( ~ paste("Home", .x, sep = "_"))

colnames(resumo17_home)
head(resumo17_home)

resumo17_away = resumo17_away |> 
  rename_with( ~ paste("Away", .x, sep = "_"))

colnames(resumo17_away)
head(resumo17_away)

# Renomeando variÃ¡veis

resumo17_away = resumo17_away |> 
  rename("Home_Team"="Away_Home_Team"  ,
         "Away_Team"="Away_Away_Team" ,
         "Team"="Away_Team",
         "Home_Away"="Away_Home_Away")

head(resumo17_away)

resumo17_home = resumo17_home |> 
  rename("Home_Team"="Home_Home_Team"  ,
         "Away_Team"="Home_Away_Team" ,
         "Team"="Home_Team",
         "Home_Away"="Home_Home_Away")

head(resumo17_home)





head(resumo17_away)

dim(resumo17_away)
dim(resumo17_home)
dim(resumo17_padrao)



colnames(resumo17_away)
colnames(resumo17_home)
colnames(resumo17_padrao)

# Excluindo variÃ¡vel Team
resumo17_away = resumo17_away |> 
  dplyr::select(-c(Team,Home_Away,Away_npxG_Expected,Away_xAG_Expected,Away_xG_Expected,Away_Min))

resumo17_home = resumo17_home |> 
  select(-c(Team,Home_Away,Home_npxG_Expected,Home_xAG_Expected,Home_xG_Expected,Home_Min))


resumo17_padrao = resumo17_padrao |> 
  select(c(Home_Team,Away_Team,Home_Score,Away_Score))

dim(resumo17_padrao)

# removendo linhas duplicadas
resumo17_padrao = distinct(resumo17_padrao)
dim(resumo17_padrao)

head(resumo17_padrao)



resumo2 = inner_join(x = resumo17_home,y = resumo17_away,
                     by = c("Home_Team","Away_Team"))

# verificando na
sum(is.na(resumo2))

View(resumo2)


resumo17_final = inner_join(x = resumo17_padrao,y = resumo2,
                            by = c("Home_Team","Away_Team"))

dim(resumo17_final)
sum(is.na(resumo17_final))



##### Passes
passes17_padrao = passes17 |> 
  select(Home_Team:Away_Red_Cards)



passes17_home = passes17 |> 
  select(Home_Team,Away_Team,Team,Home_Away,Final_Third) |> 
  filter(Home_Away == "Home")

dim(passes17_home)
passes17_away = passes17 |> 
  select(Home_Team,Away_Team,Team,Home_Away,Final_Third) |> 
  filter(Home_Away == "Away")
dim(passes17_away)

head(passes17_home)
head(passes17_away)


passes17_home = passes17_home |> 
  rename_with( ~ paste("Home", .x, sep = "_"))

colnames(passes17_home)
head(passes17_home)

passes17_away = passes17_away |> 
  rename_with( ~ paste("Away", .x, sep = "_"))

colnames(passes17_away)
head(passes17_away)

# Renomeando variÃ¡veis

passes17_away = passes17_away |> 
  rename("Home_Team"="Away_Home_Team"  ,
         "Away_Team"="Away_Away_Team" ,
         "Team"="Away_Team",
         "Home_Away"="Away_Home_Away")

head(passes17_away)

passes17_home = passes17_home |> 
  rename("Home_Team"="Home_Home_Team"  ,
         "Away_Team"="Home_Away_Team" ,
         "Team"="Home_Team",
         "Home_Away"="Home_Home_Away")

head(passes17_home)





head(passes17_away)

dim(passes17_away)
dim(passes17_home)
dim(passes17_padrao)



colnames(passes17_away)
colnames(passes17_home)
colnames(passes17_padrao)

# Excluindo variÃ¡vel Team
passes17_away = passes17_away |> 
  select(-c(Team,Home_Away))

passes17_home = passes17_home |> 
  select(-c(Team,Home_Away))


passes17_padrao = passes17_padrao |> 
  select(c(Home_Team,Away_Team))

dim(passes17_padrao)

# removendo linhas duplicadas
passes17_padrao = distinct(passes17_padrao)
dim(passes17_padrao)

head(passes17_padrao)



passes2 = inner_join(x = passes17_home,y = passes17_away,
                     by = c("Home_Team","Away_Team"))

# verificando na
sum(is.na(passes2))

#View(passes2)


passes17_final = inner_join(x = passes17_padrao,y = passes2,
                            by = c("Home_Team","Away_Team"))


dim(passes17_final)
head(passes17_final)


##### Defesa
defesa17_padrao = defesa17 |> 
  select(Home_Team:Away_Red_Cards)
#View(defesa17)
defesa17_home = defesa17 |> 
  select(Home_Team,Away_Team,Team:Clr) |> 
  filter(Home_Away == "Home")


defesa17_away = defesa17 |> 
  select(Home_Team,Away_Team,Team:Clr) |> 
  filter(Home_Away == "Away")

head(defesa17_home)
head(defesa17_away)


defesa17_home = defesa17_home |> 
  rename_with( ~ paste("Home", .x, sep = "_"))

colnames(defesa17_home)
head(defesa17_home)

defesa17_away = defesa17_away |> 
  rename_with( ~ paste("Away", .x, sep = "_"))

colnames(defesa17_away)
head(defesa17_away)

# Renomeando variÃ¡veis

defesa17_away = defesa17_away |> 
  rename("Home_Team"="Away_Home_Team"  ,
         "Away_Team"="Away_Away_Team" ,
         "Team"="Away_Team",
         "Home_Away"="Away_Home_Away")

head(defesa17_away)

defesa17_home = defesa17_home |> 
  rename("Home_Team"="Home_Home_Team"  ,
         "Away_Team"="Home_Away_Team" ,
         "Team"="Home_Team",
         "Home_Away"="Home_Home_Away")

head(defesa17_home)





head(defesa17_away)

dim(defesa17_away)
dim(defesa17_home)
dim(defesa17_padrao)



colnames(defesa17_away)
colnames(defesa17_home)
colnames(defesa17_padrao)

# Excluindo variÃ¡vel Team
defesa17_away = defesa17_away |> 
  select(-c(Team,Home_Away,Away_Tkl_Tackles,Away_Min))

defesa17_home = defesa17_home |> 
  select(-c(Team,Home_Away,Home_Tkl_Tackles,Home_Min))


defesa17_padrao = defesa17_padrao |> 
  select(c(Home_Team,Away_Team))

dim(defesa17_padrao)

# removendo linhas duplicadas
defesa17_padrao = distinct(defesa17_padrao)
dim(defesa17_padrao)

head(defesa17_padrao)



defesa2 = inner_join(x = defesa17_home,y = defesa17_away,
                     by = c("Home_Team","Away_Team"))

# verificando na
sum(is.na(defesa2))

#View(defesa2)


defesa17_final = inner_join(x = defesa17_padrao,y = defesa2,
                            by = c("Home_Team","Away_Team"))

dim(defesa17_final)
sum(is.na(defesa17_final))
colnames(defesa17_final)




##### posse
posse17_padrao = posse17 |> 
  select(Home_Team:Away_Red_Cards)
#View(posse17)
posse17_home = posse17 |> 
  select(Home_Team,Away_Team,Team:`Att 3rd_Touches`) |> 
  filter(Home_Away == "Home")


posse17_away = posse17 |> 
  select(Home_Team,Away_Team,Team:`Att 3rd_Touches`) |> 
  filter(Home_Away == "Away")

head(posse17_home)
head(posse17_away)


posse17_home = posse17_home |> 
  rename_with( ~ paste("Home", .x, sep = "_"))

colnames(posse17_home)
head(posse17_home)

posse17_away = posse17_away |> 
  rename_with( ~ paste("Away", .x, sep = "_"))

colnames(posse17_away)
head(posse17_away)

# Renomeando variÃ¡veis

posse17_away = posse17_away |> 
  rename("Home_Team"="Away_Home_Team"  ,
         "Away_Team"="Away_Away_Team" ,
         "Team"="Away_Team",
         "Home_Away"="Away_Home_Away")

head(posse17_away)

posse17_home = posse17_home |> 
  rename("Home_Team"="Home_Home_Team"  ,
         "Away_Team"="Home_Away_Team" ,
         "Team"="Home_Team",
         "Home_Away"="Home_Home_Away")

head(posse17_home)





head(posse17_away)

dim(posse17_away)
dim(posse17_home)
dim(posse17_padrao)



colnames(posse17_away)
colnames(posse17_home)
colnames(posse17_padrao)

# Excluindo variÃ¡vel Team
posse17_away = posse17_away |> 
  select(-c(Team,Home_Away,Away_Min))

posse17_home = posse17_home |> 
  select(-c(Team,Home_Away,Home_Min))


posse17_padrao = posse17_padrao |> 
  select(c(Home_Team,Away_Team))

dim(posse17_padrao)

# removendo linhas duplicadas
posse17_padrao = distinct(posse17_padrao)
dim(posse17_padrao)

head(posse17_padrao)



posse2 = inner_join(x = posse17_home,y = posse17_away,
                    by = c("Home_Team","Away_Team"))

# verificando na
sum(is.na(posse2))
dim(posse2)
#View(posse2)


posse17_final = inner_join(x = posse17_padrao,y = posse2,
                           by = c("Home_Team","Away_Team"))

dim(posse17_final)
sum(is.na(posse17_final))




##### misc
misc17_padrao = misc17 |> 
  select(Home_Team:Away_Red_Cards)
#View(misc17)
misc17_home = misc17 |> 
  select(Home_Team,Away_Team,Team,Recov,Home_Away) |> 
  filter(Home_Away == "Home")


misc17_away = misc17 |> 
  select(Home_Team,Away_Team,Team,Recov,Home_Away) |> 
  filter(Home_Away == "Away")

head(misc17_home)
head(misc17_away)


misc17_home = misc17_home |> 
  rename_with( ~ paste("Home", .x, sep = "_"))

colnames(misc17_home)
head(misc17_home)

misc17_away = misc17_away |> 
  rename_with( ~ paste("Away", .x, sep = "_"))

colnames(misc17_away)
head(misc17_away)

# Renomeando variÃ¡veis

misc17_away = misc17_away |> 
  rename("Home_Team"="Away_Home_Team"  ,
         "Away_Team"="Away_Away_Team" ,
         "Team"="Away_Team",
         "Home_Away"="Away_Home_Away")

head(misc17_away)

misc17_home = misc17_home |> 
  rename("Home_Team"="Home_Home_Team"  ,
         "Away_Team"="Home_Away_Team" ,
         "Team"="Home_Team",
         "Home_Away"="Home_Home_Away")

head(misc17_home)





head(misc17_away)

dim(misc17_away)
dim(misc17_home)
dim(misc17_padrao)



colnames(misc17_away)
colnames(misc17_home)
colnames(misc17_padrao)

# Excluindo variÃ¡vel Team
misc17_away = misc17_away |> 
  select(-c(Team,Home_Away))

misc17_home = misc17_home |> 
  select(-c(Team,Home_Away))


misc17_padrao = misc17_padrao |> 
  select(c(Home_Team,Away_Team))

dim(misc17_padrao)

# removendo linhas duplicadas
misc17_padrao = distinct(misc17_padrao)
dim(misc17_padrao)

head(misc17_padrao)



misc2 = inner_join(x = misc17_home,y = misc17_away,
                   by = c("Home_Team","Away_Team"))

# verificando na
sum(is.na(misc2))
dim(misc2)
#View(misc2)


misc17_final = inner_join(x = misc17_padrao,y = misc2,
                          by = c("Home_Team","Away_Team"))
head(misc17_final)
dim(misc17_final)
sum(is.na(misc17_final))







# ##### goleiro
# goleiro17_padrao = goleiro17 |> 
#   select(Home_Team:Away_Red_Cards)
# #View(goleiro17)
# goleiro17_home = goleiro17 |> 
#   select(Home_Team,Away_Team,Team,Home_Away,Saves_Shot_Stopping,Save_percent_Shot_Stopping) |> 
#   filter(Home_Away == "Home")
# 
# 
# goleiro17_away = goleiro17 |> 
#   select(Home_Team,Away_Team,Team,Home_Away,Saves_Shot_Stopping,Save_percent_Shot_Stopping) |> 
#   filter(Home_Away == "Away")
# 
# head(goleiro17_home)
# head(goleiro17_away)
# 
# 
# goleiro17_home = goleiro17_home |> 
#   rename_with( ~ paste("Home", .x, sep = "_"))
# 
# colnames(goleiro17_home)
# head(goleiro17_home)
# 
# goleiro17_away = goleiro17_away |> 
#   rename_with( ~ paste("Away", .x, sep = "_"))
# 
# colnames(goleiro17_away)
# head(goleiro17_away)
# 
# # Renomeando variÃ¡veis
# 
# goleiro17_away = goleiro17_away |> 
#   rename("Home_Team"="Away_Home_Team"  ,
#          "Away_Team"="Away_Away_Team" ,
#          "Team"="Away_Team",
#          "Home_Away"="Away_Home_Away")
# 
# head(goleiro17_away)
# 
# goleiro17_home = goleiro17_home |> 
#   rename("Home_Team"="Home_Home_Team"  ,
#          "Away_Team"="Home_Away_Team" ,
#          "Team"="Home_Team",
#          "Home_Away"="Home_Home_Away")
# 
# head(goleiro17_home)
# 
# 
# 
# 
# 
# head(goleiro17_away)
# 
# dim(goleiro17_away)
# dim(goleiro17_home)
# dim(goleiro17_padrao)
# 
# 
# 
# colnames(goleiro17_away)
# colnames(goleiro17_home)
# colnames(goleiro17_padrao)
# 
# # Excluindo variÃ¡vel Team
# goleiro17_away = goleiro17_away |> 
#   select(-c(Team,Home_Away))
# 
# goleiro17_home = goleiro17_home |> 
#   select(-c(Team,Home_Away))
# 
# 
# goleiro17_padrao = goleiro17_padrao |> 
#   select(c(Home_Team,Away_Team))
# 
# dim(goleiro17_padrao)
# 
# # removendo linhas duplicadas
# goleiro17_padrao = distinct(goleiro17_padrao)
# dim(goleiro17_padrao)
# goleiro17_away = distinct(goleiro17_away)
# dim(goleiro17_away)
# goleiro17_home = distinct(goleiro17_home)
# dim(goleiro17_home)
# head(goleiro17_padrao)
# 
# 
# 
# goleiro2 = inner_join(x = goleiro17_home,y = goleiro17_away,
#                     by = c("Home_Team","Away_Team"))
# 
# # verificando na
# sum(is.na(goleiro2))
# dim(goleiro2)
# View(goleiro2)
# 
# 
# goleiro17_final = inner_join(x = goleiro17_padrao,y = goleiro2,
#                            by = c("Home_Team","Away_Team"))
# 
# dim(goleiro17_final)
# sum(is.na(goleiro17_final))
# 
# 
# 
# 
# 











# Unindo todos os datasets
dim(resumo17_final)
dim(passes17_final)
dim(posse17_final)
dim(defesa17_final)
dim(misc17_final)


dados_final1 = inner_join(x = resumo17_final,y = passes17_final,
                          by = c("Home_Team","Away_Team"))
dim(dados_final1)

dados_final1 = inner_join(x = dados_final1,y = posse17_final,
                          by = c("Home_Team","Away_Team"))
dim(dados_final1)

dados_final1 = inner_join(x = dados_final1,y = defesa17_final,
                          by = c("Home_Team","Away_Team"))
dim(dados_final1)

dados_final1 = inner_join(x = dados_final1,y = misc17_final,
                          by = c("Home_Team","Away_Team"))
dim(dados_final1)



#write.csv(dados_final1,"dados_final2018.csv",row.names = FALSE)



library(tidyverse)

dados_final1 = read_csv("dados_final2018.csv")

dados_gerais2 =dados_final1

# Criando a variÃ¡vel alvo "resultado", que diz quem venceu ou se houve empate

dados_gerais2 = dados_gerais2 |> 
  mutate(resultado = if_else(Home_Score > Away_Score, "Mandante",
                             if_else(Home_Score < Away_Score,"Visitante","Empate" )))

head(dados_gerais2 |> select(Home_Team,Away_Team,Home_Score,Away_Score,resultado),)










# Criando variÃ¡vel com os nomes dos times para obtenÃ§Ã£o
# de novas variÃ¡veis com as mÃ©dias de variÃ¡veis encontradas anteriormente
# Isso porque, para uma dada observaÃ§Ã£o, o dataset nos da as informaÃ§Ãµes
# do jogo que jÃ¡ ocorreu. 
# Como queremos prever o resultado dos jogos, teremos que alterar essas 
# informaÃ§Ãµes para a mÃ©dia de partidas anteriores, afim de poder
# prever entÃ£o o resultado de tal jogo.
# Assim usamos o momento do time no campeonato, que influenciarÃ¡ na proxima 
# partida.


unique(dados_gerais2$Home_Team)

nome_times = c(  "Manchester United" ,      "Newcastle United" ,      
                 "Fulham"         ,         "Bournemouth"   ,         
                 "Watford"         ,        "Huddersfield Town"  ,    
                 "Wolverhampton Wanderers", "Southampton" ,           
                 "Liverpool"          ,     "Arsenal"   ,             
                 "Cardiff City"       ,     "West Ham United" ,       
                 "Everton"          ,       "Leicester City"   ,      
                 "Tottenham Hotspur"  ,     "Chelsea" ,               
                 "Burnley"        ,         "Manchester City" ,       
                 "Brighton & Hove Albion" , "Crystal Palace" )
length(nome_times)


head(dados_gerais2)

# Criando novo dataset a ser modificado

num_cols <- unlist(lapply(dados_gerais2, is.numeric))         
num_cols

dados_gerais3 = dados_gerais2[,num_cols]
dados_gerais4 = dados_gerais3

colunas = colnames(dados_gerais3)

for(t in 1:dim(dados_gerais3)[2]){
  if (grepl("Away",colunas[t])==TRUE) {
    print(colunas[t])
  }
}


# Função para obter a média das 3 partidas de uma observação.
# Realizando para cada uma das variáveis

for(t in 1:dim(dados_gerais3)[2]){
  if (grepl("Away",colunas[t])==TRUE) {
    for(i in 1:20) {
      a = 0
      b = 0
      c = 0
      for(j in 1:380) {
        if(dados_gerais2$Away_Team[j] == nome_times[i]){
          
          media = (a+b+c)/3
          
          dados_gerais4[j,t] = media
          a = b
          b = c
          c = dados_gerais3[j,t]
          
        } 
      }
    }
  }
  
  else{
    for(i in 1:20) {
      a = 0
      b = 0
      c = 0
      for(j in 1:380) {
        if(dados_gerais2$Home_Team[j] == nome_times[i]){
          
          media = (a+b+c)/3
          
          dados_gerais4[j,t] = media
          a = b
          b = c
          c = dados_gerais3[j,t]
          
        } 
      }
    }
  }
}

View(dados_gerais4)



# Unindo datasets
times_pre = dados_gerais2 |> 
  select(Home_Team,Away_Team,resultado)
dim(times_pre)
dados_gerais5 = cbind(times_pre,dados_gerais4)

View(dados_gerais5)

#write.csv(dados_gerais5,"dados_variaveis2018.csv",row.names = F)

#### Conferindo os resultados
# a = dados_gerais2 |> 
#   select(Away_Team,Away_Score) |> 
#   filter(Away_Team == "Arsenal")
# 
# b = dados_gerais5 |> 
#   select(Away_Team,Away_Score) |> 
#   filter(Away_Team == "Arsenal")
# 
# p = cbind(a,b)
# 
# View(p)

library(tidyverse)

# Importando base 5

dados_gerais5 = read_csv("dados_variaveis2018.csv")

nome_times = c( "Manchester United" ,      "Newcastle United" ,      
                "Fulham"         ,         "Bournemouth"   ,         
                "Watford"         ,        "Huddersfield Town"  ,    
                "Wolverhampton Wanderers", "Southampton" ,           
                "Liverpool"          ,     "Arsenal"   ,             
                "Cardiff City"       ,     "West Ham United" ,       
                "Everton"          ,       "Leicester City"   ,      
                "Tottenham Hotspur"  ,     "Chelsea" ,               
                "Burnley"        ,         "Manchester City" ,       
                "Brighton & Hove Albion" , "Crystal Palace")
length(nome_times)

# criando variavel para as pontuacoes

# Pontos dos Ãºltimos 3 jogos

# Visitante:

dados_gerais5$Away_Pontuacao_3 = NA

for(i in 1:20){
  a = 0
  b = 0
  c = 0
  for(j in 1:380) {
    
    
    if(dados_gerais5$Away_Team[j] == nome_times[i]){
      pontuacao = a+b+c
      dados_gerais5$Away_Pontuacao_3[j] = pontuacao
      a = b
      b = c
      if(dados_gerais5$resultado[j] == "Visitante") {
        c =  3
      } else {
        if(dados_gerais5$resultado[j] == "Empate") {
          c =  1
        } else {
          c =  0
        }
      }
      
    }
    
  }
}

View(dados_gerais5)

View(dados_gerais5 |> 
       select(Away_Team,resultado,Away_Pontuacao_3) |> 
       filter(Away_Team == "Arsenal"))

# Mandante

dados_gerais5$Home_Pontuacao_3 = NA

for(i in 1:20){
  a = 0
  b = 0
  c = 0
  for(j in 1:380) {
    
    
    if(dados_gerais5$Home_Team[j] == nome_times[i]){
      pontuacao = a+b+c
      dados_gerais5$Home_Pontuacao_3[j] = pontuacao
      a = b
      b = c
      if(dados_gerais5$resultado[j] == "Mandante") {
        c =  3
      } else {
        if(dados_gerais5$resultado[j] == "Empate") {
          c =  1
        } else {
          c =  0
        }
      }
      
    }
    
  }
}

#View(dados_gerais5)

View(dados_gerais5 |> 
       select(Home_Team,resultado,Home_Pontuacao_3) |> 
       filter(Home_Team == "Arsenal"))



### PontuaÃ§Ã£o geral

# Obs: pontuacao referente a quantos pontos no total o
# time chega para a prÃ³xima rodada.


dados_gerais5$Home_Pontuacao_Geral = NA

for(i in 1:20){
  
  
  c = 0
  pontuacao = 0
  for(j in 1:380) {
    
    
    if(dados_gerais5$Home_Team[j] == nome_times[i]){
      pontuacao = pontuacao + c
      dados_gerais5$Home_Pontuacao_Geral[j] = pontuacao
      
      
      if(dados_gerais5$resultado[j] == "Mandante") {
        c =  3
      } else {
        if(dados_gerais5$resultado[j] == "Empate") {
          c =  1
        } else {
          c =  0
        }
      }
      
    }else {
      if(dados_gerais5$Away_Team[j] == nome_times[i]){
        pontuacao = pontuacao + c
        #dados_gerais5$pontuacao_geral[j] = pontuacao
        
        
        if(dados_gerais5$resultado[j] == "Visitante") {
          c =  3
        } else {
          if(dados_gerais5$resultado[j] == "Empate") {
            c =  1
          } else {
            c =  0
          }
        }
        
      }
    }
    
  }
}


View(dados_gerais5 |> 
       select(Home_Team,resultado,Home_Pontuacao_Geral) |> 
       filter(Home_Team == "Arsenal"))




dados_gerais5$Away_Pontuacao_Geral = NA

for(i in 1:20){
  
  
  c = 0
  pontuacao = 0
  for(j in 1:380) {
    
    
    if(dados_gerais5$Home_Team[j] == nome_times[i]){
      pontuacao = pontuacao + c
      #dados_gerais5$home_pontuacao_geral[j] = pontuacao
      
      
      if(dados_gerais5$resultado[j] == "Mandante") {
        c =  3
      } else {
        if(dados_gerais5$resultado[j] == "Empate") {
          c =  1
        } else {
          c =  0
        }
      }
      
    }else {
      if(dados_gerais5$Away_Team[j] == nome_times[i]){
        pontuacao = pontuacao + c
        dados_gerais5$Away_Pontuacao_Geral[j] = pontuacao
        
        
        if(dados_gerais5$resultado[j] == "Visitante") {
          c =  3
        } else {
          if(dados_gerais5$resultado[j] == "Empate") {
            c =  1
          } else {
            c =  0
          }
        }
        
      }
    }
    
  }
}




View(dados_gerais5 |> 
       select(Away_Team,resultado,Away_Pontuacao_Geral) )




write.csv(dados_gerais5,"dados_complemento2018.csv",row.names = F)

colnames(dados_gerais5)


#Home_Gls,Away_Gls,Home_PK,Away_PK,Home_PKatt,Away_PKatt,


library(tidyverse)
library(caret)

library(ISLR)
library(kernlab)
library(epiDisplay)
library(mlr)

dados = read_csv("dados_complemento2018.csv")





# Passando a variÃ¡vel resultado para fator

# variÃ¡vel alvo
tab1(dados$resultado,main = "ObservaÃ§Ãµes da VariÃ¡vel Alvo")
head(dados$resultado)
#dados$resultado = factor(dados$resultado,levels = c("Mandante","Visitante","Empate"),
#                         labels = c(2,1,0))



# Adicionando variÃ¡veis para valor de mercado
valor2018 = tibble(nome_time = c( "Manchester United" ,      "Newcastle United" ,      
                                  "Fulham"         ,         "Bournemouth"   ,         
                                  "Watford"         ,        "Huddersfield Town"  ,    
                                  "Wolverhampton Wanderers", "Southampton" ,           
                                  "Liverpool"          ,     "Arsenal"   ,             
                                  "Cardiff City"       ,     "West Ham United" ,       
                                  "Everton"          ,       "Leicester City"   ,      
                                  "Tottenham Hotspur"  ,     "Chelsea" ,               
                                  "Burnley"        ,         "Manchester City" ,       
                                  "Brighton & Hove Albion" , "Crystal Palace" ),
                   media_mercado = c(22.16,6.61,7.7,8.38,6.13,4.17,7.57,7.64,
                                     30.06,15.69,3.23,9.7,13.23,11.73,26.43,
                                     21.6,5.98,26.16,4.74,8.03),
                   valor_mercado = c(797.6,244.65,254.65,310,239.1,137.45,302.9,
                                     305.4,1170,659.05,113,359,529.1,434.1,898.6,
                                     1170,197.25,1200,194.35,289))

head(valor2018)

#adicionando nos dados

for(i in 1:nrow(dados)){
  for(j in 1:nrow(valor2018)){
    if(dados$Home_Team[i] == valor2018$nome_time[j]){
      dados$Home_media_merc[i] = valor2018$media_mercado[j]
      dados$Home_valor_merc[i] = valor2018$valor_mercado[j]
    }
    if(dados$Away_Team[i] == valor2018$nome_time[j]){
      dados$Away_media_merc[i] = valor2018$media_mercado[j]
      dados$Away_valor_merc[i] = valor2018$valor_mercado[j]
    }
  }
}
View(dados |> select(Home_Team,Away_Team,Home_media_merc,
                     Home_valor_merc,Away_media_merc,
                     Away_valor_merc))








# Excluindo variaveis com nomes dos clubes
dados = dados |> 
  dplyr::select(-c(Home_Team,Away_Team))






# Passando a variÃ¡vel resultado para fator
dados$resultado = factor(dados$resultado)
head(dados$resultado) # 2 mandante, 1 visitante, 0 empate


#head(dados_gerais5$resultado)

# Renomeando variaveis com espaco em branco

dados = dados |> 
  rename("Home_Def_Pen_Touches" ="Home_Def Pen_Touches",
         "Home_Mid_3rd_Touches" = "Home_Mid 3rd_Touches",
         "Away_Def_3rd_Touches"="Away_Def 3rd_Touches",
         "Away_Att_3rd_Touches"="Away_Att 3rd_Touches",
         "Home_Def_3rd_Tackles"="Home_Def 3rd_Tackles",
         "Home_Att_3rd_Tackles"="Home_Att 3rd_Tackles",
         "Home_Def_3rd_Touches"="Home_Def 3rd_Touches",
         "Home_Att_3rd_Touches"="Home_Att 3rd_Touches",
         "Away_Def_Pen_Touches"="Away_Def Pen_Touches",
         "Away_Mid_3rd_Touches"="Away_Mid 3rd_Touches",
         "Away_Def_3rd_Tackles"="Away_Def 3rd_Tackles",
         "Away_Att_3rd_Tackles"="Away_Att 3rd_Tackles",
         "Away_Mid_3rd_Tackles"="Away_Mid 3rd_Tackles",
         "Home_Mid_3rd_Tackles"="Home_Mid 3rd_Tackles",
         "Home_Int_x"="Home_Int.x",
         "Away_Int_x"="Away_Int.x",
         "Home_Int_y"="Home_Int.y",
         "Away_Int_y"="Away_Int.y",
         "Home_Tkl_Int"="Home_Tkl+Int",
         "Away_Tkl_Int"="Away_Tkl+Int")

colnames(dados)

# write.csv(dados,
#          "dados_modelofinal2018.csv",row.names = F)





# Como no trabalho são utilizados dois campeonatos,
# a seguir os dados são unidos em uma só base

# Unindo bases
dados1 = read_csv("dados_modelofinal2017.csv")
dados2 = read_csv("dados_modelofinal2018.csv")


dim(dados1)
dim(dados2)

dim(rbind(dados1,dados2))

sum(is.na(rbind(dados1,dados2)))

dados = rbind(dados1,dados2)


dim(dados)

# write.csv(dados,
#                    "dados_modelo20172018.csv",row.names = F)





