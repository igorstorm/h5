#==============================================================================#
# Hipótese V: Elegibilidade
# Definição sobre a distribuição do tempo que os servidores recebem o abono
# Estávamos aplicando um sorteio com lambda 5 pré-reforma e lambda 7 pós-reforma
#==============================================================================#

library(tidyverse)
library(lubridate)
library(data.table)

ativos13 <- readRDS("\\\\sbsb2/DIMAC/Novo DIRETORIO NEMAC/Simulações Pessoal/Simulação Estados 2020/ativos_sim_2013.rds")
ativos14 <- readRDS("\\\\sbsb2/DIMAC/Novo DIRETORIO NEMAC/Simulações Pessoal/Simulação Estados 2020/ativos_sim_2014.rds")
ativos15 <- readRDS("\\\\sbsb2/DIMAC/Novo DIRETORIO NEMAC/Simulações Pessoal/Simulação Estados 2020/ativos_sim_2015.rds")
ativos16 <- readRDS("\\\\sbsb2/DIMAC/Novo DIRETORIO NEMAC/Simulações Pessoal/Simulação Estados 2020/ativos_sim_2016.rds")
ativos17 <- readRDS("\\\\sbsb2/DIMAC/Novo DIRETORIO NEMAC/Simulações Pessoal/Simulação Estados 2020/ativos_sim_2017.rds")
# ativos18 <- readRDS("\\\\sbsb2/DIMAC/Novo DIRETORIO NEMAC/Simulações Pessoal/Simulação Estados 2020/ativos_sim_2018.rds")

ativos13 = ativos13 %>% mutate(ano = 2013)
ativos14 = ativos14 %>% mutate(ano = 2014)
ativos15 = ativos15 %>% mutate(ano = 2015)
ativos16 = ativos16 %>% mutate(ano = 2016)
ativos17 = ativos17 %>% mutate(ano = 2017)


# filtro de inativos em ativos18 com problema

momentoApos <- function(dados) {
  idade <- dados$idade
  tempoServPub <- floor(dados$tempoServPub)
  tempoContrib <- floor(dados$tempoContrib)
  sexo <- dados$sexo
  prof <- dados$profinep
  pc <- dados$pc
  militar <- dados$militar
  idade_adm <- dados$idade_adm

  # masculino = 1, feminino = 0
  # professor = 1, não professor = 0
  # Policial civil = 1, não policial = 0 Não tem idade mínima

  # Tempos mínimos da regra permanente (Incluindo Polícia Civil)
  minIdadeRP <- (60 * sexo + (1 - sexo) * 55 - 5 * prof) * (1 - pc)
  minTempoContribRP <- 35 * sexo + (1 - sexo) * 30 - 5 * (prof + pc)
  minTempoServPubRP <- 10 + 5 * pc + 5 * sexo * pc
  idadeCompulsoria <- 75 - 10 * pc


  # Tempos mínimos dos militares
  minTempoServPubMilitar <- 28
  idadeCompulsoriaMilitar <- 52


  ################################################
  # Momento de aposentadoria da regra permanente #
  ################################################
  # Aposentadoria com a Regra Permanente do Artigo 40 (Incluindo Policiais Civis)
  #
  # Regras de aposentadoria do Artigo 40:
  # 60 anos (55 Mulher), 55 anos para professor (50 professora)
  # 35 anos de contribuição (30 Mulher), 30 anos para professor (25 professora)
  # 10 anos no serviço público

  var1 <- idade - minIdadeRP
  var2 <- tempoContrib - minTempoContribRP
  var3 <- tempoServPub - minTempoServPubRP
  reCivil <- var1 >= 0 & var2 >= 0 & var3 >= 0

  momentoCivil <- - pmin(var1, var2, var3) * (1 - reCivil)
  momentoCivil <- (idade + momentoCivil < idadeCompulsoria) * momentoCivil +
    (idade + momentoCivil >= idadeCompulsoria) * (idade < idadeCompulsoria) * (idadeCompulsoria - idade)


  ########################################################################
  #                 Momento de aposentadoria de militares                #
  ########################################################################
  var4 <- tempoServPub - minTempoServPubMilitar

  reMilitar <- var4 >= 0
  momentoMilitar <- - (1 - reMilitar) * var4
  momentoMilitar <- (idade + momentoMilitar < idadeCompulsoriaMilitar) * momentoMilitar +
    (idade + momentoMilitar >= idadeCompulsoriaMilitar) * (idade < idadeCompulsoriaMilitar) * (idadeCompulsoriaMilitar - idade)


  momento <- (1 - militar) * momentoCivil + militar * momentoMilitar
  return(list(momento = momento))
}

ativos = rbindlist(list(ativos13,ativos14,ativos15,ativos16,ativos17),use.names = T)
rm(ativos13,ativos14,ativos15,ativos16,ativos17)
calculo <- momentoApos(ativos)
ativos$momento = calculo$momento

tempo_apto <- function(dados) {
  idade <- dados$idade
  tempoServPub <- floor(dados$tempoServPub)
  tempoContrib <- floor(dados$tempoContrib)
  sexo <- dados$sexo
  prof <- dados$profinep
  pc <- dados$pc
  militar <- dados$militar
  idade_adm <- dados$idade_adm

  # masculino = 1, feminino = 0
  # professor = 1, não professor = 0
  # Policial civil = 1, não policial = 0 Não tem idade mínima

  # Tempos mínimos da regra permanente (Incluindo Polícia Civil)
  minIdadeRP <- (60 * sexo + (1 - sexo) * 55 - 5 * prof) * (1 - pc)
  minTempoContribRP <- 35 * sexo + (1 - sexo) * 30 - 5 * (prof + pc)
  minTempoServPubRP <- 10 + 5 * pc + 5 * sexo * pc
  idadeCompulsoria <- 75 - 10 * pc


  # Tempos mínimos dos militares
  minTempoServPubMilitar <- 28
  idadeCompulsoriaMilitar <- 52


  ################################################
  # Momento de aposentadoria da regra permanente #
  ################################################
  # Aposentadoria com a Regra Permanente do Artigo 40 (Incluindo Policiais Civis)
  #
  # Regras de aposentadoria do Artigo 40:
  # 60 anos (55 Mulher), 55 anos para professor (50 professora)
  # 35 anos de contribuição (30 Mulher), 30 anos para professor (25 professora)
  # 10 anos no serviço público

  var1 <- idade - minIdadeRP
  var2 <- tempoContrib - minTempoContribRP
  var3 <- tempoServPub - minTempoServPubRP
  # reCivil <- var1 >= 0 & var2 >= 0 & var3 >= 0

  # momentoCivil <- - pmin(var1, var2, var3) * (1 - reCivil)
  # momentoCivil <- (idade + momentoCivil < idadeCompulsoria) * momentoCivil +
  #   (idade + momentoCivil >= idadeCompulsoria) * (idade < idadeCompulsoria) * (idadeCompulsoria - idade)
  tempo_apto = pmin(var1, var2, var3)

  ########################################################################
  #                 Momento de aposentadoria de militares                #
  ########################################################################
  var4 <- ifelse(tempoServPub >= minTempoServPubMilitar, tempoServPub - minTempoServPubMilitar,0)
  var5 <- ifelse(idade >= idadeCompulsoriaMilitar, idade - idadeCompulsoriaMilitar,0)

  # reMilitar <- var4 >= 0
  # momentoMilitar <- - (1 - reMilitar) * var4
  # momentoMilitar <- (idade + momentoMilitar < idadeCompulsoriaMilitar) * momentoMilitar +
  #   (idade + momentoMilitar >= idadeCompulsoriaMilitar) * (idade < idadeCompulsoriaMilitar) * (idadeCompulsoriaMilitar - idade)
  tempo_apto_militar <- pmax(var4,var5)

  tempo <- (1 - militar) * tempo_apto + militar * tempo_apto_militar
  return(list(tempo = tempo))
}

ativos = ativos %>%  mutate(apto =
                              case_when(servidor == "Professor" & sexo == 0 & idade >= 50 & tempoContrib >= 25 & tempoServPub >=10 ~ 1,
                                        servidor == "Professor" & sexo == 1 & idade >= 55 & tempoContrib >= 30 & tempoServPub >=10 ~ 1,
                                        servidor == "Outros servidores" & sexo == 1 & idade >= 60 & tempoContrib >= 35 & tempoServPub >=10 ~ 1,
                                        servidor == "Outros servidores" & sexo == 0 & idade >= 55 & tempoContrib >= 30 & tempoServPub >=10 ~ 1,
                                        servidor == "Policial Civil" & sexo == 1 & tempoContrib >= 30 & tempoServPub >=20 ~ 1,
                                        servidor == "Policial Civil" & sexo == 0 & tempoContrib >= 25 & tempoServPub >=15 ~ 1,
                                        servidor == "Militar" & tempoServPub >= 28  ~ 1,
                                        servidor == "Militar" & idade >= 52 ~ 1,
                                        TRUE ~ 0))

aptos = ativos %>% filter(apto == 1) %>% group_by(cpf) %>% filter(ano == max(ano)) %>% ungroup()

calculo <- tempo_apto(aptos)
aptos$tempo_apto <- calculo$tempo
table(aptos$tempo_apto,aptos$servidor)
hist(aptos$tempo_apto)

library(ggplot2)
aptos %>% count(servidor,tempo_apto) %>% ggplot(aes(x = tempo_apto,y = n)) +
                    geom_bar(stat = "identity") + facet_wrap(~servidor)

aptos %>% group_by(servidor) %>% summarise(media = mean(tempo_apto),mediana = median(tempo_apto),.groups = "drop")


# Obtendo a estimativa de kaplan meier para o tempo esperado

library(survival)

dados_modelo = data.frame(Grupo = aptos$servidor, Tempo = aptos$tempo_apto, Censura = ifelse(aptos$inativo ==1,0,1))
dados_modelo %>% count(Grupo,Tempo,Censura) %>% filter(Tempo < 5)
temp = Surv(dados_modelo$Tempo, dados_modelo$Censura)
estimativa = survfit(temp ~ Grupo,data = dados_modelo)
summary(estimativa)
plot(estimativa, lty = c(1,2,3,4), xlab = "Tempo de Abono", ylab = "S(t) Estimada",
     col = c("#eb4034","#34eb40","#3499eb","#c934eb"))
legend("topright", c("Militar", "Outros Servidores","Policial Civil","Professor"), lty = c(1,2,3,4), bty = 'n',
       col = c("#eb4034","#34eb40","#3499eb","#c934eb"))
