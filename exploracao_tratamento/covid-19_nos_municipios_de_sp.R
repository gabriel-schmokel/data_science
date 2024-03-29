
# 1) CARREGANDO BIBLIOTECAS

#install.packages("dplyr")    # Deixe comentada essa linha se o pacote dplyr j� foi instalado.
library(dplyr)                # Carregando a biblioteca dplyr.
#install.packages("readxl")   # Deixe comentada essa linha se o pacote readxl j� foi instalado.
library(readxl)               # Carregando a biblioteca readxl.


# 2) CARREGANDO O DATASET

setwd("C:/gabriel/trabalho/analise_tratamento_exploracao/linguagem_r_para_analise_de_dados/dados-covid-sp-master/data") # Direcionando o caminho do diret�rio.
covid_sp <- read.csv('dados_covid_sp.csv', sep = ";") # Lendo o arquivo de dados.
View(covid_sp) # Visualizando o dataframe completo.
covid_sp[,] # Visualizando as 1000 primeiras linhas do DataFrame.



# 3) ALTERANDO O FORMATO DE LEITURA DO DATA SET

setwd("C:/Gabriel/trabalho/analise_tratamento_exploracao/linguagem_r_para_analise_de_dados/dados-covid-sp-master/data") # chamamos novamente o caminho do diret�rio onde se encontra o dataset para n�o ser gerado um erro ao chamar a leitura do arquivo. 
covid_sp <- read.csv2('dados_covid_sp.csv', sep =";", encoding = "UTF-8") # UTF-8 faz a leitura identificando a acentua��o.
head(covid_sp) # visualizando as 6 primeiras linhas.



# 4) MODIFICANDO O NOME DAS VARI�VEIS

covid_sp_alterado <- rename(covid_sp, municipio = nome_munic, data = datahora, casos_100_mil_habitantes = casos_pc , obitos_media_movel = obitos_mm7d, casos_media_movel = casos_mm7d, rotulo_mapa=map_leg, codigo_mapa = map_leg_s)   # alterando o nome das vari�veis.

View(covid_sp_alterado)   # visualizando o dataframe completo.
head(covid_sp_alterado)   # visualizando as 6 primeiras linhas. 



# 5) EXCLUINDO COLUNAS

covid_sp_alterado$cod_ra <- NULL # excluir uma �nica coluna 
head(covid_sp_alterado)
# COMENT�RIOS SOBRE O C�DIGO

# Para deletar uma �nica coluna, podemos passar o atributo NULL a respectiva vari�vel.

covid_sp_alterado <- select(covid_sp_alterado, -c(21:24))  # Exluindo intervalos de colunas
covid_sp_alterado <- select(covid_sp_alterado, -c(14,15,16,17))  # Exlcuindo v�rias colunas pelo seu n�mero de identifica��o.
covid_sp_alterado <- subset(covid_sp_alterado, select = -c(codigo_ibge, casos_100_mil_habitantes, obitos_pc))  # Exluindo v�rias colunas pelo seu nome de identifica��o.
covid_sp_alterado <- subset(covid_sp_alterado, select = -c(dia,mes))  # Excluindo duas colunas pelo seu nome.
head(covid_sp_alterado)  # visualizando as 6 primeiras linhas do data frame.

# 6) EXPLORANDO O ERRO NOS DADOS

summary(covid_sp_alterado)

## DUPLICIDADE

duplicado = covid_sp_alterado[duplicated(covid_sp_alterado), ] # exibe as linhas duplicadas do data frame   
duplicado[,]  # exibindo o data frame duplicado

## CONSIST�NCIA

municipios <- unique(covid_sp_alterado$municipio) # vetor contendo os munic�pios poss�veis de SP

for(interador in municipios){
  covid_municipio <- covid_sp_alterado %>% filter(municipio == interador) # Filtrando as linhas correspondente ao municipio do interador.
  if(length(unique(covid_municipio$area)) > 1){ # Condi��o que avalia se o munic�pio em quest�o tem mais de um valor para a �rea.
    covid_municipio[,] # Exibindo munic�pio que cont�m �rea n�o constante.  
  }
}

# evidenciando a falta de consist�ncia da semana_epidem
covid_sp_alterado[is.na(covid_sp_alterado$semana_epidem),] 

## COMPLETUDE

summary(covid_sp_alterado$casos_novos) 
boxplot(covid_sp_alterado$casos_novos, main="Gr�fico BoxPlot", ylab="Casos Novos") # Gerando o boxplot da vari�vel casos novos.
summary(covid_sp_alterado) 

## CONFORMIDADE
setwd("C:/gabriel/trabalho/analise_tratamento_exploracao/linguagem_r_para_analise_de_dados/dados-covid-sp-master/data")
banco_municipios <- read_excel("municipios.xls")  # importando o arquivo de dados contendo o nome dos munic�pios do Brasil.
banco_municipios <- banco_municipios[which(banco_municipios$Nome_UF == "S�o Paulo"), ]  # Filtrando as linhas apenas com os municipios de SP
banco_municipios <- select(banco_municipios, c(1, 13)) # Selecionando apenas as vari�veis que informam o estado e o munic�pio do DataFrame
banco_municipios <- unique(banco_municipios$Nome_Munic�pio)  # criando um vetor com os valores �nicos da vari�vel Nome_Munic�pio  (cont�m todos os munic�pios de SP)

municipios_sp <- unique(covid_sp_alterado$municipio)  # Selecionando os valores �nicos que aparecem na vari�vel municipio do DataFrame covid_sp_alterado

FILTRO <- rep(TRUE, length(municipios_sp))  # Filtro para selecionar qual municipio n�o est� presente no estado de SP.
contador = 1  # vari�vel para acessar os �ndices do vetor FILTRO
flag = TRUE  # Define qual elemento do FILTRO pertence a regi�o de SP.   

for (interador1 in municipios_sp){
  for(interador2 in banco_municipios){
    if(interador1 == interador2){
      flag = FALSE
    }
  }
  
  FILTRO[contador] = flag
  contador = contador + 1
  flag = TRUE
}

municipios_sp[FILTRO]  # substituindo o FILTRO no vetor de munic�pios, obtemos as regi�es que n�o pertencem ao estado de SP. Percebemos que apenas o valor 'Ignorado' n�o pertence a regi�o do estado de SP

covid_sp_alterado[which(covid_sp_alterado$municipio == "Ignorado"), ]  # Filtrando e exibindo as linhas apenas com os municipios de SP, cujo valor � 'ignorado'.


## VERIFICA��O DOS DADOS AUSENTES E INDEFINIDOS
sapply(covid_sp_alterado, function(x) sum(is.na(x)))
sapply(covid_sp_alterado, function(x) sum(is.nan(x)))

## VERIFICA��O DA CLASSE DAS VARI�VEIS
glimpse(covid_sp_alterado) # verificando a classe das vari�veis 



# 7) TRATANDO O ERRO DOS DADOS

## CONSIST�NCIA 
covid_sp_alterado2 <- replace(x = covid_sp_alterado,list = is.na(covid_sp_alterado), values = 54)
tail(covid_sp_alterado2)

covid_sp_alterado$semana_epidem[covid_sp_alterado$data >= '2021-01-01' &
                                  covid_sp_alterado$data <= '2021-01-07'  ] <- 54

covid_sp_alterado$semana_epidem[covid_sp_alterado$data >= '2021-01-08' &
                                  covid_sp_alterado$data <= '2021-01-14'  ] <- 55

covid_sp_alterado$semana_epidem[covid_sp_alterado$data >= '2021-01-15' &
                                  covid_sp_alterado$data <= '2021-01-21'  ] <- 56

covid_sp_alterado$semana_epidem[covid_sp_alterado$data >= '2021-01-22' &
                                  covid_sp_alterado$data <= '2021-01-28'  ] <- 57

covid_sp_alterado$semana_epidem[covid_sp_alterado$data >= '2021-01-29' &
                                  covid_sp_alterado$data <= '2021-02-04'  ] <- 58

covid_sp_alterado$semana_epidem[covid_sp_alterado$data >= '2021-02-05' &
                                  covid_sp_alterado$data <= '2021-02-12'  ] <- 59

covid_sp_alterado$semana_epidem[covid_sp_alterado$data >= '2021-02-13' &
                                  covid_sp_alterado$data <= '2021-02-20'  ] <- 60

covid_sp_alterado$semana_epidem[covid_sp_alterado$data >= '2021-02-21' &
                                  covid_sp_alterado$data <= '2021-02-27'  ] <- 61

covid_sp_alterado$semana_epidem[covid_sp_alterado$data >= '2021-02-28' &
                                  covid_sp_alterado$data <= '2021-03-6'  ] <- 62


View(covid_sp_alterado)
tail(covid_sp_alterado)

## COMPLETUDE
covid_sp_alterado <- covid_sp_alterado %>% filter(casos_novos >= 0) # filtrando os valores casos_novos do dataframe acima ou igual a zero
covid_sp_alterado <- covid_sp_alterado %>% filter(casos >= 0) # filtrando os valores casos do dataframe acima ou igual a zero
covid_sp_alterado <- covid_sp_alterado %>% filter(casos_media_movel >= 0) # filtrando os valores casos_media_movel do dataframe acima ou igual a zero
covid_sp_alterado <- covid_sp_alterado %>% filter(obitos_novos >= 0) # filtrando os valores obitos novos do dataframe acima ou igual a zero
covid_sp_alterado <- covid_sp_alterado %>% filter(obitos_media_movel >= 0) # filtrando os valores obitos_media_movel do dataframe acima ou igual a zero
covid_sp_alterado <- covid_sp_alterado %>% filter(pop > 0) # filtrando os valores pop do dataframe acima ou igual a zero
covid_sp_alterado <- covid_sp_alterado %>% filter(pop_60 > 0) # filtrando os valores pop_60 do dataframe acima ou igual a zero
summary(covid_sp_alterado)


## CONFORMIDADE
covid_ignorado <- covid_sp_alterado %>% filter(municipio == "Ignorado") # filtrando os valores do dataframe cujo municipio tem valor ignorado
covid_ignorado[,]  # demonstrando que n�o existe mais munic�pios ignorados

## DADOS AUSENTES 
sapply(covid_sp_alterado, function(x) sum(is.na(x)))

## TRATANDO A CLASSE DAS VARI�VEIS 
covid_sp_alterado$semana_epidem <- as.integer(covid_sp_alterado$semana_epidem) # trucando a vari�vel semana_epidem na classe inteiro. 
covid_sp_alterado$data <- as.Date(covid_sp_alterado$data, format ='%Y-%m-%d') # truncando a vari�vel data na classe date.
str(covid_sp_alterado) # Exibindo a classe das vari�veis pertencentes no data frame

# 8) CRIA��O DE COLUNAS 
covid_sp_alterado["densidade_idoso"]<-covid_sp_alterado$pop_60/covid_sp_alterado$area # Resolve a express�o acima e cria a coluna 'idosos_densidade'
View(covid_sp_alterado) # visualizando os dados
covid_sp_alterado[,]

covid_sp_alterado["densidade_populacao"]<-covid_sp_alterado$pop/covid_sp_alterado$area # Resolve a express�o acima e cria a coluna 'idosos_densidade'
View(covid_sp_alterado) # visualizando os dados
head(covid_sp_alterado)

covid_sp_alterado["area"] <- covid_sp_alterado$area/100
covid_sp_alterado["densidade_idoso"]<-covid_sp_alterado$pop_60/covid_sp_alterado$area
covid_sp_alterado["densidade_populacao"]<-covid_sp_alterado$pop/covid_sp_alterado$area

# 9) EXPORTA��O DO ARQUIVO 
write.table(covid_sp_alterado, file ="covid_sp_tratado.csv", sep = ",")
