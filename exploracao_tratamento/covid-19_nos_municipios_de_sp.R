
# 1) CARREGANDO BIBLIOTECAS

#install.packages("dplyr")    # Deixe comentada essa linha se o pacote dplyr já foi instalado.
library(dplyr)                # Carregando a biblioteca dplyr.
#install.packages("readxl")   # Deixe comentada essa linha se o pacote readxl já foi instalado.
library(readxl)               # Carregando a biblioteca readxl.


# 2) CARREGANDO O DATASET

setwd("C:/gabriel/trabalho/analise_tratamento_exploracao/linguagem_r_para_analise_de_dados/dados-covid-sp-master/data") # Direcionando o caminho do diretório.
covid_sp <- read.csv('dados_covid_sp.csv', sep = ";") # Lendo o arquivo de dados.
View(covid_sp) # Visualizando o dataframe completo.
covid_sp[,] # Visualizando as 1000 primeiras linhas do DataFrame.



# 3) ALTERANDO O FORMATO DE LEITURA DO DATA SET

setwd("C:/Gabriel/trabalho/analise_tratamento_exploracao/linguagem_r_para_analise_de_dados/dados-covid-sp-master/data") # chamamos novamente o caminho do diretório onde se encontra o dataset para não ser gerado um erro ao chamar a leitura do arquivo. 
covid_sp <- read.csv2('dados_covid_sp.csv', sep =";", encoding = "UTF-8") # UTF-8 faz a leitura identificando a acentuação.
head(covid_sp) # visualizando as 6 primeiras linhas.



# 4) MODIFICANDO O NOME DAS VARIÁVEIS

covid_sp_alterado <- rename(covid_sp, municipio = nome_munic, data = datahora, casos_100_mil_habitantes = casos_pc , obitos_media_movel = obitos_mm7d, casos_media_movel = casos_mm7d, rotulo_mapa=map_leg, codigo_mapa = map_leg_s)   # alterando o nome das variáveis.

View(covid_sp_alterado)   # visualizando o dataframe completo.
head(covid_sp_alterado)   # visualizando as 6 primeiras linhas. 



# 5) EXCLUINDO COLUNAS

covid_sp_alterado$cod_ra <- NULL # excluir uma única coluna 
head(covid_sp_alterado)
# COMENTÁRIOS SOBRE O CÓDIGO

# Para deletar uma única coluna, podemos passar o atributo NULL a respectiva variável.

covid_sp_alterado <- select(covid_sp_alterado, -c(21:24))  # Exluindo intervalos de colunas
covid_sp_alterado <- select(covid_sp_alterado, -c(14,15,16,17))  # Exlcuindo várias colunas pelo seu número de identificação.
covid_sp_alterado <- subset(covid_sp_alterado, select = -c(codigo_ibge, casos_100_mil_habitantes, obitos_pc))  # Exluindo várias colunas pelo seu nome de identificação.
covid_sp_alterado <- subset(covid_sp_alterado, select = -c(dia,mes))  # Excluindo duas colunas pelo seu nome.
head(covid_sp_alterado)  # visualizando as 6 primeiras linhas do data frame.

# 6) EXPLORANDO O ERRO NOS DADOS

summary(covid_sp_alterado)

## DUPLICIDADE

duplicado = covid_sp_alterado[duplicated(covid_sp_alterado), ] # exibe as linhas duplicadas do data frame   
duplicado[,]  # exibindo o data frame duplicado

## CONSISTÊNCIA

municipios <- unique(covid_sp_alterado$municipio) # vetor contendo os municípios possíveis de SP

for(interador in municipios){
  covid_municipio <- covid_sp_alterado %>% filter(municipio == interador) # Filtrando as linhas correspondente ao municipio do interador.
  if(length(unique(covid_municipio$area)) > 1){ # Condição que avalia se o município em questão tem mais de um valor para a área.
    covid_municipio[,] # Exibindo município que contém área não constante.  
  }
}

# evidenciando a falta de consistência da semana_epidem
covid_sp_alterado[is.na(covid_sp_alterado$semana_epidem),] 

## COMPLETUDE

summary(covid_sp_alterado$casos_novos) 
boxplot(covid_sp_alterado$casos_novos, main="Gráfico BoxPlot", ylab="Casos Novos") # Gerando o boxplot da variável casos novos.
summary(covid_sp_alterado) 

## CONFORMIDADE
setwd("C:/gabriel/trabalho/analise_tratamento_exploracao/linguagem_r_para_analise_de_dados/dados-covid-sp-master/data")
banco_municipios <- read_excel("municipios.xls")  # importando o arquivo de dados contendo o nome dos municípios do Brasil.
banco_municipios <- banco_municipios[which(banco_municipios$Nome_UF == "São Paulo"), ]  # Filtrando as linhas apenas com os municipios de SP
banco_municipios <- select(banco_municipios, c(1, 13)) # Selecionando apenas as variáveis que informam o estado e o município do DataFrame
banco_municipios <- unique(banco_municipios$Nome_Município)  # criando um vetor com os valores únicos da variável Nome_Município  (contém todos os municípios de SP)

municipios_sp <- unique(covid_sp_alterado$municipio)  # Selecionando os valores únicos que aparecem na variável municipio do DataFrame covid_sp_alterado

FILTRO <- rep(TRUE, length(municipios_sp))  # Filtro para selecionar qual municipio não está presente no estado de SP.
contador = 1  # variável para acessar os índices do vetor FILTRO
flag = TRUE  # Define qual elemento do FILTRO pertence a região de SP.   

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

municipios_sp[FILTRO]  # substituindo o FILTRO no vetor de municípios, obtemos as regiões que não pertencem ao estado de SP. Percebemos que apenas o valor 'Ignorado' não pertence a região do estado de SP

covid_sp_alterado[which(covid_sp_alterado$municipio == "Ignorado"), ]  # Filtrando e exibindo as linhas apenas com os municipios de SP, cujo valor é 'ignorado'.


## VERIFICAÇÃO DOS DADOS AUSENTES E INDEFINIDOS
sapply(covid_sp_alterado, function(x) sum(is.na(x)))
sapply(covid_sp_alterado, function(x) sum(is.nan(x)))

## VERIFICAÇÃO DA CLASSE DAS VARIÁVEIS
glimpse(covid_sp_alterado) # verificando a classe das variáveis 



# 7) TRATANDO O ERRO DOS DADOS

## CONSISTÊNCIA 
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
covid_ignorado[,]  # demonstrando que não existe mais municípios ignorados

## DADOS AUSENTES 
sapply(covid_sp_alterado, function(x) sum(is.na(x)))

## TRATANDO A CLASSE DAS VARIÁVEIS 
covid_sp_alterado$semana_epidem <- as.integer(covid_sp_alterado$semana_epidem) # trucando a variável semana_epidem na classe inteiro. 
covid_sp_alterado$data <- as.Date(covid_sp_alterado$data, format ='%Y-%m-%d') # truncando a variável data na classe date.
str(covid_sp_alterado) # Exibindo a classe das variáveis pertencentes no data frame

# 8) CRIAÇÃO DE COLUNAS 
covid_sp_alterado["densidade_idoso"]<-covid_sp_alterado$pop_60/covid_sp_alterado$area # Resolve a expressão acima e cria a coluna 'idosos_densidade'
View(covid_sp_alterado) # visualizando os dados
covid_sp_alterado[,]

covid_sp_alterado["densidade_populacao"]<-covid_sp_alterado$pop/covid_sp_alterado$area # Resolve a expressão acima e cria a coluna 'idosos_densidade'
View(covid_sp_alterado) # visualizando os dados
head(covid_sp_alterado)

covid_sp_alterado["area"] <- covid_sp_alterado$area/100
covid_sp_alterado["densidade_idoso"]<-covid_sp_alterado$pop_60/covid_sp_alterado$area
covid_sp_alterado["densidade_populacao"]<-covid_sp_alterado$pop/covid_sp_alterado$area

# 9) EXPORTAÇÃO DO ARQUIVO 
write.table(covid_sp_alterado, file ="covid_sp_tratado.csv", sep = ",")
