# Resumo: esse código tem como função fazer o tratamento, a exploração e a análise 
# dos dados presente no diretório 'data'

# instalando os pacotes para trabalhar com a exploração, manipulação e análise dos arquivos do dataset.
install.packages("dplyr") 

# carregando o pacote
library(dplyr)

# setwd referencia o diretório contendo os arquivos para a leitura
setwd("C:/Gabriel/trabalho/analise_tratamento_exploracao/linguagem_r_para_analise_de_dados/dados-covid-sp-master/data")

 
covid_sp <- read.csv('dados_covid_sp.csv', sep = ";")
# visualizando o data frame
View(covid_sp)

# na coluna nome do município percebemos as nomeclaturas erradas, pois não foi interpretada a acentuação. 
# podemos modificar adequadamente as nomeclaturas identificando a acentuação através da função read.csv2() com o encoding = 'UTF-8'
covid_sp <- read.csv2('dados_covid_sp.csv', sep = ";", encoding="UTF-8")
View(covid_sp)
# chamando o comando head() pra visualizar as primeiras linhas do data frame covid_sp
head(covid_sp)


# MODIFICANDO A NOMECLATURA DAS COLUNAS

# Modificando o índice do nome de algumas colunas, a fim de passar uma melhor interpretação na leitura do arquivo.
# Essa modiificação será salva em um novo data frame
covid_sp_alterado <- rename(covid_sp, municipio = nome_munic, data = datahora, rotulo=map_leg, codigo_mapa = map_leg_s)
View(covid_sp_alterado)


# EXCLUINDO COLUNAS
# Podemos excluir a coluna com código da região, já que iremos trabalhar o refenciamento de uma região apenas com o seu nome.
# Obs: o índice da coluna que possue o código da região é cod_ra.
covid_sp_alterado$cod_ra <- NULL

# Desejamos excluir outras colunas, como: rotulo, codigo_ibge, cod_drs, letalidade, nome_ra, cod_drs, pop, pop_60
# Na análise que iremos fazer não há necessidade de trabalhar com as colunas que serão deletadas.
# Para excluir essas colunas, iremos utilizar diferentes métodos

# excluindo a coluna pelo seu número de identificação.
covid_sp_alterado <- select(covid_sp_alterado, -c(21))
# exluindo várias colunas pelo seu nome de identificação.
covid_sp_alterado <- subset(covid_sp_alterado, select = -c(codigo_ibge, cod_drs))
# exlcuindo várias colunas pelo seu número de identificação.
covid_sp_alterado <- select(covid_sp_alterado, -c(14,15))
# exluindo um intervalos de colunas
covid_sp_alterado <- select(covid_sp_alterado, -c(17:19))


# EXCLUINDO LINHAS
# Desejamos exlucuir as linhas cuja coluna municipio apresente o resultado 'Ignorado'. Note, que não saber o municipio vai interir nos resultados da nossa análise, por isso a exclusão.
# Abaixo, segue as diferentes formas de excluir as linhas de um dataframe

# excluir uma linhas específica (deve-se passar a identificação da linha que se deseja excluir)
covid_sp_alterado <- slice(covid_sp_alterado, -c(239660))
# excluir um intervalo de linhas
covid_sp_alterado <- slice(covid_sp_alterado, -c(239661:239666))
# excluir um intervalo de linhas utilizando um filtro
covid_sp_alterado <- covid_sp_alterado %>% filter(municipio!="Ignorado")
View(covid_sp_alterado)


# VERIFICANDO VALORES AUSENTES (missing)
# NA = valores ausentes, os quais não são preenchidos
# NAN = not a number(valor indefinido)
# podemos visualizar através da função sapply() o número de valores ausentes
sapply(covid_sp_alterado, function(x) sum(is.na(x)))
sapply(covid_sp_alterado, function(x) sum(is.nan(x)))

# SUBSTITUINDO VALORES MISSING
# Deve-se antes carregar o pacote tidyr para fazer a substituição ds valres ausentes
install.packages("tidyr") 
library(tidyr)

# substituindo os dados ausentes por 54 usando a função mutate_all
# os valores alterados serão armazenados no novo data frame (covid_sp_alterado2)
covid_sp_alterado2 <- covid_sp_alterado %>% mutate_all(replace_na, 54)
View(covid_sp_alterado2)
# outra forma de alterar os dados ausentes é usando a função replace().
covid_sp_alterado2 <- replace(x = covid_sp_alterado,list = is.na(covid_sp_alterado),
                 values = 54)

# Como a semana_epidemia não vai somente até 54, a coluna que possue a variável data prova isso, é necessário utilizar uma condição associada a data 
# para atribuir a semana correta atribuida ao perído da epidemia. 
covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '2021-01-01' &
                                 covid_sp_alterado2$data <= '2021-01-07'  ] <- 54

covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '2021-01-08' &
                                   covid_sp_alterado2$data <= '2021-01-14'  ] <- 55

covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '2021-01-15' &
                                   covid_sp_alterado2$data <= '2021-01-21'  ] <- 56

covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '2021-01-22' &
                                   covid_sp_alterado2$data <= '2021-01-28'  ] <- 57

covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '2021-01-29' &
                                   covid_sp_alterado2$data <= '2021-02-04'  ] <- 58

covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '2021-02-05' &
                                   covid_sp_alterado2$data <= '2021-02-12'  ] <- 59

covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '2021-02-13' &
                                   covid_sp_alterado2$data <= '2021-02-20'  ] <- 60

covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '2021-02-21' &
                                   covid_sp_alterado2$data <= '2021-02-27'  ] <- 61

covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '2021-02-28' &
                                   covid_sp_alterado2$data <= '2021-03-6'  ] <- 62


# VERIFICAÇÃO E TRANSFORMAÇÂO DA TIPAGEM DOS ATRIBUTOS (Variáveis),

# Há duas formas de verificar a classe de uma variável
# Pela função str(...) ou pela glimpse(...)
str(covid_sp_alterado2)
glimpse(covid_sp_alterado2)

#Transformando a variável semana_epidem em inteira
covid_sp_alterado2$semana_epidem <- as.integer(covid_sp_alterado2$semana_epidem)
str(covid_sp_alterado2)

#Transformando a variável data que é carácter no tipo data
covid_sp_alterado2$data <- as.Date(covid_sp_alterado2$data, format ='%Y-%m-%d')
str(covid_sp_alterado2)

# CRIAÇÃO DE COLUNAS
# queremos criar uma nova coluna que informa o percentual de idosos presentes na população
covid_sp_alterado2["idoso(%)"]<-100*covid_sp_alterado2$pop_60/covid_sp_alterado2$pop
View(covid_sp_alterado2)

#Feita a exploração e o tratamento dos dados podemos exportar o data frame para fazer a análise estatística
#A exportação do data frame será direcionada ao local que inserimos pela função setwd("")
write.table(covid_sp_alterado2, file ="covid_sp_tratado.csv", sep = ",")


