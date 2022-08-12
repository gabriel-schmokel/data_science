

## Exploração dos Dados

### 1) Carregando as Bibliotecas
library(dplyr) # Carregando a biblioteca dplyr. 

### 2) Carregando o Dataset
setwd("C:/gabriel/trabalho/churn analysis")  # Direcidando o caminho do diretório que contém o arquivo de leitura.
df_churn_analysis = read.csv("Churn.csv", sep = ";", na.strings="", stringsAsFactors=T)  # Lendo o arquivo Churn.csv
df_churn_analysis[,]  # exibindo todo o dataset

### 3) Modifiando o Nome das Variáveis
colnames(df_churn_analysis) = c("Id","score","estado","genero","idade","Patrimonio","Saldo","Produtos","TemCartCredito","ativo","salario","saiu") # Modificando o nome das colunas.
head(df_churn_analysis)  # Exibindo as seis primeiras linhas dos dados.

### 4) Levantamento das Variáveis Categóricas e Numéricas 
summary(df_churn_analysis)

### 5) Erros de Duplicidade
duplicado = df_churn_analysis[duplicated(df_churn_analysis$Id), ]  # exibe as linhas contendo os valores duplicados para o identificador do cliente.   
duplicado[,]  # exibindo o dataframe duplicado

### 6) Erros de Consistência 
counts = table(df_churn_analysis$genero)  # Cria um vetor que informa a frequência com que os valores aparecem na variável categórica Genero. 
barplot(counts, main="Gêneros", xlab="Gêneros")  # Cria uma gráfico de barras com as informações dos valores da variável categórica Genero.


### 7) Erros de Completude
summary(df_churn_analysis$idade) 
boxplot(df_churn_analysis$idade, main="Gráfico BoxPlot", ylab="Idade") # Gerando o boxplot da variável Idade.

### 8) Erros de conformidade
counts = table(df_churn_analysis$estado)  # Cria um vetor que informa a frequência com que os valores aparecem na variável categórica estado. 
barplot(counts, xlab="Estados")  # Cria uma gráfico de barras com as informações dos valores da variável categórica estado.


### 9) Identificando Valores Ausentes e Indefinidos
sapply(df_churn_analysis, function(x) sum(is.na(x))) # verificando as variáveis com valores ausentes, e computando o número total  de NA valores para essas variáveis.
df_churn_analysis[!complete.cases(df_churn_analysis),] # exibindo apenas os clientes com valores ausentes.
sapply(df_churn_analysis, function(x) sum(is.nan(x))) # verificando as variáveis com valores indefinidos, computando o número total  de NAN valores para essas variáveis.

### 10) Vericação da Classe das Variáveis
glimpse(df_churn_analysis) # verificando a classe das variáveis 

## Tratamento dos Dados
### 1) Tratando a Classe das Variáveis
df_churn_analysis$TemCartCredito <- as.factor(df_churn_analysis$TemCartCredito) # atribuindo a classe fator a variável TemCartCredito
df_churn_analysis$ativo <- as.factor(df_churn_analysis$ativo) # atribuindo a classe fator a variável ativo
df_churn_analysis$saiu <- as.factor(df_churn_analysis$saiu) # atribuindo a classe fator a variável saiu
glimpse(df_churn_analysis) # verificando novamente a classe das variáveis 

### 2) Tratando o Erro de Duplicidade
df_churn_analysis = df_churn_analysis[-c(82),]  # selecionando todas as linhas do df, menos a de íncide 82.
x =  df_churn_analysis[duplicated(df_churn_analysis$Id),] # verificando novamente se existem IDs duplicados
x[,] # exibindo os IDs duplicados

### 3) Tratando o Erro de Consistência
df_churn_analysis[is.na(df_churn_analysis$genero) | df_churn_analysis$genero == "M",]$genero = "Masculino"   # criando um filtro e atribuindo somente o valor Masculino quando aparece o valor M. ou um valor NA a variável genero
df_churn_analysis[df_churn_analysis$genero == "F" | df_churn_analysis$genero == "Fem", ]$genero = "Feminino" # criando um filtro e atribuindo somente o valor Feminino quando aparece o valor F ou Fem na variável Genero
df_churn_analysis$genero =   factor(df_churn_analysis$genero) #removendo níveis de valores nulos
summary(df_churn_analysis$genero) #visualizar os valores da variável genero

### 4) Tratando o Erro de Completude
df_churn_analysis[df_churn_analysis$idade<0 | df_churn_analysis$idade>110 ,]$idade = median(df_churn_analysis$idade) # filtrando os dados negativos e acima de 110 anos, e atribuindo a essas dados a mediana da variável idade
summary(df_churn_analysis)

### 5) Tratando o Erros de Conformidade
df_churn_analysis[!df_churn_analysis$estado %in% c("RS","SC","PR"),]$estado = "RS"  # criando um filtro para selecionar os clientes que não pertencem ao estado do RS, SC e PR. A esses clientes armazenamos o valor do estado do RS, que corresponde a moda da variável. 
df_churn_analysis$estado =   factor(df_churn_analysis$estado) # removendo níveis de valores nulos
summary(df_churn_analysis$estado)

### 6) Tratando Valores Ausentes
df_churn_analysis[is.na(df_churn_analysis$salario),]$salario = median(df_churn_analysis$salario,na.rm = T) #atribuindo a mediana ao NAs
df_churn_analysis[!complete.cases(df_churn_analysis$salario),] # evidenciando que não existem mais linhas com valores NAS para a variável salário.

## Exportação do projeto
write.table(df_churn_analysis, file ="churn_analysis_tratado.csv", sep = ",")










