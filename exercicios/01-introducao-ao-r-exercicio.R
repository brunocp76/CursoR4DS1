# Exercícios - Introdução ao R


# objetos -----------------------------------------------------------------
# (a) Crie um vetor com o nome de tres frutas e guarde em um objeto chamado frutas.

frutas <- c("uva", "morango", "laranja")


# (b) Use a função length() para contar o tamanho do vetor. Certifique-se que retorna 3.

length(frutas)


# (c) Use [] para retornar a primeira fruta do vetor.

frutas[1]


# (d) Inspecione a saída de 'paste("eu gosto de", frutas)' e responda se o tamanho do vetor mudou.

paste0("Eu gosto de ", frutas)
## O tamanho do vetor não muda, continua sendo de 3 elementos.



# data.frames -------------------------------------------------------------
# Use o data.frame airquality para responder às questões abaixo:
# (a) quantas colunas airquality tem?

ncol(airquality)
dim(airquality)[2]


# (b) quantas linhas airquality tem?

nrow(airquality)
dim(airquality)[1]


# (c) o que a função head() retorna?

## A função head retorna as N primeiras linhas (o valor padrão de N é 6)
head(airquality)


# (d) quais são os nomes das colunas?

names(airquality)


# (e) qual é a classe da coluna Ozone? Dicas: class() e '$'

class(airquality$Ozone)


# (f) o que o código 'dplyr::glimpse(airquality) retorna?

## O código 'dplyr::glimpse(airquality) retorna um "resumo-metadados" do data.frame airquality
dplyr::glimpse(airquality)
## Rows: 153
## Columns: 6
## $ Ozone   <int> 41, 36, 12, 18, NA, 28, 23, 19, 8, ...
## $ Solar.R <int> 190, 118, 149, 313, NA, NA, 299, 99...
## $ Wind    <dbl> 7.4, 8.0, 12.6, 11.5, 14.3, 14.9, 8...
## $ Temp    <int> 67, 72, 74, 62, 56, 66, 65, 59, 61,...
## $ Month   <int> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,...
## $ Day     <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, ...



# (g) utilizando a resposta da (f), quais são os tipos de
#     cada coluna da tabela airquality?

## TODAS são colunas numéricas...
## Ozone   é coluna "integer"
## Solar.R é coluna "integer"
## Wind    é coluna "double"
## Temp    é coluna "integer"
## Month   é coluna "integer"
## Day     é coluna "integer"



# vetores e funcoes ---------------------------------------------------------
# Analise as duas linhas de códigos abaixo:
c(1,2,3) - 1           # código 1
c(1,2,3) - c(1,1,1)    # código 2
# Os resultados são iguais? Porquê?

## Os resultados são iguais por que o R replica os elementos do vetor
## menor para fazer a conta de todos os elementos do vetor maior...


# Considere o vetor booleano abaixo:
dolar_subiu <- c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE)
# este vetor tem informação de uma semana (7 dias) indicando se o dolar subiu (TRUE)
# (ou não subiu - FALSE) no respectivo dia. Interprete os números abaixo:
# (a) length(dolar_subiu)

# [1] 7


# (b) dolar_subiu[2]  (ps: a semana começa no domingo)

# [1] TRUE


# (c) sum(dolar_subiu)

# [1] 4


# (d) mean(dolar_subiu)

# [1] 0.5714286


# Agora observe a saída de as.numeric(dolar_subiu). O que o R fez?

## O R forçou o tipo lógico para o tipo numérico, transformando FALSE em 0 e TRUE em 1.


# [desafio] Use o data.frame airquality para responder às questões abaixo:
# Vamos calcular o desvio padrão de Ozone sem usar a função sd().

## É possível utilizar uma propriedade estatística de que a variância é dada por
## Var[x] = E[(x)^2] - (E[x])^2 e aí calcular a raiz quadrada da variância...

sqrt(
   mean((airquality$Ozone) ^ 2, na.rm = TRUE) -
   mean(airquality$Ozone, na.rm = TRUE) ^ 2
   )
# [1] 32.84539


# Dica: guarde os resultados de cada item em objetos para poderem ser usados depois.
# (a) tire a média da coluna Ozone. Dica: use mean(x, na.rm = TRUE).

mean(airquality$Ozone, na.rm = TRUE)
# [1] 42.12931


# (b) subtraia da coluna Ozone a sua própria média (centralização).

airquality$Ozone -
   mean(airquality$Ozone, na.rm = TRUE)


# (c) eleve o vetor ao quadrado.

(airquality$Ozone -
      mean(airquality$Ozone, na.rm = TRUE)) ^ 2


# (d) tire a média do vetor.

mean((airquality$Ozone -
         mean(airquality$Ozone, na.rm = TRUE)) ^ 2, na.rm = TRUE)
# [1] 1078.819

# (e) tire a raíz quadrada.

sqrt(mean((airquality$Ozone -
              mean(airquality$Ozone, na.rm = TRUE)) ^ 2, na.rm = TRUE))
# [1] 32.84539
## Mesmo resultado da conta feita com a variância como função da esperança aplicada à coluna...


# (f) compare o resultado com sd(airquality$Ozone)

sd(airquality$Ozone, na.rm = TRUE)
# [1] 32.98788
## O resultado é ligeiramente diferente...
## Quero crer que seja um problema de precisão numérica...


# funcoes proprias ---------------------------------------------------------
# Crie uma função que receba um número e retorne o quadrado deste
# número.

quad <- function(numero){
   return(numero ^ 2)
}


# [desafio] Crie uma função que recebe 2 números x e y
# e devolve a raiz quadrada da soma desses números.
# Depois teste a função para valores (x=2, y=3).
# Dica: sqrt() é a função para raiz quadrada.

raiz_da_soma <- function(x, y){
   return(sqrt(x + y))
}

raiz_da_soma(x = 2, y = 3)
# [1] 2.236068
sqrt(5)
# [1] 2.236068
raiz_da_soma(x = 2, y = 3) - sqrt(5)
# [1] 0


# filtro e vetores logicos ------------------------------------------------

# use o vetor numeross abaixo para responder as questoes seguintes.
numeross <- -4:2

# (a) Escreva um código que devolva apenas valores positivos do vetor numeross.

numeross[numeross > 0]
# [1] 1 2


# (b) Escreva um código que devolta apenas os valores pares
#     do vetor numeross.
#     Dica: o operador %% devolve o resto da divisão entre dois números

numeross[numeross %% 2 == 0]
# [1] -4 -2  0  2


# (c) Filtre o vetor para que retorne apenas aqueles valores que quando
#     elevados a 2 são menores do que 4.

numeross[numeross ^ 2 < 4]



# Use o data.frame airquality para responder às questões abaixo
# (a) conte quantos NAs tem na coluna Solar.R. Dica: use '$', is.na() e sum()

## Já que a função is.na gera TRUEs e FALSEs que podem ser processados como zeros e uns...
sum(is.na(airquality$Solar.R))


# (b) filtre a tabela airquality com apenas linhas em que Solar.R é NA.

airquality[is.na(airquality$Solar.R), ]


# (c) filtre a tabela airquality com apenas linhas em que Solar.R NÃO é NA.

airquality[!is.na(airquality$Solar.R), ]


# (d) filtre a tabela airquality com apenas linhas em que Solar.R NÃO é NA e Month é igual a 5.

airquality[!is.na(airquality$Solar.R) & airquality$Month == 5, ]
