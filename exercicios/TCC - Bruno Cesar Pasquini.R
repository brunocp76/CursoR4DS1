# Objetivo: fazer uma análise exploratória dos dados do IMDB.
#
# Contexto: imaginem que o Athos e o Fernando queiram investir na produção de um filme, mas eles não sabem nada de cinema. Eles tem interesse em produzir um filme que seja tanto um sucesso financeiro, quanto um sucesso de crítica, mas poderiam abandonar um dos critérios caso o outro seja muito atraente. Para isso, eles precisam saber quais os melhores diretores, atores e qual gênero de filme é mais propenso a uma produção de sucesso. Informações sobre duração e classificação etária também seriam interessantes para eles, assim como uma ideia do quanto eles precisariam desembolsar.
#
# Entrega: um relatório ou dashboard em HTML feito em R Markdown (ou flexdashboard).
#
# Prêmio: as três análises mais legais vão ganhar uma vaga em qualquer um dos nossos cursos ou workshops.
#
# Observações:
#
#    - Apenas o relatório/dashboard será avaliado. Embora vocês possam tirar dúvida sobre o código, não será preciso enviá-lo na entrega.
#
# - Embora todo conhecimento externo seja bem-vindo na hora da análise, utilizaremos como critério para determinação dos ganhadores apenas as conclusões e visualizações geradas a partir da base IMDB utilizada no curso.

# Pacotes -----------------------------------------------------------------

library(skimr)
library(plotly)
library(SmartEDA)
library(tidyverse)
library(DataExplorer)


# Base de dados -----------------------------------------------------------

imdb <- read_rds("dados/imdb.rds")


# Rápida ajeitada na base -------------------------------------------------

for (coluna in names(imdb)) {
   if (is.character(imdb[[coluna]])) {
      imdb[[coluna]] <- stringr::str_trim(imdb[[coluna]])
   }
}


# Definindo o Lucro dos filmes --------------------------------------------

imdb <- imdb %>%
   mutate(lucro = receita - orcamento)


# Jeito de ver a base -----------------------------------------------------

glimpse(imdb)
names(imdb)
View(imdb)
object.size(imdb)
skim(imdb)


# SmartEDA - Toda a Base --------------------------------------------------

ExpData(
   data = imdb,
   type = 1
)

ExpData(
   data = imdb,
   type = 2
)


# SmartEDA - Variáveis numéricas ------------------------------------------

ExpNumStat(
   data = imdb,
   gp = NULL,
   Qnt = c(0, 0.25, 0.50, 0.75, 1),
   MesofShape = 2,
   Outlier = TRUE,
   round = 2
)

ExpNumViz(
   data = imdb,
   target = NULL,
   type = 1,
   # nlim = NULL,
   # fname = NULL,
   Page = c(2, 4),
   scatter = TRUE,
   # sample = NULL,
   gtitle = "Examinando Variáveis Numéricas",
   theme = "Default"
)


# SmartEDA - Variáveis Categóricas ----------------------------------------

ExpCTable(
   data = imdb,
   Target = NULL,
   margin = 1,
   clim = 12,
   round = 2,
   per = TRUE
)

ExpCTable(
   data = imdb,
   Target = NULL,
   margin = 2,
   clim = 12,
   # nlim = NULL,
   round = 2,
   # bin = NULL,
   per = TRUE
)

ExpCatViz(
   data = imdb,
   target = NULL,
   fname = NULL,
   clim = 12,
   col = NULL,
   margin = 2,
   Page = c(1, 3),
   Flip = FALSE,
   sample = NULL,
   gtitle = "Examinando Variáveis Categóricas",
   theme = "Default"
)


# DataExplorer - Toda a Base ----------------------------------------------

# plot_str(
#    data = imdb,
#    type = "radial"
# )

plot_missing(
   data = imdb,
   title = "Examinando os dados faltantes da base",
   ggtheme = theme_dark()
)

# DataExplorer - Variáveis Numéricas --------------------------------------

plot_histogram(
   data = imdb,
   title = "Examaninando a densidade das variáveis contínuas"
)

plot_density(
   data = imdb,
   title = "Examaninando a densidade das variáveis contínuas"
)

plot_correlation(
   data = imdb,
   type = "all",
   maxcat = 10,
   title = "Examinando a correlação entre as variáveis",
   ggtheme = theme_dark()
)

plot_correlation(
   data = imdb,
   type = "continuous",
   title = "Examinando a correlação entre as variáveis",
   ggtheme = theme_dark()
)


# DataExplorer - Variáveis Categóricas ------------------------------------

plot_correlation(
   data = imdb,
   type = "discrete",
   maxcat = 10,
   title = "Examinando a correlação entre as variáveis",
   ggtheme = theme_dark()
)

plot_bar(
   data = imdb,
   maxcat = 50,
   order_bar = TRUE,
   title = "Examinando as variáveis categóricas",
   ggtheme = theme_dark()
)


# DataExplorer - Gerando um resumo da base --------------------------------

create_report(
   data = imdb,
   output_dir = getwd(),
   report_title = "Examinando a Base de Dados da IMDB"
)
