# Carregando bibliotecas --------------------------------------------------

library(tidyverse)


# Para pegar os dados -------------------
# install.packages("likert")
data("gap", package = "likert")
gap_long <- gap %>%
  tidyr::pivot_longer(-StudentId, names_to = "questao", values_to = "resposta")


# Olhando melhor os bancos de dados ---------------------------------------

gap %>%
   glimpse()

gap %>%
   head()

gap %>%
   tail()


gap_long %>%
   glimpse()

gap_long %>%
   head()

gap_long %>%
   tail()

# Use o gap_long para fazer os exercícios.
# PS: pivot_longer() é uma funcao que "derrete" o banco de dados, empilhando as colunas.
# vale a pena estudar o que essa funcao faz. Ela geralmente vai bem com ggplot2().


# Exercício 1 -------------------------------------------------------------
# Crie uma tabela com a contagem de cada resposta para cada questao.

gap_long %>%
   group_by(questao) %>%
   count(resposta) %>%
   ungroup() %>%
   arrange(questao, resposta)


# Exercício 2 -------------------------------------------------------------
# Usando a tabela acima, faca um grafico com uma barra horizontal para cada questao e
# com as contagens no eixo X. Pinte o preenchimento das barras representando as respostas (fill).

gap_long %>%
   group_by(questao) %>%
   count(resposta) %>%
   ungroup() %>%
   arrange(questao, resposta) %>%

   mutate(
      questao = forcats::fct_reorder2(questao, resposta, n)
   ) %>%
   ggplot(mapping = aes(x = n, y = questao, group = -resposta)) +
   geom_col(aes(fill = resposta)) +
   labs(
      title = "Resposta às Questões",
      x = "Quantidades por Resposta",
      y = "Questão"
   ) +
   theme(
      plot.title = element_text(hjust = 0.5)
   )


# Exercício 3 -------------------------------------------------------------
# [avancado] Mexendo com as categorias e as cores:
# a) transforme a coluna 'resposta' em factor() no mutate().
# b) adicione  + scale_fill_brewer(palette = "RdBu") no grafico.
# c) observe se uma escala indo de azul ao vermelho apareceu.

gap_long %>%
   group_by(questao) %>%
   count(resposta) %>%
   ungroup() %>%
   arrange(questao, resposta) %>%

   mutate(
      resposta = as.factor(resposta),
      questao = forcats::fct_reorder2(questao, resposta, n)
   ) %>%
   ggplot(mapping = aes(x = n, y = questao, group = -resposta)) +
   geom_col(aes(fill = resposta)) +
   labs(
      title = "Resposta às Questões",
      x = "Quantidades por Resposta",
      y = "Questão"
   ) +
   scale_fill_brewer(palette = "RdBu") +
   theme(
      plot.title = element_text(hjust = 0.5)
   )
