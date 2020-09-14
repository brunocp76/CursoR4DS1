# Exercícios de manipulação de dados usando dplyr ################################################################

# Dicas:
# entre pelo .Rproj (cubo azul) do nosso projeto.
# as funções principais são os seis verbos: mutate(), filter(), arrange(), select(), group_by(), summarise()


# dados e pacotes -------------------------------------------------------------------------------------------------
# 0. carregue o pacote tidyverse

library(tidyverse)


# 1. Carregue os dados imdb utilizados em aula e dê uma olhada nela com a função glimpse().

imdb <- read_rds("dados/imdb.rds")

imdb %>%
  glimpse()


# 1. FILTER -------------------------------------------------------------------------------------------------------
# Retorne tabelas (tibbles) apenas com:

# 1.1. filmes coloridos anteriores a 1950;

imdb %>%
  filter(cor == "Color", ano < 1950)

# 1.2. filmes do "Woody Allen" ou do "Wes Anderson";

imdb %>%
  filter(diretor %in% c("Woody Allen", "Wes Anderson") |
           ator_1 %in% c("Woody Allen", "Wes Anderson") |
           ator_2 %in% c("Woody Allen", "Wes Anderson") |
           ator_3 %in% c("Woody Allen", "Wes Anderson"))


# 1.3. filmes do "Steven Spielberg" ordenados de forma decrescente por ano, mostrando apenas as colunas "titulo" e "ano";

imdb %>%
  filter(diretor == "Steven Spielberg" |
           ator_1 == "Steven Spielberg" |
           ator_2 == "Steven Spielberg" |
           ator_3 == "Steven Spielberg") %>%
  select(titulo, ano) %>%
  arrange(desc(ano))


# 1.4. filmes que tenham "Action" OU "Comedy" entre os seus gêneros;
# dica: str_detect()

imdb %>%
  filter(str_detect(string = generos,
                    pattern = "Action") |
           str_detect(string = generos,
                      pattern = "Comedy"))


# 1.5. filmes que tenham "Action" E "Comedy" (atenção ao E, e não OU) entre os seus gêneros e tenha nota_imdb maior que 8;

imdb %>%
  filter(str_detect(string = generos,
                    pattern = "Action"),
         str_detect(string = generos,
                    pattern = "Comedy"),
         nota_imdb > 8)


# 1.6. filmes que não possuem informação tanto de receita quanto de orçamento (isto é, possuem NA em ambas as colunas).

imdb %>%
  filter(is.na(orcamento),
         is.na(receita))


# 2. SUMMARISE/GROUP_BY ---------------------------------------------------------------------------------------------
# Retorne tabelas (tibbles) apenas com:

# 2.1. a nota média do imdb dos filmes por tipo de classificacao.

imdb %>%
  group_by(classificacao) %>%
  summarise(nota_media = mean(nota_imdb, na.rm = TRUE)) %>%
  ungroup()

# 2.2. a receita média e mediana dos filmes por ano.

imdb %>%
  group_by(ano) %>%
  summarise(receita_media = mean(receita, na.rm = TRUE),
            receita_mediana = median(receita, na.rm = TRUE)) %>%
  ungroup()


# 2.3. apenas o nome dos diretores com mais de 10 filmes.

imdb %>%
  count(diretor) %>%
  filter(n > 10, !is.na(diretor)) %>%
  # Bônus
  arrange(desc(n), diretor) %>%
  # Bônus
  select(diretor)


# 2.4. colunas título e ano, ordenada por ano em ordem decrescente.

imdb %>%
  select(titulo, ano) %>%
  arrange(desc(ano))


# 2.5. colunas título e nota_imdb, ordenada por nota_imdb em ordem crescente.

imdb %>%
  select(titulo, nota_imdb) %>%
  arrange(desc(nota_imdb), titulo)


# 2.6. Usando a função select (e seus ajudantes), escreva um código que retorne a base IMDB sem as colunas
# ator_1, ator_2 e ator_3 da base imdb. Escreva todas as soluções diferentes que você conseguir pensar.
# Desafio: enviar pelo menos 4 soluções diferentes.

imdb[, 1:12]

imdb %>%
  select(names(imdb)[1:12])

imdb %>%
  select(-ator_1, -ator_2, -ator_3)

imdb %>%
  select(-starts_with("ator"))



# 3. MUTATE ---------------------------------------------------------------------------------------------

# 3.1 Fazendo apenas uma chamada da função mutate(), crie as seguintes colunas novas no imdb:
#
# a) lucro = receita - orcamento

imdb %>%
  mutate(lucro = receita - orcamento)


# b) lucro_medio

imdb %>%
  mutate(lucro = receita - orcamento,
         lucro_medio = mean(lucro, na.rm = TRUE))


# c) lucro_relativo = (lucro - lucro_medio)/lucro_medio

imdb %>%
  mutate(lucro = receita - orcamento,
         lucro_medio = mean(lucro, na.rm = TRUE),
         lucro_relativo = (lucro - lucro_medio)/lucro_medio)


# d) houve_lucro = ifelse(lucro > 0, ...)

imdb %>%
  mutate(lucro = receita - orcamento,
         houve_lucro = ifelse(test = lucro > 0,
                              yes = "Sim",
                              no = "Não"))


# 4. (Avançado) SABESP ---------------------------------------------------------------------------------------------

# 4.1 Instale o pacote 'httr' com install.packages()

install.packages('httr', dependencies = TRUE)


# 4.2 Carregue o pacote 'httr' com library()

library(httr)


# 4.3 A função abaixo, chamada baixar_sabesp(), pega informações diretamente do site da SABESP.
# Crie essa função (apenas rode o código abaixo).
# Curiosidade: SABESP é a Cia. de Saneamento Básico do Estado de São Paulo.
baixar_sabesp <- function(data) {
  u_sabesp <- paste0("http://mananciais.sabesp.com.br/api/Mananciais/ResumoSistemas/", data)
  r_sabesp <- httr::GET(u_sabesp)
  results <- httr::content(r_sabesp, simplifyDataFrame = TRUE)
  results$ReturnObj$sistemas
}

# Dê uma olhada no que essa função está fazendo:
baixar_sabesp("2020-03-13")

# 4.4. Na linha abaixo, o objeto tab_sabesp recebe a tabela do dia `Sys.Date() - 1` (ontem)
# Após rodar as linhas abaixo, confira o que tem dentro dos objetos "ontem" e "tab_sabesp".
ontem <- Sys.Date() - 1
tab_sabesp <- baixar_sabesp(Sys.Date() - 1)


# 4.5. Arrumar os dados. Siga as etapas (pense como os seis verbos podem te ajudar!)
# a) selecione apenas as colunas Nome e PrecMensal. Use select().

tab_sabesp %>%
  select(Nome, PrecMensal)


# b) A coluna PrecMensal é um texto e está com vírgulas separando o decimal.
#    Precisamos substituir a "," por "." e depois transformar de texto para numérica.
#    use as funções str_replace(PrecMensal, ",", ".") e as.numeric() para isso.
#    OBS: tem que fazer essas transformações da coluna PrecMensal dentro do mutate()!

tab_sabesp %>%
  mutate(PrecMensal = as.numeric(str_replace(PrecMensal, ",", ".")))


# c) Ordene da maior precipitação para a menor usando arrange().

dados_arrumados <- tab_sabesp
  # O seu código %>%   mutate(PrecDia = as.numeric(str_replace(PrecDia, ",", ".")))
  # vai aqui %>%       mutate(PrecMensal = as.numeric(str_replace(PrecMensal, ",", ".")))
  # etc %>%            arrange(desc(PrecMensal), desc(PrecDia))
  # etc %>%
  # e tal


# 5. [extra] [não ensinado ainda] crie um gráfico de barras da tabela dados_arrumados criada acima.
library(ggplot2)
dados_arrumados <- tab_sabesp %>%
  mutate(PrecDia = as.numeric(str_replace(PrecDia, ",", "."))) %>%
  mutate(PrecMensal = as.numeric(str_replace(PrecMensal, ",", "."))) %>%
  arrange(desc(PrecMensal), desc(PrecDia))

dados_arrumados %>%
  ggplot() +
  geom_col(mapping = aes(x = PrecMensal, y = Nome))
