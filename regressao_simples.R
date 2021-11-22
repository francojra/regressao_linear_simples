
# Regressão linear simples -----------------------------------------------------------------------------------------------------------------


# Carregando pacotes necessários -----------------------------------------------------------------------------------------------------------

library(tibble) # Pacote para ler tabela simplificada
library(dplyr) # Pacote para calcular a média, desvios, etc.
library(ggplot2) # Pacote para produzir os gráficos

# Carregando conjunto de dados -------------------------------------------------------------------------------------------------------------

datasets::trees # Escolher banco de dados disponível no R
tibble(trees) # Carrega bando de dados

# Descrição dos dados ----------------------------------------------------------------------------------------------------------------------

dados <- trees

dados %>%
  summarise(mean(Girth), mean(Height), mean(Volume),
            sd(Girth), sd(Height), sd(Volume),
            seG = sd(Girth) / sqrt(length(Girth)),
            seH = sd(Height) / sqrt(length(Height)),
            seV = sd(Volume) / sqrt(length(Volume)))


# Visualizando as correlações --------------------------------------------------------------------------------------------------------------

## Circunferência x Altura

ggplot(dados, aes(x = Girth, y = Height)) +
  geom_point()
  
## Circunferência x Volume

ggplot(dados, aes(x = Girth, y = Volume)) +
  geom_point()

## Volume x Altura

ggplot(dados, aes(x = Volume, y = Height)) +
  geom_point()


# Análises estatísticas --------------------------------------------------------------------------------------------------------------------


