
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

modelo1 <- lm(data = dados, formula = Girth ~ Height)

modelo1$coefficients
# Resultado: efeito positivo de acordo com o estimate

summary(modelo1)
# Resultado: efeito significativo de acordo com o valor de p < 0.05
# R2 = 0.2697

modelo2 <- lm(data = dados, formula = Girth ~ Volume)

modelo2$coefficients
# Resultado: efeito positivo de acordo com o estimate

summary(modelo2)
# Resultado: efeito significativo de acordo com o valor de p < 0.05
# R2 = 0.9353

modelo3 <- lm(data = dados, formula = Volume ~ Height)

modelo3$coefficients
# Resultado: efeito positivo de acordo com o estimate
# R2 = 0.3579

summary(modelo3)
# Resultado: efeito significativo de acordo com o valor de p < 0.05
