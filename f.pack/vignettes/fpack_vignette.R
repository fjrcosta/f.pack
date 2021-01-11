## ----setup, include=FALSE-----------------------------------------------------
library(knitr)

## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- fig.height=6, fig.width=9,  fig.align="center"--------------------------

# Carregando a biblioteca:
library(f.pack)

# Definindo o algoritmo para a geração de números aleatórios:
library(setRNG)
setRNG(kind="Wichmann-Hill", seed=c(979,1479,1542), normal.kind="Box-Muller")

# Gerando a amostra e definindo as variáveis para o teste: 
dados=rnorm(n=100, mean=1570, sd=120)
sigma=sd(dados)
H0=1600
alfa=0.05

# Cálculo da estatística Z:
z.calc(dados, H0, sigma)

# Valor crítico para a estatística Z:
z.crit(alfa)

# Gráfico para auxiliar a decisão:
z.hip(alfa, dados, H0, sigma)

## ---- fig.height=6, fig.width=9,  fig.align="center"--------------------------
# carregando a biblioteca:
library(f.pack)

# Carregando os dados ("trutas"):
base=trutas

# Definindo o algoritmo para a geração de números aleatórios:
library(setRNG)
setRNG(kind="Wichmann-Hill", seed=c(979,1479,1542), normal.kind="Box-Muller")

# Extraído uma amostra aleatória e definindo as variáveis para o teste:

dados=sample(10, x=base$comprimento, replace = FALSE)
H0=15
alfa=0.025

# Cálculo da estatística T:
t_calc(dados, H0)

# Valor crítico para a estatística T:
t_crit(alfa, dados)

# Gráfico para auxiliar a decisão:
t_hip(alfa, dados, H0)

