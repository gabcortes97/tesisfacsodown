# preliminares

library(tidyverse)
library(naniar)
library(psych)
library(poLCA)

wvs <- read.csv("ipo/input/Datos/wvs.csv", header = T, sep = ";")

df <- wvs %>% dplyr::select(c(Q2, Q27, Q109, Q108, Q107, Q182, Q185, 
                              Q186, Q246, Q247, Q249, Q257, Q150, Q177, Q178)) %>%
  mutate(across(starts_with("Q"),~ifelse(.<0, NA, .))) 


# Descriptivos

psych::describe(df)


# Recodificación

df2 <- df %>% mutate(across(c(Q182, Q185, Q186, Q246, Q247, Q249, Q177, Q178),
                            ~ifelse(.>5, 2,1)),
                     across(c(Q109), ~ifelse(.>5, 1, 2)),
                     Q257= case_when(Q257==1 ~ 3,
                                     Q257==2 ~ 2,
                                     Q257>2 ~ 1),
                     Q108= case_when(Q108<5 ~ 1,
                                     Q108==5 | Q108==6 ~ 2,
                                     Q108>6 ~ 3),
                     Q27= case_when(Q27==1 ~ 3,
                                    Q27==2 ~ 2,
                                    Q27>2 ~ 1))

psych::describe(df2)

f1 <- as.formula(cbind(Q109, Q177, Q178, Q246, Q247, Q249, Q182, Q185, Q186,
                       Q108, Q27, Q257, Q150)~1)

set.seed(09051931)
LCA1 <- poLCA(f1, data=df2, nclass=1, nrep=10, na.rm=T)
LCA2 <- poLCA(f1, data=df2, nclass=2, nrep=10, na.rm=T)
LCA3 <- poLCA(f1, data=df2, nclass=3, nrep=10, na.rm=T)
LCA4 <- poLCA(f1, data=df2, nclass=4, nrep=10, na.rm=T, maxiter = 5000)
LCA5 <- poLCA(f1, data=df2, nclass=5, nrep=10, na.rm=T, maxiter= 5000)
LCA6 <- poLCA(f1, data=df2, nclass=6, nrep=10, na.rm=T, maxiter=10000)

LCA4

LCA5

LCA1

