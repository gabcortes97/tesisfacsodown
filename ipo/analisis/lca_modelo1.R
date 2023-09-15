# preliminares

library(tidyverse)
library(naniar)
library(psych)
library(poLCA)

wvs <- read.csv("ipo/input/Datos/wvs.csv", header = T, sep = ";")

df <- wvs %>% dplyr::select(c(Q2, Q27, Q109, Q108, Q107, Q182, Q185, 
                              Q186, Q246, Q247, Q249, Q257, Q150, Y003)) %>%
  mutate(across(starts_with("Q"),~ifelse(.<0, NA, .))) 


# Descriptivos

psych::describe(df)


# Recodificación

df2 <- df %>% mutate(across(c(Q2, Q27, Q257), ~ifelse(.>2, 1, 2)),
                     Y003= ifelse(Y003<0, 2, 1),
                     across(c(Q182, Q185, Q186, Q246, Q247, Q249, Q108),
                            ~ifelse(.>5, 2,1)),
                     across(c(Q109, Q107), ~ifelse(.>5, 1, 2)))

psych::describe(df2)

f1 <- as.formula(cbind(Q109, Q107, Q108, Q246, Q247, Q249, Q182, Q185, Q186,
                       Y003, Q27, Q2, Q257, Q150)~1)

set.seed(09051931)
LCA1 <- poLCA(f1, data=df2, nclass=1, nrep=10, na.rm=T)
LCA2 <- poLCA(f1, data=df2, nclass=2, nrep=10, na.rm=T)
LCA3 <- poLCA(f1, data=df2, nclass=3, nrep=10, na.rm=T)
LCA4 <- poLCA(f1, data=df2, nclass=4, nrep=10, na.rm=T)
LCA5 <- poLCA(f1, data=df2, nclass=5, nrep=10, na.rm=T)
LCA6 <- poLCA(f1, data=df2, nclass=6, nrep=10, na.rm=T)


## 3 clases
lca3 <- purrr::map_dfr(LCA3$probs, ~.[, 2]) %>% 
  janitor::clean_names(.) %>% 
  mutate(Var = names(LCA3$probs)) %>% 
  relocate(Var)
lca3

## 4 clases
lca4 <- purrr::map_dfr(LCA4$probs, ~.[, 2]) %>% 
  janitor::clean_names(.) %>% 
  mutate(Var = names(LCA4$probs)) %>% 
  relocate(Var)
