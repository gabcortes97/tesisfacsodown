# preliminares

library(tidyverse)
library(naniar)
library(psych)
library(poLCA)

wvs <- read.csv("ipo/input/Datos/wvs.csv", header = T, sep = ";")

df <- wvs %>% dplyr::select(c(Q1, Q2, Q8, Q11, Q14, Q27, Q109, Q108, Q107, Q182, Q185, 
                       Q186, Q246, Q247, Q249, Q255, Q256, Q257, Q150)) %>%
  mutate(across(everything(),~ifelse(.<0, NA, .)))

var <- c("Q1", "Q2", "Q8", "Q11", "Q14", "Q27", "Q109", "Q108", "Q107", "Q182", "Q185", 
         "Q186", "Q246", "Q247", "Q249", "Q255", "Q256", "Q257", "Q150")

# casos perdidos



gg_miss_var(df)
gg_miss_upset(df)
vis_miss(df)
mcar_test(df)

missing <- wvs %>% dplyr::select(c(Q1, Q2, Q8, Q11, Q14, Q27, Q109, Q108, Q107, Q182, Q185, 
                              Q186, Q246, Q247, Q249, Q255, Q256, Q257, Q150, D_INTERVIEW, H_URBRURAL, Q260, Q262, Q275R, Q287, Q289)) %>%
  mutate(across(var,~ifelse(.<0, 1, 0)))

missing %>% group_by(Q260) %>% summarise(Q246= sum(Q246, na.rm=T),
                                        Q107= sum(Q107, na.rm=T),
                                        Q247= sum(Q247, na.rm=T),
                                        Q249= sum(Q107, na.rm=T),
                                        Q182= sum(Q182, na.rm=T))

missing %>% group_by(H_URBRURAL) %>% summarise(Q246= sum(Q246, na.rm=T),
                                         Q107= sum(Q107, na.rm=T),
                                         Q247= sum(Q247, na.rm=T),
                                         Q249= sum(Q107, na.rm=T),
                                         Q182= sum(Q182, na.rm=T))

missing %>% group_by(Q275R) %>% summarise(Q246= sum(Q246, na.rm=T),
                                               Q107= sum(Q107, na.rm=T),
                                               Q247= sum(Q247, na.rm=T),
                                               Q249= sum(Q107, na.rm=T),
                                               Q182= sum(Q182, na.rm=T))


# recodificación
df2 <- df %>% mutate(across(c(Q1, Q2, Q27, Q256, Q257, Q255), ~ifelse(.>2, 1, 2)),
                    across(c(Q8, Q11, Q14), ~ifelse(.==1, 2, 1)),
                    across(c(Q182, Q185, Q186,Q246, Q247, Q249, Q108),
                           ~ifelse(.>5, 2,1)),
                    across(c(Q109, Q107), ~ifelse(.>5, 1, 2)))

psych::describe(df2, ranges = F, skew=F)

# clases latentes

f1 <- as.formula(cbind(Q107, Q108, Q109, Q246, Q247, Q249, Q182, Q185, Q186,
                       Q8, Q11, Q14, Q27, Q1, Q2, Q256, Q257, Q255, Q150)~1)

set.seed(09051931)
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


## 4 clases
lca4 <- purrr::map_dfr(LCA4$probs, ~.[, 2]) %>% 
  janitor::clean_names(.) %>% 
  mutate(Var = names(LCA4$probs)) %>% 
  relocate(Var)

## 5 clases
lca5 <- purrr::map_dfr(LCA5$probs, ~.[, 2]) %>% 
  janitor::clean_names(.) %>% 
  mutate(Var = names(LCA5$probs)) %>% 
  relocate(Var)
