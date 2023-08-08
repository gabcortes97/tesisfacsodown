
wvs <- read.csv("wvs.csv", header = T, sep = ";")

library(tidyverse)
library(psych)
library(lavaan)

wvs %>% select(c(Q235:Q236)) %>% mutate(across(everything(), ~ifelse(.<0, NA, .))) %>% 
  cor(., use = "complete.obs")
wvs %>% select(c(Q235:Q236)) %>% mutate(across(everything(), ~ifelse(.<0, NA, .))) %>% 
  alpha()

df <- wvs %>% select(c(Q1:Q3, Q7:Q15, Q16:Q17, Q27, Q36, Q37, Q38, Q40, Q41, Q45:Q50, Q57:Q63, Q65:Q81, Q106:Q110, Q112, Q131, Q142, Q143, Q144, Q176:Q195, Q199, Q254:Q259,
                       Q150, Q149, Q250, Q235:Q239, Y003, Q32, Q196:Q198, Q241, Q244, 
                       Q246, Q247, Q249)) %>%
  mutate(Y003 = Y003,2,
         across(everything(),~ifelse(.<0, NA, .)))



# Legitimidad
legitimidad <- df %>% select(c(Q180, Q179, Q181, Q178, Q188 , Q184, Q185, Q186, Q182))
KMO(legitimidad)
fa.parallel(legitimidad, fm="ml", fa="both")
legit <- fa(legitimidad, nfactors = 2, fm="ml", rotate="varimax")
legit

legit2 <- fa(legitimidad, nfactors = 3, fm="ml", rotate="varimax")
legit2

modeloL <- "Individualismo Utilitario =~ Q180 + Q179 + Q181 + Q178
Individualismo Moral =~ Q188 + Q184
Individualismo Expresivo =~ Q185 + Q186 + Q182"

medicionL <- cfa(modeloL, data=legitimidad)
medicionL
fitmeasures(medicionL,
            fit.measures = c("rmsea", "cfi")) #rmsea= 0.068; cfi=0.973
semPaths(medicionL,
         # modelo ajustado
         what = "std",
         # mostrar cargas estandarizadas
         label.cex = 1, edge.label.cex = 1,
         # tamaño flechas y caracteres
         residuals = FALSE,
         # no mostrar residuos
         edge.color = "black")

# Concepciones
concepciones <- df %>% select(c(Q38, Q254:Q257, Q2, Q27, Q1, Q37, Q40))
KMO(concepciones)
fa.parallel(concepciones, fm="ml", fa="both")
concep <- fa(concepciones, nfactors = 5, fm="ml", rotate="varimax")
concep
concep2 <- fa(concepciones, nfactors = 3, fm="ml", rotate="varimax")
concep2

mc <- "Interdependencia Grupal =~ Q255 + Q256 + Q257
Interdependencia Relacional=~  Q37 + Q38 + Q40
"

medicionC <- cfa(mc, data=concepciones)
medicionC
fitmeasures(medicionC,
            fit.measures = c("rmsea", "cfi")) #rmsea= 0.045; cfi=0.989
summary(medicionC, standardized = T)
semPaths(medicionC,
         # modelo ajustado
         what = "std",
         # mostrar cargas estandarizadas
         label.cex = 1, edge.label.cex = 1,
         # tamaño flechas y caracteres
         residuals = FALSE,
         # no mostrar residuos
         edge.color = "black")



# Democracia
democracia <- df %>% select(c(Q235:Q239))
KMO(democracia)
fa.parallel(democracia, fm="ml", fa="both")
fad <- fa(democracia, nfactors = 2, fm="ml", rotate="varimax")
fad

mc <- "Interdependencia Grupal =~ Q255 + Q256 + Q257
Interdependencia Relacional=~  Q38 + Q37 + Q40
"

medicionC <- cfa(mc, data=concepciones)
medicionC
fitmeasures(medicionC,
            fit.measures = c("rmsea", "cfi")) #rmsea= 0.045; cfi=0.989
semPaths(medicionC,
         # modelo ajustado
         what = "std",
         # mostrar cargas estandarizadas
         label.cex = 1, edge.label.cex = 1,
         # tamaño flechas y caracteres
         residuals = FALSE,
         # no mostrar residuos
         edge.color = "black")

mod.ind <- modificationindices(medicionC)
# objeto con mi
head(mod.ind[order(mod.ind$mi, decreasing = T), ], 10)
summary(medicionC, standardized = T)

fae <- df %>% select(c(Q37, Q38, Q40, Q45,  Q48,  Q27))

KMO(fae)
fa.parallel(fae, fm="ml", fa="both")
exp <- fa(fae, nfactors = 2,
          fm = "ml", rotate = "varimax")
exp

######### ESTE ES EL MODELO######################


modelo4 <- "Individualismo Utilitario =~ Q180 + Q179 + Q181 + Q178
Individualismo Moral =~ Q188 + Q184
Individualismo Expresivo =~ Q185 + Q186 + Q182
Individualidad Delegativa =~ Q150
Interdependencia =~ Q255 + Q256 + Q257
Independencia=~  Q38 + Q37 + Q40
"


medicion <- cfa(modelo4, data = df)
medicion

fitmeasures(medicion,
            fit.measures = c("rmsea", "cfi"))  #RMSEA= 0.076; CFI= 0.912
mod.ind <- modificationindices(medicion)
# objeto con mi
head(mod.ind[order(mod.ind$mi, decreasing = T), ], 10)

summary(medicion, standardized = T)
inspect(medicion, what = "rsquare")

library(semPlot)
semPaths(medicion,
         # modelo ajustado
         what = "std",
         # mostrar cargas estandarizadas
         label.cex = 1, edge.label.cex = 1,
         # tamaño flechas y caracteres
         residuals = FALSE,
         # no mostrar residuos
         edge.color = "black")

modelo5 <- "Individualismo Utilitario =~ Q180 + Q179 + Q181 + Q178
Individualismo Moral =~ Q184 + Q185 + Q186 + Q182
Individualidad Delegativa =~ Q150
Autonomia =~ Q37 + Q38 + Q40"


medicion <- cfa(modelo5, data = df)
medicion

fitmeasures(medicion,
            fit.measures = c("rmsea", "cfi"))  #RMSEA= 0.076; CFI= 0.912
mod.ind <- modificationindices(medicion)
# objeto con mi
head(mod.ind[order(mod.ind$mi, decreasing = T), ], 10)

summary(medicion, standardized = T)
inspect(medicion, what = "rsquare")

library(semPlot)
semPaths(medicion,
         # modelo ajustado
         what = "std",
         # mostrar cargas estandarizadas
         label.cex = 1, edge.label.cex = 1,
         # tamaño flechas y caracteres
         residuals = FALSE,
         # no mostrar residuos
         edge.color = "black")




# Legitimidad
legitimidad <- df %>% select(c(Q177:Q195))
KMO(legitimidad)
fa.parallel(legitimidad, fm="ml", fa="both")
legit <- fa(legitimidad, nfactors = 4, fm="ml", rotate="varimax")
legit

legit2 <- fa(legitimidad, nfactors = 3, fm="ml", rotate="varimax")
legit2

modeloL <- "Individualismo Moral =~ Q191 + Q190 + Q189
Individualismo Expresivo =~ Q185 + Q182 + Q186
Individualismo Utilitario =~ Q180 + Q178"

medicionL <- cfa(modeloL, data=legitimidad)
medicionL
fitmeasures(medicionL,
            fit.measures = c("rmsea", "cfi")) #rmsea= 0.068; cfi=0.973
semPaths(medicionL,
         # modelo ajustado
         what = "std",
         # mostrar cargas estandarizadas
         label.cex = 1, edge.label.cex = 1,
         # tamaño flechas y caracteres
         residuals = FALSE,
         # no mostrar residuos
         edge.color = "black")

summary(medicionL, standardized = T)


df %>% select(c(Q191, Q190, Q189)) %>% alpha()
df %>% select(c(Q185, Q182, Q186)) %>% alpha()
df %>% select(c(Q180, Q178, Q177)) %>% alpha()
df %>% select(c(Q235, Q236)) %>% alpha()

df$dem_delegativa <- df %>% select(c(Q235, Q236)) %>%
  rowSums(., na.rm = FALSE, dims = 1)/2
summary(df$dem_delegativa)
hist(df$dem_delegativa)


sem <- "Individualismo Moral =~ Q191 + Q190 + Q189
Individualismo Expresivo =~ Q185 + Q182 + Q186
Individualismo Utilitario =~ Q180 + Q178
Individualidad Delegativa =~ Q150
Interdependencia Colectiva =~ Q255 + Q256 + Q257
Interdependencia Relacional=~  Q38 + Q37 + Q40
Individualismo Moral ~~ Individualismo Expresivo 
Individualismo Expresivo ~~ Individualismo Utilitario
Individualismo Utilitario ~~ Individualismo Moral
Interdependencia Colectiva ~~ Interdependencia Relacional
Democracia Delegativa =~ Q235 + Q236
Democracia Delegativa ~ Individualismo Expresivo + Individualismo Utilitario + Individualismo Moral + Individualidad Delegativa + Interdependencia Relacional + Interdependencia Colectiva
"
f_sem1 <- sem(sem, data = df)
summary(f_sem1, standardized = T, rsquare=T)

fitmeasures(f_sem1, c("chisq", "df", "pvalue", "cfi", "rmsea"))
modificationindices(f_sem1, sort. = T)

semPaths(f_sem1, residuals=F, # no mostrar residuos
         style = "OpenMX", layout = "tree", # estilo gráfico
         what="std", # mostrar coeficientes estandarizados
         weighted=F, label.cex=1, label.scale=F, # opciones gráficas
         edge.label.cex=1, cut=0, edge.width=1,
         posCol="black", negCol="black", edge.color="black",
         sizeMan = 6, rotation=2)
