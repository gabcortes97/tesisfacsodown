
#Hacer iun cambio porque si




dfa <- read.csv("wvs.csv", header = T, sep = ";")

library(tidyverse)
library(correlation)
library(psych)
library(lavaan)

dfb <- dfa %>% dplyr::select(c(Q7:Q17, Q27, Q36:Q41, Q46, Q48:Q55,
                      Q57:Q81, Q106:Q110, Q131, Q142:Q143,
                      Q177:Q182, Q184:Q186, Q188, Q191:Q193,
                      Q195, Q254:Q257, Q259, Q235:Q238,
                      Q245:Q246, Q248:Q249, Q251, Q57:Q63, Q131, Q142, Q143, Q106:Q110, Q260, Q235:Q239)) %>% mutate(across(everything(),~ifelse(.<=0, NA, .)))

library(correlation)
dfb %>% dplyr::select(c(Q235:Q238, Q239)) %>% correlation() %>% summary()
correlation(dfb[80:83]) %>% summary()

dfb %>% dplyr::select(c(Q235:Q238)) %>% fa.parallel(.,
            fm = 'ml', fa = 'fa')
fa <- dfb %>% dplyr::select(c(Q235:Q238)) %>% fa(., nfactors = 2,
   fm = "ml", rotate = "promax") %>% fa.diagram()
fa

dfb %>% dplyr::select(c(Q235:Q238, Q239)) %>% alpha(., check.keys = T)

dfb <- dfb %>% mutate(q238_i = case_when(Q238 == 1 ~ 4,
                                         Q238 == 2 ~ 3,
                                         Q238 == 3 ~ 2,
                                         Q238 == 4 ~ 1))

m1 <- 'd1=~ Q236+Q235
d2=~ q238_i+Q237'

#Individualismo utilitario
I_ut <- df %>% select(c(Q106:Q110, Q177:Q181, Q191))

a <- alpha(I_ut)

a

fa.parallel(I_ut,
            fm = 'ml', fa = 'fa')

aa <- fa(I_ut, nfactors = 2,
   fm = "ml", rotate = "varimax")
fa.diagram(aa)

#Individualismo moral
I_mor <- df %>% select(c(Q184, Q188, Q195))

b <- alpha(I_mor)

b

fa.parallel(I_mor,
            fm = 'ml', fa = 'fa')

bb <- fa(I_mor, nfactors = 1,
         fm = "ml", rotate = "varimax")
fa.diagram(bb)

#Individualismo expresivo
I_exp <- df %>% select(c(Q182, Q185, Q186, Q193))

c <- alpha(I_exp)

c

fa.parallel(I_exp,
            fm = 'ml', fa = 'fa')

cc <- fa(I_exp, nfactors = 1,
         fm = "ml", rotate = "varimax")
fa.diagram(cc)

#Legitimación
Legit <- df %>% select(c(Q185, Q186, Q182, Q188, Q184, Q195, Q180, Q179, Q181))


fa.parallel(Legit,
            fm = 'ml', fa = 'fa')

dd <- fa(Legit, nfactors = 3,
         fm = "ml", rotate = "varimax")
dd
fa.diagram(dd)

#Concepciones

concep <- df %>% select(c(Q7:Q17, Q27, Q37:Q41, Q254:Q257, Q259))
e <- alpha(concep, check.keys = T)
e


fa.parallel(concep,
            fm = 'ml', fa = 'fa')

ee <- fa(concep, nfactors = 1,
         fm = "ml", rotate = "varimax")
ee
fa.diagram(ee)

#Individualidad

indiv <- dfb %>% select(c(Q46, Q48, Q49, Q50,
                         Q131, Q142:Q143))

fa.parallel(indiv,
            fm = 'ml', fa = 'fa')

ff <- fa(indiv, nfactors = 1,
         fm = "ml", rotate = "varimax")
ff
fa.diagram(ff)

#Autoridad
auto <- dfb %>% select(c(Q235:Q238))

fa.parallel(auto,
            fm = 'ml', fa = 'fa')

gg <- fa(auto, nfactors = 2,
         fm = "ml", rotate = "varimax")
gg
fa.diagram(gg)

# Modelo

library(lavaan)

modelo <- 'Individualismo Utilitario =~ Q180+ Q179 +  Q181+ Q178
Individualismo Moral =~ Q188 + Q195
Individualismo Expresivo =~ Q185 + Q186 + Q182
Independencia=~ Q256 + Q255 + Q257 + Q254
Competencia =~ Q109 + Q110
Forma de Individualidad =~ Q142 + Q143
'

medicion <- cfa(m1, data = dfb)
medicion

library(semPlot)
# cargar paquete para graficar sem
semPaths(medicion,
         # modelo ajustado
         what = "std",
         # mostrar cargas estandarizadas
         label.cex = 1, edge.label.cex = 1,
         # tamaño flechas y caracteres
         residuals = FALSE,
         # no mostrar residuos
         edge.color = "black")

fitmeasures(medicion,
            fit.measures = c("rmsea", "cfi"))

mod.ind <- modificationindices(medicion)
# objeto con mi
head(mod.ind[order(mod.ind$mi, decreasing = T), ], 10)

mod_conf_cfa_std <- cfa(mod_conf, data=datos,
                        std.lv = T, std.ov = T,
                        meanstructure = T)

summary(medicion, standardized = T)
inspect(medicion, what = "rsquare")

basefa <- dfb %>% dplyr::select(c(Q180, Q179, Q181, Q178, Q188, Q184, Q195, Q185, Q186, Q182,
                           Q256, Q255, Q257, Q49, Q48,Q50, Q251, Q57:Q63, Q131, Q142, Q143))


library(haven)

KMO(basefa)
cortest.bartlett(basefa)

fa.parallel(basefa,
            fm = 'ml', fa = 'fa')

ind <- fa(basefa, nfactors = 7,
         fm = "ml", rotate = "varimax")
ind

fa.diagram(ind)

ind2 <- fa(basefa, nfactors = 4,
          fm = "ml", rotate = "varimax")
ind2

fa.diagram(ind2)

library(poLCA)

basecl <- dfb %>% dplyr::select(c(Q180, Q179, Q181, Q178, Q188, Q195,
      Q185, Q186, Q182,
      Q256, Q255, Q257,
      Q109, Q110,
      Q142, Q143, Q260, Q235:Q239)) %>% tibble()

head(basecl)
psych::describe(basecl)

baselca <- basecl %>% mutate(across(c(Q180, Q179, Q181, Q178, Q188, Q195,
                                      Q185, Q186, Q182,
                                      Q109, Q110), ~case_when(. <= 3 ~ 1,
                                                             . ==4|.==5 ~ 2,
                                                             . ==6|.==7 ~ 3,
                                                             . >7 ~ 4)))

psych::describe(baselca)

f <- cbind(Q180, Q179, Q181, Q178, Q188, Q195,
          Q185, Q186, Q182,
          Q256, Q255, Q257,
          Q109, Q110,
          Q142, Q143) ~ Q235+Q236+Q237+Q238+Q239

LCA1 <- poLCA(f, baselca, nclas=1, nrep = 4, graphs = T) #BIC=27438
LCA2<- poLCA(f, baselca, nclas=2, nrep = 4, graphs = T) #BIC= 25941
LCA3 <- poLCA(f, baselca, nclas=3, nrep = 5, graphs = T) #BIC= 25502
LCA4 <- poLCA(f, baselca, nclas=4, nrep = 4, graphs = T) #BIC= 25394
LCA5 <- poLCA(f, baselca, nclas=5, nrep = 50, graphs = T) #BIC= 25238
LCA5 <- poLCA(f, baselca, nclas=6, nrep = 4, graphs = T) #BIC= 25238

LCA5$predcell

poLCA.predcell(lc=LCA5, y=dfb)

LCA5[probs]
summary(LCA5$probs)
post <- poLCA.posterior(LCA5, baselca)
post <- tibble(post)

post <- post %>% mutate(classe = case_when(max == post[,1] ~ 1,
                                  max == post[,2] ~ 2,
                                  max == post[,3] ~ 3,
                                  max == post[,4] ~ 4,
                                  max == post[,5] ~ 5,))
basepost <- cbind(baselca, post)
class(basepost$classe)

basepost$sl <- ifelse(basepost$Q235 < 3, 1, 0)
basepost$cl2 <- ifelse(basepost$classe == 2, 1, 0)
basepost$classe <- as.factor(basepost$classe)

logit <- glm(sl ~ classe, data=basepost, family = binomial)
summary(logit)
coefficients(logit)


post$max <- apply(post, 1, max)

baselca <- tibble(basefa)

head(baselca)

baselca <- basefa %>% mutate(across(everything(), ~case_when(. <= 4 ~ 1,
                                                             . ==5|.==6 ~ 2,
                                                             . >=7 ~ 3)))

psych::describe(basefa)

LCA %>% ggplot(aes(x = etiquetas, fill = etiqueta_item, y = estimate)) + 
  geom_col() + 
  facet_wrap(~class, nrow = 1) + 
  scale_fill_viridis_d() + 
  theme_minimal() +
  theme(legend.position = "bottom") + 
  labs(fill = "Respuesta", 
       x = "¿Estaría dispuesto o no estaría dispuesto a
       permitir que en su casa vivieran personas ", 
       y = "Probabilidad de respuesta por clase") + 
  coord_flip()
  
f_aut <- cbind(Q235,Q236,Q237,Q238,Q239)~2

LCA2 <- poLCA(f_aut, baselca, nclas=4, nrep = 4, maxiter = 1000, graphs = T)

