# devoir
---
title: "elearning"
author: "FIANKO Kossi"
date: "13/11/2021"
output:
  pdf_document: default
  html_document: default
---

```{r}

```

# Importation des données
```{r echo=FALSE}
sit <- "https://www.dropbox.com/s/cdibrnhrkheyo5s/vlwb.csv?dl=1"
dd <- read.csv(sit, header = T, sep = ";", dec = ".")
library(tidyverse)
```

### Question 1
A partir des données dont vous disposez (voir plus bas) - combien de nourrissons présentent une IVH dans cet échantillon ?
Quelle proportion celà représente t'il?
```{r}
# Question 1
# le nombre de nourrisson presentant un ivh

Q1 <- function(){
  # le nombre de nourrisson presentant un ivh
  table(dd$ivh)
  df <- dd %>% filter(ivh == "definite" | ivh == "possible")
  a = nrow(df)
  print("le nombre est : ")
  print(a)
  # la proportion que cela represente
  b = a/(442 + 75+10)*100
  print("la proportion est :")
  print(b)
}

Q1()

```


### Question 2
Présentez un tableau où vous comparez les caractéristiques des nourrissons et/ou de leur maman en fonction de la présence ou non d'une IVH (vous pouvez choisir de regrouper la modalité "possible" soit avec "absent" ou au contraire avec "definite").
Vous décrirez les variables "bwt","gest", "twn", "sex", "race", "meth", "toc"
, "lol", "delivery", "apg1", "apg5", "vent", "dead".


```{r}
# Presentation d'un tableau comparatif
## reformulation du data selon les variables selectionnées pour étude

bm <- dd %>% select(ivh,bwt, gest, twn, sex, race, meth, toc, lol,delivery, apg1, apg5, vent, dead) #%>% na.omit()
bm$sex <- factor(bm$sex, levels = c("female", "FEMALE", "male", "Male", "MALE"),
                 labels = c("Female", "Female", "Male", "Male","Male"))

bm$ivh <- factor(bm$ivh, levels = c("absent" ,"definite" ,"possible"),
                 labels = c("absent", "present", "present"))

library(tableone)
library(survival)
var = c("bwt", "gest", "twn", "sex", "race", "meth", "toc", "lol","delivery", "apg1", "apg5", "vent", "dead")
fact = c("sex", "race","delivery")
tb1 <- CreateTableOne(vars = var, strata = "ivh", data = bm, factorVars = fact)
tb1
```


```{r}
# Representation avec gtsummary
library(gtsummary)
library(flextable)

bm <- na.omit(bm)
tg <- bm %>% tbl_summary(by = ivh,
                        statistic = list(all_continuous()~"{mean} ({sd}) [{sum}]",
                                         all_categorical() ~ "{n} / {N} ({p}%)"),
                        digits = all_continuous() ~ 2) %>% add_p() %>% 
  add_n() %>% modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~"**IVH**") %>%
  modify_caption("**Table: Tableau comparatif**") %>% bold_labels()
tg
```

### Question 3
En cas d'IVH, certains nourrissons sont asymptomatiques et d'autres peuvent présenter des anomalies difficilement détectables de la conscience, du tonus musculaire, de la respiration, des mouvements des yeux et des mouvements du corps.
Explorer si le score Apgar des nourrissons souffrants d'IVH sont changés par rapport aux autres nourrissons (n'hésitez pas à compléter par des approches statistiques pour comparer les groupes)

```{r}
# Question 3:

ag <- bm %>% select(ivh, apg1, apg5) %>% na.omit()

table(ag$apg1[ag$ivh == 'present'])
table(ag$apg5[ag$ivh == 'present'])

hist(ag$apg1,col=ag$ivh)
hist(ag$apg5)

A1= mean( ag$apg1[ag$ivh == 'absent'])
P1= mean( ag$apg1[ag$ivh == 'present'])
# la moyenne des nourrissons souffrants(P1) ou pas (A1) de la maladie ivh dans le score apg1
P1
A1
                ###########################################

A5= mean( ag$apg5[ag$ivh == 'absent'])
P5= mean( ag$apg5[ag$ivh == 'present'])
# la moyenne des nourrissons souffrants(P1) ou pas (A1) de la maladie ivh dans le score apg5
A5
P5
# Remarque: on observe une variation des moyennes entre les sujets presentant la maladies et ceux ne presantant pas la maladie 


```

