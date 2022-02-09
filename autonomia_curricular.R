library(readr)  
library(haven)    
library(sas7bdat)
library(tidyr) # manipulação de dados básico    
library(dplyr) # manipulação de dados avançado   
library(ggplot2) # produção de gráficos
library(ggthemes) # tema dos gráficos
library(lme4) # modelos multiniveis
library(BIFIEsurvey) # PISA
```

# MESTRADO
```{r}

#Baixa os dados dos estudantes
pisa2015st <- read_sav("/dados1/pisa/CY6_MS_CMB_STU_QQQ.sav") 
#Baixa os dados das escolas
sh15  <- read_sav("/dados1/pisa/CY6_MS_CMB_SCH_QQQ.sav") 

#Seleciona dados dos estudantes e escola para a America Latina

lat <- c("BRA", "CHL", "COL", "CRI", "DOM", "MEX", "PER", "QAR", "TTO", "URY")

#cria as variaveis estudantes da america latina
p15lat <- pisa2015st %>% filter(CNT %in% lat) 
#cria os dados das escolas da america latina
sh15lat <- sh15 %>% filter(CNT %in% lat) 



#combina os dados dos alunos com a escola com a função merge, junta informação de duas tabelas
pisa_2015_lat <- merge(p15lat, sh15lat, by=c("CNT","CNTSCHID") ,all=TRUE) 

#filtra por país

```

```{r bra}

pisa_2015_BRA <- pisa_2015_lat %>% filter(CNT =="BRA")




####Artigo autonomia curricular

pisa_2015_BRA <- pisa_2015_lat %>% filter(CNT =="BRA")

# create BIFIE object
bifieobj.bra <- BIFIE.data.jack(pisa_2015_BRA,
                                jktype = "RW_PISA" ,
                                wgtrep="W_FSTURWT",
                                pvpre = paste0("PV",1:10),
                                cdata=FALSE )


mod0 <- BIFIE.twolevelreg( BIFIEobj=bifieobj.bra,
                           dep="SCIE",
                           formula.fixed  = ~ 1,
                           formula.random = ~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel2 = "W_SCHGRNRABWT",
                           wgtlevel1 = "W_FSTUWT",
                           se=TRUE)

summary(mod0)


mod1 <- BIFIE.twolevelreg( BIFIEobj=bifieobj.bra,
                           dep="SCIE",
                           formula.fixed  = ~ 1 + ESCS + factor(ST004D01T) + factor(IMMIG) + factor(SCHLTYPE),
                           formula.random = ~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel2 = "W_SCHGRNRABWT",
                           wgtlevel1 = "W_FSTUWT",
                           se=TRUE)

summary(mod1)




mod2 <- BIFIE.twolevelreg( BIFIEobj=bifieobj.bra,
                           dep="SCIE",
                           formula.fixed  = ~ 1 + ESCS + factor(ST004D01T) + factor(IMMIG) + factor(SCHLTYPE) + RESPCUR,
                           formula.random = ~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel2 = "W_SCHGRNRABWT",
                           wgtlevel1 = "W_FSTUWT",
                           se=TRUE)

summary(mod2)


mod3 <- BIFIE.twolevelreg( BIFIEobj=bifieobj.bra,
                           dep="SCIE",
                           formula.fixed  = ~ 1 + ESCS + factor(ST004D01T) + factor(SCHLTYPE) + RESPCUR,
                           formula.random = ~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel2 = "W_SCHGRNRABWT",
                           wgtlevel1 = "W_FSTUWT",
                           se=TRUE)

summary(mod3)






desc <- BIFIEsurvey::BIFIE.univar( bifieobj.bra, vars=c("ESCS","RESPCUR") )
summary(desc)

desc2 <- BIFIEsurvey::BIFIE.freq( bifieobj.bra, vars=c("ST004D01T", "SCHLTYPE", "IMMIG" )  )
summary(desc2)
