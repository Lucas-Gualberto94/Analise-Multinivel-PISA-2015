library(readr)  
library(haven)    
library(sas7bdat)
library(tidyr) # manipulação de dados básico    
library(dplyr) # manipulação de dados avançado   
library(ggplot2) # produção de gráficos
library(ggthemes) # tema dos gráficos
library(lme4) # modelos multiniveis
library(BIFIEsurvey) # PISA

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




# create BIFIE object
bifieobj.bra <- BIFIE.data.jack(pisa_2015_BRA,
                                jktype = "RW_PISA" ,
                                wgtrep="W_FSTURWT",
                                pvpre = paste0("PV",1:10),
                                cdata=FALSE )


mod_cheio <- BIFIE.twolevelreg( BIFIEobj=bifieobj.bra,
                                dep="SCIE",
                                formula.fixed  = ~ 1 + ESCS + factor(ST004D01T) + factor(SCHLTYPE) + TEACHSUP + TDTEACH + PERFEED + ADINST + IBTEACH,
                                formula.random = ~ 1,
                                idcluster="CNTSCHID",
                                wgtlevel2 = "W_SCHGRNRABWT",
                                wgtlevel1 = "W_FSTUWT",
                                se=TRUE)

summary(mod_cheio)

#analise descritiva dos dados
desc <- BIFIEsurvey::BIFIE.univar( bifieobj.bra, vars=c("ESCS", "TEACHSUP", "TDTEACH", "PERFEED", "ADINST", "IBTEACH") )
summary(desc)

desc2 <- BIFIEsurvey::BIFIE.freq( bifieobj.bra, vars=c("ST004D01T", "SCHLTYPE", "ST098Q01TA",
                                                       "ST098Q02TA", "ST098Q03NA", "ST098Q05TA", "ST098Q06TA", 
                                                       "ST098Q07TA", "ST098Q08NA", "ST098Q09TA"))
summary(desc2)


#inverter a escala likert de cada questão do IBTEACH
´´´´
library(dplyr)
pisa_2015_BRA <- pisa_2015_BRA %>%
  mutate(ST098Q01TAi=5-ST098Q01TA) %>%
  mutate(ST098Q02TAi=5-ST098Q02TA) %>%
  mutate(ST098Q03NAi=5-ST098Q03NA) %>%
  mutate(ST098Q05TAi=5-ST098Q05TA) %>%
  mutate(ST098Q06TAi=5-ST098Q06TA) %>%
  mutate(ST098Q07TAi=5-ST098Q07TA) %>%
  mutate(ST098Q08NAi=5-ST098Q08NA) %>%
  mutate(ST098Q09TAi=5-ST098Q09TA)

#IBTEACH

# create BIFIE object
bifieobj.bra <- BIFIE.data.jack(pisa_2015_BRA,
                                jktype = "RW_PISA" ,
                                wgtrep="W_FSTURWT",
                                pvpre = paste0("PV",1:10),
                                cdata=FALSE )


x <- BIFIE.twolevelreg( BIFIEobj=bifieobj.bra,
                        dep="SCIE",
                        formula.fixed  = ~ 1 + ESCS + factor(ST004D01T) + factor(SCHLTYPE) + factor(ST098Q01TAi) + factor(ST098Q02TAi)  + 
                          factor(ST098Q03NAi) + factor(ST098Q05TAi) + factor(ST098Q06TAi) + factor(ST098Q07TAi) + factor(ST098Q08NAi)  + 
                          factor(ST098Q09TAi) + TEACHSUP + TDTEACH + PERFEED + ADINST,
                        formula.random = ~ 1,
                        idcluster="CNTSCHID",
                        wgtlevel2 = "W_SCHGRNRABWT",
                        wgtlevel1 = "W_FSTUWT",
                        se=TRUE)

summary(x)