###############################################################################
###############################################################################
#Calculo do SPI para as estações da bacia com séries mais longas

bacias=read.csv("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Bacias hidrográficas/baciamun",
                header=T)
#Tenho que atribuir binária para os municípios da bacia do rio
#jundiaí: Salto (3545209), Itupeva (3524006), Jundiaí (3525904),
# Varzea Paulista (3556503), Campo Limpo Paulista (3509601),Mairiporã (3528502), Atibaia e Indaiatuba


#Estacoes dos municipios da bacia

estacoes=read.csv('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/1-Dados Brutos/Estacoes/Estacao.csv',
                  header=T)
#Verificar a coordenada das estações da bacia se eleas não se referem a municipios fora da região
#de interesse

salto=as.data.frame(c(2347145,2347123))
salto$Municipio='Salto'
colnames(salto)[1]=c('Codest')

itupeva=as.data.frame(c(2347057,2347013,2347230,2347232,2347231))
itupeva$Municipio='Itupeva'
colnames(itupeva)[1]='Codest'

jundiai=as.data.frame(c(2346097,2346111,2346156,2346012,2346013,2346294,
                        2346322,2346572,2346570,2346571,2346569,2346573))

jundiai$Municipio='Jundiai'
colnames(jundiai)[1]='Codest'

#Varzea paulista não possui estação

climpo=as.data.frame(c(2346257,2346289,2346490,2346491,2346489))
climpo$Municipio='Campo Limpo'
colnames(climpo)[1]='Codest'

maripora=as.data.frame(c(2346155,2346020,2346112,2346135,2346577,2346576,2346575))
maripora$Municipio='Mairipora'
colnames(maripora)[1]='Codest'

atibaia=as.data.frame(c(2346009,2346007,2346095,2346449,2346272,2346391,2346472,2346473,
                        2346471,2346474))
atibaia$Municipio='Atibaia'
colnames(atibaia)[1]='Codest'

indaiatuba=as.data.frame(c(2347106,2347007,2347223))
indaiatuba$Municipio='Indaiatuba'
colnames(indaiatuba)[1]='Codest'

bcjundiai=rbind(salto,itupeva,jundiai,climpo,maripora,atibaia,indaiatuba)



library(RMySQL)
prec=dbConnect(MySQL(),dbname='PLUVIOSIDADE',user='root',password='BcBm#1317',host='localhost')
dbListTables(prec)
dbListFields(prec,'Precipitacao2')
lprecipitacao=list()

for (i in 1:length(bcjundiai$Codest)){
k=bcjundiai$Codest[i]
precipitacao=dbGetQuery(prec,paste("Select *from Precipitacao2 where `Codigo`=",k,sep=""))
lprecipitacao[[i]]=precipitacao
}

estacoesbcj=do.call(rbind.data.frame,lprecipitacao)
#setwd('/home/bmiyamoto/Documentos/Pesquisa/Artigo Junior/Dados da estacoes BCJ')
#write.csv(estacoesbcj,file='Estações Pluviométricas - BRJ',row.names=F)

estacoesbcj=read.csv('/home/bmiyamoto/Documentos/Pesquisa/Artigo Junior/Dados da estacoes BCJ/Estações Pluviométricas - BRJ',
                     header=T)

library(tidyr)
estacoesbcj=separate(estacoesbcj,Data,c('Dia','Mes','Ano'),sep='/')
estacoesbcj=aggregate(estacoesbcj[,c(2)],by=list(Codigo=estacoesbcj$Codigo,Mes=estacoesbcj$Mes,
                                                 Ano=estacoesbcj$Ano),sum,na.rm=T)

estacoesbcj=merge(estacoesbcj,bcjundiai,by.x=c('Codigo'),by.y=c('Codest'),all.x=T,all.y=F)

# Quais são as estações com séries mais longas em cada um dos municípios da bacia?
#mun=estacoesbcj[estacoesbcj$Municipio=='Salto',]
#Atibaia = 2346095 (1940-)
#Jundiaí = 2346097 (1939-)
#Campo Limpo = 2346289 (1939-)
#Indaiatuba = 2347007 (1939-)
#Itupeva = 2347057 (1945-)
#Mairiporã = 2346020
#Salto = 2347145 (1972-)

estacoesbcj=estacoesbcj[(estacoesbcj$Codigo==2346095&estacoesbcj$Ano>=1940)|(estacoesbcj$Codigo==2346097&estacoesbcj$Ano>=1939)|
                          (estacoesbcj$Codigo==2346289&estacoesbcj$Ano>=1939)|(estacoesbcj$Codigo==2347007&estacoesbcj$Ano>=1939)|
                          (estacoesbcj$Codigo==2347057&estacoesbcj$Ano>=1945)|estacoesbcj$Codigo==2346020|
                          (estacoesbcj$Codigo==2347145&estacoesbcj$Ano>=1972),]

colnames(estacoesbcj)[4]='Precipitacao'



mun=as.data.frame(table(estacoesbcj$Codigo))
mun=as.numeric(levels(mun[,c(1)])[mun[,c(1)]])
lmunicipio=list()

library(SCI)
lmunicipio=list()

for (i in 1:length(mun)){
  k=mun[[i]]
  municipio=estacoesbcj[estacoesbcj$Codigo==k,]
  municipio$Mes=as.numeric(municipio$Mes)
  municipio$Ano=as.numeric(municipio$Ano)
  
    #Usei esse artificio da tabela composição para compor com
  #missings a série de balanço hídrico
  Mes=rep(1:12,42)
  Ano=rep(min(municipio$Ano,na.rm=T):max(municipio$Ano,na.rm=T),12)
  #Ordena um vetor
  Ano=sort(Ano)
  composicao=as.data.frame(cbind(Ano,Mes))
  #composicao$missing=NA
  composicao$Codigo=municipio$Codigo[1]
  composicao$Municipio=municipio$Municipio[1]
  
  municipio=merge(municipio,composicao,by=c('Codigo','Municipio','Ano','Mes'),all.x=T,all.y=T)
  municipio=municipio[order(municipio$Ano,municipio$Mes),]
  chuvamun=municipio[,c(5)]
  
  spi1=fitSCI(chuvamun,first.mon = 1,time.scale = 1,distr = "gamma",p0=T)
  spi1=transformSCI(chuvamun,first.mon = 1,obj=spi1)
  
  spi3=fitSCI(chuvamun,first.mon = 1,time.scale = 3,distr = "gamma",p0=T)
  spi3=transformSCI(chuvamun,first.mon = 1,obj=spi3)
  
  spi6=fitSCI(chuvamun,first.mon = 1,time.scale = 6,distr = "gamma",p0=T)
  spi6=transformSCI(chuvamun,first.mon = 1,obj=spi6)
  
  spi9=fitSCI(chuvamun,first.mon = 1,time.scale = 9,distr = "gamma",p0=T)
  spi9=transformSCI(chuvamun,first.mon = 1,obj=spi9)
  
  spi12=fitSCI(chuvamun,first.mon = 1,time.scale = 12,distr = "gamma",p0=T)
  spi12=transformSCI(chuvamun,first.mon = 1,obj=spi12)
  
  spi15=fitSCI(chuvamun,first.mon = 1,time.scale = 15,distr = "gamma",p0=T)
  spi15=transformSCI(chuvamun,first.mon = 1,obj=spi15)
  
  spi18=fitSCI(chuvamun,first.mon = 1,time.scale = 18,distr = "gamma",p0=T)
  spi18=transformSCI(chuvamun,first.mon = 1,obj=spi18)
  
  spi21=fitSCI(chuvamun,first.mon = 1,time.scale = 21,distr = "gamma",p0=T)
  spi21=transformSCI(chuvamun,first.mon = 1,obj=spi21)
  
  spi24=fitSCI(chuvamun,first.mon = 1,time.scale = 24,distr = "gamma",p0=T)
  spi24=transformSCI(chuvamun,first.mon = 1,obj=spi24)
  
  municipio=cbind(municipio,spi1,spi3,spi6,spi9,spi12,spi15,spi18,spi21,spi24)
  #Eureca!!! É isso!!!!
  lmunicipio[[i]]=municipio
}

estacoesbcj=do.call(rbind.data.frame,lmunicipio)
setwd('/home/bmiyamoto/Documentos/Pesquisa/Artigo Junior/SPI calculado para as estacoes pluviométricas')
write.csv(estacoesbcj,file='SPI para as Estações Pluviométricas - BRJ',row.names=F)


#######################################################################################
#####################################################################################
##############SPI para os dados interpolados desde 1940

library(RMySQL)
prec=dbConnect(MySQL(),user='root',password='BcBm#1317',host='localhost',dbname='PLUVIOSIDADE')
dbListFields(prec,'Precipitacao6')
lprecipitacao=list()

#Tenho que chamar o grid de minicipios do brasil
mun=read.csv('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/lat_long_municipiosbrasil.csv',
             header=T,encoding = "latin1")
#Tenho que atribuir binária para os municípios da bacia do rio
#jundiaí: Salto (3545209) (lat 23 12 long 47 17), Itupeva (3524006) (lat 23 09 long 47 03), Jundiaí (3525904) (lat 23 11 long 46 53),
# Varzea Paulista (3556503) ( lat 23 12 long 46 49), Campo Limpo Paulista (3509601) (lat 23 12 long 46 47),Mairiporã (3528502) (lat 23 19 long 46 35), 
#Atibaia (lat 23 07 long 46 33) e Indaiatuba (lat 23 05 long 47 13)
mun2=c(3504107,3545209,3524006,3525904,3556503,3509601,3528502,3520509)
lprecipitacao=list()
for (i in 1:length(mun2)){
k=mun2[[i]]
precipitacao=dbGetQuery(prec,paste("select *from Precipitacao5 where `Codigo`=",k,sep=""))
lprecipitacao[[i]]=precipitacao
}

precipitacao=do.call(rbind.data.frame,lprecipitacao)

precipitacao=precipitacao[precipitacao$Ano>=1940&precipitacao$Ano<2015,c(2,3,7,8,9)]


mun=as.data.frame(table(precipitacao$Codigo))
mun=as.numeric(levels(mun[,c(1)])[mun[,c(1)]])
lmunicipio=list()

library(SCI)
lmunicipio=list()

for (i in 1:length(mun)){
  k=mun[[i]]
  municipio=precipitacao[precipitacao$Codigo==k,]
  municipio$Mes=as.numeric(municipio$Mes)
  municipio$Ano=as.numeric(municipio$Ano)
  
  #Usei esse artificio da tabela composição para compor com
  #missings a série de balanço hídrico
  Mes=rep(1:12,42)
  Ano=rep(min(municipio$Ano,na.rm=T):max(municipio$Ano,na.rm=T),12)
  #Ordena um vetor
  Ano=sort(Ano)
  composicao=as.data.frame(cbind(Ano,Mes))
  #composicao$missing=NA
  composicao$Codigo=municipio$Codigo[1]
  composicao$mun=municipio$mun[1]
  
  municipio=merge(municipio,composicao,by=c('Codigo','mun','Ano','Mes'),all.x=T,all.y=T)
  municipio=municipio[order(municipio$Ano,municipio$Mes),]
  chuvamun=municipio[,c(5)]
  
  spi1=fitSCI(chuvamun,first.mon = 1,time.scale = 1,distr = "gamma",p0=T)
  spi1=transformSCI(chuvamun,first.mon = 1,obj=spi1)
  
  spi3=fitSCI(chuvamun,first.mon = 1,time.scale = 3,distr = "gamma",p0=T)
  spi3=transformSCI(chuvamun,first.mon = 1,obj=spi3)
  
  spi6=fitSCI(chuvamun,first.mon = 1,time.scale = 6,distr = "gamma",p0=T)
  spi6=transformSCI(chuvamun,first.mon = 1,obj=spi6)
  
  spi9=fitSCI(chuvamun,first.mon = 1,time.scale = 9,distr = "gamma",p0=T)
  spi9=transformSCI(chuvamun,first.mon = 1,obj=spi9)
  
  spi12=fitSCI(chuvamun,first.mon = 1,time.scale = 12,distr = "gamma",p0=T)
  spi12=transformSCI(chuvamun,first.mon = 1,obj=spi12)
  
  spi15=fitSCI(chuvamun,first.mon = 1,time.scale = 15,distr = "gamma",p0=T)
  spi15=transformSCI(chuvamun,first.mon = 1,obj=spi15)
  
  spi18=fitSCI(chuvamun,first.mon = 1,time.scale = 18,distr = "gamma",p0=T)
  spi18=transformSCI(chuvamun,first.mon = 1,obj=spi18)
  
  spi21=fitSCI(chuvamun,first.mon = 1,time.scale = 21,distr = "gamma",p0=T)
  spi21=transformSCI(chuvamun,first.mon = 1,obj=spi21)
  
  spi24=fitSCI(chuvamun,first.mon = 1,time.scale = 24,distr = "gamma",p0=T)
  spi24=transformSCI(chuvamun,first.mon = 1,obj=spi24)
  
  municipio=cbind(municipio,spi1,spi3,spi6,spi9,spi12,spi15,spi18,spi21,spi24)
  #Eureca!!! É isso!!!!
  lmunicipio[[i]]=municipio
}

estacoesbcj=do.call(rbind.data.frame,lmunicipio)
setwd('/home/bmiyamoto/Documentos/Pesquisa/Artigo Junior/SPI para os municipios 1940-2014')
write.csv(estacoesbcj,file='SPI para os municípios - BRJ (1940-2014)',row.names=F)


####################################################################################
####################################################################################
##Junior quer que mande todos os dados climáticos que eu tenho da bacia, tanto os interpolados
#quanto os originais. Vou checar se tenho dados do rio jundiai

#Incorporar os arquivos CSV de cotas de rios a um banco SQL
library(RSQLite)

setwd('/home/bmiyamoto/Documentos/Pesquisa/Artigo Junior/Base fluviométrica')
cota=dbConnect(SQLite(),dbname='COTAS')

#Dados txt
setwd('/home/bmiyamoto/Documentos/Pesquisa/Artigo Junior/Dados fluviométricos Brasil - ANA/Dados/Cotas/Dados')
cot=list.files(patter='')

#Salvar em um banco SQLite
for (i in 1:length(cot)){
dbWriteTable(conn=cota,name='Cota1',value=as.data.frame(read.csv(cot[i],skip=16,sep=';',header=T)),append=T)
}

##########################################
#Preciso saber quais estações estão na bacia do rio jundiai
 
#estacoes fluviais
estacoes=read.csv('/home/bmiyamoto/Documentos/Pesquisa/Artigo Junior/Dados fluviométricos Brasil - ANA/Estacoes/Estacaofluvio.csv',
                  header=T)

#Com isso eu consigo delimitar aproximadamente as estações fluviométricas da bacia
estacoes=estacoes[estacoes$Latitude>(-23.20)&estacoes$Latitude<(-23.10)&
                    estacoes$Longitude>(-47.20)&estacoes$Longitude<(-46.30),]

#Carregar dados de cotas do brasil
library(RSQLite)
setwd('/home/bmiyamoto/Documentos/Pesquisa/Artigo Junior/Base fluviométrica')
cota=dbConnect(SQLite(),dbname='COTAS')

cot=dbGetQuery(cota,"select *from Cota1")

#Selecionar apenas as estações referentes as coordenadas delimitadas anteriormente
cot=merge(cot,estacoes,by.x=c('X..EstacaoCodigo'),by.y=c('Codigo'),all.x=F,all.y=F)
cot=cot[,c(1:4,81,83,84,5:78)]

setwd('/home/bmiyamoto/Documentos/Pesquisa/Artigo Junior/Dados dos rios da BRJ')
write.csv(cot,file='Dados diários de Cota dos Rios da Bacia',row.names=F)

#Dados das estacoes pluviométricas da Bacia ANA
estacoesbcj=read.csv('/home/bmiyamoto/Documentos/Pesquisa/Artigo Junior/Dados da estacoes BCJ/Estações Pluviométricas - BRJ',
                     header=T)

library(tidyr)
estacoesbcj=separate(estacoesbcj,Data,c('Dia','Mes','Ano'),sep='/')
estacoesbcj=aggregate(estacoesbcj[,c(2)],by=list(Codigo=estacoesbcj$Codigo,Mes=estacoesbcj$Mes,
                                                 Ano=estacoesbcj$Ano),sum,na.rm=T)

estacoesbcj=merge(estacoesbcj,bcjundiai,by.x=c('Codigo'),by.y=c('Codest'),all.x=T,all.y=F)

estacoes=read.csv('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/1-Dados Brutos/Estacoes/Estacao.csv',
                  header=T)

estacoesbcj=merge(estacoesbcj,estacoes,by=c('Codigo'),all.x=T,all.y=F)
estacoesbcj=estacoesbcj[,c(1,6:8,2:4)]
colnames(estacoesbcj)[7]='Precipitacao'

setwd('/home/bmiyamoto/Documentos/Pesquisa/Artigo Junior/Dados de todas as estações pluviometricas da ANA na BRJ')
write.csv(estacoesbcj,'Dados pluviométricos das estações presentes na BRJ - ANA',row.names=F)

#######################################################
#Outros dados climáticos interpolados para os municípios da BRJ

#Dados interpolados
load('/home/bmiyamoto/Documentos/Pesquisa/Tese/INMET/Medidas/Arquivos finais/dadosclimaanobr.RData')

climaBRJ=dadosclimaanobr[dadosclimaanobr$Codigo==3504107|dadosclimaanobr$Codigo==3545209|
                                dadosclimaanobr$Codigo==3524006|dadosclimaanobr$Codigo==3525904|
                                dadosclimaanobr$Codigo==3556503|dadosclimaanobr$Codigo==3509601|
                                dadosclimaanobr$Codigo==3528502|dadosclimaanobr$Codigo==3520509,]
climaBRJ=climaBRJ[,c(1:8,10)]

setwd('/home/bmiyamoto/Documentos/Pesquisa/Artigo Junior/Dados climáticos do INMET interpolados')
write.csv(climaBRJ,file='Dados climáticos interpolados a partir de estacoes do INMET',row.names=F)


###SPI para todas as estações da BCJ com séries com mais de 30 anos
####################################################################

estacoesbcj=read.csv('/home/bmiyamoto/Documentos/Pesquisa/Artigo Junior/Dados da estacoes BCJ/Estações Pluviométricas - BRJ',
                     header=T)

library(tidyr)
estacoesbcj=separate(estacoesbcj,Data,c('Dia','Mes','Ano'),sep='/')
estacoesbcj=aggregate(estacoesbcj[,c(2)],by=list(Codigo=estacoesbcj$Codigo,Mes=estacoesbcj$Mes,
                                                 Ano=estacoesbcj$Ano),sum,na.rm=T)


colnames(estacoesbcj)[4]='Precipitacao'

#Selecionar as estações com séries iguais ou superiores a 30 anos
estacoes30=as.data.frame(table(estacoesbcj$Codigo))
estacoes30$Freq=estacoes30$Freq/12
estacoes30=estacoes30[estacoes30$Freq>=30,]

estacoesbcj=merge(estacoesbcj,estacoes30,by.x=c('Codigo'),by.y=c('Var1'))

mun=as.data.frame(table(estacoesbcj$Codigo))
mun=as.numeric(levels(mun[,c(1)])[mun[,c(1)]])

library(SCI)
lmunicipio=list()

for (i in 1:length(mun)){
  k=mun[[i]]
  municipio=estacoesbcj[estacoesbcj$Codigo==k,]
  municipio$Mes=as.numeric(municipio$Mes)
  municipio$Ano=as.numeric(municipio$Ano)
  
  #Usei esse artificio da tabela composição para compor com
  #missings a série de balanço hídrico
  Mes=rep(1:12,42)
  Ano=rep(min(municipio$Ano,na.rm=T):max(municipio$Ano,na.rm=T),12)
  #Ordena um vetor
  Ano=sort(Ano)
  composicao=as.data.frame(cbind(Ano,Mes))
  #composicao$missing=NA
  composicao$Codigo=municipio$Codigo[1]
  composicao$Municipio=municipio$Municipio[1]
  
  municipio=merge(municipio,composicao,by=c('Codigo','Ano','Mes'),all.x=T,all.y=T)
  municipio=municipio[order(municipio$Ano,municipio$Mes),]
  chuvamun=municipio[,c(4)]
  
  spi1=fitSCI(chuvamun,first.mon = 1,time.scale = 1,distr = "gamma",p0=T)
  spi1=transformSCI(chuvamun,first.mon = 1,obj=spi1)
  
  spi3=fitSCI(chuvamun,first.mon = 1,time.scale = 3,distr = "gamma",p0=T)
  spi3=transformSCI(chuvamun,first.mon = 1,obj=spi3)
  
  spi6=fitSCI(chuvamun,first.mon = 1,time.scale = 6,distr = "gamma",p0=T)
  spi6=transformSCI(chuvamun,first.mon = 1,obj=spi6)
  
  spi9=fitSCI(chuvamun,first.mon = 1,time.scale = 9,distr = "gamma",p0=T)
  spi9=transformSCI(chuvamun,first.mon = 1,obj=spi9)
  
  spi12=fitSCI(chuvamun,first.mon = 1,time.scale = 12,distr = "gamma",p0=T)
  spi12=transformSCI(chuvamun,first.mon = 1,obj=spi12)
  
  spi15=fitSCI(chuvamun,first.mon = 1,time.scale = 15,distr = "gamma",p0=T)
  spi15=transformSCI(chuvamun,first.mon = 1,obj=spi15)
  
  spi18=fitSCI(chuvamun,first.mon = 1,time.scale = 18,distr = "gamma",p0=T)
  spi18=transformSCI(chuvamun,first.mon = 1,obj=spi18)
  
  spi21=fitSCI(chuvamun,first.mon = 1,time.scale = 21,distr = "gamma",p0=T)
  spi21=transformSCI(chuvamun,first.mon = 1,obj=spi21)
  
  spi24=fitSCI(chuvamun,first.mon = 1,time.scale = 24,distr = "gamma",p0=T)
  spi24=transformSCI(chuvamun,first.mon = 1,obj=spi24)
  
  municipio=cbind(municipio,spi1,spi3,spi6,spi9,spi12,spi15,spi18,spi21,spi24)
  #Eureca!!! É isso!!!!
  lmunicipio[[i]]=municipio
}

estacoesbcj=do.call(rbind.data.frame,lmunicipio)

#Atribuir coordenadas as estacoesbcj
estacoesnomes=read.csv('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/1-Dados Brutos/Estacoes/Estacao.csv',
                    header=T)

estacoesbcj=merge(estacoesbcj,estacoesnomes,by=c('Codigo'))
estacoesbcj=estacoesbcj[,c(1,16:18,2:4,6:14)]

setwd('/home/bmiyamoto/Documentos/Pesquisa/Artigo Junior/SPI calculado para as estacoes pluviométricas')
write.csv(estacoesbcj,file='SPI para todas as Estações Pluviométricas da BRJ com séries maiores do que 30 anos',row.names=F)

#####################################################################
#####################################################################
#####################################################################
###Mann-Kendall modificado para os municípios que o junior 
## utilizou

#Atibaia, Cabreuva, Campo Limpo Paulista, Indaiatuba, Itu, Jarinu,
# Jundiai, Mairiporã, Salto, Várzea Paulista

spi=read.csv('/home/bmiyamoto/Documentos/Pesquisa/Artigo- Gori_e_Junior/Base final - Pluviosidade/SPI São Paulo 1961-2014',
             header=T)
precipitacao=spi[spi$mun=='ATIBAIA'|spi$mun=='CABREÚVA'|spi$mun=='CAMPO LIMPO PAULISTA'|spi$mun=='INDAIATUBA'|
          spi$mun=='ITU'|spi$mun=='JARINU'|spi$mun=='JUNDIAÍ'|spi$mun=='MAIRIPORÃ'|spi$mun=='SALTO'|
          spi$mun=='VÁRZEA PAULISTA', c(1,2,4,5,11)]
precipitacao$mun=as.character(precipitacao$mun)


library(fume)

codigomun=as.data.frame(table(precipitacao$Codigo))
codigomun=as.numeric(levels(codigomun[,c(1)])[codigomun[,c(1)]])
lprecmun=list()

for (i in 1:length(codigomun))
{
  municipio=codigomun[[i]]
  precmun=precipitacao[precipitacao$Codigo==municipio,]
  precmun=precmun[!is.na(precmun$Codigo),]
  precmun=precmun[order(precmun$Ano,precmun$Mes),]
  mk12=precmun[,c(5)]
  mk12=mkTrend(mk12,ci=0.95)
  mk=cbind(precmun$Codigo[1],as.character(precmun$mun[1]),mk12$Z,mk12$p.value,mk12$Zc,mk12$`Corrected p.value`,
           mk12$tau,mk12$`N/N*s`,mk12$`Sen's Slope`)
  
  colnames(mk)=c('Codigo','Municipio','mk12Z','mk12p.value','mk12Zc','mk12Corrected p.value',
                 'mk12tau','mk12N/N*s','mk12SensSlope')
  
  lprecmun[[i]]=mk
}

precmun=do.call(rbind.data.frame,lprecmun)


setwd('/home/bmiyamoto/Documentos/Pesquisa/Artigo Junior/mann kendall spi/1961-2014')
write.csv(precmun,file='Mann-Kendall modificado para SPI - 1965-2014',row.names=F)


#########################################
#mand kendalll modificado spi 1975-2014
library(RMySQL)

spi=dbConnect(MySQL(),user='root',password='BcBm#1317',host='localhost',dbname='PLUVIOSIDADE')
dbListTables(spi)

dbListFields(spi,'Precipitacao6')

#Atibaia, Cabreuva, Campo Limpo Paulista, Indaiatuba, Itu, Jarinu,
# Jundiai, Mairiporã, Salto, Várzea Paulista

spi12=dbGetQuery(spi,"select *from Precipitacao6 where `mun`='ATIBAIA' or
                 `mun`='CABREÚVA' or `mun`='CAMPO LIMPO PAULISTA' or
                 `mun`='INDAIATUBA' or `mun`='ITU' or `mun`='JARINU' or
                 `mun`='JUNDIAÍ' or `mun`='MAIRIPORÃ' or `mun`='SALTO' or
                 `mun`='VÁRZEA PAULISTA'")
precipitacao=spi12[,c(2,3,5,6,12)]


library(fume)

codigomun=as.data.frame(table(precipitacao$Codigo))
codigomun=as.numeric(levels(codigomun[,c(1)])[codigomun[,c(1)]])
lprecmun=list()

for (i in 1:length(codigomun))
{
  municipio=codigomun[[i]]
  precmun=precipitacao[precipitacao$Codigo==municipio,]
  precmun=precmun[!is.na(precmun$Codigo),]
  precmun=precmun[order(precmun$Ano,precmun$Mes),]
  mk12=precmun[,c(5)]
  mk12=mkTrend(mk12,ci=0.95)
  mk=cbind(precmun$Codigo[1],as.character(precmun$mun[1]),mk12$Z,mk12$p.value,mk12$Zc,mk12$`Corrected p.value`,
           mk12$tau,mk12$`N/N*s`,mk12$`Sen's Slope`)
  
  colnames(mk)=c('Codigo','Municipio','mk12Z','mk12p.value','mk12Zc','mk12Corrected p.value',
                 'mk12tau','mk12N/N*s','mk12SensSlope')
  
  lprecmun[[i]]=mk
}

precmun=do.call(rbind.data.frame,lprecmun)


setwd('/home/bmiyamoto/Documentos/Pesquisa/Artigo Junior/mann kendall spi/1961-2014')
write.csv(precmun,file='Mann-Kendall modificado para SPI - 1975-2015',row.names=F)

#######################################################
#######################################################
#Período 1975 a -2015 também não deu nada
# Vou calcular SPI e Mann-Kendall considerando a seŕie 1985-2015
#Não deu em nada, Não houve tendência de aumento de seca em nenhum dos
#períodos que eu testei

library(RMySQL)
prec=dbConnect(MySQL(),user='root',password='BcBm#1317',host='localhost',dbname='PLUVIOSIDADE')
dbListFields(prec,'Precipitacao5')

precipitacao=dbGetQuery(prec,"select *from Precipitacao5 where `mun`='ATIBAIA' or
                 `mun`='CABREÚVA' or `mun`='CAMPO LIMPO PAULISTA' or
                 `mun`='INDAIATUBA' or `mun`='ITU' or `mun`='JARINU' or
                 `mun`='JUNDIAÍ' or `mun`='MAIRIPORÃ' or `mun`='SALTO' or
                 `mun`='VÁRZEA PAULISTA' or `mun`='ITUPEVA'")
precipitacao=precipitacao[precipitacao$Ano>=1961&precipitacao$Ano<=2014,c(2,3,7:9)]
precbacia=aggregate(precipitacao[,c(5)],by=list(Mes=precipitacao$Mes,Ano=precipitacao$Ano),
                    mean,na.rm=T)
precbacia$Codigo=1
precbacia$mun='Bacia (Valor médio)'
precbacia=precbacia[,c(4,5,1,2,3)]
colnames(precbacia)[5]=c('Prectotalmes')
precipitacao=rbind(precipitacao,precbacia)

library(SCI)
mun=as.data.frame(table(precipitacao$Codigo))
mun=as.numeric(levels(mun[,c(1)])[mun[,c(1)]])
lmunicipio=list()

library(SCI)
lmunicipio=list()

for (i in 1:length(mun)){
  k=mun[[i]]
  municipio=precipitacao[precipitacao$Codigo==k,]
  municipio$Mes=as.numeric(municipio$Mes)
  municipio$Ano=as.numeric(municipio$Ano)
  
  #Usei esse artificio da tabela composição para compor com
  #missings a série de balanço hídrico
  Mes=rep(1:12,42)
  Ano=rep(min(municipio$Ano,na.rm=T):max(municipio$Ano,na.rm=T),12)
  #Ordena um vetor
  Ano=sort(Ano)
  composicao=as.data.frame(cbind(Ano,Mes))
  #composicao$missing=NA
  composicao$Codigo=municipio$Codigo[1]
  composicao$mun=municipio$mun[1]
  
  municipio=merge(municipio,composicao,by=c('Codigo','mun','Ano','Mes'),all.x=T,all.y=T)
  municipio=municipio[order(municipio$Ano,municipio$Mes),]
  chuvamun=municipio[,c(5)]
  
  spi1=fitSCI(chuvamun,first.mon = 1,time.scale = 1,distr = "gamma",p0=T)
  spi1=transformSCI(chuvamun,first.mon = 1,obj=spi1)
  
  spi3=fitSCI(chuvamun,first.mon = 1,time.scale = 3,distr = "gamma",p0=T)
  spi3=transformSCI(chuvamun,first.mon = 1,obj=spi3)
  
  spi6=fitSCI(chuvamun,first.mon = 1,time.scale = 6,distr = "gamma",p0=T)
  spi6=transformSCI(chuvamun,first.mon = 1,obj=spi6)
  
  spi9=fitSCI(chuvamun,first.mon = 1,time.scale = 9,distr = "gamma",p0=T)
  spi9=transformSCI(chuvamun,first.mon = 1,obj=spi9)
  
  spi12=fitSCI(chuvamun,first.mon = 1,time.scale = 12,distr = "gamma",p0=T)
  spi12=transformSCI(chuvamun,first.mon = 1,obj=spi12)
  
  spi15=fitSCI(chuvamun,first.mon = 1,time.scale = 15,distr = "gamma",p0=T)
  spi15=transformSCI(chuvamun,first.mon = 1,obj=spi15)
  
  spi18=fitSCI(chuvamun,first.mon = 1,time.scale = 18,distr = "gamma",p0=T)
  spi18=transformSCI(chuvamun,first.mon = 1,obj=spi18)
  
  spi21=fitSCI(chuvamun,first.mon = 1,time.scale = 21,distr = "gamma",p0=T)
  spi21=transformSCI(chuvamun,first.mon = 1,obj=spi21)
  
  spi24=fitSCI(chuvamun,first.mon = 1,time.scale = 24,distr = "gamma",p0=T)
  spi24=transformSCI(chuvamun,first.mon = 1,obj=spi24)
  
  municipio=cbind(municipio,spi1,spi3,spi6,spi9,spi12,spi15,spi18,spi21,spi24)
  #Eureca!!! É isso!!!!
  lmunicipio[[i]]=municipio
}

spi19252015=do.call(rbind.data.frame,lmunicipio)

#setwd('/home/bmiyamoto/Documentos/Pesquisa/Artigo Junior/SPI para municipios 1925-2015')
#write.csv(spi19252015,file='SPI municípios - 1925-2015',row.names=F)

precipitacao=spi19252015[,c(1:4,8,10,14)]


library(fume)

codigomun=as.data.frame(table(precipitacao$Codigo))
codigomun=as.numeric(levels(codigomun[,c(1)])[codigomun[,c(1)]])
lprecmun=list()

for (i in 1:length(codigomun))
{
  municipio=codigomun[[i]]
  precmun=precipitacao[precipitacao$Codigo==municipio,]
  precmun=precmun[!is.na(precmun$Codigo),]
  precmun=precmun[order(precmun$Ano,precmun$Mes),]
  mk6=precmun[,c(5)]
  mk6=mkTrend(mk6,ci=0.95)
  mk12=precmun[,c(6)]
  mk12=mkTrend(mk12,ci=0.95)
  mk24=precmun[,c(7)]
  mk24=mkTrend(mk24,ci=0.95)
  mk=cbind(precmun$Codigo[1],as.character(precmun$mun[1]),mk6$Z,mk6$p.value,mk6$Zc,mk6$`Corrected p.value`,
           mk6$tau,mk6$`N/N*s`,mk6$`Sen's Slope`,mk12$Z,mk12$p.value,mk12$Zc,mk12$`Corrected p.value`,
           mk12$tau,mk12$`N/N*s`,mk12$`Sen's Slope`,mk24$Z,mk24$p.value,mk24$Zc,mk24$`Corrected p.value`,
           mk24$tau,mk24$`N/N*s`,mk24$`Sen's Slope`)
  
  colnames(mk)=c('Codigo','Municipio','mk6Z','mk6p.value','mk6Zc','mk6Corrected p.value',
                 'mk6tau','mk6N/N*s','mk6SensSlope','mk12Z','mk12p.value','mk12Zc','mk12Corrected p.value',
                 'mk12tau','mk12N/N*s','mk12SensSlope','mk24Z','mk24p.value','mk24Zc','mk24Corrected p.value',
                 'mk24tau','mk24N/N*s','mk24SensSlope')
  
  lprecmun[[i]]=mk
}

precmun=do.call(rbind.data.frame,lprecmun)

setwd('/home/bmiyamoto/Documentos/Pesquisa/Artigo Junior/Novas mediddas')
write.csv(spi19252015,file='SPI para os muncípios da Bacia do Rio Jundiaí',row.names=F)
write.csv(precmun,file='Teste de Mann-Kendall para os valores de SPI6, SPI12 e SPI24.csv',
          row.names=F)
################################################################################
################################################################################
# Como o SPI não deu certo, vou tentar trabalhar com outras medidas que captem extremos de
#precipitação

library(RMySQL)
prec=dbConnect(MySQL(),user='root',password='BcBm#1317',host='localhost',dbname='PLUVIOSIDADE')
dbListFields(prec,'Precipitacao4')


codigomun=dbGetQuery(prec,"select `Codigo`,`mun` from Precipitacao6 where `mun`='ATIBAIA' or
                        `mun`='CABREÚVA' or `mun`='CAMPO LIMPO PAULISTA' or
                        `mun`='INDAIATUBA' or `mun`='ITU' or `mun`='JARINU' or
                        `mun`='JUNDIAÍ' or `mun`='MAIRIPORÃ' or `mun`='SALTO' or
                        `mun`='VÁRZEA PAULISTA' or `mun`='ITUPEVA'")
codigomun=unique(codigomun)
codigomun=codigomun$Codigo

precipitacao=dbGetQuery(prec,"select *from Precipitacao4 where `Codigo`=3504107 or `Codigo`=3508405 or
                        `Codigo`=3509601 or `Codigo`=3520509 or `Codigo`=3523909 or `Codigo`=3525201 or
                        `Codigo`=3525904 or `Codigo`=3528502 or `Codigo`=3545209 or `Codigo`=3556503 or
                        `Codigo`=3524006")


precipitacao=precipitacao[precipitacao$Ano>=1961&precipitacao$Ano<=2014,c(2,3,28:32)]

bcjmedia=aggregate(precipitacao[,c(3:7)],by=list(Ano=precipitacao$Ano),mean,na.rm=T)
bcjmedia$Codigo=1
precipitacao=rbind(precipitacao,bcjmedia)

codigomun=precipitacao$Codigo
codigomun=unique(codigomun)


library(Kendall)
lmktest=list()

for (i in 1:length(codigomun)){
  mun=precipitacao[precipitacao$Codigo==codigomun[i],]
  mun=mun[order(mun$Ano),]
  mktest=MannKendall(mun[,c(3)])
  tauprec=mktest$tau[1]
  pvalueprec=mktest$sl[1]
  
  mktest=MannKendall(mun[,c(4)])
  taudesv=mktest$tau[1]
  pvaluedesv=mktest$sl[1]
  
  mktest=MannKendall(mun[,c(5)])
  tau25=mktest$tau[1]
  pvalue25=mktest$sl[1]
  
  mktest=MannKendall(mun[,c(6)])
  tau50=mktest$tau[1]
  pvalue50=mktest$sl[1]
  
  mktest=MannKendall(mun[,c(7)])
  tausem=mktest$tau[1]
  pvaluesem=mktest$sl[1]
  
  mun=as.data.frame(cbind(mun$Codigo[1],tauprec,pvalueprec,taudesv,pvaluedesv,tau25,pvalue25,
                          tau50,pvalue50,tausem,pvaluesem))
  #colnames(mun)=c('Codigo','Tau','2 Sided pvalue')
  lmktest[[i]]=mun
}

mktest=do.call(rbind.data.frame,lmktest)

#Incorporar as medidas de quartis
lquant=list()

for (i in 1:length(codigomun)){
#Quartis para precipitação
quant=precipitacao[precipitacao$Codigo==codigomun[[i]],]
quantil=quantile(quant$Prectotal,c(0.10,0.90))
quant$seco[quant$Prectotal<=quantil[[1]]]=1
quant$chuvademais[quant$Prectotal>=quantil[[2]]]=1
lquant[[i]]=quant
}

quant=do.call(rbind.data.frame,lquant)
quant=quant[,c(1,2,8,9)]

precipitacao=merge(precipitacao,quant,by=c('Codigo','Ano'))

codigomun=dbGetQuery(prec,"select `Codigo`,`mun` from Precipitacao6 where `mun`='ATIBAIA' or
                        `mun`='CABREÚVA' or `mun`='CAMPO LIMPO PAULISTA' or
                        `mun`='INDAIATUBA' or `mun`='ITU' or `mun`='JARINU' or
                        `mun`='JUNDIAÍ' or `mun`='MAIRIPORÃ' or `mun`='SALTO' or
                        `mun`='VÁRZEA PAULISTA' or `mun`='ITUPEVA'")
codigomun=unique(codigomun)


precipitacao=merge(precipitacao,codigomun,by=c('Codigo'),all.x=T)
precipitacao$mun[is.na(precipitacao$mun)]='Bacia (Valor Médio)'

#Não sei ainda se envio o mktest ou se deixo as análises de tendência para o Junior
mktest=merge(mktest,codigomun,by.x=c('V1'),by.y=c('Codigo'),all.x=T)

#Nesse mesmo arquivo vou incorporar as análises de dias consecutivos sem chover.
#Para isso preciso trabalhar com os dados diários
dbListFields(prec,'Precipitacao3')

precdia=dbGetQuery(prec,"select *from Precipitacao3 where `Codigo`=3504107 or `Codigo`=3508405 or
                        `Codigo`=3509601 or `Codigo`=3520509 or `Codigo`=3523909 or `Codigo`=3525201 or
                        `Codigo`=3525904 or `Codigo`=3528502 or `Codigo`=3545209 or `Codigo`=3556503 or
                        `Codigo`=3524006")

precdia=precdia[precdia$Ano>=1961,]
codigomun=codigomun$Codigo
Ano=precdia$Ano
Ano=unique(Ano)

lconseq=list()
k=1

for (i in 1:length(codigomun)){
for (j in 1:length(Ano)){
consecsemchuva=precdia[precdia$Codigo==codigomun[[i]]&precdia$Ano==Ano[j],]
consecsemchuva=consecsemchuva[order(consecsemchuva$Mes,consecsemchuva$Dia),]
consecsemchuva$consec[consecsemchuva$Precipitacao<=1|is.na(consecsemchuva$Precipitacao)]=1
diaseco=rle(consecsemchuva$consec)
diaseco=diaseco$lengths[diaseco$values==1]
pmaximo=max(diaseco,na.rm=T)
#Quantos períodos com 10 dias consecutivos sem chover houve no ano?
diaseco=diaseco[diaseco>=20&!is.na(diaseco)]
nperiodos=length((diaseco))


diasconsecusemchuv=cbind(consecsemchuva$Codigo[[1]],consecsemchuva$Municipio[[1]],
                         consecsemchuva$Ano[[1]],nperiodos,pmaximo)

lconseq[[k]]=diasconsecusemchuv
k=k+1
}
}

diasconseq=do.call(rbind.data.frame,lconseq)
diasconseq$V1=as.numeric(levels(diasconseq$V1)[diasconseq$V1])
diasconseq$V3=as.numeric(levels(diasconseq$V3)[diasconseq$V3])
diasconseq$nperiodos=as.numeric(levels(diasconseq$nperiodos)[diasconseq$nperiodos])
diasconseq$pmaximo=as.numeric(levels(diasconseq$pmaximo)[diasconseq$pmaximo])
diasconseq=diasconseq[diasconseq$V3<=2014,c(1,3,4,5)]
diasconseqbaciamedia=aggregate(diasconseq[,c(3,4)],by=list(Ano=diasconseq$V3),
                               mean,na.rm=T)
diasconseqbaciamedia$Codigo=1
diasconseqbaciamedia=diasconseqbaciamedia[,c(4,1:3)]
colnames(diasconseq)[1:2]=c('Codigo','Ano')
diasconseq=rbind(diasconseq,diasconseqbaciamedia)

precipitacao=merge(precipitacao,diasconseq,by=c('Codigo','Ano'))

precipitacao=precipitacao[,c(1,10,2:9,11,12)]
colnames(precipitacao)[10]=c('Excesso de precipitacao')

setwd('/home/bmiyamoto/Documentos/Pesquisa/Artigo Junior/Novas mediddas')
write.csv(precipitacao,file='Medidas de precipitacão e de eventos extremos',
          row.names=F)


################################################################################
################################################################################
#Gráfico SPI para a BAcia do Rio Jundiaí como um todo
spi=read.csv('/home/bmiyamoto/Documentos/Pesquisa/Artigo Junior/Novas mediddas/SPI para os muncípios da Bacia do Rio Jundiaí.csv')
spi=spi[spi$Codigo==1,]

spi=as.matrix(spi[,c(6:14)])

#2D plot
spi.breaks <- c(-2.4,-2,-1.6,-1.3,-0.8,-0.5,0.5,0.8,1.3,1.6,2,2.4)
spi.cols <- colorRampPalette(c("darkred","red","yellow","white","green","blue","darkblue"),space="rgb")
dates<- seq(from=1960+1/24, to= 2014,by=1/12)
filled.contour(dates,seq(0,24,by=3),spi,col=spi.cols(11),xlab="",ylab="Tempo (meses)",cex.lab=1.5,font.axis=2,font.lab=2,levels=spi.breaks,key.title="SPI",
               plot.axes = {axis(1,cex.axis=2.2)
                 axis(2,cex.axis=2)})
title(main=paste('SPI -',k),cex.main=2)



###########################################################
###########################################################
#Preciso fazer um mapa das estações utilizadas na interpolação para atender ao
#pedido do parecerista. Enviar a tabela para o Junior, caso ele queira fazer 
#outro mapa.

####Os dados do mapa estão errados mas dexei a rotina salva apenas para manter o exemplo
# de mapa de cículos proporcionais
est_ut=merge(bcjundiai,estacoes,by.x=c('Codest'),by.y=c('Codigo'),all.x=T,all.y=F)
library(maptools)
mun=readShapePoly('/home/bmiyamoto/Documentos/Pesquisa/Artigo Junior/Mapas/municipios_2010/municipios_2010.shp')
munsp=mun[mun$uf=='SP',]
uf=readShapePoly('/home/bmiyamoto/Documentos/Pesquisa/Artigo Junior/Mapas/estados_2010/estados_2010.shp')
ufsp=uf[uf$sigla=='SP',]
plot(munsp,border=T,lwd=.2,axes=F,las=2)
plot(ufsp,add=T)
points(est_ut$Longitude,est_ut$Latitude,pch=21,col='darkgreen',bg=adjustcolor("darkgreen",0.5),cex=0.9,lwd=0.9)
library(maps)
map.scale(x=-47, y=-24.8,relwidth=0.18,metric=T,ratio=F,cex=0.8)
setwd('/home/bmiyamoto/Documentos/Pesquisa/Artigo Junior')
source(compassRose(-46,-24.3))

#salvar a tabela utilizada para fazer o mapa
setwd('/home/bmiyamoto/Documentos/Pesquisa/Artigo Junior/Artigo - Revista Confins')
write.csv(est_ut,file='estacoes',row.names=F)

###############################################################################