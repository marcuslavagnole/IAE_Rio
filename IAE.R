### Bibliotecas
library(openxlsx)
library(forecast)
library(seasonal)
library(httr)
library(jsonlite)
library(imputeTS)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

### Datas de entrada
first_year_data      = "2011"
first_month_data     = "01"
last_year_data       = "2022"
last_month_data      = "12"


### Dados PMS
aux_PMS       = GET(paste("http://api.sidra.ibge.gov.br/values/t/5906/n3/33/v/7168/p/",
                          first_year_data,first_month_data,"-",last_year_data,last_month_data,
                          "/c11046/56726/f/u",sep=""))
data_PMS      = as.data.frame(fromJSON(rawToChar(aux_PMS$content)))
colnames(data_PMS)= data_PMS[1,] 
PMS           = data_PMS[-1,]
PMS           = as.data.frame(as.numeric(PMS$Valor))
colnames(PMS) = 'PMS'
PMS           = ts(PMS, 
                   start= c(as.numeric(first_year_data),as.numeric(first_month_data)),
                   end  = c(as.numeric(last_year_data),as.numeric(last_month_data)),
                   frequency=12)


### Dados PMC
aux_PMC       = GET(paste("http://api.sidra.ibge.gov.br/values/t/8880/n3/33/v/7170/p/",
                          first_year_data,first_month_data,"-",last_year_data,last_month_data,
                          "/c11046/56734/f/u",sep=""))
data_PMC      = as.data.frame(fromJSON(rawToChar(aux_PMC$content)))
colnames(data_PMC)= data_PMC[1,] 
PMC           = data_PMC[-1,]
PMC           = as.data.frame(as.numeric(PMC$Valor))
colnames(PMC) = 'PMC'
PMC           = ts(PMC, 
                   start= c(as.numeric(first_year_data),as.numeric(first_month_data)),
                   end  = c(as.numeric(last_year_data),as.numeric(last_month_data)),
                   frequency=12)


### Dados IPCA
# 2011-2019 
IPCA_aux1     = c(0.00,0.69,0.67,0.82,0.6,0.12,0.11,0.47,0.42,0.43,0.46,0.66,
                  1.11,0.95,-0.05,0.81,0.07,0.23,0.54,0.45,0.74,0.47,0.5,1.29,
                  0.73,0.25,0.27,0.59,0.63,0.65,-0.16,0.19,0.4,0.54,0.75,1.16,
                  0.5,1.07,1.28,0.42,0.55,0.4,-0.08,0.42,0.36,0.53,0.52,1.39,
                  1.71,1.19,1.35,0.81,0.35,0.65,0.46,-0.02,0.49,0.59,1.24,1.24,
                  1.82,0.68,0.29,0.62,0.6,0.38,0.5,1,-0.17,0.15,0.04,0.25,
                  0.4,0.68,0.38,0.38,0.22,-0.09,-0.03,0.02,0.13,0.1,0.26,0.54,
                  0.42,0.72,0.12,0.3,0.28,1.2,0.59,-0.38,0.38,0.21,-0.02,0.4,
                  0.49,0.48,0.83,0.46,-0.05,0.05,0.3,-0.06,-0.13,0.27,0.17,1.19)
# 2020-Atual
aux_IPCA      = GET(paste("http://api.sidra.ibge.gov.br/values/t/7060/n7/3301/v/63/p/",
                          first_year_data,first_month_data,"-",last_year_data,last_month_data,
                          "/c315/7169/f/u",sep=""))
data_IPCA     = as.data.frame(fromJSON(rawToChar(aux_IPCA$content)))
colnames(data_IPCA)= data_IPCA[1,] 
IPCA_aux2     = data_IPCA[-1,]
IPCA          = c(IPCA_aux1,as.numeric(IPCA_aux2$Valor))
IPCA          = ts(IPCA, 
                   start= c(as.numeric(first_year_data),as.numeric(first_month_data)),
                   end  = c(as.numeric(last_year_data),as.numeric(last_month_data)),
                   frequency=12)
# Acumulando
IPCA_acum     = cumprod(IPCA/100 + 1)


### Dados ISS
ISS           = read.xlsx("G:/Meu Drive/SUBDEI/IndicadorServicos/ISS.xlsx")
ISS           = ts(ISS[,2], 
                   start= c(as.numeric(first_year_data),as.numeric(first_month_data)),
                   end  = c(as.numeric(last_year_data),as.numeric(last_month_data)),
                   frequency=12)
# Deflacionando
ISS_real       = ISS / IPCA_acum
# Dessazonalizando
decomp_iss     = seas(ISS_real)
iss_trend      = seasadj(decomp_iss)
# Reescalando
ISS_indice     = sqrt(iss_trend)


### Dados ICMS
ICMS          = read.xlsx("G:/Meu Drive/SUBDEI/IndicadorServicos/ICMS.xlsx")
options(digits = 2)
ICMS$ICMS     = as.numeric(gsub(',','',ICMS$ICMS))
ICMS          = ts(ICMS[,2], 
                   start= c(as.numeric(first_year_data),as.numeric(first_month_data)),
                   end  = c(as.numeric(last_year_data),as.numeric(last_month_data)),
                   frequency=12)
# Deflacionando
ICMS_real     = ICMS / IPCA_acum
# Imputando NA
ICMS_real     = na_seasplit(ICMS_real,algorithm="kalman")
# Dessazonalizando
decomp_icms   = seas(ICMS_real)
icms_trend    = seasadj(decomp_icms)
# Reescalando
ICMS_indice   = sqrt(icms_trend)


### Normalizacao dos Dados para Base 100
iss_normalizado       = (ISS_indice/ISS_indice[1])*100
icms_normalizado      = (ICMS_indice/ICMS_indice[1])*100
pms_normalizado       = (PMS/PMS[1])*100
pmc_normalizado       = (PMC/PMC[1])*100


### Calculo do Indicador 
indicador_normalizado = 0.87*(0.70*iss_normalizado+0.25*pms_normalizado+0.05*pmc_normalizado)+
                          0.13*icms_normalizado
indicador_normalizado = ts(indicador_normalizado, 
                           start=c(2011,1),frequency=12)