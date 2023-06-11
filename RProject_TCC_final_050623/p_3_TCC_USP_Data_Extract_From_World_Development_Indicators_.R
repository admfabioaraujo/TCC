#........................................................................................
#                                                                                        #
#      Regressão Múltipla - Previsão GDP - Gross Domestic Product (PIB) - Brasil         #
#  fonte do dataset: https://databank.worldbank.org/source/world-development-indicators  #
#                                                                                        #
#.........................................................................................

## Configurando o diretório de trabalho:
getwd()

## Versão utilizada - https://cran.r-project.org/bin/windows/base/old/4.2.1/
R.version # version.string R version 4.2.1 (2022-06-23 ucrt)
sessionInfo()

## Carregando os pacotes inaugurais:
if(!require(pacman)) install.packages("pacman")
library(pacman)
pacman::p_load(dplyr, car, rstatix, lmtest, ggpubr, 
               QuantPsyc, psych, scatterplot3d)

## Carregando os demais pacotes:
pacotes <- c("readxl","Amelia","visdat", "e1071", "tidyverse","tidyr","car","rgl","scatterplot3", 
             "caret", "rpart", "janitor", "faraway","knitr","kableExtra","plotly","nortest","olsrr",
             "jtools", "huxtable","stargazer", "ggplotly", "ggside",
             "correlation","ggplot2","see","PerformanceAnalytics","ggraph","lmtest","MASS")
if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T)
} else { 
  sapply (pacotes, require, character = T)
}
(.packages())

## Carregando o data bank:
library("readxl")
d7_puro <- read_excel("all_P_Data_Extract_From_World_Development_Indicators.xlsx", sheet = "transposta_formatada_data_a")
View(d7_puro) #_ shape original: 63 X 1443.

## Exlcuindo a linha 1 "series code", considerada como um subcabeçalho: 
library("dplyr")
d7_2 <- d7_puro
line <- c(1)
d7_2 <- d7_2[-line, ]
View(d7_2)
d7_3 <- d7_2[1:62,]
View(d7_3) #_ shape: 62 X 1443

## Convertendo o objeto, de tibble para data-frame:
d7_3 <- as.data.frame(d7_3)
class(d7_3)

## Extraindo caracteres desnecessários das observações da variável 'series name':
d7_3$`Series Name` <- substr(d7_3$`Series Name`, 1,4)
View(d7_3)

## Convertendo todas as variáveis para num:
d7_4 <- d7_3
for(i in 1:ncol(d7_3)) {
  d7_4[,i] <- as.numeric(d7_4[,i])
}
View(d7_4) #_ 62 X 1443

## Visualização e contagem das variáveis e dos valores ausentes:
library(Amelia)
library(visdat)
missmap(d7_4) #_ missing 53%
is.na.data.frame(d7_4)
library(dplyr)
is_na_GPD_current <- d7_4 %>% count(is.na(d7_4$`GDP (current US$)`))
print(is_na_GPD_current) #_ variável target com NAs (false 62)
sapply(d7_4, function(x) sum(is.na(x)))  
NAs <- round(colSums(is.na(d7_4))*100/nrow(d7_4), 2)
NAs[NAs>0]
NAs[NAs == 0] #_ + - 151 variáveis "full"
any(is.na(d7_4)) #_ T há dados ausentes
sum(is.na(d7_4)) #_ 47647
sum(!is.na(d7_4)) #_ 41819
nas <- sum(is.na(d7_4)) #_ 47647
no_nas <- sum(!is.na(d7_4)) #_ 41819  
dados_ausentes <- nas/(nas+no_nas)*100 #_ 53,25% de dados ausentes na base.

## Exclusão de variáveis compostas por mais de 30% de NAs:
library("caret")
library("rpart")
del_variaveis_acima_30_NAs <- names(NAs[NAs>30]) #_ 960 variáveis
d7_5 <- d7_4
d7_5 <- dplyr::select(d7_5,-del_variaveis_acima_30_NAs)
View(d7_5) #_ 62 X 483: 33,48% das 1443 features

## Limpando os nomes das variáveis:
library(janitor)
names(d7_5)
d7_6 <- d7_5 %>% clean_names()  
names(d7_6) 
View(d7_6) #_ 62 X 483 

## Imputação - modelo da árvore de decisão - variáveis compostas por até 29% de NAs:
library(rpart)
for (i in 1:ncol(d7_6)){  
  d7_7 <- d7_6[!is.na(d7_6[,i]),] #_ separando os dados sem NAs  
  modelo_ad_NA <- rpart(d7_7[,i]~., data = d7_7)  
  d7_6[,i][is.na(d7_6[,i])] <- predict(modelo_ad_NA,d7_6[is.na(d7_6[,i]),])                                                 
}

## Conferindo a imputação, com a ausência de valores ausentes no data bank:  
sapply(d7_6, function(x) sum(is.na(x)))  
NAs <- round(colSums(is.na(d7_6))*100/nrow(d7_6), 2)
NAs[NAs>0]
NAs[NAs == 0] #_ all
any(is.na(d7_6)) #_ F ausência de dados faltantes
sum(is.na(d7_6)) #_ 0
sum(!is.na(d7_6)) #_ 29946
na2 <- sum(is.na(d7_6)) #_ 0
no_na2 <- sum(!is.na(d7_6)) #_ 29946   
dados_ausentes2 <- na2/(na2+no_na2)*100 #_ 0%

## Salvando o data bank sem valores ausentes:
save(d7_6, file = "d7_6.RData") #_ 62 X 483 
file.info("d7_6")
load(file = "d7_6.RData")
sum(is.na(d7_6))

## Gerando um bloxplot da variável dependente:
options(scipen = 999)
dev.off()
library(ggplot2)
library(plotly)
library(tibble)
ggplotly(
  ggplot(d7_6,aes(x = "", y = gdp_current_us)) +
    geom_boxplot(fill = "deepskyblue",    # cor da caixa
                 alpha = 0.7,             # transparência
                 color = "black",         # cor da borda
                 outlier.colour = "red",  # cor dos outliers
                 outlier.shape = 15,      # formato dos marcadores dos outliers
                 outlier.size = 1.5) +    # tamanho dos marcadores dos outliers
    geom_jitter(width = 0.1, alpha = 0.3, size = 2.0, color = "darkorchid") +
    labs(y = "Real_Values_USD") +
    theme(panel.background = element_rect("white"),
          panel.grid = element_line("grey95"),
          panel.border = element_rect(NA),
          legend.position="none",
          plot.title = element_text(size=15)) +
    ggtitle("Boxplot da variável de interesse - Gross Domestic Product") +
    xlab("")
)
dev.off()

# Criando um gráfico kernel density estimation (KDE) - função densidade de probabilidade da variável dependente com histograma:
ggplotly(
  ggplot(d7_6, aes(x = gdp_current_us)) +
    geom_density(aes(x = gdp_current_us), 
                 position = "identity", color = "black", size = 1) +
    geom_histogram(aes(y = ..density..), color = "white", fill = "deepskyblue",
                   bins = 30) +
    theme_classic()
)

## Selecionando as variáveis com coeficientes numéricos a partir do modelo lm_62_483, gerando a base com 62 X 62, que alimentou o lm_62_62:
library(dplyr)
library(faraway)
options(scipen = 999)
lm_62_483 <- lm(d7_6$gdp_current_us ~., data = d7_6) 
summary(lm_62_483) #_ R Squared = 1   R Ajuested = NaN
d7_7 <- d7_6 
d7_7 <- d7_6 %>% dplyr::select(c(1:10,11:27,29:50, 53:56, 58, 60:65,67:68)) #_ variáveis com coeficientes numéricos, descartando os NAs
View(d7_7) #_ shape 62 X 62
sum(is.na(d7_7)) #_ 0
sum(!is.na(d7_7)) #_ 3844
lm_62_62 <- lm(d7_7$gdp_current_us ~., data = d7_7) 
summary(lm_62_62) #_ R Squared = 1   R Ajuested = NaN 

## Identificação e exclusão das correlações acima de 0.8:
View(d7_7)
d7_7_no_taget <- d7_7
d7_7_no_taget$gdp_current_us <- NULL
View(d7_7_no_taget) #_ shape 62 X 61
correlation <- cor(d7_7_no_taget)
library(caret)
findCorrelation(correlation, cutoff = 0.80, verbose = T, names = T) 
d7_8 <- d7_7
View(d7_8)
library(dplyr)
d7_8 <- d7_7 %>% dplyr::select(-(60),-(5),-(26),-(29),-(25),-(62),-(12),-(53),
                               -(32),-(41),-(34),-(8),-(21),-(45),-(3),-(37),
                               -(49),-(55),-(6),-(11),-(33),-(31),-(1),-(52),
                               -(27),-(18),-(7),-(13),-(35),-(39),-(22),-(43),
                               -(24),-(9),-(42),-(61),-(56),-(44),-(48),-(54),
                               -(50),-(36))
View(d7_8) #_ shape 62 X 20

## Apurando e visualizando as variáveis inflacionadas:
lm_62_20 <- lm(d7_8$gdp_current_us ~., data = d7_8) 
summary(lm_62_20) #_ R Squared = 0.9778   R Adjusted = 0.9678 
library(car)
vif(lm_62_20)   

## Excluindo do data bank as variáveis inflacionadas acima de 10 e criando outro modelo:
d7_9 <- d7_8
d7_9 <- d7_8 %>% dplyr::select(-(2),-(5),-(6),-(8),-(17))
View(d7_9) #_ shape 62 X 15
lm_62_15 <- lm(d7_9$gdp_current_us ~., data = d7_9)
summary(lm_62_15) #_ R Squared = 0.8908   R Adjusted = 0.8582
vif(lm_62_15) #_ conferindo a ausência de variáveis inflacionadas acima de 10

## Checando a correlação e variáveis inflacionadas no data bank em manipulação:
d7_9_no_target <- d7_9
d7_9_no_target$gdp_current_us <- NULL
View(d7_9_no_target)
correlation_VIF <- cor(d7_9_no_target) 
findCorrelation(correlation_VIF, cutoff = 0.80, verbose = T, names = T) #_ All correlations <= 0.8 character(0)
vif(lm_62_15) #_ 15 variáveis entre 1,15 e 5,48

## Salvando o data bank sem correlações acima de 0.8 e sem variáveis inflacionadas acimda de 10:
save(d7_9, file = "d7_9.RData") #_ 62 X 15
load(file = "d7_9.RData")

## Salvando o modelo gerado com dataset sem correlações acima de 0.8 e sem variáveis inflacionadas acimda de 10:
save(lm_62_15, file = "lm_62_15.RData")
load(file = "lm_62_15.RData")

## Seleção de variáveis com a função stepwise:
library(dplyr)
library(MASS)
step_62_15 <- step(lm_62_15, direction = "both")
summary(step_62_15) #_ R Squared = 0.8869   R Adjusted = 0.8723
#... códigos de significância:    1 ***; 3 ***; 6 ***; 9 *; 12 **; 13 *** e 14 *

## Criando um dataset com a variável target e as varíáveis explicativas estatisticamente significantes, com pelo menos 0.05:
d7_10 <- d7_9
d7_10 <- d7_9 %>% dplyr::select(c(1,2,3,6,9,12,13,14))
View(d7_10) #_ shape 62 X 8

## Salvando o dataset higienizado com as variáveis escolhidas pelo procedimento stepwise:
save(d7_10, file = "d7_10.RData")
file.info("d7_10")
load(file = "d7_10.RData")

## Renomeando o dataset higienizado e suas variáveis:
final_dataset <- d7_10
View(final_dataset)
library(tidyverse)
final_dataset <- rename(final_dataset, adolescent_fertility = "adolescent_fertility_rate_births_per_1_000_women_ages_15_19")
final_dataset <- rename(final_dataset, gdp_current_usd = "gdp_current_us")
final_dataset <- rename(final_dataset, gcf_percent_gdp = "gross_capital_formation_percent_of_gdp") 
final_dataset <- rename(final_dataset, net_official_assistence_usd = "net_official_development_assistance_and_official_aid_received_current_us") 
final_dataset <- rename(final_dataset, savings_carbon_percent_of_gni = "adjusted_savings_carbon_dioxide_damage_percent_of_gni")
final_dataset <- rename(final_dataset, savings_mineral_depletion_percent_of_gni = "adjusted_savings_mineral_depletion_percent_of_gni")
final_dataset <- rename(final_dataset, agricultural_raw_percent_of_merchandise_exports = "agricultural_raw_materials_exports_percent_of_merchandise_exports")
final_dataset <- rename(final_dataset, agricultural_raw_percent_of_merchandise_imports = "agricultural_raw_materials_imports_percent_of_merchandise_imports")
View(final_dataset) #_ shape 62 X 8

## Salvando o dataset acabado:
save(final_dataset, file = "final_dataset.RData")
load(file = "final_dataset.RData")
file.info("final_dataset.RData")
sum(is.na(final_dataset))
sum(!is.na(final_dataset))

## Criando o modelo higienizado, com base no final_dataset:  
View(final_dataset)
final_model <- lm(final_dataset$gdp_current_usd ~., data = final_dataset)
summary(final_model) #_ R Squared = 0.8869   R Adjusted = 0.8723

## Salvando o modelo final:
save(final_model, file = "final_model.RData") #_ 62 X 8
load(file = "final_model.RData")

## Calculando o sumário, agrupamento, somatória, skewness, kurtosis, moda, desvio padrão, quartis e range da variável target: 
GDP_summarise <- summarise(final_dataset,
                           observações=n(),
                           média=mean(`gdp_current_usd`),
                           mediana=median(`gdp_current_usd`),
                           desv_pad=sd(`gdp_current_usd`),
                           mínimo=min(`gdp_current_usd`),
                           máximo=max(`gdp_current_usd`),
                           quartil_3=quantile(`gdp_current_usd`, type=5, 0.75))
options(scipen = 999)
View(GDP_summarise) 
print(GDP_summarise)
library(e1071)
skewness(final_dataset$gdp_current_usd) #_ 1.087796 - o coeficiente indica assimetria positiva, à direita, sendo a moda > média
kurtosis(final_dataset$gdp_current_usd) #_ -0.1536578 - o coeficiente indica  alto grau de achatamento, a distribuição é platicúrtica
var(final_dataset$gdp_current_usd) #_ 609519101833220288784640
sum(final_dataset$gdp_current_usd) #_ 44215922922349
library(tidyverse)
library(tidyr)
options(scipen = 999)
quantile((final_dataset$gdp_current_usd), probs = c(0.01, 0.99))
quantile((final_dataset$gdp_current_usd), seq(from = 0, to = 1, by = 0.20))
IQR(final_dataset$gdp_current_usd) #_ diferença entre q3 e q1
range(final_dataset$gdp_current_usd)
diff(range(final_dataset$gdp_current_usd))
summary(final_dataset)

# Criando e salvando o gráfico Box Plot de todas as variáveis, em ZScore: 
final_dataset_scaled <- as.data.frame(scale(final_dataset[,1:8]))
View(final_dataset)
outliers_geral <- final_dataset_scaled
outliers_geral <- lapply(outliers_geral, function(x) boxplot.stats(x)$out)
options(scipen = 999)
print(outliers_geral)
library(dplyr)
glimpse(outliers_geral) #_ list
class(outliers_geral)
View(final_dataset_scaled)
statisc_list <- list() # lista vazia
for (i in seq_along(final_dataset_scaled)) {
  statisc_list[[i]] <- boxplot.stats(final_dataset_scaled[[i]])$stats[2]
}
boxplot(final_dataset_scaled, col="deepskyblue") -> boxplot_higienizado
points(1:length(statisc_list), statisc_list, col="red") -> boxplot_higienizado
axis(side = 1, at = 1:ncol(final_dataset), labels = colnames(final_dataset)) -> boxplot_higienizado
title(main = "Gráfico Boxplot do final_dataset - shape 62 X 8") -> boxplot_higienizado
save(boxplot_higienizado, file = "boxplot_higienizado.RData")

## Importância relativa de cada parâmetro beta, adicionando a caracterização da distribição normal no IC:
plot_summs(step_62_15, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95, colors = "deepskyblue")

## Visualização do final_dataset:
library(dplyr)
library(knitr)
library(kableExtra)
dev.off()
options(scipen = 999)
final_dataset %>%
  select(adolescent_fertility,gdp_current_usd,gcf_percent_gdp, net_official_assistence_usd, 
         savings_carbon_percent_of_gni,savings_mineral_depletion_percent_of_gni,
         agricultural_raw_percent_of_merchandise_exports, 
         agricultural_raw_percent_of_merchandise_imports) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)

## Estatísticas univariadas:
summary(final_dataset)

## Gráfico 3D com scatter
install.packages("scatterplot3d") 
library(scatterplot3d)
scatter3d(gdp_current_usd ~ net_official_assistence_usd + savings_carbon_percent_of_gni,
          data = final_dataset,
          surface = T,
          point.col = "#440154FF",
          axis.col = rep(x = "black",
                         times = 3))

## Diagrama de interrelação entre as variáveis e a suas magnitudes:
library(correlation)
library(dplyr)
library(see)
library(ggraph)
dev.off()
final_dataset %>%
  correlation(method = "pearson") %>%
  plot()

## Apresentação das distribuições das variáveis, scatters, valores das correlações e suas respectivas significâncias < 0,8:
library(PerformanceAnalytics)
chart.Correlation((final_dataset[1:8]), histogram = TRUE) #_ gdp_current_usd X net_official_assistence_usd = 0.79

## Apresentação das distribuições das variáveis, scatters, valores das correlações e suas respectivas significâncias < 0,8:
library(psych)
pairs.panels(final_dataset[1:8],
             smooth = TRUE,
             lm = TRUE,
             scale = FALSE,
             density = TRUE,
             ellipses = FALSE,
             method = "pearson",
             pch = 1,
             cor = TRUE,
             hist.col = "aquamarine",
             breaks = 12,
             stars = T,  
             ci = TRUE, alpha = 0.05)

## Apresentação das distribuições das variáveis, scatters, valores das correlações e suas respectivas significâncias < 0,8:
library(metan)
final_dataset %>%
  corr_plot(adolescent_fertility,gdp_current_usd,gcf_percent_gdp, net_official_assistence_usd, 
     savings_carbon_percent_of_gni,savings_mineral_depletion_percent_of_gni,
     agricultural_raw_percent_of_merchandise_exports, 
     agricultural_raw_percent_of_merchandise_imports,
            shape.point = 21,
            col.point = "black",
            fill.point = "#FDE725FF",
            size.point = 2,
            alpha.point = 0.6,
            maxsize = 4,
            minsize = 2,
            smooth = TRUE,
            col.smooth = "black",
            col.sign = "#440154FF",
            upper = "corr",
            lower = "scatter",
            diag.type = "density",
            col.diag = "#440154FF",
            pan.spacing = 0,
            lab.position = "bl")

## Análise gráfica do final_dataset: 
dev.off()
par(mfrow=c(2,2))
plot(final_model)
#... Residual vs Fitted (linearidade dos resíduos): n, Normal QQ(normalidade na distribuição dos resíduos): s, 
#... Scale Location (homocedasticidade - variancia constante dos erros ao longo do tempo): com cone e Residuals vs Leverage (outliers -2 +3): 
#... interpretação: https://data.library.virginia.edu/diagnostic-plots/

## Análise estatística do modelo_higienizado - aplicação de testes estatísticos clássicos:

# PRESSUPOSTO 1 - normalidade dos resíduos _ 
# ..Shapiro Francia
# h0: distribuição dos dados é normal (p > 0,05); 
# h1: distribuição dos dados não é normal (p <= 0,05) 
library(nortest)
sf.test(final_model$residuals) #_ p-value = 0.9251 > 0,05 - distribuição é normal
summary(rstandard(final_model)) #_ resíduos entre -2 e 2: 

# PRESSUPOSTO 2 - independência dos resíduos _
# .. Durbin Watson:
# h0: não há correlação entre os resíduos;
# h1: os resíduos são autocorrelacionados;
# Se (d) for menor que 1,5 ou maior que 2,5, existe um potencial problema de autocorrelação. 
# Caso contrário, se (d) estiver entre 1,5 e 2,5, a autocorrelação não será motivo de preocupação. 
library(car)
durbinWatsonTest(final_model) #_ DW Statistic = 1.502281 > p_0.05 - não há correlação

# PRESSUPOSTO 3 - variância dos erros (homocedasticidade) _
# .. Breusch Pagan: 
# h0: a homocedasticidade está presente;
# h1: a heterocedasticidade está presente;
# Os resíduos devem estar distribuídos com variância igual para não violar a homocedasticidade.
library(lmtest)
library(olsrr) 
bptest(final_model) #_ p-value = 0.07159 > 0.05 - a homocedasticidade está presente
#... teste para diagnóstico de heterocedasticidade - em havendo, os valores estimados de t e a estatística F podem não ser confiáveis.
# h0: não há heterocedasticidade;
# h1: a heterocedasticidade está presente; 
library(olsrr)
ols_test_breusch_pagan(final_model) # Chi2 8.567826 > p_0.05 - não há heterocedasticidade

# PRESSUPOSTO 4 - ausência de multicolinearidade do modelo _
# install.packages("psych", dependencies = T)
library(psych)
dev.off()
pairs.panels(final_dataset) # (-) gdp_current_usd X adolescent_fertility =  -0.79 | (+) gdp_current_usd X net_official_assistence_usd 

## Outras maneiras de apresentar os outputs e parâmetros do modelo_higienizado:
library(jtools)
library(huxtable)
confint(final_model, level = 0.95) # significância de 5%
summ(final_model, confint = T, digits = 4, ci.width = .95)

## Carregando os objetos higienizados e não-higienizados (antes do stepwise):
load(file = "final_dataset.RData")
load(file = "final_model.RData")
load(file = "d7_9.RData")
load(file = "lm_62_15.Rdata")

## Comparando os parâmetros dos modelos: final_model X lm_62_15:
# ... Renomeando as variáveis temporariamente:
library(tidyverse)
d7_9 <- rename(d7_9, adolescent_fertility = "adolescent_fertility_rate_births_per_1_000_women_ages_15_19")
d7_9 <- rename(d7_9, gdp_current_usd = "gdp_current_us")
d7_9 <- rename(d7_9, gcf_percent_gdp = "gross_capital_formation_percent_of_gdp") 
d7_9 <- rename(d7_9, net_official_assistence_usd = "net_official_development_assistance_and_official_aid_received_current_us") 
d7_9 <- rename(d7_9, savings_carbon_percent_of_gni = "adjusted_savings_carbon_dioxide_damage_percent_of_gni")
d7_9 <- rename(d7_9, savings_mineral_depletion_percent_of_gni = "adjusted_savings_mineral_depletion_percent_of_gni")
d7_9 <- rename(d7_9, agricultural_raw_percent_of_merchandise_exports = "agricultural_raw_materials_exports_percent_of_merchandise_exports")
d7_9 <- rename(d7_9, agricultural_raw_percent_of_merchandise_imports = "agricultural_raw_materials_imports_percent_of_merchandise_imports")
View(d7_9)
lm_62_15_renamed <- lm(d7_9$gdp_current_usd ~., data = d7_9)
cotejano <-  export_summs(final_model, lm_62_15_renamed,
             model.names = c("final_model","modelo LM_62_15 renomeado (pré stepwise)"),
             scale = F, digits = 4)
class(cotejano)
cotejano_2 <- as.data.frame(cotejano)
class(cotejano_2)
View(cotejano_2)
library(dplyr)
library(knitr)
library(kableExtra)
dev.off()
options(scipen = 999)
cotejano_2 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 13)

## Comparando os ICs dos betas dos dois modelos: 
plot_summs(final_model, lm_62_15_renamed, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95, colors = c("deepskyblue", "grey90"))

## Análise estatística do modelo lm_62_15 (sem COR e VIF, mas pré stepwise) - aplicação de testes estatísticos clássicos:

# PRESSUPOSTO 1 - normalidade dos resíduos _ 
# ..Shapiro Francia
# h0: distribuição dos dados é normal (p > 0,05); 
# h1: distribuição dos dados não é normal (p <= 0,05) 
library(nortest)
sf.test(lm_62_15$residuals) #_ p-value = 0.8731 > 0,05 - distribuição é normal
summary(rstandard(lm_62_15)) #_ resíduos entre -2 e 2: 

# PRESSUPOSTO 2 - independência dos resíduos _
# .. Durbin Watson:
# h0: não há correlação entre os resíduos;
# h1: os resíduos são autocorrelacionados;
# Se (d) for menor que 1,5 ou maior que 2,5, existe um potencial problema de autocorrelação. 
# Caso contrário, se (d) estiver entre 1,5 e 2,5, a autocorrelação não será motivo de preocupação. 
library(car)
durbinWatsonTest(lm_62_15) #_ DW Statistic = 1.534629 > p_0.05 - não há correlação

# PRESSUPOSTO 3 - variância dos erros (homocedasticidade) _
# .. Breusch Pagan: 
# h0: a homocedasticidade está presente;
# h1: a heterocedasticidade está presente;
# Os resíduos devem estar distribuídos com variância igual para não violar a homocedasticidade.
library(lmtest)
library(olsrr) 
bptest(lm_62_15) #_ p-value = 0.4061 > 0.05 - a homocedasticidade está presente
#... teste para diagnóstico de heterocedasticidade - em havendo, os valores estimados de t e a estatística F podem não ser confiáveis.
# h0: não há heterocedasticidade;
# h1: a heterocedasticidade está presente; 
library(olsrr)
ols_test_breusch_pagan(lm_62_15) # Chi2 7.75866 > p_0.05 - não há heterocedasticidade

# PRESSUPOSTO 4 - ausência de multicolinearidade do modelo _
# install.packages("psych", dependencies = T)
library(psych)
dev.off()
pairs.panels(d7_9)

## Evidenciando as métricas dos 2 (dois) modelos: 
# ... Hlavac, Marek (2022). stargazer: Well-Formatted Regression and Summary Statistics Tables. R package version 5.2.3. https://CRAN.R-project.org/package=stargazer
library(stargazer)
stargazer_mh_list <- list(Shape = c("final_model"),
                          R_Squared = c(0.8869),
                          R_Adjusted = c(0.8723),
                          F_Statistic = c(60.51),
                          p_Value_F = c(0.00000000000000022),     
                          Shapiro_Francia = c(0.9251),
                          Durbin_Watson = c(1.502281),
                          Breusch_Pagan = c(0.07159),
                          Breusch_Pagan_Chi_Q = c(8.567826))
stargazer_mh_list
stargazer_mh_df <- as.data.frame(do.call(cbind, stargazer_mh_list))  
options(scipen = 999)
View(stargazer_mh_df)
stargazer_lm_62_15_list <- list(Shape = c("modelo LM_62_15 (pré stepwise)"),
                         R_Squared = c(0.8908),
                         R_Adjusted = c(0.8582),
                         F_Statistic = c(27.37),
                         p_Value_F = c(0.00000000000000022),
                         Shapiro_Francia = c(0.8731),
                         Durbin_Watson = c(1.534629),
                         Breusch_Pagan = c(0.4061),
                         Breusch_Pagan_Chi_Q = c(7.75866))
stargazer_lm_62_15_list
stargazer_lm_62_15_df <- as.data.frame(do.call(cbind, stargazer_lm_62_15_list))  
options(scipen = 999)
View(stargazer_lm_62_15_df) 
stargazer(stargazer_mh_df[1,1:9], summary=F, rownames= F, type = "text")
stargazer(stargazer_lm_62_15_df[1,1:9], summary=F, rownames= F, type = "text")

## Juntando as métricas por linhas:
library(tidyverse)
stargazer_dois_modelos <- bind_rows(stargazer_mh_df, stargazer_lm_62_15_df)
class(stargazer_dois_modelos)
options(scipen = 999)
View(stargazer_dois_modelos)
str(stargazer_dois_modelos)
print(stargazer_dois_modelos)

## Visualizando as métricas dos modelos: 
library(dplyr)
library(knitr)
library(kableExtra)
dev.off()
stargazer_dois_modelos %>%
  select(Shape, R_Squared, R_Adjusted,
         F_Statistic, p_Value_F, Shapiro_Francia, Durbin_Watson, Breusch_Pagan, Breusch_Pagan_Chi_Q) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)

## Salvando o stargazer_dois_modelos:
save(stargazer_dois_modelos, file = "stargazer_dois_modelos.RData")
load(file = "stargazer_dois_modelos.RData")
file.info("stargazer_dois_modelos.RData")
View(stargazer_dois_modelos)

## Comparando os parâmetros dos modelos, com posterior gravação e visualização:
library(jtools)
library(huxtable)
export_summs(final_model, lm_62_15_renamed,scale = F, digits = 4)
preditoras_dois <- export_summs(final_model, lm_62_15_renamed,scale = F, digits = 4)
class(preditoras_dois)
regressores_dois_modelos <- as.data.frame(preditoras_dois)
class(regressores_dois_modelos)
save(regressores_dois_modelos, file = "regressores_dois_modelos.RData")
load(file = "regressores_dois_modelos.RData")
View(regressores_dois_modelos)
renomeado_regressores_dois_modelos <- regressores_dois_modelos
View(renomeado_regressores_dois_modelos)
library(tidyverse)
renomeado_regressores_dois_modelos <- rename(renomeado_regressores_dois_modelos, variaveis_independentes = "names")
renomeado_regressores_dois_modelos <- rename(renomeado_regressores_dois_modelos, final_model = "Model 1")
renomeado_regressores_dois_modelos <- rename(renomeado_regressores_dois_modelos, modelo_LM_62_15_renomeado_pre_stepwise = "Model 2")
View(renomeado_regressores_dois_modelos)
library(dplyr)
library(knitr)
library(kableExtra)
dev.off()
renomeado_regressores_dois_modelos %>%
  select(variaveis_independentes, final_model, modelo_LM_62_15_renomeado_pre_stepwise) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)
save(renomeado_regressores_dois_modelos, file = "renomeado_regressores_dois_modelos.RData")
load(file = "renomeado_regressores_dois_modelos.RData")
View(renomeado_regressores_dois_modelos)

## Gráficos de pontos do final_dataset correlação positiva e negativa:
library(plotly)
options(scipen = 999)
ggplotly(
  ggplot(final_dataset, aes(x = gdp_current_usd, y = net_official_assistence_usd)) +
    geom_point(color = "#39568CFF", size = 1.5) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", se = F, size = 1) +
    xlab("gdp_current_usd") +
    ylab("net_official_assistence_usd") +
    scale_color_manual("(final model) Legenda:",
                       values = "grey50") +
    theme_classic()
)
library(plotly)
options(scipen = 999)
ggplotly(
  ggplot(final_dataset, aes(x = gdp_current_usd, y = adolescent_fertility)) +
    geom_point(color = "#39568CFF", size = 1.5) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", se = F, size = 1) +
    xlab("gdp_current_usd") +
    ylab("adolescent_fertility") +
    scale_color_manual("(final model) Legenda:",
                       values = "grey50") +
    theme_classic()
)

## Incluindo e salvando os fitted values (variável y_Hat) e residuals (variável eRRos) no final_dataset:
final_dataset_fe <- final_dataset
View(final_dataset_fe) #_ shape 62 X 8
summary(final_dataset$gdp_current_usd)
plot (final_model$fitted.values)
final_dataset_fe$y_Hat <- final_model$fitted.values
final_dataset_fe$eRRos <- final_model$residuals
View(final_dataset_fe)

## Salvando o final_dataset (8 variáveis) + fitted values e residuals:
save(final_dataset_fe, file = "final_dataset_fe.RData")
load(file = "final_dataset_fe.RData")
file.info("final_dataset_fe.RData")

## Visualizando o final_dataset (8 variáveis) com as variáveis y_Hat e eRRos:
final_dataset_fe %>%
    kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

## Histograma dos resíduos do modelo OLS linear:
options(scipen = 999)
library(dplyr)
library(ggplot2)
dev.off()
final_dataset_fe %>% 
  mutate(residuos = final_model$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "deepskyblue", 
                 bins = 15,
                 alpha = 1.0) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(final_model$residuals),
                            sd = sd(final_model$residuals)),
                aes(color = "curva normal teórica"),
                size = 3) +
  scale_color_manual("legenda:",
                     values = "#FDE725FF") +
  labs(x = "resíduos",
       y = "frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("gray80"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

## Visualização do comportamento dos resíduos em função dos fitted values:
library(tidyverse)
library(ggside)
final_dataset %>%
  ggplot(aes(x = final_model$fitted.values, y = final_model$residuals)) +
  geom_point(color = "#FDE725FF", size = 2.5) +
  geom_smooth(aes(color = "Fitted Values"),
              method = "lm", formula = y ~ x, se = F, size = 2) +
  geom_xsidedensity(aes(y = after_stat(density)),
                    alpha = 0.5,
                    size = 1,
                    position = "stack") +
  geom_ysidedensity(aes(x = after_stat(density)),
                    alpha = 0.5,
                    size = 1,
                    position = "stack") +
  xlab("Fitted Values") +
  ylab("Resíduos") +
  #  scale_color_tq() +
  scale_fill_gradient() +
  #  scale_fill_tq() +
  scale_fill_gradient2() +
  #  theme_tq() +
  theme_minimal()
theme(ggside.panel.scale.x = 0.4,
      ggside.panel.scale.y = 0.4)

## Fazendo predições com o modelo OLS linear final, ceteris paribus:

#... qual é o Produto Interno Bruto registrado em 1960 ?
options(scipen = 999)
predict(object = final_model,
        data.frame(adolescent_fertility = 89.88180,
                   gcf_percent_gdp = 17.52323,
                   net_official_assistence_usd = 39580002,
                   savings_carbon_percent_of_gni = 0.5876489,
                   savings_mineral_depletion_percent_of_gni = 0.1378949,
                   agricultural_raw_percent_of_merchandise_exports = 14.237061,
                   agricultural_raw_percent_of_merchandise_imports = 2.7826802),
        interval = "confidence", level = 0.95) #_  fit: -126075332779   lwr: -315303773823  upr: 63153108266

final_dataset_fe$y_Hat[1] #_ -126075007953
final_dataset$gdp_current_usd[1] #_ 17030465539

#... qual é o Produto Interno Bruto registrado em 1974 ?
options(scipen = 999)
predict(object = final_model,
        data.frame(adolescent_fertility = 73.82800,
                   gcf_percent_gdp = 24.31146,
                   net_official_assistence_usd = 129809998,
                   savings_carbon_percent_of_gni = 0.5057826,
                   savings_mineral_depletion_percent_of_gni = 0.2198064,
                   agricultural_raw_percent_of_merchandise_exports = 5.991149,
                   agricultural_raw_percent_of_merchandise_imports = 2.0148768),
        interval = "confidence", level = 0.95) #_  fit: 303229467078    lwr: 128649527043    upr: 477809407114

final_dataset_fe$y_Hat[15] #_ 303229128520
final_dataset$gdp_current_usd[15] #_ 109794519728

#... qual é o Produto Interno Bruto registrado em 1989 ?
options(scipen = 999)
predict(object = final_model,
        data.frame(adolescent_fertility = 82.54740,
                   gcf_percent_gdp = 26.90279,
                   net_official_assistence_usd = 232399994,
                   savings_carbon_percent_of_gni = 0.6703514,
                   savings_mineral_depletion_percent_of_gni = 0.26229659,
                   agricultural_raw_percent_of_merchandise_exports = 3.514469,
                   agricultural_raw_percent_of_merchandise_imports = 3.4280610),
        interval = "confidence", level = 0.95) #_  fit: 164709531966     lwr: -132243242687    upr: 461662306618

final_dataset_fe$y_Hat[30] #_ 164709783223
final_dataset$gdp_current_usd[30] #_ 347028139590

#... qual é o Produto Interno Bruto registrado em 1998 ?
options(scipen = 999)
predict(object = final_model,
        data.frame(adolescent_fertility = 83.57800,
                   gcf_percent_gdp = 18.16475,
                   net_official_assistence_usd = 260609985,
                   savings_carbon_percent_of_gni = 0.6046420,
                   savings_mineral_depletion_percent_of_gni = 0.15656106,
                   agricultural_raw_percent_of_merchandise_exports = 3.763963,
                   agricultural_raw_percent_of_merchandise_imports = 2.1034568),
        interval = "confidence", level = 0.95) #_  fit: 651759095987       lwr: 480217539844       upr: 823300652131

final_dataset_fe$y_Hat[39] #_ 651758940451
final_dataset$gdp_current_usd[39] #_ 863711007325

#... qual é o Produto Interno Bruto registrado em 2021 ?
options(scipen = 999)
predict(object = final_model,
        data.frame(adolescent_fertility = 62.93113,
                   gcf_percent_gdp = 18.91975,
                   net_official_assistence_usd = 241033332,
                   savings_carbon_percent_of_gni = 0.7082946,
                   savings_mineral_depletion_percent_of_gni = 0.25245396,
                   agricultural_raw_percent_of_merchandise_exports = 4.781218,
                   agricultural_raw_percent_of_merchandise_imports = 1.0468559),
        interval = "confidence", level = 0.95) #_  fit: 1152938157036       lwr: 983117602231       upr: 1322758711840

final_dataset_fe$y_Hat[62] #_ 1152938200169
final_dataset$gdp_current_usd[62] #_  1608981220812

#... qual seria o Produto Interno Bruto para os seguintes registros ?
options(scipen = 999)
predict(object = final_model,
        data.frame(adolescent_fertility = 68.00,
                   gcf_percent_gdp = 15.00,
                   net_official_assistence_usd = 300000000,
                   savings_carbon_percent_of_gni = 0.50,
                   savings_mineral_depletion_percent_of_gni = 0.80,
                   agricultural_raw_percent_of_merchandise_exports = 3.00,
                   agricultural_raw_percent_of_merchandise_imports = 1.00),
        interval = "confidence", level = 0.95) 
#_  fit: 2063177706724       lwr: 1681353123344       upr: 2445002290103

# Visualização dos valores observados, preditos e resíduos da variável de interesse:
load(file = final_dataset_fe)
load(file = "d7_6.RData")
final_dataset_fea <- final_dataset_fe
final_dataset_fea$ano <- d7_6$series_name
View(final_dataset_fea)
final_dataset_fea %>%
  select(ano, gdp_current_usd, y_Hat, eRRos) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 18)

## Salvando o final_dataset (8 variáveis) + fitted values e residuals:
save(final_dataset_fea, file = "final_dataset_fea.RData")
load(file = "final_dataset_fea.RData")
file.info("final_dataset_fea.RData")

# ...

## Convertendo RData em XLSX, para o Power BI:
install.packages("clipr")
library(clipr)
write_clip(final_dataset_fe)
write_clip(final_dataset_fea)


# ................................ observações .................................

# O mesmo somatório - observado e previsto:
sum(final_dataset_fe$gdp_current_usd) #_ 44215922922349
sum(final_dataset_fe$y_Hat) #_ 44215922922349
44215922922349 - 44215922922349 #_ 0

sum(final_dataset_fe$eRRos) #_ 0.0003681183


# Desvio-padrão desigual:
sd(final_dataset_fe$gdp_current_usd) #_ 780717043386
sd(final_dataset_fe$y_Hat) #_ 735256380760

sd(final_dataset_fe$eRRos) #_ 262520773244


# A mesma média - observado e previsto:
mean(final_dataset_fe$gdp_current_usd) #_ 713160047135
mean(final_dataset_fe$y_Hat) #_ 713160047135
713160047135 - 713160047135 #_ 0

mean(final_dataset_fe$eRRos) #_ 0.000005926817

# valores mínimo, médio e máximo dos erros:
min(final_dataset_fe$eRRos) #_ -552599607547
mean(final_dataset_fe$eRRos) #_ 0.000005926817
max(final_dataset_fe$eRRos) #_ 699742995244
