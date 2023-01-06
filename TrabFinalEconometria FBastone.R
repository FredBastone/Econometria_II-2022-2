##### Baixar Pacotes #####

install.packages('tidyverse')
install.packages('dplyr')
install.packages('purrr')
install.packages('broom')
install.packages("writexl")
install.packages("sjPlot")
install.packages("zoo")
install.packages('tidyquant')
install.packages('caret')

library(tidyverse)
library(dplyr)
library(purrr)
library(broom)
library(writexl)
library(sjPlot)
library(zoo)
library(tidyquant)
library(caret)

##### Baixar Dados #####

zcb <- read.csv("C:/Users/fredm/Downloads/feds200628.csv")
nber <- read.csv("C:/Users/fredm/Downloads/USRECD.csv")
ffr <- read.csv("C:/Users/fredm/Downloads/DFF.csv")
ebp <- read.csv("C:/Users/fredm/Downloads/ebp_csv.csv")

##### Modificar dados #####

zcb <- zcb %>%
  rename(date = Date,
         level = BETA0,
         slope = BETA1, 
         curve = BETA2, 
         curve2 = BETA3, 
         j1 = SVENY01, j2 = SVENY02, j3 = SVENY03, j4 = SVENY04, j5 = SVENY05,
         j6 = SVENY06, j7 = SVENY07, j8 = SVENY08, j9 = SVENY09, j10 = SVENY10,
         j11 = SVENY11, j12 = SVENY12, j13 = SVENY13, j14 = SVENY14, j15 = SVENY15,
         j16 = SVENY16, j17 = SVENY17, j18 = SVENY18, j19 = SVENY19, j20 = SVENY20,
         j21 = SVENY21, j22 = SVENY22, j23 = SVENY23, j24 = SVENY24, j25 = SVENY25,
         j26 = SVENY26, j27 = SVENY27, j28 = SVENY28, j29 = SVENY29, j30 = SVENY30)

zcb <- zcb %>%
  select(date, level, slope, curve, curve2,
         j1, j2, j3, j4, j5, j6, j7, j8, j9, j10, 
         j11, j12, j13, j14, j15, j16, j17, j18, j19, j20, 
         j21, j22, j23, j24, j25, j26, j28, j29, j30)

zcb <- zcb %>%
  mutate(empiricalslope1 = (j2 - j10), 
         empiricalslope2 = (j1 - j7),
         empiricallevel = (j1 + j7 + j30)/3,
         empiricalcurve = 2*j7 - j2 - j30)

nber <- nber %>%
  rename(date = DATE, recession = USRECD)

nber <- nber[nber$date >= "1961-06-14",]

ffr <- ffr %>%
  rename(date = DATE, ffr = DFF)

ffr <- ffr[ffr$date >= "1961-06-14",]

ebp$month <- format(as.Date(ebp$date), "%Y-%m")

ebp <- ebp %>%
  select(month, gz_spread, ebp)

dados <- left_join(zcb, nber, by = "date")
dados <- left_join(dados, ffr, by = "date") 

dados$month <- format(as.Date(dados$date), "%Y-%m")

dados <- left_join(dados, ebp, by = "month")

dados$date <- as.Date(dados$date)

##### Baixar dados do DOW #####

ticker <- tq_index("DOW")

dow <- tq_get(ticker,
              get = "stock.prices",
              from = "1961-06-14",
              to = "2022-11-18"
)

dow <- dow %>% 
  arrange(date) %>%
  mutate(ret = (adjusted / lag(adjusted)) -1) %>%
  select(symbol, date, sector, ret)

dowdaily <- dow %>%
  group_by(date) %>%
  summarize(retdaily = mean(ret)) %>%
  drop_na()

dowdaily$date <- as.Date(dowdaily$date)

dados <- left_join(dados, dowdaily, by = "date")

dados <- dados %>%
  drop_na(level)

dados <- dados %>%
  mutate(medrets = rollmean(retdaily, k = 5, fill = 0))


##### Criar os Lags da recessão #####

dados <- dados %>%
  mutate(recession_band = ifelse(rollmean(recession, k = 130, fill = 0) > 0, 1, 0),
         recession_band2 = ifelse(rollmean(recession, k = 65, fill = 0) > 0, 1, 0),
         recession_band3 = ifelse(rollmean(recession, k = 520, fill = 0) > 0, 1, 0))


dados <- dados %>%
  mutate(recession_1s = lead(recession_band, order_by = date, 65),
         recession_2s = lead(recession_band, order_by = date, 195),
         recession_3s = lead(recession_band, order_by = date, 325),
         recession_4s = lead(recession_band, order_by = date, 455),
         recession_2y = lead(recession_band3, order_by = date, 260),
         recession_3m = lead(recession_band2, order_by = date, 33))

##### Rodando os Modelos #####

# Limpar os Dados para obter consistência amostral

dados <- dados %>%
  drop_na(recession_1s,
          recession_2s,
          recession_3s,
          recession_4s,
          recession_2y,
          recession_3m,
          empiricalslope1,
          medrets,
          ebp,
          ffr)

# Probit Univariado com a inclinação de NS


probit1_1s <- dados %>% 
  glm(formula = recession_1s ~ slope, family = binomial(link = "probit"))
probit1_2s <- dados %>% 
  glm(formula = recession_2s ~ slope, family = binomial(link = "probit"))
probit1_3s <- dados %>% 
  glm(formula = recession_3s ~ slope, family = binomial(link = "probit"))
probit1_4s <- dados %>% 
  glm(formula = recession_4s ~ slope, family = binomial(link = "probit"))
probit1_2y <- dados %>% 
  glm(formula = recession_2y ~ slope, family = binomial(link = "probit"))
probit1_3m <- dados %>% 
  glm(formula = recession_3m ~ slope, family = binomial(link = "probit"))

tab_model(probit1_2y, probit1_4s, probit1_3s, probit1_2s, probit1_1s, probit1_3m, 
          show.ci = FALSE, p.style = "stars")


# Probit Univariado com a Inclinação observada

probit2_1s <- dados %>% 
  glm(formula = recession_1s ~ empiricalslope1, family = binomial(link = "probit"))
probit2_2s <- dados %>% 
  glm(formula = recession_2s ~ empiricalslope1, family = binomial(link = "probit"))
probit2_3s <- dados %>% 
  glm(formula = recession_3s ~ empiricalslope1, family = binomial(link = "probit"))
probit2_4s <- dados %>% 
  glm(formula = recession_4s ~ empiricalslope1, family = binomial(link = "probit"))
probit2_2y <- dados %>% 
  glm(formula = recession_2y ~ empiricalslope1, family = binomial(link = "probit"))
probit2_3m <- dados %>% 
  glm(formula = recession_3m ~ empiricalslope1, family = binomial(link = "probit"))


tab_model(probit2_2y, probit2_4s, probit2_3s, probit2_2s, probit2_1s, probit2_3m, 
          show.ci = FALSE, p.style = "stars")

# Probit com todos os fatores 

probit2_1sb <- dados %>% 
  glm(formula = recession_1s ~ empiricalslope1 + level + curve, family = binomial(link = "probit"))
probit2_2sb <- dados %>% 
  glm(formula = recession_2s ~ empiricalslope1 + level + curve, family = binomial(link = "probit"))
probit2_3sb <- dados %>% 
  glm(formula = recession_3s ~ empiricalslope1 + level + curve, family = binomial(link = "probit"))
probit2_4sb <- dados %>% 
  glm(formula = recession_4s ~ empiricalslope1 + level + curve, family = binomial(link = "probit"))
probit2_2yb <- dados %>% 
  glm(formula = recession_2y ~ empiricalslope1 + level + curve, family = binomial(link = "probit"))
probit2_3mb <- dados %>% 
  glm(formula = recession_3m ~ empiricalslope1 + level + curve, family = binomial(link = "probit"))

tab_model(probit2_2yb, probit2_4sb, probit2_3sb, probit2_2sb, probit2_1sb, probit2_3mb, 
          show.ci = FALSE, p.style = "stars")

# Probit com FFR

probit2_1s_ffr <- dados %>% 
  glm(formula = recession_1s ~ ffr, family = binomial(link = "probit"))
probit2_2s_ffr <- dados %>% 
  glm(formula = recession_2s ~ ffr, family = binomial(link = "probit"))
probit2_3s_ffr <- dados %>% 
  glm(formula = recession_3s ~ ffr, family = binomial(link = "probit"))
probit2_4s_ffr <- dados %>% 
  glm(formula = recession_4s ~ ffr, family = binomial(link = "probit"))
probit2_2y_ffr <- dados %>% 
  glm(formula = recession_2y ~ ffr, family = binomial(link = "probit"))
probit2_3m_ffr <- dados %>% 
  glm(formula = recession_3m ~ ffr, family = binomial(link = "probit"))

tab_model(probit2_2y_ffr, probit2_4s_ffr, probit2_3s_ffr, probit2_2s_ffr, probit2_1s_ffr, probit2_3m_ffr, show.ci = FALSE,
          p.style = "stars")

# Probit com Excess Bond Premium

probit2_1s_ebp <- dados %>% 
  glm(formula = recession_1s ~ ebp, family = binomial(link = "probit"))
probit2_2s_ebp <- dados %>% 
  glm(formula = recession_2s ~ ebp, family = binomial(link = "probit"))
probit2_3s_ebp <- dados %>% 
  glm(formula = recession_3s ~ ebp, family = binomial(link = "probit"))
probit2_4s_ebp <- dados %>% 
  glm(formula = recession_4s ~ ebp, family = binomial(link = "probit"))
probit2_2y_ebp <- dados %>% 
  glm(formula = recession_2y ~ ebp, family = binomial(link = "probit"))
probit2_3m_ebp <- dados %>% 
  glm(formula = recession_3m ~ ebp, family = binomial(link = "probit"))

tab_model(probit2_2y_ebp, probit2_4s_ebp, probit2_3s_ebp, probit2_2s_ebp, probit2_1s_ebp, probit2_3m_ebp, 
          show.ci = FALSE, p.style = "stars")

# Probit com retornos do dow

probit2_1s_dow <- dados %>% 
  glm(formula = recession_1s ~ medrets, family = binomial(link = "probit"))
probit2_2s_dow <- dados %>% 
  glm(formula = recession_2s ~ medrets, family = binomial(link = "probit"))
probit2_3s_dow <- dados %>% 
  glm(formula = recession_3s ~ medrets, family = binomial(link = "probit"))
probit2_4s_dow <- dados %>% 
  glm(formula = recession_4s ~ medrets, family = binomial(link = "probit"))
probit2_2y_dow <- dados %>% 
  glm(formula = recession_2y ~ medrets, family = binomial(link = "probit"))
probit2_3m_dow <- dados %>% 
  glm(formula = recession_3m ~ medrets, family = binomial(link = "probit"))

tab_model(probit2_2y_dow, probit2_4s_dow, probit2_3s_dow, probit2_2s_dow, probit2_1s_dow, probit2_3m_dow, 
          show.ci = FALSE, p.style = "stars")


# Logit Univariado com a Inclinação observada

logit1_1s <- dados %>% 
  glm(formula = recession_1s ~ empiricalslope1, family = binomial(link = "logit"))
logit1_2s <- dados %>% 
  glm(formula = recession_2s ~ empiricalslope1, family = binomial(link = "logit"))
logit1_3s <- dados %>% 
  glm(formula = recession_3s ~ empiricalslope1, family = binomial(link = "logit"))
logit1_4s <- dados %>% 
  glm(formula = recession_4s ~ empiricalslope1, family = binomial(link = "logit"))
logit1_2y <- dados %>% 
  glm(formula = recession_2y ~ empiricalslope1, family = binomial(link = "logit"))
logit1_3m <- dados %>% 
  glm(formula = recession_3m ~ empiricalslope1, family = binomial(link = "logit"))

tab_model(logit1_2y, logit1_4s , logit1_3s , logit1_2s, logit1_1s, logit1_3m, show.ci = FALSE)

# Probit bivariado com a Inclinação Observada e a Federal Funds Rate

probit3_1s <- dados %>% 
  glm(formula = recession_1s ~ empiricalslope1 + ffr, family = binomial(link = "probit"))
probit3_2s <- dados %>% 
  glm(formula = recession_2s ~ empiricalslope1 + ffr, family = binomial(link = "probit"))
probit3_3s <- dados %>% 
  glm(formula = recession_3s ~ empiricalslope1 + ffr, family = binomial(link = "probit"))
probit3_4s <- dados %>% 
  glm(formula = recession_4s ~ empiricalslope1 + ffr, family = binomial(link = "probit"))
probit3_2y <- dados %>% 
  glm(formula = recession_2y ~ empiricalslope1 + ffr, family = binomial(link = "probit"))
probit3_3m <- dados %>% 
  glm(formula = recession_3m ~ empiricalslope1 + ffr, family = binomial(link = "probit"))


tab_model(probit3_2y, probit3_4s, probit3_3s, probit3_2s, probit3_1s, probit3_3m, show.ci = FALSE,
          p.style = "stars")

# Probit bivariado com a Inclinação Observada e a Excess Bond Premium

probit4_1s <- dados %>% 
  glm(formula = recession_1s ~ empiricalslope1 + ebp, family = binomial(link = "probit"))
probit4_2s <- dados %>% 
  glm(formula = recession_2s ~ empiricalslope1 + ebp, family = binomial(link = "probit"))
probit4_3s <- dados %>% 
  glm(formula = recession_3s ~ empiricalslope1 + ebp, family = binomial(link = "probit"))
probit4_4s <- dados %>% 
  glm(formula = recession_4s ~ empiricalslope1 + ebp, family = binomial(link = "probit"))
probit4_2y <- dados %>% 
  glm(formula = recession_2y ~ empiricalslope1 + ebp, family = binomial(link = "probit"))
probit4_3m <- dados %>% 
  glm(formula = recession_3m ~ empiricalslope1 + ebp, family = binomial(link = "probit"))


tab_model(probit4_2y, probit4_4s, probit4_3s, probit4_2s, probit4_1s, probit4_3m, show.ci = FALSE,
          p.style = "stars")

# Probit com a Inclinação observada e o retorno do DOW

probit5_1s <- dados %>% 
  glm(formula = recession_1s ~ empiricalslope1 + medrets, family = binomial(link = "probit"))
probit5_2s <- dados %>% 
  glm(formula = recession_2s ~ empiricalslope1 + medrets, family = binomial(link = "probit"))
probit5_3s <- dados %>% 
  glm(formula = recession_3s ~ empiricalslope1 + medrets, family = binomial(link = "probit"))
probit5_4s <- dados %>% 
  glm(formula = recession_4s ~ empiricalslope1 + medrets, family = binomial(link = "probit"))
probit5_2y <- dados %>% 
  glm(formula = recession_2y ~ empiricalslope1 + medrets, family = binomial(link = "probit"))
probit5_3m <- dados %>% 
  glm(formula = recession_3m ~ empiricalslope1 + medrets, family = binomial(link = "probit"))


tab_model(probit5_2y, probit5_4s, probit5_3s, probit5_2s, probit5_1s, probit5_3m, show.ci = FALSE,
          p.style = "stars")

# Probit com a Inclinação observada, a Federal Funds Rate e o Excess Bond Premium

probit6_1s <- dados %>% 
  glm(formula = recession_1s ~ empiricalslope1 + ffr + ebp, family = binomial(link = "probit"))
probit6_2s <- dados %>% 
  glm(formula = recession_2s ~ empiricalslope1 + ffr + ebp, family = binomial(link = "probit"))
probit6_3s <- dados %>% 
  glm(formula = recession_3s ~ empiricalslope1 + ffr + ebp, family = binomial(link = "probit"))
probit6_4s <- dados %>% 
  glm(formula = recession_4s ~ empiricalslope1 + ffr + ebp, family = binomial(link = "probit"))
probit6_2y <- dados %>% 
  glm(formula = recession_2y ~ empiricalslope1 + ffr + ebp, family = binomial(link = "probit"))
probit6_3m <- dados %>% 
  glm(formula = recession_3m ~ empiricalslope1 + ffr + ebp, family = binomial(link = "probit"))


tab_model(probit6_2y, probit6_4s, probit6_3s, probit6_2s, probit6_1s, probit6_3m, show.ci = FALSE)

# Probit com a Inclinação observada, a Federal Funds Rate, o Excess Bond Premium e retornos do dow

probit7_1s <- dados %>% 
  glm(formula = recession_1s ~ empiricalslope1 + ffr + ebp + medrets, family = binomial(link = "probit"))
probit7_2s <- dados %>% 
  glm(formula = recession_2s ~ empiricalslope1 + ffr + ebp + medrets, family = binomial(link = "probit"))
probit7_3s <- dados %>% 
  glm(formula = recession_3s ~ empiricalslope1 + ffr + ebp + medrets, family = binomial(link = "probit"))
probit7_4s <- dados %>% 
  glm(formula = recession_4s ~ empiricalslope1 + ffr + ebp + medrets, family = binomial(link = "probit"))
probit7_2y <- dados %>% 
  glm(formula = recession_2y ~ empiricalslope1 + ffr + ebp + medrets, family = binomial(link = "probit"))
probit7_3m <- dados %>% 
  glm(formula = recession_3m ~ empiricalslope1 + ffr + ebp + medrets, family = binomial(link = "probit"))


tab_model(probit7_2y, probit7_4s, probit7_3s, probit7_2s, probit7_1s, probit7_3m, show.ci = FALSE,
          p.style = "stars")

tab_model(probit2_2y, probit3_2y, probit4_2y, probit5_2y, show.ci = FALSE,
          p.style = "stars")

# Testes de Acurácia

corte <- 0.5

confusionMatrix(table(ifelse(fitted(probit7_2y) > corte, 1,0), dados$recession_2y))
confusionMatrix(table(ifelse(fitted(probit7_4s) > corte, 1,0), dados$recession_4s))
confusionMatrix(table(ifelse(fitted(probit7_3s) > corte, 1,0), dados$recession_3s))
confusionMatrix(table(ifelse(fitted(probit7_2s) > corte, 1,0), dados$recession_2s))
confusionMatrix(table(ifelse(fitted(probit7_1s) > corte, 1,0), dados$recession_1s))
confusionMatrix(table(ifelse(fitted(probit7_3m) > corte, 1,0), dados$recession_3m))

confusionMatrix(table(ifelse(fitted(probit2_2y) > corte, 1,0), dados$recession_2y))
confusionMatrix(table(ifelse(fitted(probit2_4s) > corte, 1,0), dados$recession_4s))
confusionMatrix(table(ifelse(fitted(probit2_3s) > corte, 1,0), dados$recession_3s))
confusionMatrix(table(ifelse(fitted(probit2_2s) > corte, 1,0), dados$recession_2s))
confusionMatrix(table(ifelse(fitted(probit2_1s) > corte, 1,0), dados$recession_1s))
confusionMatrix(table(ifelse(fitted(probit2_3m) > corte, 1,0), dados$recession_1s))

# In-Sample vs Out-of-Sample Forecasting

dadosIN <- dados[dados$date <= "1991-03-31",]
dadosOUT <- dados[dados$date > "1991-03-31",]

probitIN_1s <- dadosIN %>% 
  glm(formula = recession_1s ~ empiricalslope1 + ffr + ebp + medrets, family = binomial(link = "probit"))
probitIN_2s <- dadosIN %>% 
  glm(formula = recession_2s ~ empiricalslope1 + ffr + ebp + medrets, family = binomial(link = "probit"))
probitIN_3s <- dadosIN %>% 
  glm(formula = recession_3s ~ empiricalslope1 + ffr + ebp + medrets, family = binomial(link = "probit"))
probitIN_4s <- dadosIN %>% 
  glm(formula = recession_4s ~ empiricalslope1 + ffr + ebp + medrets, family = binomial(link = "probit"))
probitIN_2y <- dadosIN %>% 
  glm(formula = recession_2y ~ empiricalslope1 + ffr + ebp + medrets, family = binomial(link = "probit"))
probitIN_3m <- dadosIN %>% 
  glm(formula = recession_3m ~ empiricalslope1 + ffr + ebp + medrets, family = binomial(link = "probit"))

tab_model(probitIN_2y, probitIN_4s, probitIN_3s, probitIN_2s, probitIN_1s, probitIN_3m, show.ci = FALSE,
          p.style = "stars")

# Definindo as previsões out-of-sample

OOS_1s <- predict(probitIN_1s, newdata = dadosOUT)
OOS_1s <- exp(OOS_1s)

OOS_2s <- predict(probitIN_2s, newdata = dadosOUT)
OOS_2s <- exp(OOS_2s)

OOS_3s <- predict(probitIN_3s, newdata = dadosOUT)
OOS_3s <- exp(OOS_3s)

OOS_4s <- predict(probitIN_4s, newdata = dadosOUT)
OOS_4s <- exp(OOS_4s)

OOS_2y <- predict(probitIN_2y, newdata = dadosOUT)
OOS_2y <- exp(OOS_2y)

OOS_3m <- predict(probitIN_3m, newdata = dadosOUT)
OOS_3m <- exp(OOS_3m)

confusionMatrix(table(ifelse(OOS_1s > corte, 1,0), dadosOUT$recession_1s))
confusionMatrix(table(ifelse(OOS_2s > corte, 1,0), dadosOUT$recession_2s))
confusionMatrix(table(ifelse(OOS_3s > corte, 1,0), dadosOUT$recession_3s))
confusionMatrix(table(ifelse(OOS_4s > corte, 1,0), dadosOUT$recession_4s))
confusionMatrix(table(ifelse(OOS_2y > corte, 1,0), dadosOUT$recession_2y))
confusionMatrix(table(ifelse(OOS_3m > corte, 1,0), dadosOUT$recession_3m))


