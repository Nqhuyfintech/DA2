# Import library
library(readxl)
library(tidyverse)
library(SciViews)
library(dplyr)
library(magrittr)
library(tidyr)
library(zoo)
library(AER)
library(plm)
library(stargazer)
library(pastecs)
library(corrplot)
# Data collection and input
path <- "D:/Học Kỳ 6/GPMTC 2/Baitap 21-03-2024/data.xlsx"
data <- read_excel(path)
df <- read_excel(path, sheet = "Sheet2")
df <- pdata.frame(df, index = c("Name", "Year"))
        
# obtain the dimension and inspect the structure
is.data.frame(df)
dim(df)
str(df)
# list the first few observations
head(df)
# summarize the variables 'state' and 'year'
summary(df[, c(1, 2)])

# Trích xuất năm từ cột Year
df$Year <- as.numeric(substr(df$Year, 1, 4))

# Trích xuất các ký tự trước dấu "." trong cột Name
df$Name <- sapply(strsplit(as.character(df$Name), "\\."), `[`, 1)
# Chuyển đổi các cột số sang numeric
df <- df %>% 
  mutate(across(-c(Year, Name), as.numeric))
df1 <- transmute(df,
                Name = Name,
                Year = Year,
                FF = (Cash / Totalassets) + (1-(TotalLiabilities/Totalassets)),
                Cash = Cash / Totalassets,
                Tax = Tax / Totalassets,
                Tang = Fixedassets / Totalassets,
                ROA = NetIncomeafterTax / Totalassets,
                LEV = Debt / Totalassets)
df1 <- df1[complete.cases(df1), ]

requireNamespace("magrittr", quietly = TRUE)


# Split df1 into two parts based on the year
before_covid <- df1 %>%
  filter(Year >= 2008 & Year <= 2019)
before_covid_numeric <- before_covid %>% select(-c(Name, Year))

during_covid <- df1 %>%
  filter(Year >= 2020 & Year <= 2021)
during_covid_numeric <- during_covid %>% select(-c(Name, Year))

after_covid <- df1 %>%
  filter(Year > 2021)
after_covid_numeric <- after_covid %>% select(-c(Name, Year))
# Thống kê mô tả 
round(stat.desc(before_covid_numeric),4)
round(stat.desc(during_covid_numeric),4)
round(stat.desc(after_covid_numeric),4)
#hstq
round(cor(before_covid_numeric,use="complete.obs"),4)
round(cor(during_covid_numeric,use="complete.obs"),4)
round(cor(after_covid_numeric,use="complete.obs"),4)
#Data visualization
#Đồ thị phân tán giữa các biến trước, trong covid và sau Covid
op <- par(mfrow = c(1,3))
plot(Cash ~ FF, data = before_covid, pch = 16, col = 'blue', main = "Before:Cash vs FF")
plot(Cash ~ FF, data = during_covid, pch = 16, col = 'blue', main = "During:Cash vs FF")
plot(Cash ~ FF, data = after_covid, pch = 16, col = 'blue', main = "After:Cash vs FF")
par(op)

op1 <- par(mfrow = c(1,3))
plot( Tax~ FF, data = before_covid, pch = 16, col = 'blue', main = "Before:Tax vs FF")
plot( Tax~ FF, data = during_covid, pch = 16, col = 'blue', main = "During:Tax vs FF")
plot( Tax~ FF, data = after_covid, pch = 16, col = 'blue', main = "After:Tax vs FF")
par(op1)

op2 <- par(mfrow = c(1,3))
plot( Tang~ FF, data = before_covid, pch = 16, col = 'blue', main = "Before:Tang vs FF")
plot( Tang~ FF, data = during_covid, pch = 16, col = 'blue', main = "During:Tang vs FF")
plot( Tang~ FF, data = after_covid, pch = 16, col = 'blue', main = "After:Tang vs FF")
par(op2)

op3 <- par(mfrow = c(1,3))
plot( ROA~ FF, data = before_covid, pch = 16, col = 'blue', main = "Before:ROA vs FF")
plot( ROA~ FF, data = during_covid, pch = 16, col = 'blue', main = "During:Tang vs FF")
plot( ROA~ FF, data = after_covid, pch = 16, col = 'blue', main = "After:ROA vs FF")
par(op3)

op4 <- par(mfrow = c(1,3))
plot( LEV~ FF, data = before_covid, pch = 16, col = 'blue', main = "Before:LEV vs FF")
plot( LEV~ FF, data = during_covid, pch = 16, col = 'blue', main = "During:LEV vs FF")
plot( LEV~ FF, data = after_covid, pch = 16, col = 'blue', main = "After:LEV vs FF")
par(op4)
#Cor
raqMatrix <- cor(before_covid_numeric %>% select(FF, Cash, 
                                      Tax, Tang, ROA, LEV))
corrplot(raqMatrix, method = "circle")

raqMatrix <- cor(during_covid_numeric %>% select(FF, Cash, 
                                                 Tax, Tang, ROA, LEV))
corrplot(raqMatrix, method = "circle")

raqMatrix1 <- cor(after_covid_numeric %>% select(FF, Cash, 
                                                 Tax, Tang, ROA, LEV))
corrplot(raqMatrix1, method = "circle")

# subset the data
df2019 <- subset(df1, Year == "2019")
df2023 <- subset(df1, Year == "2023")
# Biểu diễn năm trước covid và năm sau covid using 2019 and 2023 data
df2019_cash <- lm(FF ~ Cash, data = df2019)
df2019_tax <- lm(FF ~ Tax, data = df2019)
df2019_tang <- lm(FF ~ Tang, data = df2019)
df2019_roa <- lm(FF ~ ROA, data = df2019)
df2019_lev <- lm(FF ~ LEV, data = df2019)
df2023_cash <- lm(FF ~ Cash , data = df2023)
df2023_tax <- lm(FF ~ Tax, data = df2023)
df2023_tang <- lm(FF ~ Tang, data = df2023)
df2023_roa <- lm(FF ~ ROA, data = df2023)
df2023_lev <- lm(FF ~ LEV, data = df2023)
diff_ff <- df2023$FF - df2019$FF
diff_cash <- df2023$Cash - df2019$Cash
diff_tax <- df2023$Tax - df2019$Tax
diff_tang <- df2023$Tang - df2019$Tang
diff_roa <- df2023$ROA - df2019$ROA
diff_lev <- df2023$LEV - df2019$LEV
diff_ff_cash <- lm(diff_ff ~ diff_cash)
diff_ff_tax <- lm(diff_ff ~ diff_tax)
diff_ff_tang <- lm(diff_ff ~ diff_tang)
diff_ff_roa <- lm(diff_ff ~ diff_roa)
diff_ff_lev <- lm(diff_ff ~ diff_lev)
# plot the observations and add the estimated regression line in FF and Cash for 2019 and 2023 data and plot the differenced data
cash1 <- par(mfrow = c(1,3))
plot(x = as.double(df2019$Cash),
     y = as.double(df2019$FF),
     xlab = "Cash",
     ylab = "FF",
     main = "FF and Cash in 2019",
     ylim = c(0, 2),
     cex.main=1,
     pch = 20,
     col = "steelblue")
abline(df2019_cash, lwd = 1.5, col="darkred")
legend("topright",lty=1,col="darkred","Estimated Regression Line")

plot(x = as.double(df2023$Cash),
     y = as.double(df2023$FF),
     xlab = "Cash",
     ylab = "FF",
     main = "FF and Cash in 2023",
     cex.main=1,
     ylim = c(0, 2),
     pch = 20,
     col = "steelblue")
abline(df2023_cash, lwd = 1.5, col="darkred")
legend("bottomright",lty=1,col="darkred","Estimated Regression Line")

plot(x = as.double(diff_cash),
     y = as.double(diff_ff),
     xlab = "Change Cash",
     ylab = "Change FF",
     main = "Change in 2019-2023",
     cex.main=1,
     xlim = c(-0.6, 0.6),
     ylim = c(-3,3),
     pch = 20,
     col = "steelblue")
abline(diff_ff_cash, lwd = 1.5,col="darkred")
legend("topright",lty=1,col="darkred","Estimated Regression Line")

# plot the observations and add the estimated regression line in FF and Tax for 2019 and 2023 data and plot the differenced data
tax1 <- par(mfrow = c(1,3))
plot(x = as.double(df2019$Tax),
     y = as.double(df2019$FF),
     xlab = "Tax",
     ylab = "FF",
     main = "FF and Tax in 2019",
     ylim = c(0, 2),
     cex.main=1,
     pch = 20,
     col = "steelblue")
abline(df2019_tax, lwd = 1.5, col="darkred")
legend("topright",lty=1,col="darkred","Estimated Regression Line")

plot(x = as.double(df2023$Tax),
     y = as.double(df2023$FF),
     xlab = "Tax",
     ylab = "FF",
     main = "FF and Tax in 2023",
     cex.main=1,
     ylim = c(0, 2),
     pch = 20,
     col = "steelblue")
abline(df2023_tax, lwd = 1.5, col="darkred")
legend("bottomright",lty=1,col="darkred","Estimated Regression Line")

plot(x = as.double(diff_tax),
     y = as.double(diff_ff),
     xlab = "Change Tax",
     ylab = "Change FF",
     main = "Change in 2019-2023",
     cex.main=1,
     xlim = c(-0.6, 0.6),
     ylim = c(-3,3),
     pch = 20,
     col = "steelblue")
abline(diff_ff_tax, lwd = 1.5,col="darkred")
legend("topright",lty=1,col="darkred","Estimated Regression Line")
# plot the observations and add the estimated regression line in FF and Tang for 2019 and 2023 data and plot the differenced data
tang1 <- par(mfrow = c(1,3))
plot(x = as.double(df2019$Tang),
     y = as.double(df2019$FF),
     xlab = "Tang",
     ylab = "FF",
     main = "FF and Tang in 2019",
     ylim = c(-2, 2),
     cex.main=1,
     pch = 20,
     col = "steelblue")
abline(df2019_tang, lwd = 1.5, col="darkred")
legend("topright",lty=1,col="darkred","Estimated Regression Line")

plot(x = as.double(df2023$Tang),
     y = as.double(df2023$FF),
     xlab = "Tang",
     ylab = "FF",
     main = "FF and Tang in 2023",
     cex.main=1,
     ylim = c(-2, 2),
     pch = 20,
     col = "steelblue")
abline(df2023_tang, lwd = 1.5, col="darkred")
legend("bottomright",lty=1,col="darkred","Estimated Regression Line")

plot(x = as.double(diff_tang),
     y = as.double(diff_ff),
     xlab = "Change Tang",
     ylab = "Change FF",
     main = "Change in 2019-2023",
     cex.main=1,
     xlim = c(-0.6, 0.6),
     ylim = c(-2,2),
     pch = 20,
     col = "steelblue")
abline(diff_ff_tang, lwd = 1.5,col="darkred")
legend("topright",lty=1,col="darkred","Estimated Regression Line")
# plot the observations and add the estimated regression line in FF and ROA for 2019 and 2023 data and plot the differenced data
roa1 <- par(mfrow = c(1,3))
plot(x = as.double(df2019$ROA),
     y = as.double(df2019$FF),
     xlab = "ROA",
     ylab = "FF",
     main = "FF and ROA in 2019",
     ylim = c(0, 2),
     cex.main=1,
     pch = 20,
     col = "steelblue")
abline(df2019_roa, lwd = 1.5, col="darkred")
legend("topright",lty=1,col="darkred","Estimated Regression Line")

plot(x = as.double(df2023$ROA),
     y = as.double(df2023$FF),
     xlab = "ROA",
     ylab = "FF",
     main = "FF and ROA in 2023",
     cex.main=1,
     ylim = c(0, 2),
     pch = 20,
     col = "steelblue")
abline(df2023_roa, lwd = 1.5, col="darkred")
legend("bottomright",lty=1,col="darkred","Estimated Regression Line")

plot(x = as.double(diff_roa),
     y = as.double(diff_ff),
     xlab = "Change ROA",
     ylab = "Change FF",
     main = "Change in 2019-2023",
     cex.main=1,
     xlim = c(-0.6, 0.6),
     ylim = c(-3,3),
     pch = 20,
     col = "steelblue")
abline(diff_ff_roa, lwd = 1.5,col="darkred")
legend("topright",lty=1,col="darkred","Estimated Regression Line")

# plot the observations and add the estimated regression line in FF and LEV for 2019 and 2023 data and plot the differenced data
lev1 <- par(mfrow = c(1,3))
plot(x = as.double(df2019$LEV),
     y = as.double(df2019$FF),
     xlab = "LEV",
     ylab = "FF",
     main = "FF and LEV in 2019",
     xlim = c(0, 1),
     ylim = c(0, 2),
     cex.main=1,
     pch = 20,
     col = "steelblue")
abline(df2019_lev, lwd = 1.5, col="darkred")
legend("topright",lty=1,col="darkred","Estimated Regression Line")

plot(x = as.double(df2023$LEV),
     y = as.double(df2023$FF),
     xlab = "LEV",
     ylab = "FF",
     main = "FF and LEV in 2023",
     cex.main=1,
     xlim = c(0, 1),
     ylim = c(0, 2),
     pch = 20,
     col = "steelblue")
abline(df2023_lev, lwd = 1.5, col="darkred")
legend("bottomright",lty=1,col="darkred","Estimated Regression Line")

plot(x = as.double(diff_lev),
     y = as.double(diff_ff),
     xlab = "Change LEV",
     ylab = "Change FF",
     main = "Change in 2019-2023",
     cex.main=1,
     xlim = c(-0.6, 0.6),
     ylim = c(-3,3),
     pch = 20,
     col = "steelblue")
abline(diff_ff_lev, lwd = 1.5,col="darkred")
legend("topright",lty=1,col="darkred","Estimated Regression Line")

#POOL OLS
fat_mod1 <- lm(FF ~ Cash + Tax + Tang + ROA + LEV, data = before_covid)
fat_mod2 <- lm(FF ~ Cash + Tax + Tang + ROA + LEV, data = during_covid)
fat_mod3 <- lm(FF ~ Cash + Tax + Tang + ROA + LEV, data = after_covid)
#FEM
fat_mod4 <- plm(FF ~ Cash + Tax + Tang + ROA + LEV, data = before_covid,index = c("Name", "Year"),model = "within")
fat_mod5 <- plm(FF ~ Cash + Tax + Tang + ROA + LEV, data = during_covid,index = c("Name", "Year"),model = "within")
fat_mod6 <- plm(FF ~ Cash + Tax + Tang + ROA + LEV, data = after_covid,index = c("Name", "Year"),model = "within")
#REM
fat_mod7 <- plm(FF ~ Cash + Tax + Tang + ROA + LEV, data = before_covid, 
                index = c("Name", "Year"), model = "random")
fat_mod8 <- plm(FF ~ Cash + Tax + Tang + ROA + LEV, data = during_covid, 
                index = c("Name", "Year"), model = "random")
fat_mod9 <- plm(FF ~ Cash + Tax + Tang + ROA + LEV, data = after_covid, 
                index = c("Name", "Year"), model = "random")
# Nhóm fat_mod1 fat_mod4 fat_mod7 vào một bảng
stargazer(fat_mod1, fat_mod4, fat_mod7,
          digits = 3,
          header = FALSE,
          type = "latex",
          se = list(sqrt(diag(vcovHC(fat_mod1, type = "HC1"))), 
                    sqrt(diag(vcovHC(fat_mod4, type = "HC1"))),
                    sqrt(diag(vcovHC(fat_mod7, type = "HC1")))),
          title = "Linear Regression Models (Before COVID-19)",
          model.numbers = FALSE,
          column.labels = c("OLS", "FEM", "REM"))

# Nhóm fat_mod2 fat_mod5 fat_mod8 vào một bảng
stargazer(fat_mod2, fat_mod5, fat_mod8,
          digits = 3,
          header = FALSE,
          type = "latex",
          se = list(sqrt(diag(vcovHC(fat_mod2, type = "HC1"))),
                    sqrt(diag(vcovHC(fat_mod5, type = "HC1"))), 
                    sqrt(diag(vcovHC(fat_mod8, type = "HC1")))),
          title = "Linear Regression Models (During COVID-19)",
          model.numbers = FALSE,
          column.labels = c("OLS", "FEM", "REM"))
# Nhóm fat_mod3 fat_mod6 fat_mod9 vào một bảng
stargazer(fat_mod3, fat_mod6, fat_mod9,
          digits = 3,
          header = FALSE,
          type = "latex",
          se = list(sqrt(diag(vcovHC(fat_mod3, type = "HC1"))),
                    sqrt(diag(vcovHC(fat_mod6, type = "HC1"))), 
                    sqrt(diag(vcovHC(fat_mod6, type = "HC1")))),
          title = "Linear Regression Models (After COVID-19)",
          model.numbers = FALSE,
          column.labels = c("OLS", "FEM", "REM"))

