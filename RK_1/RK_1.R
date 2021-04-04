#задание 1

infoNA <- function(datas){
  for(c in names(datas)) {
    sumNa <- sum(is.na(datas[c]))
    col <- sum(!is.na(datas[c]))
    if(sumNa>0) {
      propusk <- round(sumNa / NROW(datas[c]) * 100,3)
      print(paste(c,' - Пропущено',sumNa,'значений, это',propusk,'% от набора данных'))
    }
    else print(paste(c ,' - Full column data - ',col,'не нулевых значений'))
  }
}


datas <- read.csv("/home/hino/life.csv")
head(datas)
summary(datas)
infoNA(datas)
x <- datas$bmi #анализируемое значение (ИМТ)
hist(x, breaks = 20, freq = FALSE, col = "lightblue",
     xlab = "BMI",
     ylab = "Плотность вероятности",
     main = "Гистограмма")
abline(v=mean(x,na.rm = TRUE),col='blue')
abline(v=median(x,na.rm = TRUE),col='red')
library(modeest)
abline(v=mfv(x,na_rm = TRUE),col='green')
library(ggpubr)
ggqqplot(datas$bmi, ylab = "BMI")
q3 <- quantile(x, probs=c(3/4), names = FALSE, na.rm = TRUE)  
EX <- q3 + 1.5 * IQR(x,na.rm = TRUE)
mean(x,na.rm = TRUE) + 3 * sd(x,na.rm = TRUE)
EX
datas$bmi[is.na(datas$bmi)] <- EX
x <- datas$bmi
hist(x, breaks = 20, freq = FALSE, col = "lightblue",
     xlab = "BMI",
     ylab = "Плотность вероятности",
     main = "Гистограмма")
abline(v=mean(x,na.rm = TRUE),col='blue')
abline(v=median(x,na.rm = TRUE),col='red')
abline(v=mfv(x,na_rm = TRUE),col='green')
abline(v=EX,col='yellow')

#задание 29

emp.data <- data.frame(
  emp_id = c (1:5), 
  emp_name = c("Rick","Dan","Michelle","Ryan","Gary"),
  constants = c(1,1.1,1,0.9,1), 
  salary = c(623.3,515.2,611.0,729.0,843.25), 
  
  start_date = as.Date(c("2020-01-01", "2020-09-23", "2020-11-15", "2020-05-11",
                         "2020-03-27")),  stringsAsFactors = FALSE
)
emp.data #вывод в консоль
var(emp.data$constants)
emp.data$constants <- NULL
emp.data

#дополнительное задание

library(ggplot2)
ggplot(datas, aes(x=region, y=life_expect)) + 
  geom_violin(trim=FALSE, fill=rainbow(3072),col =rainbow(3072)) +
  geom_boxplot(width=0.1, color="black", alpha=0.2) + theme_minimal() 
