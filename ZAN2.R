getwd() 
#подключение tidyverse: 
require(tidyverse) 
require(lubridate)
#Исходные данные хранятся в файле "eddypro.csv":
#Считывание данных из файла, пропуск первой строки и замена текстовах NA, пустых и сгенерированных пороговых значений на NA, игнорирование строки с "[":
data=read.csv("eddypro.csv",skip=1,na =c("","NA","-9999","-9999.0"), comment=c("[")) 
#Удаление ещё одной строки:
data=data[-1,]
#Удаление невостребованных колонок:
data=data[, c(-1, -3, -9, -12, -15, -18, -21, -30, -35, -63 , -70, -88:-99) ] 
#Преобразование значений в факторные:
data=data %>% mutate_if(is.character,factor) 
#Замена конфликтующих заков колонок: 
names(data)=names(data) %>% str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>% 
  str_replace_all("[-]","_minus_") %>% 
  str_replace_all("[@]","_at_") %>% 
  str_replace_all("[$]","_dollar_") %>% 
  str_replace_all("[#]","_hash_") %>% 
  str_replace_all("[/]","_div_") %>% 
  str_replace_all("[%]","_perc_") %>% 
  str_replace_all("[&]","_amp_") %>% 
  str_replace_all("[\\^]","_power_") %>% 
  str_replace_all("[()]","_") 
#Отображение результата:
glimpse(data) 
summary(data)
#Выбор ночного времени:
data$daytime=as.logical(data$daytime,FALSE)
#Выбор данных по весеннему периоду 2013 года:
data=data[data$DOY>=60&data$DOY<=151&year(data$date)==2013,c(1:ncol(data))] 
#Выбор всех переменных типа numeric:
data_numeric=data[,sapply(data,is.numeric)] 
#Выбор всех прочих переменных: 
data_non_numeric=data[,!sapply(data,is.numeric)] 
#Создание матрицы для корелляционного анализа и преобразование её в таблицу (данные потоков co2):
cor_td=cor(drop_na(data_numeric)) %>% as.data.frame %>% select(co2_flux) 
#Выбор строк с коэффициентом детерминации более 0.2:
vars=row.names(cor_td)[cor_td$co2_flux^2>0.2] %>% na.exclude;vars 
#Создание тестирующей и обучающей выборки:
row_numbers=1:length(data$date) 
teach=sample(row_numbers, floor(length(data$date)*.7)) 
test=row_numbers[-teach] 
#Непересекающиеся подвыборки: 
teaching_tbl_unq=data[teach,] 
testing_tbl_unq=data[test,] 

#Уравнение регрессии:
formula=co2_flux~Tau+rand_err_Tau+H+
  LE+rand_err_LE+h2o_flux+rand_err_h2o_flux+
  co2_molar_density+co2_mixing_ratio+
  air_temperature+u.+T.+un_H+un_LE+LE_scf+un_co2_flux+un_h2o_flux 
#Создание модели линейной регрессии:
model=lm(formula,data=data);model 
#коэффициенты:
coef(model) 
#Остатки:
resid(model) 
#доверительный интервал:
confint(model) 
#P-значения по модели:
summary(model) 
#дисперсионный анализ:
anova(model) 
#графическое представление модели: 
plot(model)
