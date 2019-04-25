getwd()
#Подключение библиотек:
install.packages("tidyverse")
install.packages("rnoaa")
require("lubridate")
require("tidyverse")
require("rnoaa")
#Создание необходимых векторов для рассчёта Р:
afi=c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)
bfi=c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
dfi=c(0.00,0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)
#Коэффициент использования ФАР:
Kf=300
#Калорийность урожая культуры:
Qj=1600
#Сумма частей основной и побочной продукции:
Lj=2.2
#Стандартная влажность культуры:
Ej=25
#Получение списка всех станций:
station_data=ghcnd_stations() 
write.csv(station_data,"station_data.csv") 
station_data=read.csv("station_data.csv");station_data
#Получение списка станций ближайших к столице выбранного региона:
kazan = data.frame(id = "KAZAN", latitude = 55.78874, longitude = 49.12214);kazan

#Поиск станций по критериям:
kazan_around = meteo_nearby_stations(lat_lon_df = kazan, station_data = station_data,
                                             limit = 25, var = c("PRCP", "TAVG"),
                                             year_min = 2011, year_max = 2011)
#Определение списка станций:
kazan_id=kazan_around[["KAZAN"]][["id"]]
summary(kazan_id)
str(kazan_id)

#Создание таблицы:
all_data = tibble()
for (i in 1:length(kazan_around$KAZAN[,1]))
  
{
  
  print(i)
  print(kazan_id)
  #Загрузка данных для метеостации:
  
  data = meteo_tidy_ghcnd(stationid = kazan_id[i],
                          var="TAVG",
                          date_min="2011-01-01",
                          date_max="2011-12-31")
  print(data)
  #Объединение полученных данных в таблице:
  all_data = bind_rows(
    all_data, data %>%
      #Добавление колонок для группировки по году и месяцу:
      mutate(year = year(date), month = month(date)) %>%
      group_by(month, year) %>%
      mutate(tavg=tavg/10)%>%
      filter(tavg>10)%>%
      #Cуммарная средняя активная температура по месяцу за год:
      summarise (sum = sum(tavg))
  )
}

write.csv(all_data, "all_kazan_data.csv")
all_data
#Сохранение изменений таблицы в векторе clean_data:
clean_data = all_data %>%
#Добавление колонки month для группировки данных:
group_by(month) %>%
#Рассчёт месячного d и cуммы активных тмператур для каждой станции:
summarise(s = mean(sum, na.rm = TRUE)) %>%
#Добавление данных из таблицы
#Добавление колонок для вычислений:
mutate(a = afi[4:10], b = bfi[4:10], d = dfi[4:10]) %>%
#Рассчёт урожайности для каждого месяца:
mutate (fert = ((a + b * 1.0 * s) * d * Kf) / (Qj * Lj * (100-Ej)) )
#Урожайность пшеницы в 2011 году в Республике Татарстан составила (ц/га):
Yield = sum(clean_data$fert)
Yield
