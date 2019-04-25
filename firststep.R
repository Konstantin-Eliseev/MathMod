#Введение в R
#Вычислить среднее для каждой колонки таблицы Iris, за исключением колонки "species' и собрать результат в список (List)
iris
#Создание таблицы Edgar Anderson's Iris Data за исключением колонки species
df.list=iris[1:150,1:4];df.list
#Рассчитаем среднее для каждой колонки и запишем результат в вектор list
list=apply(df.list,2,mean);list
#Рассчитаем среднее для каждой строки и запишем результат в вектор list2
list2=apply(df.list,1,mean);list2
#Создадим случайные 1000 нуклеотидов и сохраним их в вектор DNA
dna=c("T","G","C","A");dna
DNA=sample(dna,1000,TRUE);DNA
#Посчитаем количество нуклеотидов T и A, их долю от общей длинны ДНК и запишем в вектор dna_at
s=table(DNA);s
ratio=summary(factor(DNA))/length(DNA);ratio
dna_at=c(s[1],s[4],ratio[1],ratio[4]);dna_at
#Создадим вектор в котором записан произвольный набор латинских букв длинной не менее 10000 символов и посчитаем количество гласных:
Lat=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z");Lat
LAT=sample(Lat,10000,TRUE);LAT
Lat_s=table(LAT);Lat_s
Vowels=c(Lat_s[1],Lat_s[5],Lat_s[9],Lat_s[15],Lat_s[21],Lat_s[25]);Vowels
#отсортируем все виды таблицы iris по средней длинне лепестков:
mpl=iris[iris$Species%in%c("setosa","virginica","versicolor"),c("Petal.Length","Species")]
apply(mpl,2,sort)