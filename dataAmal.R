
library(tidyverse) # metapackage with lots of helpful functions
library(repr)

df<-read_csv("C:/Users/Dell/Documents/IBM SkillsBuild Data Analytics Internship Certificates/Data analysis on Education in India/Statement_SES_2011-12-Enrlment.csv")
head(df)



df1 <- df %>% select(-contains("Total"))
# Change the NA values to 0 in all the data frame where ever they are occuring
df1[is.na(df1)] <- 0

head(df1)

newNames <- c('Year','AC-Class-I-V-Boys','AC-Class-I-V-Girls','AC-Class-VI-VIII-Boys','AC-Class-VI-VIII-Girls','AC-Class-IX-X-Boys',
'AC-Class-IX-X-Girls','AC-Class-XI-XII-Boys','AC-Class-XI-XII-Girls','SC-Class-I-V-Boys','SC-Class-I-V-Girls','SC-Class-VI-VIII-Boys',
'SC-Class-VI-VIII-Girls','SC-Class-IX-X-Boys','SC-Class-IX-X-Girls','SC-Class-XI-XII-Boys','SC-Class-XI-XII-Girls',
'ST-Class-I-V-Boys','ST-Class-I-V-Girls','ST-Class-VI-VIII-Boys','ST-Class-VI-VIII-Girls','ST-Class-IX-X-Boys','ST-Class-IX-X Girls',
'ST-Class-XI-XII-Boys','ST-Class-XI-XII-Girls')

# Change the column names
colnames(df1)<- newNames


#Added a column with all totals of rows
df1 <- df1 %>% mutate(All_Total = rowSums (df1[,2:25], na.rm = FALSE, dims = 1))


#visualization (1)

all_boys = sum(df1[,grep('Boys', colnames(df1))])
all_girls = sum(df1[,grep('Girls', colnames(df1))])


options(repr.plot.width=15, repr.plot.height=8)
names <- c("All_Boys", "All_Girls")
values = c(all_boys, all_girls)
percents <- round((values/sum(values))*100,1)
pie(values,
    labels = percents,
    col = rainbow(length(values)),
    main = "Boys Vs Girls - Enrolment",
    cex = 2,
    cex.main = 4)
legend("topright", names, cex =1, fill = rainbow(length(values)))



#new visualization (2)


set1<- df1 %>% select(Year, All_Total)


options(repr.plot.width=30, repr.plot.height=10)
barplot(height = set1$All_Total, 
        xlim = c(0,35),
        main = "Student Enrolment Over Years",
        cex.main = 4,
        names.arg = set1$Year,
        cex.names =1.2,
        cex.axis = 2,
        col = "#f7be8f",
        border = "#362a20",
        xlab = "Years",
        ylab = "Student Enrolment in Millions",
        cex.lab = 1.8)
text (x = 2, y = 200, labels = "1950-51  --- 23.8M", cex = 2)
text (x = 2, y = 180, labels = "2011-12  --- 334M", cex = 2)
text (x = 2, y = 160 , labels = "Overall Growth - 14 Times ", col = 'Blue', cex = 2)



#new visualization (3)

boys <- df1[,grepl("Boys",colnames(df1))]
girls <- df1[,grepl("Girls",colnames(df1))]
df1 <- df1 %>% mutate( Boys = rowSums(boys), Girls = rowSums(girls))
df1 <- df1 %>% mutate(Ratio = Boys/Girls)


values <- cbind(matrix(df1$Boys , ncol = 1), matrix(df1$Girls, ncol = 1))
barplot(height = t(values),
       names.arg = set1$Year,
       cex.names =1.2,
       col = c("#5e4619","#f7be8f"),
       main = "Boys Vs Girls Enrolment",
       cex.main = 4,
       xlab = "Years",
       ylab = "Student Enrolment in Millions",
       cex.lab = 1.8,
       cex.axis = 1.8)
legend(1, 275, legend=c("Boys", "Girls"),
      fill=c("#5e4619","#f7be8f" ), lty=1:2, cex=2, text.font=4)


#new visualization (4)

plot(df1$Ratio, type = 'b', col = 'Blue',
     pch = "*", cex = 4,
     main ="Ratio Boys Vs Girls over years", xaxt="n" ,
     cex.main = 4,
     xlab = 'Years',
     ylab = 'Ratio',
     cex.axis =2, 
      cex.lab = 4)
axis(side =1, at=c(1:30), labels=c(set1$Year),cex.axis = 1.5)
text (x = 2.5, y = 2.9, labels = "<--1950-51 = 2.9 ", cex = 2, col = "Green")
text (x = 30, y = 1.5, labels = "2011-12 = 1.1 ", cex = 2, col = "Red")



#new visualization (5)

primary <- df1[,grepl("Class-I-V",colnames(df1))]
Upperprimary <- df1[,grepl("Class-VI-VIII",colnames(df1))]
middle <- df1[,grepl("Class-IX-X",colnames(df1))]
Highschool <- df1[,grepl("Class-XI-XII",colnames(df1))]


primary_boys = primary[,grepl("Boys",colnames(primary))]
primary_girls = primary[,grep("Girls",colnames(primary))]


values <- cbind(matrix(rowSums(primary_boys)), matrix(rowSums(primary_girls)))
barplot(height =  t(values), beside = T,
       names.arg = set1$Year,
       cex.names =1.2,
       col = c("#5e4619","#f7be8f"),
       main = "Primary Boys Vs Girls Enrolment",
       cex.main = 4,
       xlab = "Years",
       ylab = "Student Enrolment in Millions",
       cex.lab = 1.8,
       cex.axis = 1.8)
legend(1,75, legend=c("Boys", "Girls"),
      fill=c("#5e4619","#f7be8f" ), lty=1:1, cex=1.5, text.font=3)






#new visualization (6)



Upperprimary_boys = Upperprimary[,grepl("Boys",colnames(Upperprimary))]
Upperprimary_girls = Upperprimary[,grep("Girls",colnames(Upperprimary))]


values <- cbind(matrix(rowSums(Upperprimary_boys)), matrix(rowSums(Upperprimary_girls)))
barplot(height =  t(values), beside = T,
       names.arg = set1$Year,
       cex.names =1.2,
       col = c("#5e4619","#f7be8f"),
       main = "UpperPrimary Boys Vs Girls Enrolment",
       cex.main = 4,
       xlab = "Years",
       ylab = "Student Enrolment in Millions",
       cex.lab = 1.8,
       cex.axis = 1.8)
legend(1,75, legend=c("Boys", "Girls"),
      fill=c("#5e4619","#f7be8f" ), lty=1:1, cex=1.5, text.font=3)






#new visualization (7)
middle_boys = middle[,grepl("Boys",colnames(middle))]
middle_girls = middle[,grep("Girls",colnames(middle))]


values <- cbind(matrix(rowSums(middle_boys)), matrix(rowSums(middle_girls)))
barplot(height =  t(values), beside = T,
       names.arg = set1$Year,
       cex.names =1.2,
       col = c("#5e4619","#f7be8f"),
       main = "Middleschool Boys Vs Girls Enrolment",
       cex.main = 4,
       xlab = "Years",
       ylab = "Student Enrolment in Millions",
       cex.lab = 1.8,
       cex.axis = 1.8)
legend(1,75, legend=c("Boys", "Girls"),
      fill=c("#5e4619","#f7be8f" ), lty=1:1, cex=1.5, text.font=3)



#new visualization (8)


Highschool_boys = Highschool[,grepl("Boys",colnames(Highschool))]
Highschool_girls = Highschool[,grep("Girls",colnames(Highschool))]

values <- cbind(matrix(rowSums(Highschool_boys)), matrix(rowSums(Highschool_girls)))
barplot(height =  t(values), beside = T,
       names.arg = set1$Year,
       cex.names =1.2,
       col = c("#5e4619","#f7be8f"),
       main = "High school Boys Vs Girls Enrolment",
       cex.main = 4,
       xlab = "Years",
       ylab = "Student Enrolment in Millions",
       cex.lab = 1.8,
       cex.axis = 1.8)
legend(1,12, legend=c("Boys", "Girls"),
      fill=c("#5e4619","#f7be8f" ), lty=1:1, cex=1.5, text.font=3)




#new visualization (9)

sc = df1[,grepl("SC",colnames(df1))]
st = df1[,grepl("ST",colnames(df1))]
sc_boys = sum(sc[,grepl("Boys",colnames(sc))])
sc_girls = sum(sc[,grepl("Girls",colnames(sc))])
st_boys = sum(st[,grepl('Boys', colnames(st))])
st_girls = sum(st[,grepl('Girls', colnames(st))])



values = c(sc_boys, sc_girls, st_boys, st_girls)
percents = percents <- round((values/sum(values))*100,1)
names = c("SC-Boys", "SC-Girls", "ST-Boys", "ST-Girls")
cols = c("#76a6f5", "#1d283b", "#f0d18d", "#4f452f")
pie(values, 
    col = cols,
    labels = percents,
   main = "SC-ST Boys Vs Girls Entrolment",
   cex.main = 4,
   cex = 4)
legend("topright", names, cex =2, fill = cols)






