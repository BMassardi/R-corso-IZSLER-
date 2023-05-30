# Lezione 30 Maggio 2023 

# cource: File con dentro le librerie. Da lanciare sempre

source(here('R', 'librerie.R'))

library(janitor)

library(readxl)
titanic <- read_excel("~/R/CORSO_R_IZSLER_2023_Tranquillo/titanic.xlsx")
View(titanic)


head(titanic)
str(titanic)
glimpse(titanic) #funzione in Tidyverse

names(titanic)

titanic<-clean_names(titanic) #Per pulire le variabili ossia mette tutto in minuscolo 
# e toglie gli spazi mettendo under-score
titanic

#rinominiamo una variabile con il"verbo" "rename"
titanic<-rename(titanic, sbls=siblings_and_spouses_on_board, parch=parent_children_on_board )


#per selezionare 
select(titanic, pclass, name, sex)
select(titanic, -name)
select(titanic, -c(6:8))
glimpse(titanic)

select(titanic, where(is.numeric))
top_n(titanic, 10) #fa vedere le prime 10 osservazioni
arrange(titanic, fare) # riordinare secondo la variabile fare
glimpse(arrange(titanic, fare))

# di de
view(arrange(titanic, fare)) 

view(arrange(titanic, desc(fare)))


unique(titanic$sex)

#filter
# == significa "uguale a "
filter(titanic, sex == "male")
filter(titanic, sex != "male")

# Posso filtrare con piu'crirteri
filter(titanic, sex!="male", sex!="Male", sex!="male_")

filter(titanic, !sex %in% c("male","Male","male_"))

View(filter(titanic, 
            sex == "female",
            pclass==3,
            !is.na(cabin)))


# mutate permette di aggiungere una nuova colonna al dataset
mutate(titanic, sex= if_else(sex %in% c("Male","male_"), "male", sex))
view(mutate(titanic, sex= if_else(sex %in% c("Male","male_"), "male", sex)))
glimpse((mutate(titanic, sex= if_else(sex %in% c("Male","male_"), "male", sex))))

#POsso usare mutate per trasformare variabili in fattori
mutate(titanic, servived = as.factor(survived))
glimpse(mutate(titanic, across(c ("survived", "sex", "embarked"), as.factor)))


# MANCA ROBA

view (summarise(titanic,
                mediatariffa=mean(fare, na.rm = T),
                          std= sd(fare, na.rm=T),
                          n=n(),
                          tar1000=fare*1000
                )
)
dtgrouped <- group_by(titanic, pclass)

summarise(dtgrouped, mediat = mean(fare, na.rm =T),
          n=n())
tab1<-summarise(dtgrouped, mediat = mean(fare, na.rm =T),
          n=n())  
write.xlsx(tab1, file="tabella.xlsx")

# Oppure

write.xlsx(summarise(dtgrouped, mediat = mean(fare, na.rm =T),
                     n=n()), file="tabella2.xlsx")

surv<-filter(titanic, survived ==1)
mean(titanic$survived)
summarise(dtgrouped, surv = mean(survived, na.rm=T))

##############################################################################
#
# RIFACCIAMO IL CODICE PRECEDENTE CON IL "PIPE" 
#
##############################################################################

library(readxl)
titanic <- read_excel("~/R/CORSO_R_IZSLER_2023_Tranquillo/titanic.xlsx")
View(titanic)
titanic <-clean_names(titanic)

tabsurv<-titanic  %>% rename (sbls=siblings_and_spouses_on_board, 
                     parch=parent_children_on_board) %>% 
                      mutate(age = replace(age, age %in% c("//", "ND"), NA),
                      age = as.numeric(age),
                      sex = if_else (sex %in% c("Male", "male_"),"male", sex),
                      across(c("sex", "embarked", "pclass"), as.factor)) %>% 
                      group_by(pclass, sex) %>% 
                      summarise (Surv = round(mean (survived),2), 
                      n=n()) %>% 
                      mutate(Surv = paste0 (Surv, "(" , n, ")")) %>% 
                      select (-n)  %>% 
                      pivot_wider(names_from = "sex", values_from = "Surv")

view(tabsurv)



# Titanic senza crew

unzip(here("dati", "titanic.zip"), list = T)
dt1 <- read_csv(unzip(here("dati","titanic.zip"), "test.csv"))
dt2 <- read_csv(unzip(here("dati","titanic.zip"), "train.csv"))   
survtest <- readd_csv(unzip(here("dati","titanic.zip"), "gender_submission.csv"))

#
dt1 <- dt1 %>% 
  left_join(survtest, by = "PassengerId")
  bind_rows(df2)
#
  
write.xlsx(df, here(""))



########## altro esempio usando tabelle 
table1
table2
table3 
table4a
table4b


tcasi<-table4a %>%  
  pivot_longer(cols = 2:3, names_to = "years", values_to = "cases")


tpop<-table4b %>%  
  pivot_longer(cols = 2:3, names_to = "years", values_to = "population")
   

table4a %>% 
  pivot_longer(cols = 2:3, names_to = "years", values_to = "cases") %>% 
  left_join(table4b %>% 
              pivot_longer(cols = 2:3, names_to = "years", values_to = "population")
) %>% 
mutate (rate = 1000*cases/population)


################################################################################
#
#  dataset WHO
#
################################################################################

data(who)
force(who)
view(who)
names(who)

#vedi foto.....



################################################################################
#
#  DATASET COVID
#
################################################################################

df<-read_csv("C:\\Users\\massa\\Documents\\R\\CORSO_R_IZSLER_2023_Tranquillo\\Dati_covid_IZSLER\\data-2023-05-30.csv")
df

names(df)
glimpse(df)

summary(df)
unique(factor(df$Materiale))
table (df$Materiale)


unique(factor(df$Reparto))
