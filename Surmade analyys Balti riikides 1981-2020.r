# 1. Kui suur on naiste ja meeste keskmine surmajuhtumite arv antud vanuses? 

# 2. Milline on vanuse ja suremuse suhe erinevates Balti riikides?
  
# 3. Kuidas andmed aastate jooksul muutusid ja millise perioodi kohta oli erinevus kõige suurem?
  
# 4. Kas suremuse määr on ajaga suurenenud või vähenenud?

library(readxl)
library(ggplot2)
library(dplyr)
library(stringr)                  
library(RColorBrewer)  
library(scales)
library(ggthemes)
library(xtable)
library(reshape)
library(reshape2)

setwd(dirname(rstudioapi::getActiveDocumentContext()[[2]]))

deaths <- read_excel("deaths.xlsx", sheet = "WHOMortalityDatabase_Trends_yea")
deaths <- deaths %>% 
  filter(Country %in% c("Estonia", "Latvia", "Lithuania"), !Age_group %in% c("[All]", "[Unknown]"), Sex != "All", Year > 1980)

# kustutame "[]"
deaths$Age_group <- str_sub(deaths$Age_group, 2, -2)

#############################
# 1. Kui suur on naiste ja meeste keskmine surmajuhtumite arv antud vanuses? 
# https://rpubs.com/m_dev/tables_and_plots


# abi funktsioon, et teha esimene rea veergude nimedeks(kasutatakse "summary" tabelides)
first_row_as_names <- function(table_name){
  names(table_name) <- table_name[1,] 
  table_name <- table_name[-1,]
  table_name <- table_name[,c(1 ,2, 11, 3 , 4:10, 12:19)] # vanuse gruppid õiges järjekorras, 
  # arrange(Age groups) sorteerib arvude järgi ja vahemik 5-9 on kuskil keskel
  print(table_name)
  return(table_name)
}

# keskmine surmajuhtumite arv vanusese ja soo järgi 

age_sex <- deaths %>%
  group_by(Sex, Age_group) %>%
  summarise(mean = round(mean(Number), 2)) %>%
  group_by(Age_group) %>%
  mutate(All = sum(mean), prop = mean/All) %>%
  arrange(Age_group) %>%
  select(-All)

age_sex

# keskmine surmajuhtumite arv vanusese järgi
age_sex2 <- deaths %>%
  group_by(Age_group) %>%
  summarise(mean = round(mean(Number),1))

age_sex2

# koostame naiste ja meeste keskmine surmajuhtumite arv antud vanusese tabeli
female_tab <- age_sex %>% 
  filter(Sex == "Female") %>%
  arrange(Age_group)
groups <- female_tab$Age_group
female <- female_tab$mean

male_tab <- age_sex %>% 
  filter(Sex == "Male")%>%
  arrange(Age_group)
male <- male_tab$mean

mean <- age_sex2$mean

summary1 <- data.frame(t(data.frame(groups, female, male, mean)))

summary1 <- first_row_as_names(summary1)
rownames(summary1) <- c("Naine", "Mees", "Keskmine")
groups <- names(summary1) #õiges järjekorras
summary1


# vanused
ggplot(data = data.frame(age_sex), mapping = aes(x = factor(Age_group, level = groups),
                                                      y = prop, fill = Sex)) + geom_col(position = "fill") + 
  scale_y_continuous(labels = percent) + labs(x = "Vanuserühm", y = "Osakaal") + theme_bw() +
  scale_fill_brewer(palette = "Paired", labels = c("Naised", "Mehed"), name = "Sugu") 

# vanused2
ggplot(age_sex, aes(x=factor(Age_group, level = groups), y=mean, col=Sex, group = Sex)) + geom_line(size=1) + geom_point() + 
  geom_line(data = age_sex2, mapping = aes(x = factor(Age_group, level = groups), y = mean, group = 2), color = "grey", size=1) +
  scale_color_brewer(palette="Paired", labels = c("Naised", "Mehed"), name = "Sugu") +  labs(x = "Vanuserühm", y = "Surmade arv") + theme_bw() 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# laadime tabelit alla

print(xtable(summary1[, 1:10], type = "latex"), file = "summary1.tex")
print(xtable(summary1[,11:ncol(summary1)], type = "latex"), file = "summary2.tex")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#############################
# 2. Milline on vanuse ja suremuse suhe erinevates Balti riikides?


# ümardamisele ja protsenti teisendusele funktsioon
round_and_percent <- function(probability){
  return(paste(round(probability*100, 2), "%", sep = ""))
}

country_ratio <- deaths %>%
  group_by(Country, Age_group) %>%
  summarise(All_deaths_in_group = sum(Number)) %>%
  group_by(Country) %>%
  mutate(All_deaths = sum(All_deaths_in_group), prop = All_deaths_in_group/All_deaths)

country_ratio

# riikide suhe
ggplot(data = country_ratio, aes(x=factor(Age_group, level = groups), y = prop, group = Country)) + 
  geom_line(aes(colour = Country)) + scale_y_continuous(labels = percent) + geom_point(aes(colour = Country)) +  
  scale_color_manual(values = c("#00AFBB", "#E7B800","#FC4E07"), name="Riik", labels = c("Eesti", "Läti","Leedu")) +
  labs(y ="Osakaal", x = "Vanuserühm") + theme_bw() 

# Riikide min ja max tõenäosused
min_max <- country_ratio %>% 
  group_by(Country) %>%
  summarise(min = round_and_percent(min(prop)), max = round_and_percent(max(prop)))

colnames(min_max) <- c("Riik", "Min", "Max")
min_max[4,] <- list(NA, "10-14", "85+")
min_max[,1] <- c("Eesti", "Läti", "Leedu", NA)

min_max


# Koostame vanuse ja suremuse suhe erinevates Balti riikides tabeli
Estonia <- country_ratio %>%
  filter(Country=="Estonia")
groups2 <- Estonia$Age_group
Estonia <- round_and_percent(Estonia$prop)

Latvia <- country_ratio %>%
  filter(Country=="Latvia")
Latvia <- round_and_percent(Latvia$prop)

Lithuania <- country_ratio %>%
  filter(Country=="Lithuania")
Lithuania <- round_and_percent(Lithuania$prop)

summary2 <- data.frame(t(data.frame(groups2, Estonia, Latvia, Lithuania)))
summary2 <- first_row_as_names(summary2)
rownames(summary2) <- c("Eesti", "Läti", "Leedu")
summary2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# laadime tabelit alla

print(xtable(min_max, type = "latex"), file = "min_max.tex", include.rownames=FALSE)
print(xtable(summary2[, 1:10], type = "latex"), file = "summary3.tex")
print(xtable(summary2[,11:ncol(summary2)], type = "latex"), file = "summary4.tex")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#####################################
# 3. Kuidas andmed aastate jooksul muutusid ja millise perioodi kohta oli erinevus kõige suurem?

ratio_by_year <- deaths %>%
  group_by(Year, Country) %>%
  summarise(Deaths = sum(Number)) %>%
  group_by(Country) %>%
  mutate(All_time_Deaths = sum(Deaths), prop = Deaths/All_time_Deaths)

ratio_by_year

# max ja min riigides
ratio_by_year %>%
  group_by(Country) %>%
  summarise(max = max(Deaths), min = min(Deaths))

min_max2 <- ratio_by_year %>%
  filter(Deaths %in% c(22097, 41736, 46466, 15181, 27486, 35040)) %>%
  arrange(Country) %>%
  select(-All_time_Deaths, -prop) %>%
  arrange(Deaths)

min_max2 <- t(min_max2)[c(2,1,3),c(1:3,5,4,6)]
rownames(min_max2) <- c("Riik", "Aasta", "Surmade arv")
min_max2[1,] <- c("Eesti","Eesti", "Läti","Läti", "Leedu","Leedu")
colnames(min_max2) <- c("min", "max", "min", "max", "min", "max")
min_max2

# aastate suhe
ggplot(ratio_by_year, aes(x = Year, y = Deaths, color = factor(Country))) + geom_line(size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800","#FC4E07"), name="Riik", labels = c("Eesti", "Läti","Leedu")) +
  labs(y ="Surmade arv", x = "Aasta") + theme_bw() +
  geom_segment(aes(x = 1994, y = 0, xend = 1994, yend = 46466),linetype="dashed", color = "red") +
  geom_segment(aes(x = 2019, y = 0, xend = 2019, yend = 27486),linetype="dashed", color = "blue") +
  geom_segment(aes(x = 1982, y = 0, xend = 1982, yend = 35040),linetype="dashed", color = "blue")+
  geom_point(aes(x = 1994, y = 46466), color = "red" ) +
  geom_point(aes(x = 1994, y = 41736), color = "red") +
  geom_point(aes(x = 1994, y = 22097), color = "red") +
  geom_point(aes(x = 2019, y = 15181), color = "blue") +
  geom_point(aes(x = 2019, y = 27486), color = "blue") +
  geom_point(aes(x = 1982, y = 35040), color = "blue") 


# vanuserühma surmuse määr 1981-2020 perioodil
age_years <- deaths %>% 
  group_by(Country, Year, Age_group) %>%
  summarise(all = sum(Number)) %>%
  mutate(Age = case_when(                                    # eraldame vanuserühmad
    Age_group %in% groups[0:5] ~ "noor",                     # 0-19 - noor
    Age_group %in% groups[6:13] ~ "täiskasvanu",             # 20-59 - täiskasvanu
    Age_group %in% groups[14:19] ~ "pensionär")) %>%         # 60+ - pensionär
  group_by(Country, Year, Age) %>%
  summarise(deaths = sum(all))

Age <- data.frame(c("noor", "0-19"), c("täiskasvanu", "20-59"), c("pensionär", "60+"))


# Eesti
age_years %>% 
  filter(Country=="Estonia") %>%
  ggplot(aes(x=Year, y=deaths, color=factor(Age, levels = c("pensionär", "täiskasvanu", "noor")))) +
  geom_line(size = 1) + scale_color_brewer(palette="Paired", name = "Vanus") +
  labs(x = "Aasta", y = "Surmade arv") + theme_bw() 

# Läti
age_years %>% 
  filter(Country=="Latvia") %>%
  ggplot(aes(x=Year, y=deaths, color=factor(Age, levels = c("pensionär", "täiskasvanu", "noor")))) +
  geom_line(size = 1) + scale_color_brewer(palette="Paired", name = "Vanus") +
  labs(x = "Aasta", y = "Surmade arv") + theme_bw() 

# Leedu
age_years %>% 
  filter(Country=="Lithuania") %>%
  ggplot(aes(x=Year, y=deaths, color=factor(Age, levels = c("pensionär", "täiskasvanu", "noor")))) +
  geom_line(size = 1) + scale_color_brewer(palette="Paired", name = "Vanus") +
  labs(x = "Aasta", y = "Surmade arv") + theme_bw() 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# laadime tabelit alla

print(xtable(min_max2, type = "latex"), file = "min_max2.tex") 
print(xtable(Age, type = "latex"), file = "age.tex", include.rownames = F, include.colnames = F) 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  
##########################################  
# 4. Kas suremuse määr on ajaga suurenenud või vähenenud?

# Teeme muudatuste graafik kõikideks aastateks
ggplot(ratio_by_year, aes(x = Year, y = prop, color = factor(Country))) + geom_line(size=1)+
  scale_color_manual(values = c("#00AFBB", "#E7B800","#FC4E07"), name = "Riik", labels = c("Eesti", "Läti","Leedu")) +
  labs(y ="Osakaal", x = "Aasta") + theme_bw() + scale_y_continuous(labels = percent)

min_max3 <- t(ratio_by_year %>%
  group_by(Country) %>%
  summarise(min = round_and_percent(min(prop)), max = round_and_percent(max(prop))))
min_max3[1,] <- c("Eesti", "Läti", "Leedu")
rownames(min_max3) <- c("Riik", "min", "max")
min_max3

# Teeme üldistatud trendiga muutuste graafik
ggplot(ratio_by_year, aes(x = Year, y = prop, color = factor(Country))) + geom_smooth(se = FALSE, method = lm) + scale_y_continuous(labels = percent) +
  scale_color_manual(values = c("#00AFBB", "#E7B800","#FC4E07"), name = "Riik", labels = c("Eesti", "Läti","Leedu")) +
  labs(y ="Osakaal", x = "Aasta") + theme_bw() + scale_y_continuous(labels = percent)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# laadime tabelit alla

print(xtable(min_max3, type = "latex"), file = "min_max3.tex", include.colnames = F) 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
