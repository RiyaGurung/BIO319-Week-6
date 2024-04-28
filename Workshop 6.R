#2
install.packages('tidyr')
install.packages('dplyr')
library(tidyr)
library(dplyr)
beetles <- read.table("dung_beetles.csv", sep=",",header=T)
beetles <- beetles %>% select(c(Month, Site, contains('_')))
#we are combining these columns and removing the extra X columns that R has made 
beetles <- beetles %>% select(!starts_with('X'))
#this does the same thing but using '!' which removes only the columns you want 

#3 - Filter will select rows instead 
beetles %>% filter(Onthophagus_sideki > 10)
#selects rows where this species has more than 10 
beetles %>% filter(Onthophagus_sideki & Ochicanthon_woroae > 10)
#this combines the 2 columns that have more than 10 and reduces the number of rows 

beetles %>% filter(Ochicanthon_woroae > 15 & Month =='July'
#removes rows where Ochicanthon has less than 15 and no July  

#4
fixcopris <- function(x)
{gsub('opis','opris', x)}
#here we made a function that would replace copis to copris 
beetles <- rename_with(beetles, fixcopris, .cols = contains('_'))
#we used the rename_with function that is used with other functions to incorporate our fixcorpis function into our columns with species names 

rename_with(gsub(beetles,'opris','opis'), .cols = contains('_'))
# havent figured this out but this is essentilly an easier way to do the same thing without needing to make a function

beetles <- beetles %>% select(c(Month, Site, contains('_'))) %>% rename_with(fixcopris, .cols = contains('_')) %>% pivot_longer(cols = contains('_'), names_to = 'Spp')
#we started by piping everything (including the pivot so the species names were all in one column) and then assigned it to beetles 

#4.1
?rename_with
beetles <- rename_with(beetles,tolower)
#this makes the column names all lower case

#5
?separate_wider_delim
beetles %>% separate_wider_delim(cols = 'spp',delim = '_',names = c('genus','species'))
#separating the species names into genus and species but the 'delim' argument has to come after cols otherwise it won't work
#by doing this you can select and count for just genus

#6 - Mutate
#6.1
beetles %>% mutate('spp'=gsub( '_', ' ',spp)) 
#again, look at the order of the arguments: the 'pattern' has to come first, then the 'replacement' and then x?

report <- read.table('WMR2022_reported_cases_3.txt', sep = '\t', header = T, na.strings = c(''))
# this should be connected with the piping, but separated so i can comment
#this df had NAs in multiple columns, which is why we had to use c() with na.strings
%>% fill(country) %>% pivot_longer(cols = starts_with('X'), names_to = 'year', values_to = 'cases') %>% 
mutate('year'=gsub('X','',year)) 
# answer didnt do this, but i removed the 'X' at the beginning of the years 
%>% pivot_wider(names_from = 'method', values_from = 'cases')
# so we moved the long ass repetitive rows into different columns so we have 3 different methods and the number of cases are integrated into them 

report <- report %>% rename('suspected'='Suspected cases') %>% rename('examined'='Microscopy examined') %>% rename('positive'= 'Microscopy positive')
# we renamed the columns so they are easier to work with 

#6.2
str(report)
#using str we can see that R thinks year is a character vector instead of a numerical value 
report$year <-  as.numeric(as.character(report$year))
#this converts the 'year' column to a numeric vector

#6.3
report <- report %>% mutate('country' = gsub('[1-9]', '',country))
# got rid of the numbers in the country column

#6.4
report <- report %>% mutate("suspected"=as.numeric(gsub("[^0-9]","",suspected))) 
#the '^' means anything else that is not a number and as.numeric directly converts the suspected column to a numeric vector 

clean_number <- function(x){
  as.numeric(gsub('[^0-9]', '',x))
} #we made a function to do the same thing so we don't have to rewrite the code when we want to use it for a different column

#6.5
?across
report %>% mutate(across(.cols = c('suspected','examined','positive'), clean_number))
#the across function allows us to apply the same function to different columns
?tidyr_tidy_select
report %>% mutate(across(.cols = contains('[0-9]'), clean_number))
#why does this not work?
report <- report %>% mutate(across(.cols= !country, clean_number))
#this is the 'tidy' way to select everything other than country, which is what we could have done too 

#6.6
report <- report %>% mutate(test_positivity = examined/positive)%>% mutate(test_positivity = signif(2))
#a new column called test_positivity has been made which has values of examined/positive and has been rounded to 2 significant figures 

#7
report <- report %>% mutate(country = as.factor(country))
#converting country from a character vector to a factor, as it is more flexible 
levels(report$country)
#this shows us all the different categories for the factor column
report <- report %>% mutate(country = gsub('ae','ea', country)) %>% mutate(country = as.factor(country))
#correcting the mispelling of Eritrea and converting it to a factor again 

#8
?write.table
write.table(report, 'WMR2022_reported_cases_clean.txt', quote = FALSE, sep = '\t', col.names = TRUE, row.names = FALSE)
#slayy we made a file with our corrected dataframe!

