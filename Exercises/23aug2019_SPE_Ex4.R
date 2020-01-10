## Exercises 
# Ex.1.4 - Dplyr

## Clean my environment
rm(list = ls())

## load packages 
library(dplyr)
library(Epi)
library(kableExtra)

## Get births data 
data(births)

## tibble vs data.frame
class(births)
head(births)
str(births)

# convert to a tibble 
# tibble objects inherit from data.frame objects 
# functions working on data.frame will work on tibble objects 
# opposite not true
births_tbl <- as_tibble(births)
class(births_tbl)
head(births_tbl)
glimpse(births_tbl)

## piping function 
# classical way
grepl('r','spe-r')
# using piping
'spe-r' %>% grepl('r',.)

## mutate columns 
births_tbl <- births_tbl %>% mutate(
  # modify hyp variable
  hyp = factor(hyp, labels = c("normal", "hyper")), 
  # modify sex var 
  sex = factor(sex, labels = c("M", "F")),
  # creating new var 
  agegrp = cut(matage, breaks = seq(from = 20, to = 45, by = 5), right = F), 
  gest4 = cut(gestwks, breaks = c(20,35,37,39,45), right = F)
  
)
head(births_tbl)

## select columns, filter, arrange rows 
births_tbl %>% filter(bweight > 4000) %>% select(id,sex,bweight,agegrp) 
# select can be useful to reorder and rename 
births_tbl %>% select(
  id,
  'Age group' = agegrp, 
  'Sex' = sex, 
  'Birth weight' = bweight
) %>% arrange(
  desc(`Birth weight`) # note the back quotes here!
)

## group_by and summarise
births.01 <- births_tbl %>% group_by(sex) %>% summarise(count = n())
births.01
births.02 <- births.01 %>% mutate(
  percent = count/sum(count)*100
)
births.02
births.03 <- births.02 %>% summarise_if(is.numeric, sum)
births.03
births.03 %>% rename_all(
  ~ paste0(.,'.tot')
)
births.05 <- births_tbl %>% group_by(sex) %>% summarise(
  count_births = n(), 
  mean_bweight = mean(bweight, na.rm = T)
)
births.05
births.05 %>% summarise(
  mean_bweight_tot = weighted.mean(mean_bweight,count_births), 
  count_births_tot = sum(count_births)
)


## Multiple grouping 

births.06 <- births_tbl %>% group_by(sex,lowbw) %>% summarise(count = n())
births.06
births.06 %>% mutate(
  percent = count/sum(count)*100
)

## Bind and Join tables
# create two tables
age <- tibble(
  pid = 1:6, 
  age = sample(15:25, size = 6, replace = T)
)
center <- tibble(
  pid = c(1,2,3,4,10), 
  center = c("A","B","A","B","C")
)
age
center

# Bind rows
# if col don't match -> filled with NAs
bind_rows(age,center)

# left join
age %>% left_join(center)

# right join
age %>% right_join(center)

# inner join 
age %>% inner_join(center)
merge(age,center)

# full join 
age %>% full_join(center)
merge(age,center,all=T)

## Rendering Tables 
births.08 <- births_tbl %>% filter(
  !is.na(gest4)
) %>% group_by(gest4) %>% 
  summarise(
    N = n()
  ) %>% mutate(
    `(%)` = (N/sum(N)) %>% scales::percent()
  )
births.08
births.08 %>% kable() %>% kable_styling(
  bootstrap_options = c("stripped","hover","condensed","responsive"), 
  full_width = F
) %>% save_kable(file = 'births.08.html', self.contained = T)




