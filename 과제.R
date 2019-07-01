install.packages("ggplot2")
library(ggplot2)
mpg


View(mpg)

#1
mpg1 = filter(mpg, displ < 4)
mpg2 = filter(mpg, displ > 5)
mean(mpg1$hwy); mean(mpg2$hwy)

mpg %>% #2
  group_by(manufacturer) %>%
  filter(manufacturer %in% c('audi','toyota')) %>%
  summarise(average = mean(hwy))

mpg %>% #3
  group_by(manufacturer) %>%
  filter(manufacturer %in% c('chevrolet','ford','honda')) %>%
  summarise_each(funs(mean),hwy)

mpg3 = select(mpg,class,cty)
head(mpg3); tail(mpg3)

mpg %>%
  select(class,cty)

mpg %>%
  group_by(class) %>%
  select(cty,class) %>%
  filter(class %in% c('suv','compact')) %>%
  summarise_each(funs(mean),cty)

mpg %>%  # 6
  group_by(manufacturer) %>%
  select(model,hwy) %>%
  filter(manufacturer %in% c('audi')) %>%
  arrange(desc(hwy))


mpg %>% #7
  group_by(manufacturer) %>%
  select(manufacturer,model,class,cty,year,displ,hwy) %>%
  mutate(cty_hwy = cty + hwy) %>%
  mutate(ch = cty_hwy / 2) %>%
  arrange(desc(ch))

mpg %>%
  group_by(manufacturer) %>%
  select(manufacturer,cty,class) %>%
  summarise_each(funs(mean),cty)

mpg %>%
  group_by(class) %>%
  select(manufacturer,cty,class) %>%
  summarise_each(funs(mean),cty) %>%
  arrange(desc(cty))

mpg %>%
  group_by(manufacturer) %>%
  select(manufacturer,hwy) %>%
  summarise_each(funs(mean),hwy) %>%
  arrange(desc(hwy))

mpg %>%
  group_by(manufacturer) %>%
  select(manufacturer,class) %>%
  filter(class %in% c('compact')) %>%
  summarise_each(funs(n())) %>%
  arrange(desc(class))

