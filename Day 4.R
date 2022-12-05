

library(dplyr)
library(tidyr)
library(readr)
library(sjmisc)

# Space needs to be cleared before the last supplies can be unloaded from the ships, and so several Elves have been assigned the job of cleaning up sections of the camp. Every section has a unique ID number, and each Elf is assigned a range of section IDs.

# However, as some of the Elves compare their section assignments with each other, they've noticed that many of the assignments overlap. To try to quickly find overlaps and reduce duplicated effort, the Elves pair up and make a big list of the section assignments for each pair (your puzzle input).

# For example, consider the following list of section assignment pairs:

df_test <- data.frame(assignment_pairs = c('2-4,6-8','2-3,4-5','5-7,7-9', '2-8,3-7', '6-6,4-6', '2-6,4-8')) %>% 
  mutate(comma_position = str_start(assignment_pairs, ',')) %>% 
  mutate(Pair_1 = substr(assignment_pairs, 1, as.numeric(comma_position) - 1)) %>% 
  mutate(Pair_2 = substr(assignment_pairs, as.numeric(comma_position)+1, nchar(assignment_pairs))) %>% 
  mutate(Start_pair_1 = substr(Pair_1, 1, as.numeric(str_start(Pair_1, '-')) - 1)) %>% 
  mutate(End_pair_1 = substr(Pair_1, as.numeric(str_start(Pair_1, '-')) + 1, nchar(Pair_1))) %>% 
  mutate(Start_pair_2 = substr(Pair_2, 1, as.numeric(str_start(Pair_2, '-')) - 1)) %>% 
  mutate(End_pair_2 = substr(Pair_2, as.numeric(str_start(Pair_2, '-')) + 1, nchar(Pair_2))) %>% 
  mutate(Pair_1_enclosed = ifelse(Start_pair_1 >= Start_pair_2 & End_pair_1 <= End_pair_2, 'Yaas', 'No')) %>% 
  mutate(Pair_2_enclosed = ifelse(Start_pair_2 >= Start_pair_1 & End_pair_2 <= End_pair_1, 'Yaas', 'No')) %>% 
  mutate(Any_enclosed = ifelse(Pair_1_enclosed == 'Yaas' | Pair_2_enclosed == 'Yaas', 'Yaas', 'Nooooo'))

df_test %>% 
  group_by(Any_enclosed) %>% 
  summarise(n())

# For the first few pairs, this list means:
df <- read_csv("advent2022/inputs/day_4.csv",
               col_names = 'assignment_pairs') %>% 
  mutate(comma_position = str_start(assignment_pairs, ',')) %>% 
  mutate(Pair_1 = substr(assignment_pairs, 1, as.numeric(comma_position) - 1)) %>% 
  mutate(Pair_2 = substr(assignment_pairs, as.numeric(comma_position)+1, nchar(assignment_pairs))) %>% 
  mutate(Start_pair_1 = as.numeric(substr(Pair_1, 1, as.numeric(str_start(Pair_1, '-')) - 1))) %>% 
  mutate(End_pair_1 = as.numeric(substr(Pair_1, as.numeric(str_start(Pair_1, '-')) + 1, nchar(Pair_1)))) %>% 
  mutate(Start_pair_2 = as.numeric(substr(Pair_2, 1, as.numeric(str_start(Pair_2, '-')) - 1))) %>% 
  mutate(End_pair_2 = as.numeric(substr(Pair_2, as.numeric(str_start(Pair_2, '-')) + 1, nchar(Pair_2)))) %>% 
  mutate(Pair_1_enclosed = ifelse(Start_pair_1 >= Start_pair_2 & End_pair_1 <= End_pair_2, 'Yaas', 'No')) %>% 
  mutate(Pair_2_enclosed = ifelse(Start_pair_2 >= Start_pair_1 & End_pair_2 <= End_pair_1, 'Yaas', 'No')) %>% 
  mutate(Any_enclosed = ifelse(Pair_1_enclosed == 'Yaas' | Pair_2_enclosed == 'Yaas', 'Yaas', 'Nooooo'))

df %>% 
  filter(Any_enclosed == 'Yaas') %>% 
  nrow()

## Part two

# It seems like there is still quite a bit of duplicate work planned. Instead, the Elves would like to know the number of pairs that overlap at all.

# In the above example, the first two pairs (2-4,6-8 and 2-3,4-5) don't overlap, while the remaining four pairs (5-7,7-9, 2-8,3-7, 6-6,4-6, and 2-6,4-8) do overlap:

df %>% 
  select(Pair_1, Pair_2, Start_pair_1, End_pair_1, Start_pair_2, End_pair_2) %>% 
  mutate(Non_overlapping = ifelse(Start_pair_1 > End_pair_2, 'Distinct', ifelse(Start_pair_2 > End_pair_1, 'Distinct', 'Overlapping'))) %>% 
  group_by(Non_overlapping) %>% 
  summarise(n())
    


