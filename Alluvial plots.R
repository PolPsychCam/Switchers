#you will need these packages:
library(magrittr)
library(dplyr)
install.packages('alluvial')
library(alluvial)

#relabeling categories of categorical variables for alluvial plot

Voters2010and2015$profile_past_vote_2010rev = Voters2010and2015$profile_past_vote_2010 %>%
  (function(x) dplyr::case_when( x == 2 ~ "Con",
                                 x == 3 ~ "Lab",
                                 x == 4 ~ "LibDem",
                                 x == 5 ~ "SNP",
                                 x == 6 ~ "Plaid Cymru",
                                 x == 7 ~ "BNP",
                                 x == 8 ~ "Green",
                                 x == 10 ~ "UKIP"))

Voters2010and2015$generalElectionVoteW6rev = Voters2010and2015$generalElectionVoteW6 %>%
  (function(x) dplyr::case_when( x == 1 ~ "Con",
                                 x == 2 ~ "Lab",
                                 x == 3 ~ "LibDem",
                                 x == 4 ~ "SNP",
                                 x == 5 ~ "Plaid Cymru",
                                 x == 6 ~ "UKIP",
                                 x == 7 ~ "Green",
                                 x == 8 ~ "BNP"))

# Voters2010and2015$generalElectionVoteW6[Voters2010and2015$generalElectionVoteW6 %in% 9999] = NA
# Voters2010and2015$generalElectionVoteW6 %<>% as.character # make sure none of the variables you need are factors
# Voters2010and2015$generalElectionVoteW6 %<>% ifelse(is.na(.), "Other/Didn't vote", .) # make sure there are no NA categories

# first you have to get the Voters2010and2015 in the right format. I do this in three steps:
formatted <- 
  dplyr::select(Voters2010and2015, profile_past_vote_2010rev, generalElectionVoteW6rev) %>% # 1. 
  group_by_all %>%                                    # 2.
  dplyr::summarise(freq = n())                        # 3.

# 1. select the Voters2010and2015 you want to visualise - it should contain columns which code for the category each participant is in at each time
# 2. group by all the categorical variables, so each group is a unique combination of categories across time periods
# 3. use summarise to count how many people are in each group (I call this freq but it doesn't matter)

# now feed this into alluvial function:
# the first argument needs to be a table coding for the categories each group belong to in each time period
# the second argument, 'freq', needs to be how many people are in each group 
alluvial(formatted[,1:2], 
         freq = formatted$freq,
         axis_labels = c("2010", "2015"),
         col = recode_factor(formatted$generalElectionVoteW6rev, !!!pol_palette)) 

# this is nice but it doesn't have any colour

# to add colour you need to specify a 'col' argument
# this should be a vector of colours - each group will receive the corresponding colour in the vector
# usually you want the colours to depend upon the categories in some way
# the best way I have found to do it is to create a palette as a named vector, where
# the names correspond to the categories:
pol_palette <- c("Lab" = "#dc241f", 
                 "Con" = "#0087dc", 
                 "LibDem" = "#fdbb30",
                 "Green" = "#6ab023",  
                 "SNP" = "#ffff00",
                 "Plaid Cymru" = "#e1cc00", 
                 "UKIP" = "#70147a",
                 "BNP" = "black")



# you can then use recode_factor and !!! to conve=rt the categories to colours:
alluvial(formatted[,1:2], freq = formatted$freq, col = recode_factor(formatted$freq, !!!pol_palette))

# recode_factor(formatted$voted_party, !!!pol_palette_hex) 
# this line of code ^^^ works because the entries in the column are the same as the names of the entries in the palette
# therefore it changes each entry in the column into the colour which has been given the same name

# you can also colour the 'ribbons' by the correct party:
alluvial(formatted[,1:2], freq = formatted$freq, col = recode_factor(formatted$correct_party, !!!pol_palette))

# if some of the ribbons are really thin, you can remove them by filtering out frequencies below
# a certain threshold when you create the formatted table:
formatted_filtered <- 
  dplyr::select(Voters2010and2015, correct_party, voted_party) %>% 
  group_by_all %>%                                    
  dplyr::summarise(freq = n()) %>%
  filter(freq > 500)                                  # frequency has to be at least 500

alluvial(formatted_filtered[,1:2], 
         freq = formatted_filtered$freq, 
         col = recode_factor(formatted_filtered$voted_party, !!!pol_palette))


# you can even make the colour depend upon a factor which isn't included in the plot itself
# here I colour the ribbons depending on whether each person's vote was for the 'correct' party 
# (binary = 1, green) or not (binary = 0, red) or whether they didn't vote (binary = NA, grey)
formatted2 <- 
  dplyr::select(Voters2010and2015, correct_party, voted_party, binary) %>% # make sure to include the variable here
  group_by_all %>%                                    
  dplyr::summarise(freq = n())

alluvial(formatted2[,1:2], 
         freq = formatted2$freq, 
         col = formatted2$binary %>% (function(x) case_when(x == 1 ~ "green", # using the case_when function
                                                            x == 0 ~ "red",   # is the nicest way to do this
                                                            is.na(x) == TRUE ~ "grey")))

# there are also ways to change the ordering of the blocks but I haven't got the hang of this yet!


         
         