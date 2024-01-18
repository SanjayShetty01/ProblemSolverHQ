tournament <- function(input) {
  
}




################################################################################

input <- c(
  "Courageous Californians;Devastating Donkeys;win",
  "Allegoric Alaskans;Blithering Badgers;win",
  "Devastating Donkeys;Allegoric Alaskans;loss",
  "Courageous Californians;Blithering Badgers;win",
  "Blithering Badgers;Devastating Donkeys;draw",
  "Allegoric Alaskans;Courageous Californians;draw"
  
)


g = read.table(text = input, sep = ";", header = F, 
               col.names = c("Team_1", "Team_2", "Result"))
g

# g$Winner <- ifelse(g$Result == "win", g$Team_1, g$Team_2)
# 
# g$loser <- ifelse(g$Result == "lose", g$Team_2, g$Team_1)

library(dplyr)

g <- g %>%
  mutate(Winner = case_match(
    Result,
    "win" ~ Team_1,
    "loss" ~ Team_2,
    "draw" ~ "-"
  ),
  Loser = case_match(
    Result,
    "win" ~ Team_2,
    "loss" ~ Team_1,
    "draw" ~ "-"
  )
)



g$Drew <- ifelse(g$Result == "draw", T, F)

g

names <- unique(c(g$Team_1, g$Team_2))

a = table(c(g$Team_1, g$Team_2))


table(g$Team_1)


table(g$Team_2)

s = length(unique(c(g$Team_1, g$Team_2)))

score_card <- data.frame(matrix(nrow = s, ncol = 6))

colnames(score_card) <- c("Team", "MP", "W", "D", "L", "P")

score_card$Team <- names



a_1 = a |> c() 

g$Result
