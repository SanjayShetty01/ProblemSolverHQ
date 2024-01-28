tournament <- function (input) {
  input <- input[grepl("^[a-zA-Z ]*;[a-zA-Z ]*;(win|loss|draw)$", input)]
  
  
  data = read.table(text = input, sep = ";", header = F, 
                    col.names = c("Team_1", "Team_2", "Result"))
  
  teams <- unique(c(data$Team_1, data$Team_2)) |> sort()
  
  n <- length(teams)    
  MP <- numeric(n); names(MP) <- teams
  W  <- numeric(n); names(W)  <- teams
  L  <- numeric(n); names(L)  <- teams
  D  <- numeric(n); names(D)  <- teams
  
  for (i in 1:nrow(data)) {
    Team_1 <- data$Team_1[i]
    Team_2 <- data$Team_2[i]
    
    Result <- data$Result[i]
    
    MP[Team_1] <- MP[Team_1] + 1
    MP[Team_2] <- MP[Team_2] + 1
    
    if (Result == "win") {
      W[Team_1] <- W[Team_1] + 1
      L[Team_2] <- L[Team_2] + 1
    } else if (Result == "loss") {
      L[Team_1] <- L[Team_1] + 1
      W[Team_2] <- W[Team_2] + 1
    } else {
      D[Team_1] <- D[Team_1] + 1
      D[Team_2] <- D[Team_2] + 1
    }
  }
  
  P <- W * 3 + D
  
  score_card <- data.frame(Team = teams, MP = MP, W = W, D = D, L = L, P = P)
  score_card <- score_card[order(-P, teams),]
  row.names(score_card) <- 1:nrow(score_card)
  score_card
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


data = read.table(text = input, sep = ";", header = F, 
               col.names = c("Team_1", "Team_2", "Result"))

teams <- unique(c(data$Team_1, data$Team_2)) |> sort()

n <- length(teams)    
MP <- numeric(n); names(MP) <- teams
W  <- numeric(n); names(W)  <- teams
L  <- numeric(n); names(L)  <- teams
D  <- numeric(n); names(D)  <- teams

for (i in 1:nrow(data)) {
  Team_1 <- data$Team_1[i]
  Team_2 <- data$Team_2[i]
  
  Result <- data$Result[i]
  
  MP[Team_1] <- MP[Team_1] + 1
  MP[Team_2] <- MP[Team_2] + 1
  
  if (Result == "win") {
    W[Team_1] <- W[Team_1] + 1
    L[Team_2] <- L[Team_2] + 1
  } else if (Result == "loss") {
      L[Team_1] <- L[Team_1] + 1
      W[Team_2] <- W[Team_2] + 1
    } else {
      D[Team_1] <- D[Team_1] + 1
      D[Team_2] <- D[Team_2] + 1
    }
}

P <- W * 3 + D

score_card <- data.frame(Team = teams, MP = MP, W = W, D = D, L = L, P = P)
score_card <- score_card[order(-P, teams),]
row.names(score_card) <- 1:nrow(score_card)
score_card




score_card <- data.frame(matrix(nrow = length(teams), 
                                ncol = 6))

colnames(score_card) <- c("Team", "MP", "W", "D", "L", "P")

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

names <- unique(c(g$Team_1, g$Team_2)) |> sort()

a = table(c(g$Team_1, g$Team_2))


table(g$Team_1)


table(g$Team_2)

s = length(unique(c(g$Team_1, g$Team_2)))

score_card <- data.frame(matrix(nrow = s, ncol = 6))

colnames(score_card) <- c("Team", "MP", "W", "D", "L", "P")

score_card$Team <- names



a_1 = a |> c() 

g$Result
