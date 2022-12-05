#Part 1
rounds <- read.table('input.txt') 
names(rounds) <-c('opponent', 'you') 

my_outcomes <- list(
  X = list(A = 3,B = 0,C = 6),
  Y = list(A = 6,B = 3,C = 0),
  Z = list(A = 0,B = 6,C = 3)
)

hand_pts <- list(X = 1, Y = 2, Z = 3)

rounds$winning_points <- numeric(nrow(rounds))

for(i in 1:nrow(rounds)){
  rounds$winning_points[i] <- my_outcomes[[rounds$you[i]]][[rounds$opponent[i]]]
}

rounds$hand_points <- numeric(nrow(rounds))

for(i in 1:nrow(rounds)){
  rounds$hand_points[i] <- hand_pts[[rounds$you[i]]]
}

rounds$round_points <- rounds$hand_points + rounds$winning_points

sum(rounds$round_points)

rm(list = ls())

#Part 2 full strategy 
rounds <- read.table('input.txt') 
names(rounds) <-c('opponent', 'strategy')

play_guide <- list(
  A = list(X = 'C', Y = 'A', Z = 'B'),
  B = list(X = 'A', Y = 'B', Z = 'C'),
  C = list(X = 'B', Y = 'C', Z = 'A')
)
shape_pts <- list(A = 1, B = 2, C = 3)
round_pts <- list(X = 0, Y = 3, Z = 6)

rounds$you <- character(nrow(rounds))
for(i in 1:nrow(rounds)){
  rounds$you[i] <- play_guide[[rounds$opponent[i]]][[rounds$strategy[i]]]
}
rounds$win_points <- numeric(nrow(rounds))
rounds$shape_points <- numeric(nrow(rounds))
for(i in 1:nrow(rounds)){
  rounds$win_points[i] <- round_pts[[rounds$strategy[i]]]
  rounds$shape_points[i] <- shape_pts[[rounds$you[i]]]
}

rounds$round_total <- rounds$shape_points + rounds$win_points

sum(rounds$round_total)
