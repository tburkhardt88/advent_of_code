rocks <- readLines('input.txt')
rocks <- lapply(rocks, function(rk){lapply(strsplit(rk, "\\s->\\s")[[1]], function(str){as.numeric(strsplit(str, ',')[[1]])})})
rocks <- lapply(rocks, function(rk){as.data.frame(do.call(rbind, rk)) |> setNames(c('x','y'))})
rocks <- lapply(rocks, function(rk){
  df <- rk
  df$xl <- numeric(length = nrow(df))
  df$yl <- numeric(length = nrow(df))
  for(i in 1:nrow(df)){
    df$xl[i] <- df$x[i + 1]
    df$yl[i] <- df$y[i + 1]
  }
  return(df[!is.na(df$xl),])
})
rocks <- lapply(rocks, function(rk){
  rock_vector <- list()
  for(i in 1:nrow(rk)){
    if(rk$x[i] == rk$xl[i]){
      y_coords <- as.list(seq(rk$y[i], rk$yl[i]))
      x_coords <- as.list(rep(rk$x[i], length(y_coords)))
      coords <- list()
      for(j in 1:length(y_coords)){
        coords[[j]] <- c(x_coords[[j]], y_coords[[j]])
      }
    } 
    if(rk$y[i] == rk$yl[i]){
      x_coords <- as.list(seq(rk$x[i], rk$xl[i]))
      y_coords <- as.list(rep(rk$y[i], length(x_coords)))
      coords <- list()
      for(j in 1:length(y_coords)){
        coords[[j]] <- c(x_coords[[j]], y_coords[[j]])
      }
    }
    rock_vector <- c(rock_vector, coords)
  }
  return(rock_vector)
})
new_rocks <- list()
for(i in 1:length(rocks)){new_rocks <- c(new_rocks, rocks[[i]])}
new_rocks <- new_rocks[!duplicated(new_rocks)]

max_x <- max(sapply(new_rocks, function(nr){nr[1]}))
max_y <- max(sapply(new_rocks, function(nr){nr[2]}))

#Part 1

rock_mat <- matrix(nrow = max_y + 10, ncol = max_x + 10, data = F)
for(i in 1:length(new_rocks)){
  rock_mat[new_rocks[[i]][2],new_rocks[[i]][1]] <- T
}

sand_grains <- matrix(nrow = max_y + 10, ncol = max_x + 10, data = F)

start <- c(0, 500)

next_step <- function(pt){
  if(pt[1] > max_y){
    fall_past_last_rock <<- TRUE
  } else if(!rock_mat[pt[1] + 1,pt[2]] & !sand_grains[pt[1] + 1,pt[2]]){
    point <<- c(pt[1] + 1,pt[2])
  } else if(!rock_mat[pt[1] + 1,pt[2] - 1] & !sand_grains[pt[1] + 1,pt[2] - 1]){
    point <<- c(pt[1] + 1,pt[2] - 1)
  } else if(!rock_mat[pt[1] + 1,pt[2] + 1] & !sand_grains[pt[1] + 1,pt[2] + 1]){
    point <<- c(pt[1] + 1,pt[2] + 1)
  } else {
    point <<- c(pt[1], pt[2])
    sand_grains[pt[1], pt[2]] <<- TRUE
  }
  
  return(point)
}

fall_past_last_rock <- FALSE
while(!fall_past_last_rock){
  point <- next_step(start)
  while(!sand_grains[point[1], point[2]]){
    next_step(point)
    if(fall_past_last_rock){break}
  }
}

length(which(sand_grains))

#Part 2
rock_mat2 <- matrix(nrow = max_y + 10, ncol = 1000, data = F)
for(i in 1:length(new_rocks)){
  rock_mat2[new_rocks[[i]][2],new_rocks[[i]][1]] <- T
}
rock_mat2[max_y + 2,] <- TRUE

sand_grains2 <- matrix(nrow = dim(rock_mat2)[1], ncol = dim(rock_mat2)[2], data = F)

next_step2 <- function(pt){
  if(!rock_mat2[pt[1] + 1,pt[2]] & !sand_grains2[pt[1] + 1,pt[2]]){
    point <<- c(pt[1] + 1,pt[2])
  } else if(!rock_mat2[pt[1] + 1,pt[2] - 1] & !sand_grains2[pt[1] + 1,pt[2] - 1]){
    point <<- c(pt[1] + 1,pt[2] - 1)
  } else if(!rock_mat2[pt[1] + 1,pt[2] + 1] & !sand_grains2[pt[1] + 1,pt[2] + 1]){
    point <<- c(pt[1] + 1,pt[2] + 1)
  } else {
    point <<- c(pt[1], pt[2])
    sand_grains2[pt[1], pt[2]] <<- TRUE
  }
  
  return(point)
}

while(!all(rock_mat2[1,499:501]) & !all(sand_grains2[1,499:501])){
  point <- next_step2(start)
  while(!sand_grains2[point[1], point[2]]){
    next_step2(point)
    if(fall_past_last_rock){break}
  }
}

length(which(sand_grains2)) + 1
