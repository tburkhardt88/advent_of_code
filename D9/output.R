moves <- read.table('input.txt')
names(moves) <- c('direction', 'steps')
mv_vector <- character()
for(i in 1:nrow(moves)){mv_vector <- c(mv_vector, rep(moves$direction[i], moves$steps[i]))}
rm(moves)

locations <- data.frame(
  step = 0,
  Tx = 0, Ty =0,
  Hx = 0, Hy = 0
)

move_head <- list(
  U = function(rx, ry){c(rx, ry + 1)},
  L = function(rx, ry){c(rx - 1, ry)},
  D = function(rx, ry){c(rx, ry - 1)},
  R = function(rx, ry){c(rx + 1, ry)}
)

for(i in 1:length(mv_vector)){
  head_loc <- move_head[[mv_vector[i]]](locations$Hx[i], locations$Hy[i])
  prev_tail_loc <- c(locations$Tx[i], locations$Ty[i])
  
  tail_close = all(head_loc == prev_tail_loc) |
    any(sapply(lapply(list(c(0,1), c(-1,0),c(0,-1),c(1,0)), function(dg){prev_tail_loc + dg}), function(dg){all(head_loc == dg)})) |
    any(sapply(lapply(list(c(1,1), c(-1,1),c(-1,-1),c(1,-1)), function(dg){prev_tail_loc + dg}), function(dg){all(head_loc == dg)}))
  
  if(tail_close){
    tail_loc = prev_tail_loc
  } else {
    tail_loc <- c(0,0)
    differences <- head_loc - prev_tail_loc
    biggest_change <- which(abs(differences) > 1)
    tail_loc[biggest_change] <- prev_tail_loc[biggest_change] + 1 * sign(differences[biggest_change])
    alt <- setdiff(1:2, biggest_change)
    if(all(abs(differences) > 0)){
      tail_loc[alt] <- prev_tail_loc[alt] + 1 * sign(differences[alt])
    } else {
      tail_loc[alt] <- prev_tail_loc[alt]
    }
  }
  
  new_row <- data.frame(
    step = i,
    Tx = tail_loc[1],
    Ty = tail_loc[2],
    Hx = head_loc[1],
    Hy = head_loc[2]
  )
  locations <- rbind(locations, new_row)
}

length(unique(paste0(locations$Tx,',',locations$Ty)))

#Part 2
rope <- list(rep(list(c(0,0)),10))

for(i in 1:length(mv_vector)){
  rope[[i + 1]] <- list(move_head[[mv_vector[i]]](rope[[i]][[1]][1], rope[[i]][[1]][2]))
  
  for(knot in 2:10){
    leading_knot <- rope[[i+1]][[knot-1]]
    prev_loc <- rope[[i]][[knot]]
    
    tail_close = all(leading_knot == prev_loc) |
      any(sapply(lapply(list(c(0,1), c(-1,0),c(0,-1),c(1,0)), function(dg){prev_loc + dg}), function(dg){all(leading_knot == dg)})) |
      any(sapply(lapply(list(c(1,1), c(-1,1),c(-1,-1),c(1,-1)), function(dg){prev_loc + dg}), function(dg){all(leading_knot == dg)}))
    
    if(tail_close){
      tail_loc = prev_loc
    } else {
      tail_loc <- c(0,0)
      differences <- leading_knot - prev_loc
      biggest_change <- which(abs(differences) > 1)
      tail_loc[biggest_change] <- prev_loc[biggest_change] + 1 * sign(differences[biggest_change])
      alt <- setdiff(1:2, biggest_change)
      if(all(abs(differences) > 0)){
        tail_loc[alt] <- prev_loc[alt] + 1 * sign(differences[alt])
      } else {
        tail_loc[alt] <- prev_loc[alt]
      }
    }
    
    rope[[i + 1]][[knot]] <- tail_loc
  }
}

length(unique(sapply(rope, function(rp){paste(rp[[10]], collapse = ",")})))