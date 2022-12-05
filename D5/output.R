#reading in data
moves <- read.table('input.txt', skip = 10)
moves <- subset(moves, select = c(V2, V4, V6))
names(moves) <- c('move', 'from', 'to')


stack <- head(read.delim('input.txt', header = F, sep = "\t", dec = "."), 8)$V1
st <- seq(2,34,4)-1
sp <- seq(2,34,4)+1
stack_listed <- lapply(stack, function(strng){unlist(lapply(1:length(st), function(i){substr(strng, st[i], sp[i])}))})
stack_t <- lapply(1:9, function(column){unlist(lapply(rev(stack_listed), function(rw){rw[column]}))})
lgl <- lapply(stack_t, function(x){sapply(x, function(col){grepl('[A-Z]', col)}, USE.NAMES = F)})

stacks <- sapply(1:length(stack_t), function(i){stack_t[[i]][lgl[[i]]]})

rm(list = setdiff(ls(), c('moves', 'stacks')))

#Part 1
move_function <- function(move, from, to, stk){
  l <- length(stk[[from]])
  to_remove <- rev(stk[[from]][(l-move+1):l])
  if(l - move == 0){
    stk[[from]] <- character(0)
  } else {
    stk[[from]] <- stk[[from]][1:(l-move)]
  }
  stk[[to]] <- c(stk[[to]], to_remove)
  return(stk)
}

stack_steps <- list()
stack_steps[[1]] <- stacks
for(i in 2:(nrow(moves) + 1)){
  stack_steps[[i]] <-
    move_function(
      move = moves$move[i - 1],
      from = moves$from[i - 1],
      to = moves$to[i - 1],
      stk = stack_steps[[i - 1]]
    )
}

top_crates <- sapply(stack_steps[[length(stack_steps)]], function(column){
  l <- length(column)
  column[l]
})

top_crates

#Part 2
move_function2 <- function(move, from, to, stk){
  l <- length(stk[[from]])
  to_remove <- stk[[from]][(l-move+1):l]
  if(l - move == 0){
    stk[[from]] <- character(0)
  } else {
    stk[[from]] <- stk[[from]][1:(l-move)]
  }
  stk[[to]] <- c(stk[[to]], to_remove)
  return(stk)
}

stack_steps2 <- list()
stack_steps2[[1]] <- stacks
for(i in 2:(nrow(moves) + 1)){
  stack_steps2[[i]] <-
    move_function2(
      move = moves$move[i - 1],
      from = moves$from[i - 1],
      to = moves$to[i - 1],
      stk = stack_steps2[[i - 1]]
    )
}

top_crates2 <- sapply(stack_steps2[[length(stack_steps2)]], function(column){
  l <- length(column)
  column[l]
})

top_crates2

