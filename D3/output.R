
vector <- scan('input.txt', character())

priorities <- c(1:52) 
names(priorities) <- c(letters[1:26], LETTERS[1:26])

#Part 1
priority <- numeric(length = length(vector))
for(i in 1:length(vector)){
  strng <- vector[i]
  
  l <- nchar(strng)
  c1 <- sub(paste0('.{',l/2,'}$'), "", strng)
  c2 <- sub(paste0('^.{',l/2,'}'), "", strng)
  
  prioritised <- lapply(strsplit(c(c1,c2), ''), function(x){priorities[x]})
  
  priority[i] <- intersect(prioritised[[1]], prioritised[[2]])
  rm(list = c('strng','l','c1','c2','prioritised'))
}

sum(priority)

#Part 2
start_ind <- seq(1,length(vector)-2,3)
grp_priority <- numeric(length = length(start_ind))
for(i in 1:length(start_ind)){
  grp <- vector[start_ind[i]:(start_ind[i]+2)]
  
  prioritised <- lapply(strsplit(grp, ''), function(x){priorities[x]})
  
  grp_priority[i] <- intersect(intersect(prioritised[[1]], prioritised[[2]]), prioritised[[3]])
}

sum(grp_priority)













