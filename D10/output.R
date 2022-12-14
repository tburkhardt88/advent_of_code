commands <- readLines('input.txt')

#Part 1
X = 1
for(i in 1:length(commands)){
  list_length <- length(X)
  
  X[list_length + 1] <- X[list_length]
  if(grepl("^addx", commands[i])){
    additive_factor <- as.numeric(sub("^addx\\s", "", commands[i]))
    X[list_length + 2] <- X[list_length + 1] + additive_factor
  }
}
X <- X[-length(X)]

sum(sapply(c(20,60,100,140,180,220), function(cycle){X[cycle] * cycle}))

#Part 2
x_register <- matrix(X, nrow = 6, ncol = 40, byrow = T)

CRT <- matrix(nrow = 6, ncol = 40)

for(i in 1:dim(CRT)[1]){
  for(j in 1:dim(CRT)[2]){
    if(j %in% x_register[i,j]:(x_register[i,j] + 2)){
      CRT[i,j] = '#'
    } else {
      CRT[i,j] = '.'
    }
  }
}

image(t(CRT == "#"))






