lines <- read.table('input.txt')$V1
lines <- lapply(lines, function(ln){strsplit(ln, "")[[1]]})

grid <- matrix(nrow = length(lines), ncol = length(lines[[1]]))
for(i in 1:nrow(grid)){
  for(j in 1:ncol(grid)){
    grid[i,j] <- lines[[i]][j]
  }
}
start <- which(grid == 'S')
start <- start%%dim(grid)[1]
end <- which(grid == 'E')
grid <- ifelse(grid == 'S', 'a', grid)
grid <- ifelse(grid == 'E', 'z', grid)

height_grid = matrix(nrow = dim(grid)[1], ncol = dim(grid)[2])
for(i in 1:nrow(grid)){
  for(j in 1:ncol(grid)){
    height_grid[i,j] <- which(letters == grid[i,j])
  }
}

rm(list = setdiff(ls(), 'height_grid'))

