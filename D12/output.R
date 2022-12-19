lines <- read.table('input.txt')$V1
lines <- lapply(lines, function(ln){strsplit(ln, "")[[1]]})

grid <- matrix(nrow = length(lines), ncol = length(lines[[1]]))
for(i in 1:nrow(grid)){
  for(j in 1:ncol(grid)){
    grid[i,j] <- lines[[i]][j]
  }
}
start <- c(which(grid == 'S')%%dim(grid)[1],which(grid == 'S')%/%dim(grid)[1] + 1)
end <- c(which(grid == 'E')%%dim(grid)[1], which(grid == 'E')%/%dim(grid)[1] + 1)
grid <- ifelse(grid == 'S', 'a', grid)
grid <- ifelse(grid == 'E', 'z', grid)

height_grid = matrix(nrow = dim(grid)[1], ncol = dim(grid)[2])
for(i in 1:nrow(grid)){
  for(j in 1:ncol(grid)){
    height_grid[i,j] <- which(letters == grid[i,j])
  }
}

rm(list = setdiff(ls(), c('height_grid', 'start', 'end')))

#Part 1
directions <- list(
  u = function(current_location) c(current_location[1] + 1, current_location[2]),
  r = function(current_location) c(current_location[1], current_location[2] + 1),
  d = function(current_location) c(current_location[1] - 1, current_location[2]),
  l = function(current_location) c(current_location[1], current_location[2] - 1)
)

grab_potential_locations <- function(current_location){
  ago = which(
    c(
      height_grid[current_location[1] + 1, current_location[2]] <= height_grid[current_location[1], current_location[2]] + 1,
      height_grid[current_location[1], current_location[2] + 1] <= height_grid[current_location[1], current_location[2]] + 1,
      height_grid[current_location[1] - 1, current_location[2]] <= height_grid[current_location[1], current_location[2]] + 1,
      height_grid[current_location[1], current_location[2] - 1] <= height_grid[current_location[1], current_location[2]] + 1
    )
  )
  
  coord_list = lapply(ago, function(index){directions[[c('u','r','d','l')[index]]](current_location)})
  fresh_coords = which(sapply(coord_list, function(coord) !been_there[coord[1], coord[2]]))
  
  return(coord_list[fresh_coords])
}



path_tree <- list(list(start))
been_there <- matrix(FALSE, nrow = dim(height_grid)[1], ncol = dim(height_grid)[2])
been_there[start[1], start[2]] <- TRUE
end_found <- FALSE
step = 1

while(!end_found){
  locs <- lapply(path_tree[[step]], function(cl){grab_potential_locations(cl)})
  locs <- lapply(rapply(locs, enquote, how="unlist"), eval)
  
  for(i in 1:length(locs)){been_there[locs[[i]][1], locs[[i]][2]] = TRUE}
  
  path_tree[[step + 1]] <- locs
  
  step = step + 1
  end_found = any(sapply(locs, function(lc){all(lc == end)}))
}

path_tree[[21]]

