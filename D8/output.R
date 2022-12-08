forest <- t(sapply(strsplit(readLines('input.txt'), ''), function(row){as.numeric(row)}))
rws <- dim(forest)[1]
cls <- dim(forest)[2]

#Part 1
visibility_matrix <- matrix(nrow =rws, ncol = cls)
for(i in 1:rws){
  for(j in 1:cls){
    if(i %in% c(1,rws) | j %in% c(1, cls)){
      visibility_matrix[[i,j]] <- T
    } else{
      left_trees <- all(forest[i, 1:(j-1)] < forest[i,j])
      top_trees <- all(forest[1:(i-1), j] < forest[i,j])
      right_trees <- all(forest[i, (j+1):cls] < forest[i,j])
      bottom_trees <- all(forest[(i+1):rws, j] < forest[i,j])
      
      visibility_matrix[[i,j]] <- any(left_trees, top_trees, right_trees, bottom_trees)
    }
  }
}

sum(visibility_matrix)

#Part 2
scenic_scores <- matrix(nrow =rws, ncol = cls) 
for(i in 1:rws){
  for(j in 1:cls){
    l = 0
    for(l_ind in (j-1):1){
      if(j == 1){
        break
      } else if(forest[i, l_ind] < forest[i,j]){
        l = l + 1
      } else {
        l = l + 1
        break
      }
    }
    
    u = 0
    for(u_ind in (i-1):1){
      if(i == 1){
        break
      } else if(forest[u_ind, j] < forest[i,j]){
        u= u + 1
      } else {
        u= u + 1
        break
      }
    }
    
    r = 0
    for(r_ind in (j+1):cls){
      if(j == cls){
        break
      } else if(forest[i, r_ind] < forest[i,j]){
        r = r + 1
      } else {
        r = r + 1
        break
      }
    }
    
    d = 0
    for(d_ind in (i+1):rws){
      if(i == rws){
        break
      } else if(forest[d_ind, j] < forest[i,j]){
        d = d + 1
      } else {
        d = d + 1
        break
      }
    }

    scenic_scores[[i,j]] <- l * u * r * d
  }
}

max(scenic_scores)
