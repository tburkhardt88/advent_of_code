
data <- read.table('input.txt') 
pairs_chr <- strsplit(data$V1, ',')
pairs <- lapply(pairs_chr, function(pr){lapply(pr, function(x){as.numeric(strsplit(x, '-')[[1]])})})

#Part 1
logical_list <- unlist(lapply(pairs, function(pr) {
  (pr[[1]][1] >= pr[[2]][1] & pr[[1]][2] <= pr[[2]][2]) | (pr[[2]][1] >= pr[[1]][1] & pr[[2]][2] <= pr[[1]][2])
}))

sum(logical_list)

#Part 2

pairs_extended <- lapply(pairs, function(pr){lapply(pr, function(ind){seq(ind[1],ind[2])})})

overlapping_pairs <- unlist(lapply(pairs_extended, function(pr){intersect(pr[[1]], pr[[2]])}) |> lapply(function(overlap){length(overlap) > 0}))

sum(overlapping_pairs)
