lines <- readLines('input.txt')
pairs <- list(list(lines[1]))
pair_number <- 1
for(i in 2:length(lines)){
  if(lines[[i-1]] == ""){
    pair_number = pair_number + 1
    pairs[[pair_number]] = list(lines[i])
  } else if(lines[i] != ""){
    pairs[[pair_number]][[2]] = lines[i]
  }
}

x <- "[[7,1,[8,9,[6,8],7,8],[3,2],2],[[5,[4,6,10,3,7],[5,6,10,7],3,[7,5,7,10,2]],[[4,5,10,6,10],[],7],[7,[9],[10,9,9]],4],[0,[5,3,[9,8]],[4,5,6,0,0],7],[2,1,[[2],[],[6,10],[8],[6,6,10,4,7]]]]"

list_ends <- function(character_index, x){
  count_open <- 0
  count_close <- 0
  character_close <- 0
  for(chr in (character_index + 1):nchar(x)){
    if(substr(x, chr, chr) == '['){count_open = count_open + 1}
    if(substr(x, chr, chr) == ']'){count_close = count_close + 1}
    if(count_close == count_open + 1){
      character_close = chr
      break
    }
  }
  return(character_close)
}

lists <- data.frame(
  A = which(strsplit(x, "")[[1]]== '['),
  B = sapply(which(strsplit(x, "")[[1]]== '['), function(ci){list_ends(ci, x)})
)
lists$range <- lists$B - lists$A - 1
lists <- lists[order(-lists$range),]


