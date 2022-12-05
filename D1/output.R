vector <- scan('input.txt', numeric(), blank.lines.skip = F)

elf_list <- list()
sublist_i <- 1
subsublist_i <- 1
for(i in 1:length(vector)){
  if(subsublist_i == 1){
    elf_list[[sublist_i]] <- vector[[i]]
  } else{
    elf_list[[sublist_i]][subsublist_i] <- vector[[i]]
  }
  
  
  if(is.na(vector[i])){
    elf_list[[sublist_i]] <- elf_list[[sublist_i]][-length(elf_list[[sublist_i]])]
    sublist_i <- sublist_i + 1
    subsublist_i <- 1
  } else {
    subsublist_i <- subsublist_i + 1
  }
}
#Part1
biggest_elf <- which.max(lapply(elf_list, sum))
sum(elf_list[[biggest_elf]])

#Part2
sum(head(sort(as.numeric(lapply(elf_list, sum)), decreasing = T), 3))
