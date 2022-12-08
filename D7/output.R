terminal_lines <- readLines('input.txt')

command_list <- list()
ls_no <- 0
for(i in 1:length(terminal_lines)){
  if(grepl('^\\$', terminal_lines[i])){
    ls_no = ls_no + 1
    command_list[[ls_no]] <- c(terminal_lines[i])
  } else {
    command_list[[ls_no]] <- c(command_list[[ls_no]], terminal_lines[i])
  }
}
rm(ls_no)
list_actions <- function(lst){
  list_items = lst[-1]
  size_df <- data.frame(raw = list_items[grep('^\\d',list_items)])
  split_files <- strsplit(size_df$raw, '\\s')
  size_df$sz = as.numeric(sapply(split_files, function(i){i[1]}))
  size_df$filename = sapply(split_files, function(i){i[2]})
  size_df <- size_df[,-1]
  
  dir_names <- list_items[grep('^dir',list_items)] 
  dir_names <- sapply(dir_names, function(d){sub('^dir\\s', '', d)}, USE.NAMES = F)
  dirs <- vector('list', length(dir_names))
  names(dirs) <- dir_names
  
  return(c(dirs, list(files = size_df,size = sum(size_df$sz))))
}


current_directory <- "/"
tree <- vector('list', 1)
names(tree) <- current_directory

for(i in 2:length(command_list)){
  type_of_command <- ifelse(grepl('^\\$\\sls', command_list[[i]][1]), 'ls', 'cd')
  
  if(type_of_command == 'cd'){
    cmd <- sub("^\\$\\scd\\s", "", command_list[[i]][1])
    if(cmd == ".."){
      current_directory <- current_directory[-length(current_directory)]
    }
    if(grepl('^\\w', cmd)){
      current_directory <- c(current_directory,cmd)
    } 
    if(cmd == "/"){
      current_directory = "/"
    }
  }
  
  if(type_of_command == 'ls' & is.null(tree[[current_directory]])){
    tree[[current_directory]] <- list_actions(command_list[[i]])
  }
}
rm(list = c('cmd','i','type_of_command', 'current_directory'))

directory_sizes <- unlist(tree)[grepl('size$', names(unlist(tree)))]
dir_df <- data.frame(
  dir = names(directory_sizes),
  size = as.numeric(directory_sizes),
  n = nchar(names(directory_sizes))
) 
dir_df <- dir_df[order(dir_df$n),]
dir_df <- subset(dir_df, select = -n)

for(i in 1:nrow(dir_df)){
  dir = sub('\\.size$', '', dir_df$dir[i])
  dir_df$total_size[i] <- sum(dir_df$size[grepl(paste0('^',dir), dir_df$dir)]) 
}

#Part 1
sum(subset(dir_df, total_size <= 100000)$total_size)

#Part 2
size_to_delete <- 30000000 - (70000000 - dir_df[1,]$total_size)

big_enough_df <- dir_df[dir_df$total_size >= size_to_delete, ]

big_enough_df$total_size[which.min(big_enough_df$total_size)]










