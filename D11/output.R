txt <- readLines('input.txt', skipNul = T)
txt <- txt[!grepl("^$", txt)]

monkey_list <- list()
l = 0
for(i in 1:length(txt)){
  if(grepl('^Monkey', txt[i])){
    l = l + 1
    monkey_list[[l]] = list()
  } else {
    monkey_list[[l]] = c(monkey_list[[l]], sub('^\\s+','',txt[i]))
  }
}

monkeys <- lapply(monkey_list, function(mks){
  list(
    starting = as.numeric(regmatches(mks[1],gregexpr("\\d+",mks[1]))[[1]]),
    operation = eval(parse(text = paste0('function(old) { return(' , sub("Operation:\\snew\\s=\\s","",mks[2]) , ')}', sep=''))),
    test = function(new){new%%as.numeric(regmatches(mks[3],gregexpr("\\d+",mks[3]))[[1]]) == 0},
    which_monkey = function(test){if(test){as.numeric(regmatches(mks[4],gregexpr("\\d+",mks[4]))[[1]]) + 1} else {as.numeric(regmatches(mks[5],gregexpr("\\d+",mks[5]))[[1]]) + 1}}
  )
})

#Part 1
current_holdings <- list(lapply(monkeys, function(mk){mk$starting}))
for(round in 1:20){
  current_holdings[[round + 1]] <- list()
  for(mk in 1:length(monkeys)){
    worry_levels <- current_holdings[[round]][[mk]]
    if(length(worry_levels) != 0){
      for(i in 1:length(worry_levels)){
        new = monkeys[[mk]]$operation(worry_levels[i])
        new = floor(new/3)
        test = monkeys[[mk]]$test(new)
        monkey_to = monkeys[[mk]]$which_monkey(test)
        if(monkey_to < mk){
          current_holdings[[round+1]][[monkey_to]] <- c(unlist(current_holdings[[round+1]][[monkey_to]]), new)
        } else {
          current_holdings[[round]][[monkey_to]] <- c(unlist(current_holdings[[round]][[monkey_to]]), new)
        }
      }
    }
    current_holdings[[round + 1]][[mk]] <- list()
  }
}

top2 <- sort(unlist(lapply(1:8, function(mk){sum(sapply(current_holdings[1:20], function(ch){sum(length(ch[[mk]]))}))})), decreasing = T)[1:2]
top2[1] * top2[2]

#Part 2

divisors <- as.numeric(sapply(monkey_list, function(mk){sub("^Test:\\sdivisible\\sby\\s", "", mk[[3]])}))
modulo <- prod(divisors)


current_holdings <- list(lapply(monkeys, function(mk){mk$starting}))
for(round in 1:10000){
  current_holdings[[round + 1]] <- list()
  for(mk in 1:length(monkeys)){
    worry_levels <- current_holdings[[round]][[mk]]
    if(length(worry_levels) != 0){
      for(i in 1:length(worry_levels)){
        new = monkeys[[mk]]$operation(worry_levels[i])
        new = new%%modulo
        test = monkeys[[mk]]$test(new)
        monkey_to = monkeys[[mk]]$which_monkey(test)
        if(monkey_to < mk){
          current_holdings[[round+1]][[monkey_to]] <- c(unlist(current_holdings[[round+1]][[monkey_to]]), new)
        } else {
          current_holdings[[round]][[monkey_to]] <- c(unlist(current_holdings[[round]][[monkey_to]]), new)
        }
      }
    }
    current_holdings[[round + 1]][[mk]] <- list()
  }
}

top2 <- sort(unlist(lapply(1:8, function(mk){sum(sapply(current_holdings[-length(current_holdings)], function(ch){sum(length(ch[[mk]]))}))})), decreasing = T)[1:2]

top2[1] * top2[2]










