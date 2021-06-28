
# creates buttons. I don't remember what for though. 

create_buttons <- function(num){
  l = list()
  buttons = for(i in 1:num){
    l[[i]] = paste0('date', i, 'buttons')
  }
  return(unlist(l))
}