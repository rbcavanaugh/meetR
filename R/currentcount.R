currentcount <- function(sel_week, sel_day, data_in, counts_in, ret = 1, counts, choice, halfhour_in){
  if(isTruthy(halfhour_in==TRUE)){
    cs = c("8 am", "8:30 am", "9 am", "9:30 am", "10 am", "10:30 am", "11 am", "11:30 am",
           "12 pm", "12:30 pm", "1 pm", "1:30 pm", "2 pm", "2:30 pm", "3 pm", "3:30pm",
           "4 pm", "4:30 pm", "5 pm", "5:30 pm")
  } else if (isTruthy(unique(choice[["halfhour"]])==1)) {
    cs = c("8 am", "8:30 am", "9 am", "9:30 am", "10 am", "10:30 am", "11 am", "11:30 am",
           "12 pm", "12:30 pm", "1 pm", "1:30 pm", "2 pm", "2:30 pm", "3 pm", "3:30pm",
           "4 pm", "4:30 pm", "5 pm", "5:30 pm")
  } else {
    cs = c("8 am", "9 am", "10 am", "11 am", "12 pm", "1 pm", "2 pm", "3 pm", "4 pm", "5 pm")
  }
  
  l = list()
  for (i in 1:length(cs)){
    l[[i]] = paste0(cs[i], " (", counts %>%
                      dplyr::filter(week == sel_week, day == sel_day, selected == cs[i]) %>%
                      dplyr::select(n) %>%
                      pluck(1, .default = "0"), ")")
  }
  cs_new = unlist(l)
  if(ret == 1){
    return(cs_new[-c(choice %>%
                       dplyr::filter(week == sel_week, day == sel_day) %>%
                       mutate(sel_num = match(selected, cs)) %>%
                       dplyr::select(sel_num) %>%
                       drop_na() %>%
                       pluck(1,.default=50))])
  } else {
    return(cs_new)
  }
}