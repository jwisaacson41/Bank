expectation<-function(throw){
  dice_outcomes<-data.frame(d1 = c(rep(1,6), rep(2, 6), rep(3, 6), rep(4, 6), rep(5,6), rep(6,6))
                     , d2 = c(rep(1:6, 6))
                     )
  dice_outcomes$total1_3<-ifelse(dice_outcomes$d1+dice_outcomes$d2==7, 70, dice_outcomes$d1+dice_outcomes$d2)
  dice_outcomes$total4<-ifelse(dice_outcomes$d1+dice_outcomes$d2==7, 0, dice_outcomes$d1+dice_outcomes$d2)
  dice_outcomes$double<-ifelse(dice_outcomes$d1==dice_outcomes$d2, 1, 0)
  
  if(throw < 4){
    throw_probability<-dice_outcomes %>%
      group_by(total1_3) %>%
      summarize(prob1_3 = n()/nrow(dice_outcomes)
      )
  }
  
  if(throw >= 4){
    throw_probability<-dice_outcomes %>%
      group_by(total4) %>%
      summarize(prob4 = n()/nrow(dice_outcomes)
      )
  }
  x<-list(Expectation = sum(throw_probability$total4*throw_probability$prob4)
          , Probabilites = throw_probability
          )
  return(x)
}

