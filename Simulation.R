library(dplyr)

# Functions
roll_dice<-function(n){
  sample(1:6, n, replace = T)
}



# Round Simulation
n<-1000 # number of games

sim<-data.frame(games = NULL
                , round = NULL
                , throw = NULL
                , d1 = NULL
                , d2 = NULL
                , total = NULL
                , bank = NULL
                , doubles = NULL
                , bust = NULL)

# Progress Bar
pb<-txtProgressBar(min = 0, max = n, style = 3)

for (k in 1:n){
  for(j in 1:10){
    
    # Restart each round
    i<-1
    bank<-0
    total<-0
    bank<-0
    bust<-F # Binary variable to flag when a game ends (throws/i > 3 and total = 7)
    # Throws in each round
    while(bust == F){
      d1<-roll_dice(1) # Dice 1
      d2<-roll_dice(1) # Dice 2
      total<-d1+d2
      bank<-case_when(
        i<=3 & total != 7 ~ bank+total
        , i<=3 & total == 7 ~ bank+70
        , i>3 & total == 7 ~ 0
        , i>3 & d1==d2 ~ bank*2
        , i>3 & d1!=d2 ~ bank+total
      )
      bust<-ifelse(total==7 & i>3
                   , T
                   , F)
      
      # Store results and calculate the bank amount
      x<-data.frame(game = k
                    , round = j
                    , throw = i
                    , d1 = d1
                    , d2 = d2
                    , total = total
                    , bank = bank
                    , doubles = ifelse(i>3 & d1 == d2, 1, 0)
                    , bust = ifelse(i>3 & total == 7, 1, 0)
      )
      
      # Breaking Statement
      if(total==7 && i>3){
        sim<-rbind(sim, x)
        total<-0
        break
      } else{
        sim<-rbind(sim, x)
        i<-i+1
      }
    }
  }
  if (k %% 2==0){
    setTxtProgressBar(pb,k)
  }
}

close(pb)


# Summary Statistics
# round_summary_stats<-sim %>%
#   filter(bust != 1) %>%
#   group_by(round) %>%
#   summarize(max_throw = max(throw)
#             , max_bank = max(bank)
#             , total_doubles = sum(doubles))
# 
# round_summary_stats

game_round_summary_stats<-sim %>%
  filter(bust != 1) %>%
  group_by(game, round) %>%
  summarize(max_throw = max(throw)
            , max_bank = max(bank)
            , avg_throwpoints = mean(total)
            , total_doubles = sum(doubles))


rm(i, j, bank, bust, d1, d2)

hist(game_round_summary_stats$max_throw
     , main = 'Distribution of Throws')

summary(game_round_summary_stats)

game_round_summary_stats %>%
  group_by(max_throw) %>%
  summarize(count = n()) %>%
  print(n = 75)


strats<-sim %>%
  distinct(game, round)

#p1 - Bank after 3 throws
p1<-sim %>%
  group_by(game, round) %>%
  filter(throw==3) %>%
  select(game, round, bank, throw) %>%
  rename('P1'= bank)

#p2 Bank after 100 points
p2<-sim %>%
  group_by(game, round) %>%
  filter(bank > 100) %>%
  select(game, round, bank, throw) %>%
  slice(1) %>%
  rename('P2'= bank)

#p3 - Bank after 200 points
p3<-sim %>%
  group_by(game, round) %>%
  filter(bank > 200) %>%
  select(game, round, bank, throw) %>%
  slice(1) %>%
  rename('P3'= bank)

#P4 Bank after 5 rounds
p4<-sim %>%
  group_by(game, round) %>%
  filter(throw==5) %>%
  select(game, round, bank, throw) %>%
  rename('P4'= bank)

#P5 Bank after first double
p5<-sim %>%
  group_by(game, round) %>%
  filter(throw > 3
         , doubles==1) %>%
  select(game, round, bank, throw) %>%
  slice(1) %>%
  rename('P5'= bank)

#P6 Wait until all other conditions have past (aka last player standing)
p6<-sim %>%
  group_by(game, round) %>%
  filter(bank > 200
         , sum(doubles)>1) %>%
  select(game, round, bank, throw) %>%
  slice(1) %>%
  rename('P6'= bank)


strats<-strats %>%
  left_join(p1, by=c('game' = 'game', 'round'='round')) %>%
  left_join(p2, by=c('game' = 'game', 'round'='round')) %>%
  left_join(p3, by=c('game' = 'game', 'round'='round')) %>%
  left_join(p4, by=c('game' = 'game', 'round'='round')) %>%
  left_join(p5, by=c('game' = 'game', 'round'='round')) %>%
  left_join(p6, by=c('game' = 'game', 'round'='round')) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

strats<-strats %>%
  rowwise() %>%
  mutate(roundwinner = c('P1', 'P2', 'P3', 'P4', 'P5', 'P6')[which.max(c_across(P1:P6))])

strats$roundwinner<-as.factor(strats$roundwinner)

strats_games <-strats %>%
  group_by(game) %>%
  summarize(P1 = sum(P1)
            ,P2 = sum(P2)
            ,P3 = sum(P3)
            ,P4 = sum(P4)
            ,P5 = sum(P5)
            ,P6 = sum(P6)
  ) %>%
  rowwise() %>%
  mutate(gamewinner = c('P1', 'P2', 'P3', 'P4', 'P5', 'P6')[which.max(c_across(P1:P6))])

strats_games$gamewinner <- as.factor(strats_games$gamewinner)

plot_table<-strats_games %>%
  group_by(gamewinner) %>%
  summarize(n = n()) %>%
  mutate(n/sum(n))

barplot(plot_table$n, names.arg = plot_table$gamewinner)
summary(strats_games)

summary(strats)

plot_table
