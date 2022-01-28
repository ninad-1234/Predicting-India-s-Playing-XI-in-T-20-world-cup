## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(ggplot2)

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Top Order", tabName = "TOP_ORDER", icon = icon("dashboard")),
      menuItem("Mid Order", tabName = "MID_ORDER", icon = icon("th")),
      menuItem("Lower Mid Order", tabName = "LOWER_MID_ORDER", icon = icon("th")),
      menuItem("Tail Order", tabName = "TAIL_ORDER", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "TOP_ORDER",
              h2("Choose Your Top Order "),
              h2("Based on number of balls played"),
              fluidRow(
                box(plotOutput("plot1", height = 350)),
                
                box(
                  title = "The Top One",
                  sliderInput("slider", "Select the top number of batsman :", 1, 20, 5)
                )
              ),
              h2("Best Wicket Prevntor"),
              fluidRow(
                box(plotOutput("plot2", height = 350)),
                
                box(
                  title = "The Top One",
                  sliderInput("slider2", "Select the top number of batsman :", 1, 20, 5)
                )
              )
              
              
      ),
      
      # Second tab content
      tabItem(tabName = "MID_ORDER",
              h2("Choose Your Middle Order "),
              h2("Power Play Striker"),
              fluidRow(
                box(plotOutput("plot3", height = 350)),
                
                box(
                  title = "The Top One",
                  sliderInput("slider3", "Select the top number of batsman :", 1, 20, 5)
                )
              )
      ),
      
      # Third tab content
      tabItem(tabName = "LOWER_MID_ORDER",
              h2("Choose Your Lower Middle Order "),
              h2("Death Over Striker"),
              fluidRow(
                box(plotOutput("plot4", height = 350)),
                
                box(
                  title = "The Top One",
                  sliderInput("slider4", "Select the top number of batsman :", 1, 20, 5)
                )
              )
      ),
      
      # Fourth tab content
      tabItem(tabName = "TAIL_ORDER",
              h2("Choose Your Tail Order "),
              h2("Spin Options"),
              fluidRow(
                box(plotOutput("plot5", height = 350)),
                
                box(
                  title = "The Top One",
                  sliderInput("slider5", "Select the top number of bowler :", 1, 20, 5)
                )
              ),
              h2("Pace Options"),
              fluidRow(
                box(plotOutput("plot6", height = 350)),
                
                box(
                  title = "The Top One",
                  sliderInput("slider6", "Select the top number of bowler :", 1, 20, 5)
                )
              ),
              h2("Pace Economic Options"),
              fluidRow(
                box(plotOutput("plot7", height = 350)),
                
                box(
                  title = "The Top One",
                  sliderInput("slider7", "Select the top number of bowler :", 1, 20, 5)
                )
              ),
              h2("Pacers with best death over economy"),
              fluidRow(
                box(plotOutput("plot8", height = 350)),
                
                box(
                  title = "The Top One",
                  sliderInput("slider8", "Select the top number of bowler :", 1, 20, 5)
                )
              )
              
      )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  df <- read.csv("D:/5th sem Subject/CSE-3505 ( FDA )/PROJECT/REVIEW-2/MadABoutSPort_resources/ipl_ball_by_ball_data.csv", header=T, sep=",")
  cntry_df <- read.csv("D:/5th sem Subject/CSE-3505 ( FDA )/PROJECT/REVIEW-2/MadABoutSPort_resources/player_country.csv", header=T, sep=",")
  bowltype_df <- read.csv("D:/5th sem Subject/CSE-3505 ( FDA )/PROJECT/REVIEW-2/MadABoutSPort_resources/indians_bowling_type.csv", header=T, sep=",")
  
  # Calculate Total balls played by each played 
  balls <- count(df, df$striker)
  colnames(balls)[1] <- "playerName"
  colnames(balls)[2] <- "balls"
  balls <- balls[order(balls$balls, decreasing = TRUE),]
  row.names(balls) <- NULL
  
  
  output$plot1 <- renderPlot({
    
    no_of_balls <- head(balls,input$slider)
    
    x1= list(no_of_balls$balls)
    #rev(x1)
    #print(x1)
    y1= list(no_of_balls$playerName)
    #rev(y1)
    #print(y1)
    
    df_x1_y1= data.frame(x1,y1)
    
    colnames(df_x1_y1)[1] <- "balls"
    colnames(df_x1_y1)[2] <- "playerName"
    #df_x1_y1
    
    fig <-ggplot(data=df_x1_y1, aes(x= reorder(playerName, +balls) , y=balls)) +
      geom_bar(stat="identity", fill="steelblue")+
      ggtitle("Players who played most no of balls in IPL 2016 - 2021") +
      xlab("Player") + ylab("Number of Balls faced")+
      theme_minimal()
    fig <- fig+coord_flip()
    
    #no_of_balls
    data <- histdata[seq_len(input$slider)]
    fig
  })
  
  # Calculate wickets of each player 
  
  dismiss <- subset(df,df$player_dismissed != "" )
  outs <- count(dismiss ,dismiss$player_dismissed)
  colnames(outs)[1] <- "playerName"
  colnames(outs)[2] <- "dismissals"
  outs <- outs[order(outs$dismissals, decreasing = TRUE),]
  row.names(outs) <- NULL
  outs <- subset(outs, outs$dismissals>=1)
  #head(outs,10)
  #dim(outs)
  
  ## Balls per wicket for each player
  
  bpw_df <- merge( x=balls, y=outs,by="playerName", all.x = TRUE)
  bpw_df$bpw <- bpw_df$balls/bpw_df$dismissals
  bpw_df[is.na(bpw_df)]=0
  bpw_df <- merge( bpw_df, cntry_df,by="playerName")
  #head(bpw_df,10)
  #dim(bpw_df)
  
  ## BPW of Only Indian Player who played more than 500 balls 
  
  bpw_df <- subset(bpw_df,bpw_df$country =='India')
  bpw_df <- bpw_df[order(bpw_df$bpw, decreasing = TRUE),]
  row.names(bpw_df) <- NULL
  plot_bpw <- subset(bpw_df,bpw_df$balls >500)
  plot_bpw <- plot_bpw[order(plot_bpw$bpw, decreasing = TRUE),]
  row.names(plot_bpw) <- NULL
  
  p3 <- head(plot_bpw,20)
  row.names(p3) <- NULL
  #p3
  #dim(p3)
  
  #plot_bpw <- head(plot_bpw,10)
  #row.names(plot_bpw) <- NULL
  
  output$plot2 <- renderPlot({
    
    plot_bpw <- head(plot_bpw,input$slider2)
    
    #Best wicket preventors:
    
    x2= list(plot_bpw$bpw)
    #rev(x2)
    #print(x2)
    y2= list(plot_bpw$playerName)
    #rev(y2)
    #print(y2)
    
    df_x2_y2= data.frame(x2,y2)
    
    colnames(df_x2_y2)[1] <- "bpw"
    colnames(df_x2_y2)[2] <- "playerName"
    
    fig <-ggplot(data=df_x2_y2, aes(x= reorder(playerName, +bpw) , y=bpw)) +
      geom_bar(stat="identity", fill="steelblue")+
      ggtitle("Players with most balls per wicket in IPL 2016 - 2021") +
      xlab("Player") + ylab("Balls per wicket faced")+
      theme_minimal()
    fig<- fig+coord_flip()
    fig
  })
  
  ## Dividng Match in Phase
  
  phase <- function(a){
    
    ans <- as.integer(a)
    #print(ans)
    #print(ans)
    if(ans<=5){
      return('Powerplay')
    }
    else if(ans<=15){
      return('Middle')
    }
    
    else {
      return('Death')
    }
    
  }
  
  df$phase <- lapply(df$ball, phase)
  
  bowltype_df$bowler <- bowltype_df$playerName
  
  df <- left_join(df, bowltype_df,by="bowler")
  
  
  ## Calculating Strike rate of batsman :-
  
  df_pp <- subset(df,df$phase =='Powerplay')  
  
  df_pp_runs <- aggregate(df_pp["runs_off_bat"], by=df_pp["striker"], sum)
  colnames(df_pp_runs)[1] <- "striker"
  colnames(df_pp_runs)[2] <- "runs"
  
  df_pp_balls <- aggregate(df_pp["runs_off_bat"], by=df_pp["striker"], FUN = length)
  colnames(df_pp_balls)[1] <- "striker"
  colnames(df_pp_balls)[2] <- "balls"
  
  df_pp_sr <- merge( x=df_pp_balls, y=df_pp_runs,by="striker", all.x = TRUE)
  df_pp_sr['strike_rate'] = 100*df_pp_sr['runs'] /df_pp_sr['balls']
  colnames(df_pp_sr)[1] <- "playerName"
  
  df_pp_sr <- merge(x=df_pp_sr, y=cntry_df,by="playerName",all.x = TRUE)
  df_pp_sr <- subset(df_pp_sr,df_pp_sr$country =='India')
  
  df_pp_sr <- subset(df_pp_sr,df_pp_sr$balls>300)
  df_pp_sr <- df_pp_sr[order(df_pp_sr$strike_rate, decreasing = TRUE),]
  #df_pp_sr <-head(df_pp_sr,40)
  #dim(df_pp_sr)
  #head(df_pp_sr,10)
  
  output$plot3 <- renderPlot({
    
    df_pp_sr <- head(df_pp_sr,input$slider3)
    
    # Powerplay Stats
    
    xp1= list(df_pp_sr$strike_rate)
    #rev(xp1)
    #print(xp1)
    yp1= list(df_pp_sr$playerName)
    #rev(yp1)
    #print(yp1)
    
    df_xp1_yp1= data.frame(xp1,yp1)
    
    colnames(df_xp1_yp1)[1] <- "Strike_rate"
    colnames(df_xp1_yp1)[2] <- "playerName"
    #df_xp1_yp1
    
    
    fig <-ggplot(data=df_xp1_yp1, aes(x= reorder(playerName, +Strike_rate) , y=Strike_rate)) +
      geom_bar(stat="identity", fill="steelblue")+
      ggtitle("Players with high S/R in Powerplay since IPL 2016") +
      xlab("Player") + ylab("Strike Rate")+
      theme_minimal()
    fig <- fig+coord_flip()
    fig
    
  })
  
  ## Runs scored By each Player :-
  
  runs_df <- aggregate(df["runs_off_bat"], by=df["striker"], sum)
  colnames(runs_df)[1] <- "playerName"
  colnames(runs_df)[2] <- "runs"
  runs_df <- runs_df[order(runs_df$runs, decreasing = TRUE),]
  row.names(runs_df) <- NULL
  #head(runs_df,10)
  #dim(runs_df)
  
  ## Strike rate of player :-
  
  sr_df <- merge(x=runs_df, y=balls,by="playerName",all.x = TRUE)
  sr_df$sr = 100*sr_df$runs/sr_df$balls
  sr_df <- merge( x=sr_df, y=cntry_df,by="playerName",all.x = TRUE)
  
  # Only considering Indian Player
  
  sr_df <- subset(sr_df,sr_df$country =='India')
  row.names(sr_df) <- NULL
  
  # Merging BPW and strikerate :-
  
  temp_bpw_df <- data.frame(bpw_df$playerName, bpw_df$bpw)
  colnames(temp_bpw_df)[1] <- "playerName"
  colnames(temp_bpw_df)[2] <- "bpw"
  
  sr_df <- merge(x=sr_df, y=temp_bpw_df,by="playerName",all.x = TRUE)
  sr_df <- sr_df[order(sr_df$playerName, decreasing = FALSE),]
  #head(sr_df,10)
  #dim(sr_df)
  
  output$plot4 <- renderPlot({
    
    
    #Middle Order: Finishers
    #H Pandya, K Pandya, Pant, DK, Samson, Jadeja, S Dube, ....
    
    plot_sr_runs = subset(sr_df, sr_df$balls>250)
    plot_sr_runs <- plot_sr_runs[order(plot_sr_runs$sr, decreasing = TRUE),]
    plot_sr_runs <- head(plot_sr_runs,input$slider4)
    row.names(plot_sr_runs) <- NULL
    #plot_sr_runs
    
    x6= list(plot_sr_runs$sr)
    #rev(x6)
    #print(xp1)
    y6= list(plot_sr_runs$playerName)
    #rev(y6)
    #print(y6)
    
    df_x6_y6= data.frame(x6,y6)
    
    colnames(df_x6_y6)[1] <- "Strike_rate"
    colnames(df_x6_y6)[2] <- "playerName"
    #df_x6_y6
    
    
    fig <-ggplot(data=df_x6_y6, aes(x= reorder(playerName, +Strike_rate) , y=Strike_rate)) +
      geom_bar(stat="identity", fill="steelblue")+
      ggtitle("Players with high S/R since IPL 2016") +
      xlab("Player") + ylab("Strike Rate")+
      theme_minimal()
    fig <- fig+coord_flip()
    fig
    
  })
  
  ## Considering Bowlers attributes :
  
  ppdf <- subset(df, df$phase =='Powerplay')
  
  temp_pp1 <- data.frame(ppdf$match_id, ppdf$bowler)
  colnames(temp_pp1)[1] <- "matches"
  colnames(temp_pp1)[2] <- "bowler"
  temp2_pp1 <- temp_pp1
  colnames(temp2_pp1)[1] <- "powerplay_balls"
  temp_pp1 <- unique(temp_pp1)
  
  pp1 <- aggregate(temp_pp1["matches"], by=temp_pp1["bowler"], FUN = length)
  pp2 <- aggregate(temp2_pp1["powerplay_balls"], by=temp2_pp1["bowler"], FUN = length)
  pp3 <- merge( x=pp1, y=pp2,by="bowler", all.x = TRUE)
  head(pp3,10)
  dim(pp3)
  
  # Runs Given by bowler in powerplay :-
  
  ppec1 <- aggregate(ppdf["runs_off_bat"], by=ppdf["bowler"], sum)
  colnames(ppec1)[1] <- "bowler"
  colnames(ppec1)[2] <- "runs_off_bat"
  
  ppec2 <- aggregate(ppdf["extras"], by=ppdf["bowler"], sum)
  colnames(ppec2)[1] <- "bowler"
  colnames(ppec2)[2] <- "extras"
  
  ppec3 <- merge( x=ppec1, y=ppec2,by="bowler")
  ppec3['total_runs'] = ppec3['runs_off_bat'] + ppec3['extras']
  colnames(ppec3)[1] <- "playerName"
  
  head(ppec3,10)
  dim(ppec3)
  
  ## Calculating Economy of Bowler :-
  
  
  pp3['total_balls'] = pp3['matches']*4*6
  pp3['pp_percentage'] = pp3['powerplay_balls']/pp3['total_balls']
  colnames(pp3)[1] <- "playerName"
  pp3 <- merge( pp3, cntry_df,by="playerName")
  pp3 <- subset(pp3,pp3$country =='India')
  
  
  temp_bowltype_df<- data.frame(bowltype_df$playerName,bowltype_df$bowlType)
  colnames(temp_bowltype_df)[1] <- "playerName"
  colnames(temp_bowltype_df)[2] <- "bowlType"
  
  pp3 <- merge( pp3, temp_bowltype_df,by="playerName")
  
  temp_ppec3<- data.frame(ppec3$playerName, ppec3$total_runs)
  colnames(temp_ppec3)[1] <- "playerName"
  colnames(temp_ppec3)[2] <- "total_runs"
  
  pp3 <- merge( x=pp3, y=temp_ppec3,by="playerName",all.x = TRUE)
  pp3['economy'] = 6*pp3['total_runs']/pp3['powerplay_balls']
  head(pp3,10)
  dim(pp3)
  
  # For spin type :-
  
  pp3_spin = subset(pp3,pp3$bowlType=='Spin')
  head(pp3_spin,10)
  dim(pp3_spin)
  
  # For Pace type :-
  
  pp3_pace = subset(pp3,pp3$bowlType=='Pace')
  head(pp3_pace,10)
  dim(pp3_pace)
  
  # Economy for pacer:- 
  
  plot_eco_pp_pace <- pp3_pace[order(pp3_pace$pp_percentage,decreasing = TRUE ),]
  plot_eco_pp_pace <- head(plot_eco_pp_pace,20)
  plot_eco_pp_pace['pp_percentage'] = plot_eco_pp_pace['pp_percentage']*100
  row.names(plot_eco_pp_pace) <- NULL
  head(plot_eco_pp_pace,10)
  dim(plot_eco_pp_pace)
  
  # Economy for spinner:- 
  
  plot_eco_pp <- pp3_spin[order(pp3_spin$pp_percentage,decreasing = TRUE ),]
  plot_eco_pp <- head(plot_eco_pp,20)
  plot_eco_pp['pp_percentage'] = plot_eco_pp['pp_percentage']*100
  row.names(plot_eco_pp) <- NULL
  head(plot_eco_pp,10)
  dim(plot_eco_pp)
  
  # Considering Death overs and calculating same attributes i.e ecnomy for bowlers :-
  
  # bowles by bowler in death overs :- 
  
  dtdf <- subset(df, df$phase =='Death')
  
  temp_dt1 <- data.frame(dtdf$match_id, dtdf$bowler)
  colnames(temp_dt1)[1] <- "matches"
  colnames(temp_dt1)[2] <- "bowler"
  temp2_dt1 <- temp_dt1
  colnames(temp2_dt1)[1] <- "death_balls"
  temp_dt1 <- unique(temp_dt1)
  
  
  dt1 <- aggregate(temp_dt1["matches"], by=temp_dt1["bowler"], FUN = length)
  
  dt2 <- aggregate(temp2_dt1["death_balls"], by=temp2_dt1["bowler"], FUN = length)
  
  dt3 <- merge( x=dt1, y=dt2,by="bowler", all.x = TRUE)
  
  head(dt3,10)
  dim(dt3)
  
  # Runs given by bowlers in death over :- 
  
  dtec1 <- aggregate(dtdf["runs_off_bat"], by=dtdf["bowler"], sum)
  colnames(dtec1)[1] <- "bowler"
  colnames(dtec1)[2] <- "runs_off_bat"
  
  dtec2 <- aggregate(dtdf["extras"], by=dtdf["bowler"], sum)
  colnames(dtec2)[1] <- "bowler"
  colnames(dtec2)[2] <- "extras"
  
  dtec3 <- merge( x=dtec1, y=dtec2,by="bowler")
  dtec3['total_runs'] = dtec3['runs_off_bat'] + dtec3['extras']
  colnames(dtec3)[1] <- "playerName"
  
  
  colnames(dt2)[1] <- "playerName"
  colnames(dt2)
  death_eco <- merge( x=dtec3, y=dt2,by="playerName")
  head(death_eco,10)
  dim(death_eco)
  
  # Economy of each bowler in death overs :-
  
  death_eco['economy'] = 6*death_eco['total_runs']/death_eco['death_balls']
  
  death_eco <- merge( x=death_eco, y=cntry_df,by="playerName", all.x = TRUE)
  
  death_eco <- merge( x=death_eco, y=temp_bowltype_df,by="playerName", all.x = TRUE)
  
  death_eco <- subset(death_eco,death_eco$death_balls > 50 & death_eco$country == 'India' & death_eco$bowlType == 'Pace' )
  head(death_eco,10)
  dim(death_eco)
  
  
  ## Wickets by bowlers :-
  
  
  wk_type <- list('caught', 'bowled', 'lbw', 'stumped', 'caught and bowled', 'hit wicket')
  
  df$isOut <- ifelse(df$player_dismissed=="",0,1)
  
  bowl_wk <- subset(df,df$wicket_type=="caught" | df$wicket_type=="bowled" |
                      df$wicket_type=="lbw" |df$wicket_type=="stumped" |
                      df$wicket_type=="caught and bowled" | df$wicket_type=="hit wicket")
  
  bowl_wk_death = subset(bowl_wk, bowl_wk$phase=='Death')
  bowl_wk_pp = subset(bowl_wk, bowl_wk$phase=='Powerplay')
  bowl_wk_mid = subset(bowl_wk, bowl_wk$phase=='Middle')
  
  # WIckets in Death Overs :-
  
  bowl_wk_death <- aggregate(bowl_wk_death$isOut, by=bowl_wk_death["bowler"], sum)
  row.names(bowl_wk_death) <- NULL
  colnames(bowl_wk_death)[1] <- "playerName"
  colnames(bowl_wk_death)[2] <- "wickets"
  head(bowl_wk_death,10)
  dim(bowl_wk_death)
  
  # WIckets in Powerplay Overs :-
  
  bowl_wk_pp <- aggregate(bowl_wk_pp$isOut, by=bowl_wk_pp["bowler"], sum)
  row.names(bowl_wk_pp) <- NULL
  colnames(bowl_wk_pp)[1] <- "playerName"
  colnames(bowl_wk_pp)[2] <- "wickets"
  head(bowl_wk_pp,10)
  dim(bowl_wk_pp)
  
  
  # COnsidering Indian bowlers arrnged in order based on death over wickets :-
  
  bowl_wk_death <- merge( x=bowl_wk_death, y=cntry_df,by="playerName",all.x = TRUE)
  bowl_wk_death <- merge( x=bowl_wk_death, y=temp_bowltype_df,by="playerName", all.x = TRUE)
  
  bowl_wk_pp <- merge( x=bowl_wk_pp, y=cntry_df,by="playerName",all.x = TRUE)
  bowl_wk_pp <- merge( x=bowl_wk_pp, y=temp_bowltype_df,by="playerName", all.x = TRUE)
  
  bowl_wk_death <- subset(bowl_wk_death,bowl_wk_death$country =='India')
  bowl_wk_pp <- subset(bowl_wk_pp,bowl_wk_pp$country =='India')
  
  bowl_wk_death <- bowl_wk_death[order(bowl_wk_death$wickets, decreasing = TRUE),]
  row.names(bowl_wk_death) <- NULL
  
  bowl_wk_death <-head(bowl_wk_death,10)
  bowl_wk_death
  dim(bowl_wk_death)
  
  
  # Considering Indian bowlers arrnged in order based on popwerplay over wickets :-
  
  bowl_wk_pp <- bowl_wk_pp[order(bowl_wk_pp$wickets, decreasing = TRUE),]
  row.names(bowl_wk_pp) <- NULL
  head(bowl_wk_pp,10)
  dim(bowl_wk_pp)
  
  # SPiners poweplay wickets :-
  
  bowl_wk_pp_spin <- subset(bowl_wk_pp,bowl_wk_pp$bowlType=='Spin')
  row.names(bowl_wk_pp_spin) <- NULL
  head(bowl_wk_pp_spin,10)
  dim(bowl_wk_pp_spin)
  
  # Pacers poweplay wickets :-
  
  bowl_wk_pp_pace <- subset(bowl_wk_pp,bowl_wk_pp$bowlType=='Pace')
  row.names(bowl_wk_pp_pace) <- NULL
  head(bowl_wk_pp_pace,10)
  dim(bowl_wk_pp_pace)
  
  
  output$plot5 <- renderPlot({
    
    bowl_wk_pp_spin <- head(bowl_wk_pp_spin,input$slider5)
    fig <-ggplot(data=bowl_wk_pp_spin, aes(x= reorder(playerName , +wickets ) , y=wickets )) +
      geom_bar(stat="identity", fill="steelblue")+
      ggtitle("Spin bowler with their number of wickets since 2016 IPL") +
      xlab("Player") + ylab("Number of wickets ")+
      theme_minimal()
    fig <-fig+coord_flip()
    fig
    
  })
  
  output$plot6 <- renderPlot({
    
    bowl_wk_pp_pace <- head(bowl_wk_pp_pace,input$slider6)
    fig <-ggplot(data=bowl_wk_pp_pace, aes(x= reorder(playerName , +wickets ) , y=wickets )) +
      geom_bar(stat="identity", fill="steelblue")+
      ggtitle("Pace bowler with their number of wickets since 2016 IPL") +
      xlab("Player") + ylab("Number of wickets ")+
      theme_minimal()
    fig<- fig+coord_flip()
    fig
    
  })
  
  output$plot7 <- renderPlot({
    
    plot_death_eco <- head(plot_death_eco,input$slider7)
    fig <-ggplot(data=plot_death_eco, aes(x= reorder(playerName , -economy ) , y=economy )) +
      geom_bar(stat="identity", fill="steelblue")+
      ggtitle("Pace bowler with best economy  since 2016 IPL") +
      xlab("Player") + ylab("Number of wickets ")+
      theme_minimal()
    fig<- fig+coord_flip()
    fig
    
  })
  
  output$plot8 <- renderPlot({
    
    bowl_wk_death <- head(bowl_wk_death,input$slider8)
    fig <-ggplot(data=bowl_wk_death, aes(x= reorder(playerName , +wickets ) , y=wickets )) +
      geom_bar(stat="identity", fill="steelblue")+
      ggtitle("Pace bowler with their number of wickets in death overs since 2016 IPL") +
      xlab("Player") + ylab("Number of wickets ")+
      theme_minimal()
    fig <- fig+coord_flip()
    fig
    
  })
  
  
  

}

shinyApp(ui, server)