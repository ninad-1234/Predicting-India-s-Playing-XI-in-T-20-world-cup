
## Required Libraries 

library(tidyverse)
library(dplyr)


## Loading Dataset 

df <- read.csv("D:/5th sem Subject/CSE-3505 ( FDA )/PROJECT/REVIEW-2/MadABoutSPort_resources/ipl_ball_by_ball_data.csv", header=T, sep=",")
head(df,10)
dim(df)

cntry_df <- read.csv("D:/5th sem Subject/CSE-3505 ( FDA )/PROJECT/REVIEW-2/MadABoutSPort_resources/player_country.csv", header=T, sep=",")
head(cntry_df,10)
dim(cntry_df)


bowltype_df <- read.csv("D:/5th sem Subject/CSE-3505 ( FDA )/PROJECT/REVIEW-2/MadABoutSPort_resources/indians_bowling_type.csv", header=T, sep=",")
head(bowltype_df,10)
dim(bowltype_df)


## Strat Processing :-

## Filtering IPL data from 2016 -2021

df$start_date <- as.POSIXct(df$start_date, format= "%Y-%m-%d")
df$year <- format(df$start_date,format = "%Y")

df <- subset(df, df$year>=2016 & df$year<=2021)
row.names(df) <- NULL
head(df,10)
dim(df)

# Calculate Total balls played by each played 

balls <- count(df, df$striker)
colnames(balls)[1] <- "playerName"
colnames(balls)[2] <- "balls"
balls <- balls[order(balls$balls, decreasing = TRUE),]
row.names(balls) <- NULL
no_of_balls <- head(balls,10)
no_of_balls
dim(no_of_balls)

# Calculate wickets of each player 

dismiss <- subset(df,df$player_dismissed != "" )
outs <- count(dismiss ,dismiss$player_dismissed)
colnames(outs)[1] <- "playerName"
colnames(outs)[2] <- "dismissals"
outs <- outs[order(outs$dismissals, decreasing = TRUE),]
row.names(outs) <- NULL
outs <- subset(outs, outs$dismissals>=1)
head(outs,10)
dim(outs)

## Balls per wicket for each player

bpw_df <- merge( x=balls, y=outs,by="playerName", all.x = TRUE)
bpw_df$bpw <- bpw_df$balls/bpw_df$dismissals
bpw_df[is.na(bpw_df)]=0
bpw_df <- merge( bpw_df, cntry_df,by="playerName")
head(bpw_df,10)
dim(bpw_df)

## BPW of Only Indian Player who played more than 500 balls 

bpw_df <- subset(bpw_df,bpw_df$country =='India')
bpw_df <- bpw_df[order(bpw_df$bpw, decreasing = TRUE),]
row.names(bpw_df) <- NULL
plot_bpw <- subset(bpw_df,bpw_df$balls >500)
plot_bpw <- plot_bpw[order(plot_bpw$bpw, decreasing = TRUE),]
row.names(plot_bpw) <- NULL

p3 <- head(plot_bpw,20)
row.names(p3) <- NULL
p3
dim(p3)

plot_bpw <- head(plot_bpw,10)
row.names(plot_bpw) <- NULL


## Runs scored By each Player :-

runs_df <- aggregate(df["runs_off_bat"], by=df["striker"], sum)
colnames(runs_df)[1] <- "playerName"
colnames(runs_df)[2] <- "runs"
runs_df <- runs_df[order(runs_df$runs, decreasing = TRUE),]
row.names(runs_df) <- NULL
head(runs_df,10)
dim(runs_df)

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
head(sr_df,10)
dim(sr_df)


## Number of Fours ,Sixes and dots by players :-


df$isfour <- ifelse(df$runs_off_bat==4,1,0)
df$issix <- ifelse(df$runs_off_bat==6,1,0)
df$isdot <- ifelse(df$runs_off_bat==0,1,0)

temp_fours_df <- data.frame(df$striker, df$isfour)
colnames(temp_fours_df)[1] <- "playerName"
colnames(temp_fours_df)[2] <- "fours"

fours_df <- aggregate(temp_fours_df["fours"], by=temp_fours_df["playerName"], sum)
colnames(fours_df)[1] <- "playerName"
colnames(fours_df)[2] <- "fours"

temp_sixes_df <- data.frame(df$striker, df$issix)
colnames(temp_sixes_df)[1] <- "playerName"
colnames(temp_sixes_df)[2] <- "sixes"

sixes_df <- aggregate(temp_sixes_df["sixes"], by=temp_sixes_df["playerName"], sum)
colnames(sixes_df)[1] <- "playerName"
colnames(sixes_df)[2] <- "sixes"


fours_df <- subset(fours_df, fours_df$fours>1)
head(fours_df,10)
dim(fours_df)
sixes_df <- subset(sixes_df, sixes_df$sixes>1)
head(sixes_df,10)
dim(sixes_df)

# FOurs and sixes ratio :-

fps_df <- merge(x=fours_df, y=sixes_df,by="playerName",all.x = TRUE)
fps_df$fps <- fps_df$fours/fps_df$sixes
fps_df$spf <- 1/fps_df$fps
fps_df <- fps_df[order(fps_df$spf, decreasing = TRUE),]
head(fps_df,10)
dim(fps_df)

# COnsidering Only Indian Player :-

spf_df <- merge(x=fps_df, y=cntry_df,by="playerName", all.x=TRUE)
spf_df <- subset(spf_df,spf_df$country =='India')
head(spf_df,10)
dim(spf_df)

# Dots balls :-

temp_dots_df <- data.frame(df$striker, df$isdot)
colnames(temp_dots_df)[1] <- "playerName"
colnames(temp_dots_df)[2] <- "dots"

dots_df <- aggregate(temp_dots_df["dots"], by=temp_dots_df["playerName"], sum)
colnames(dots_df)[1] <- "playerName"
colnames(dots_df)[2] <- "dots"

dbp_df <- merge( x=balls, y=dots_df,by="playerName", all.x = TRUE)
dbp_df$dbp <- dbp_df$dots/dbp_df$balls
dbp_df <- merge(x=dbp_df, y=cntry_df,by="playerName",all.x = TRUE)
dbp_df <- subset(dbp_df,dbp_df$country =='India')
dbp_df <- dbp_df[order(dbp_df$dbp, decreasing = TRUE),]

head(dbp_df,10)
dim(dbp_df)

p_list = c('N Rana', 'Ishan Kishan', 'RG Sharma', 'D Padikkal', 'RD Gaikwad', 'PP Shaw', 'RA Tripathi')

plot_dots= subset(dbp_df, dbp_df$balls>200 & (dbp_df$playerName=='N Rana' |
                                                dbp_df$playerName=='Ishan Kishan'|
                                              dbp_df$playerName=='RG Sharma'|
                                              dbp_df$playerName=='D Padikkal'|
                                              dbp_df$playerName=='RD Gaikwad'|
                                              dbp_df$playerName=='PP Shaw'|
                                              dbp_df$playerName=='RA Tripathi'))

plot_dots <- plot_dots[order(plot_dots$dbp, decreasing = TRUE),]
plot_dots <- head(plot_dots,20)
plot_dots

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

# COnsidering Middle Phase strikerate :-

midd_sr <- subset(df, df$phase =='Middle')

mid_sr <- subset(df, df$phase =='Middle' & df$bowlType =='Spin')

ms1 <- aggregate(mid_sr["runs_off_bat"], by=mid_sr["striker"], sum)
colnames(ms1)[1] <- "striker"
colnames(ms1)[2] <- "runs"

ms2 <- count(mid_sr, mid_sr$striker)
colnames(ms2)[1] <- "striker"
colnames(ms2)[2] <- "balls"

ms3 <- left_join( ms1, ms2,by="striker")

ms3 <- subset(ms3,ms3$balls>150)
ms3$strike_rate <- 100*ms3$runs/ms3$balls
head(ms3,10)
dim(ms3)

p2_list = c('V Kohli', 'S Dhawan', 'RG Sharma', 'KL Rahul')
rms3= subset(ms3,ms3$striker=='V Kohli' | 
               ms3$striker=='S Dhawan' |
               ms3$striker=='RG Sharma' |
               ms3$striker=='KL Rahul')

# Considering Death Phase strikerate :-

death_sr <- subset(df, df$phase =='Death')

ds1 <- aggregate(death_sr["runs_off_bat"], by=death_sr["striker"], sum)
colnames(ds1)[1] <- "striker"
colnames(ds1)[2] <- "runs"

ds2 <- aggregate(death_sr["runs_off_bat"], by=death_sr["striker"], FUN = length)
colnames(ds2)[1] <- "striker"
colnames(ds2)[2] <- "balls"

ds3 <- left_join( ds1, ds2,by="striker")


ds3 <- subset(ds3,ds3$balls>150)
ds3$strike_rate <- 100*ds3$runs/ds3$balls

colnames(ds3)[1] <- "playerName"

ds3 <- left_join( ds3, cntry_df,by="playerName")

ds3 <- subset(ds3,ds3$country =='India')

row.names(ds3) <- NULL
head(ds3,10)
dim(ds3)


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
df_pp_sr <-head(df_pp_sr,20)
dim(df_pp_sr)
head(df_pp_sr,10)




########################Review 3 #####################################
library(ggplot2)

#India's XI - Exploration:
#Top Order - Mid Order - Lower Mid - Tail

# 1. Top Order:

x1= list(no_of_balls$balls)
rev(x1)
print(x1)
y1= list(no_of_balls$playerName)
rev(y1)
print(y1)

df_x1_y1= data.frame(x1,y1)

colnames(df_x1_y1)[1] <- "balls"
colnames(df_x1_y1)[2] <- "playerName"
df_x1_y1


fig <-ggplot(data=df_x1_y1, aes(x= reorder(playerName, +balls) , y=balls)) +
  geom_bar(stat="identity", fill="steelblue")+
  ggtitle("Players who played most no of balls in IPL 2016 - 2021") +
  xlab("Player") + ylab("Number of Balls faced")+
  theme_minimal()
fig+coord_flip()

#Best wicket preventors:

x2= list(plot_bpw$bpw)
rev(x2)
print(x2)
y2= list(plot_bpw$playerName)
rev(y2)
print(y2)

df_x2_y2= data.frame(x2,y2)

colnames(df_x2_y2)[1] <- "bpw"
colnames(df_x2_y2)[2] <- "playerName"
df_x2_y2


fig <-ggplot(data=df_x2_y2, aes(x= reorder(playerName, +bpw) , y=bpw)) +
  geom_bar(stat="identity", fill="steelblue")+
  ggtitle("Players with most balls per wicket in IPL 2016 - 2021") +
  xlab("Player") + ylab("Balls per wicket faced")+
  theme_minimal()
fig+coord_flip()




#Summary of Anchors:

library(ggrepel)

ggplot(p3, aes(x=balls, y=bpw)) + geom_point()+
geom_text_repel(aes(label = playerName), size = 3)+
  geom_vline(xintercept = 1750,linetype="dashed")+geom_hline(yintercept = 30,linetype="dashed")

# Fours per Six

spf_df= subset(spf_df,spf_df$fours>100 & spf_df$sixes>50)
spf_df <- spf_df[order(spf_df$fps, decreasing = TRUE),]
spf_df <- head(spf_df,15)
row.names(spf_df) <- NULL
spf_df


#High dot ball percentage - Boundary Riders?
x5= list(plot_dots$dbp)
rev(x5)
print(x5)
y5= list(plot_dots$playerName)
rev(y5)
print(y5)

df_x5_y5= data.frame(x5,y5)

colnames(df_x5_y5)[1] <- "dbp"
colnames(df_x5_y5)[2] <- "playerName"
df_x5_y5


fig <-ggplot(data=df_x5_y5, aes(x= reorder(playerName, +dbp) , y=dbp)) +
  geom_bar(stat="identity", fill="steelblue")+
  ggtitle("Players with high dot ball % since IPL 2016") +
  xlab("Player") + ylab("Dot ball percentage")+
  theme_minimal()
fig+coord_flip()


# Powerplay Stats

xp1= list(df_pp_sr$strike_rate)
rev(xp1)
print(xp1)
yp1= list(df_pp_sr$playerName)
rev(yp1)
print(yp1)

df_xp1_yp1= data.frame(xp1,yp1)

colnames(df_xp1_yp1)[1] <- "Strike_rate"
colnames(df_xp1_yp1)[2] <- "playerName"
df_xp1_yp1


fig <-ggplot(data=df_xp1_yp1, aes(x= reorder(playerName, +Strike_rate) , y=Strike_rate)) +
  geom_bar(stat="identity", fill="steelblue")+
  ggtitle("Players with high S/R in Powerplay since IPL 2016") +
  xlab("Player") + ylab("Strike Rate")+
  theme_minimal()
fig+coord_flip()


# Struggle of our anchors in the Middle overs:

#Strike Rates against Spin in Middle Overs

row.names(rms3) <- NULL
rms3


# Far too many anchors, Solution: ?

S_G_df= subset(df,df$striker=='Shubman Gill')
head(S_G_df)
A_S_G_df <- aggregate(S_G_df["runs_off_bat"], by=S_G_df["season"], sum)
A_S_G_df
G_A_S_G_df <- ggplot(data= A_S_G_df, aes(x=season,y=runs_off_bat ))+
  geom_bar(stat="identity", fill="red")+
  ggtitle("Shubman Gill Stats ") +
  xlab("Season") + ylab("Runs Scored")+geom_hline(yintercept = 400,linetype="dashed")+
  scale_x_continuous(labels=as.character(A_S_G_df$season),breaks=A_S_G_df$season)
G_A_S_G_df

PP_S_df= subset(df,df$striker=='PP Shaw')
head(PP_S_df)
A_PP_S_df <- aggregate(PP_S_df["runs_off_bat"], by=PP_S_df["season"], sum)
A_PP_S_df
G_PP_S_df <- ggplot(data= A_PP_S_df, aes(x=season,y=runs_off_bat ))+
  geom_bar(stat="identity", fill="red")+
  ggtitle("PP Shaw Stats ") +
  xlab("Season") + ylab("Runs Scored")+geom_hline(yintercept = 400,linetype="dashed")+
  scale_x_continuous(labels=as.character(A_PP_S_df$season),breaks=A_PP_S_df$season)

G_PP_S_df

N_R_df= subset(df,df$striker=='N Rana')
head(N_R_df)
A_N_R_df <- aggregate(N_R_df["runs_off_bat"], by=N_R_df["season"], sum)
A_N_R_df
G_N_R_df <- ggplot(data= A_N_R_df, aes(x=season,y=runs_off_bat ))+
  geom_bar(stat="identity", fill="red")+
  ggtitle("N Rana Stats ") +
  xlab("Season") + ylab("Runs Scored")+geom_hline(yintercept = 400,linetype="dashed")+
  scale_x_continuous(labels=as.character(A_N_R_df$season),breaks=A_N_R_df$season)

G_N_R_df


M_A_G_df= subset(df,df$striker=='MA Agarwal')
head(M_A_G_df)
A_M_A_G_df <- aggregate(M_A_G_df["runs_off_bat"], by=M_A_G_df["season"], sum)
A_M_A_G_df
G_M_A_G_df <- ggplot(data= A_M_A_G_df, aes(x=season,y=runs_off_bat ))+
  geom_bar(stat="identity", fill="red")+
  ggtitle("MA Agarwal Stats ") +
  xlab("Season") + ylab("Runs Scored")+geom_hline(yintercept = 400,linetype="dashed")+
  scale_x_continuous(labels=as.character(A_M_A_G_df$season),breaks=A_M_A_G_df$season)

G_M_A_G_df

I_K_df= subset(df,df$striker=='Ishan Kishan')
head(I_K_df)
A_I_K_df <- aggregate(I_K_df["runs_off_bat"], by=I_K_df["season"], sum)
A_I_K_df
G_I_K_df <- ggplot(data= A_I_K_df, aes(x=season,y=runs_off_bat ))+
  geom_bar(stat="identity", fill="red")+
  ggtitle("Ishan Kishan Stats ") +
  xlab("Season") + ylab("Runs Scored")+geom_hline(yintercept = 400,linetype="dashed")+
  scale_x_continuous(labels=as.character(A_I_K_df$season),breaks=A_I_K_df$season)

G_I_K_df

R_A_df= subset(df,df$striker=='RA Tripathi')
head(R_A_df)
A_R_A_df <- aggregate(R_A_df["runs_off_bat"], by=R_A_df["season"], sum)
A_R_A_df
G_R_A_df <- ggplot(data= A_R_A_df, aes(x=season,y=runs_off_bat ))+
  geom_bar(stat="identity", fill="red")+
  ggtitle("RA Tripathi Stats ") +
  xlab("Season") + ylab("Runs Scored")+geom_hline(yintercept = 400,linetype="dashed")+
  scale_x_continuous(labels=as.character(A_R_A_df$season),breaks=A_R_A_df$season)

G_R_A_df

#library(ggpubr)
#theme_set(theme_pubr())
      
#figure <- ggarrange(G_A_S_G_df, G_PP_S_df, G_N_R_df, G_M_A_G_df, G_I_K_df ,G_R_A_df, ncol = 3, nrow = 3)
#figure

# SKY Vs Iyer 

SKY_df= subset(df,df$striker=='SA Yadav')
head(SKY_df)
A_SKY_df <- aggregate(SKY_df["runs_off_bat"], by=SKY_df["season"], sum)
A_SKY_df
G_SKY_df <- ggplot(data= A_SKY_df, aes(x=season,y=runs_off_bat ))+
  geom_bar(stat="identity", fill="blue")+
  ggtitle("SA Yadav Stats ") +
  xlab("Season") + ylab("Runs Scored")+geom_hline(yintercept = 400,linetype="dashed")+
  scale_x_continuous(labels=as.character(A_SKY_df$season),breaks=A_SKY_df$season)

G_SKY_df

S_IY_df= subset(df,df$striker=='SS Iyer')
head(S_IY_df)
A_S_IY_df <- aggregate(S_IY_df["runs_off_bat"], by=S_IY_df["season"], sum)
A_S_IY_df
G_S_IY_df <- ggplot(data= A_S_IY_df, aes(x=season,y=runs_off_bat ))+
  geom_bar(stat="identity", fill="green")+
  ggtitle("SS Iyer Stats ") +
  xlab("Season") + ylab("Runs Scored")+geom_hline(yintercept = 400,linetype="dashed")+
  scale_x_continuous(labels=as.character(A_S_IY_df$season),breaks=A_S_IY_df$season)

G_S_IY_df


# KL v Rohit - Recent Form against ENG
# KL Rahul last few scores against Eng: 14, 0, 0, 1
# RG Sharma last few scores against Eng: 64, 12, 15
# Rohit's captaincy skills will be handy

#Final Top 4:
#1. Rohit Sharma
#2. Shikhar Dhawan
#3. Virat Kohli 
#4. Surya Kumar Yadav


#Middle Order: Finishers
#H Pandya, K Pandya, Pant, DK, Samson, Jadeja, S Dube, ....

plot_sr_runs = subset(sr_df, sr_df$balls>250)
plot_sr_runs <- plot_sr_runs[order(plot_sr_runs$sr, decreasing = TRUE),]
plot_sr_runs <- head(plot_sr_runs,15)
row.names(plot_sr_runs) <- NULL
plot_sr_runs

ggplot(plot_sr_runs, aes(x=balls, y=sr)) + geom_point()+
  geom_text_repel(aes(label = playerName), size = 3)+
  geom_vline(xintercept = 1750,linetype="dashed")+geom_hline(yintercept = 140,linetype="dashed")+ylab("Strike Rate")


x6= list(plot_sr_runs$sr)
rev(x6)
print(xp1)
y6= list(plot_sr_runs$playerName)
rev(y6)
print(y6)

df_x6_y6= data.frame(x6,y6)

colnames(df_x6_y6)[1] <- "Strike_rate"
colnames(df_x6_y6)[2] <- "playerName"
df_x6_y6


fig <-ggplot(data=df_x6_y6, aes(x= reorder(playerName, +Strike_rate) , y=Strike_rate)) +
  geom_bar(stat="identity", fill="steelblue")+
  ggtitle("Players with high S/R since IPL 2016") +
  xlab("Player") + ylab("Strike Rate")+
  theme_minimal()
fig+coord_flip()


# Coming to Death overs:

D_spec= c("HH Pandya", "KD Karthik","RA Jadeja","RR Pant","SV Samson","AR Patel")
for (i in D_spec){
  print(i)
  co="red"
  if(i=="HH Pandya" | i== "RR Pant"){
    co="green"
  }
  DS_df= subset(df,df$striker==i)
  #head(DS_df)
  A_DS_df_df <- aggregate(DS_df["runs_off_bat"], by=DS_df["season"], sum)
  #A_DS_df_df
  title= paste0(i," Stats")
  G_DS_df <- ggplot(data= A_DS_df_df, aes(x=season,y=runs_off_bat ))+
    geom_bar(stat="identity", fill=co)+
    ggtitle(title) +
    xlab("Season") + ylab("Runs Scored")+geom_hline(yintercept = 400,linetype="dashed")+
    scale_x_continuous(labels=as.character(A_DS_df_df$season),breaks=A_DS_df_df$season)
 print(G_DS_df) 
}

#Final Top 6:
#1. Rohit Sharma
#2. Shikhar Dhawan
#3. Virat Kohli
#4. Surya Kumar Yadav
#5. Rishab Pant
#6. Hardik Pandya


##Lower Middle Order, Spin & Pace options:

ggplot(plot_eco_pp, aes(x=pp_percentage, y=economy)) + geom_point()+
  geom_text_repel(aes(label = playerName), size = 3)+
  geom_vline(xintercept = 40,linetype="dashed")+geom_hline(yintercept = 8,linetype="dashed")

#Question on Spinners bowling in Powerplay!
bowl_wk_pp_spin
fig <-ggplot(data=bowl_wk_pp_spin, aes(x= reorder(playerName , +wickets ) , y=wickets )) +
  geom_bar(stat="identity", fill="steelblue")+
  ggtitle("Spin bowler with their number of wickets since 2016 IPL") +
  xlab("Player") + ylab("Number of wickets ")+
  theme_minimal()
fig+coord_flip()


#Requirements:
 # Counter lefties
#Ability to bat/field
#Ability to bowl in PP/Mid

#W Sundar - way ahead of others as PP option, also handy to bat & takes the ball away from Lefties
#Alternate choice - Ashwin (track record against Lefties)
#R Jadeja - recent form, finishing ability & gun fielding
#Alternate choice - AR Patel (performance against Eng in test matches)
#other options - wrist spin / mystery spin:
#  R Chahar, Chahal, Bishnoi, Varun,

# Final Top 8:
#   1. Rohit Sharma
# 2. Shikhar Dhawan
# 3. Virat Kohli
# 4. Surya Kumar Yadav
# 5. Rishab Pant (Wk)
# 6. Hardik Pandya
# 7. R Jadeja
# 8. Washington Sundar

##Tail

ggplot(plot_eco_pp_pace, aes(x=pp_percentage, y=economy)) + geom_point()+
  geom_text_repel(aes(label = playerName), size = 3)+
  geom_vline(xintercept = 60,linetype="dashed")+geom_hline(yintercept = 8,linetype="dashed")

bowl_wk_pp_pace <- head(bowl_wk_pp_pace,10)
fig <-ggplot(data=bowl_wk_pp_pace, aes(x= reorder(playerName , +wickets ) , y=wickets )) +
  geom_bar(stat="identity", fill="steelblue")+
  ggtitle("Pace bowler with their number of wickets since 2016 IPL") +
  xlab("Player") + ylab("Number of wickets ")+
  theme_minimal()
fig+coord_flip()


plot_death_eco = death_eco[order(death_eco$economy, decreasing = FALSE),]
plot_death_eco = head(plot_death_eco,20)
plot_death_eco

fig <-ggplot(data=plot_death_eco, aes(x= reorder(playerName , -economy ) , y=economy )) +
  geom_bar(stat="identity", fill="steelblue")+
  ggtitle("Pace bowler with best economy  since 2016 IPL") +
  xlab("Player") + ylab("Number of wickets ")+
  theme_minimal()
fig+coord_flip()


fig <-ggplot(data=bowl_wk_death, aes(x= reorder(playerName , +wickets ) , y=wickets )) +
  geom_bar(stat="identity", fill="steelblue")+
  ggtitle("Pace bowler with their number of wickets in death overs since 2016 IPL") +
  xlab("Player") + ylab("Number of wickets ")+
  theme_minimal()
fig+coord_flip()


# Bumrah walks into the team any day!
#   Others:
#   
#   D Chahar - (excellent in PP, decent in Death)
# B Kumar - (perfect for swinging conditions, performance against Eng)
# Shami - (nail yorkers, 140+)
# T Natarajan - (with confidence of Aus tour)
# M Siraj - (excellent IPL 2021 season)
# N Saini - (high pace, low accuracy)
# Shardul, Unadkat, Tyagi, Avesh, Harshal, Arshdeep - (inconsistent, low experience)
# Bowlers Consistency

B_spec= c("JJ Bumrah", "B Kumar","Mohammed Shami","Mohammed Siraj","DL Chahar","SN Thakur","T Natarajan","JD Unadkat")
for (i in B_spec){
  print(i)
  co="red"
  if(i=="JJ Bumrah" | i== "B Kumar" | i== "Mohammed Shami"){
    co="green"
  }
  DS_df= subset(df,df$bowler==i)
  #head(DS_df)
  A_DS_df_df <- aggregate(DS_df["isOut"], by=DS_df["season"], sum)
  #A_DS_df_df
  title= paste0(i," Stats")
  G_DS_df <- ggplot(data= A_DS_df_df, aes(x=season,y=isOut ))+
    geom_bar(stat="identity", fill=co)+
    ggtitle(title) +
    xlab("Season") + ylab("Number of wickets")+geom_hline(yintercept = 15,linetype="dashed")+
    scale_x_continuous(labels=as.character(A_DS_df_df$season),breaks=A_DS_df_df$season)
  print(G_DS_df) 
}

# India's Best XI against England (T20I) :
# 1. Rohit Sharma (vc)
# 2. Shikhar Dhawan
# 3. Virat Kohli (c)
# 4. Surya Kumar Yadav
# 5. Rishab Pant (Wk)
# 6. Hardik Pandya
# 7. Ravindra Jadeja
# 8. Washington Sundar
# 9. Bhuvaneshwar Kumar
# 10. Jasprit Bumrah
# 11. Mohammed Shami

# Others:
#   S Iyer, KL Rahul, R Ashwin, D Chahar










