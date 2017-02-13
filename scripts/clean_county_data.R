####
# Clean county data
####
library(tidyverse)
setwd('/Users/treny/Dropbox/git_election_data')
files <- list.files()[grep('export',list.files())]
files
dat <- read_csv(files[1],skip=2)
dat <- dat[grep('County',dat$AreaType),]
head(dat)
for(i in 2:length(files)){
  temp <- read_csv(files[i],skip=2)
  temp <- temp[grep('County',temp$AreaType),]
  dat <- rbind(dat,temp)
}

write.csv(dat,row.names=F,'pres_1968.csv')
file.remove(files)


files <- list.files()[grep('pres', list.files())][1:6]
dat <- read.csv('all_county_returns_1992_2016.csv',stringsAsFactors = F)
dat <- dat[,c('State','Area','TotalVotes','RepVotesTotalPercent','DemVotesTotalPercent','ThirdVotesTotalPercent','OtherVotesTotalPercent')]
names(dat) <- c('state','area','total_votes','rep_votes','dem_votes','third_votes','other_votes')
dat$year <- files[1]
head(dat)
for(i in 1:length(files)){
  temp <- read.csv(files[i],stringsAsFactors=F)
  temp <- temp[,c('State','Area','TotalVotes','RepVotesTotalPercent','DemVotesTotalPercent','ThirdVotesTotalPercent','OtherVotesTotalPercent')]
  names(temp) <- c('state','area','total_votes','rep_votes','dem_votes','third_votes','other_votes')
  temp$year <- files[i]
  dat <- rbind(dat,temp)
}
dat$year[dat$year=='pres_1972.csv'] <- 1972
dat$year[dat$year=='pres_1976.csv'] <- 1976
dat$year[dat$year=='pres_1980.csv'] <- 1980
dat$year[dat$year=='pres_1984.csv'] <- 1984
dat$year[dat$year=='pres_1988.csv'] <- 1988
dat$year[dat$year=='pres_1968.csv'] <- 1968
unique(dat$year)

write.csv(dat,row.names = F,'county_president_1968-2016.csv')

dat %>% arrange(desc(year)) %>% head()

out <- read.csv(list.files()[7],stringsAsFactors = F)
out <- data.frame(state=out$state_abbr,
                  area=out$county_name,
                  total_votes=out$total_votes,
                  rep_votes=out$per_gop*100,
                  dem_votes=out$per_dem*100)
out$third_votes <- 100-(out$dem_votes+out$rep_votes)
out$other_votes <- NA
out$area <- as.character(out$area)
out$area <- toupper(gsub(' County','',out$area))
out$state <- as.character(out$state)
key <- data.frame(state=state.abb,state.name)
out$state <- state.name[match(out$state,key$state)]
out$year <- list.files()[7]

total <- rbind(dat,out)
total$year <- as.numeric(gsub('pres_|.csv','',total$year))
total$rep_votes <- total$rep_votes/100
total$dem_votes <- total$dem_votes/100
total$third_votes <- total$third_votes/100
total$other_votes <- total$other_votes/100
write.csv(total,row.names = F,'all_county_returns_1992_2016.csv')

setwd('/Users/treny/Dropbox/git_election_data/')
df <- read.csv('county_demo_2015.csv',stringsAsFactors = F)
area <- toupper(gsub(' County','',sapply(str_split(df$county,', '),function(x)x[1])))
state <- sapply(str_split(df$county,', '),function(x)x[2])
df$area <- area
df$state  <- state
df <- df %>%
  select(-county)

out <- merge(total,df,by=c('state','area'))
write.csv(out,row.names=F,'all_county_returns_1992_2016_demo.csv')

df <- read.csv('demo_1990.csv',stringsAsFactors = F)
df$county
area <- toupper(gsub(' County','',sapply(str_split(df$county,', '),function(x)x[1])))
state <- sapply(str_split(df$county,', '),function(x)x[2])
df$area <- area
df$state  <- state
df <- df %>%
  select(-county)

df$pct_white_1990 <- df$pct_white
df <- df[,c('state','area','pct_white_1990')]
foo <- merge(out,df,by=c('state','area'))
write.csv(foo,row.names=F,'all_county_returns_1992_2016_demo.csv')

out <- foo
ggplot(out,aes(x=pct_white,y=pct_white_1990)) + geom_point() +
  geom_abline(slope = 1) +
  scale_y_continuous(limits=c(0,1))





# Cleaning demo data

library(stringr)
df <- read.csv('demo_1990.csv',stringsAsFactors = F)
df <- df[-1,]
head(df)
grep('Puerto Rico',df$Qualifying.Name)
df <- df[-grep('Puerto Rico',df$Qualifying.Name),]
area <- toupper(gsub(' County','',sapply(str_split(df$Qualifying.Name,', '),function(x)x[1])))
state <- sapply(str_split(df$Qualifying.Name,', '),function(x)x[2])
total_pop <- as.numeric(df[['Total.Population']])
white <- as.numeric(df[['Total.Population..Non.Hispanic..White']])
black <- as.numeric(df[['Total.Population..Non.Hispanic..Black']])
latino <- as.numeric(df[['Total.Population..Hispanic']])
non_white <- total_pop - white
dat <- data.frame(state, area, total_pop, white, black, latino, non_white)
write.csv(dat,row.names = F,'demo_1990.csv')


df <- read.csv('demo_2000.csv',stringsAsFactors = F)
df <- df[-1,]
head(df)
grep('Puerto Rico',df$Qualifying.Name)
df <- df[-grep('Puerto Rico',df$Qualifying.Name),]
area <- toupper(gsub(' County','',sapply(str_split(df$Qualifying.Name,', '),function(x)x[1])))
state <- sapply(str_split(df$Qualifying.Name,', '),function(x)x[2])
total_pop <- as.numeric(df[['Total.Population']])
white <- as.numeric(df[['Not.Hispanic.or.Latino..White.Alone']])
black <- as.numeric(df[['Not.Hispanic.or.Latino..Black.or.African.American.Alone']])
latino <- as.numeric(df[['Hispanic.or.Latino']])
non_white <- total_pop - white
dat <- data.frame(state, area, total_pop, white, black, latino, non_white)
write.csv(dat,row.names = F,'demo_2000.csv')

df <- read.csv('demo_2010.csv',stringsAsFactors = F)
df <- df[-1,]
head(df)
area <- toupper(gsub(' County','',sapply(str_split(df$Qualifying.Name,', '),function(x)x[1])))
state <- sapply(str_split(df$Qualifying.Name,', '),function(x)x[2])
total_pop <- as.numeric(df[['Total.population']])
white <- as.numeric(df[['Total.population..Not.Hispanic.or.Latino..White.alone']])
black <- as.numeric(df[['Total.population..Not.Hispanic.or.Latino..Black.or.African.American.alone']])
latino <- as.numeric(df[['Total.population..Hispanic.or.Latino']])
non_white <- total_pop - white
dat <- data.frame(state, area, total_pop, white, black, latino, non_white)
dat <- dat[-which(dat$state=='Puerto Rico'),]
write.csv(dat,row.names = F,'demo_2010.csv')


df <- read.csv('demo_2012.csv',stringsAsFactors = F)
df <- df[-1,]
head(df)
area <- toupper(gsub(' County','',sapply(str_split(df$Qualifying.Name,', '),function(x)x[1])))
state <- sapply(str_split(df$Qualifying.Name,', '),function(x)x[2])
names(df)
total_pop <- as.numeric(df[['Total.Population']])
white <- as.numeric(df[['Total.Population..Not.Hispanic.or.Latino..White.Alone']])
black <- as.numeric(df[['Total.Population..Not.Hispanic.or.Latino..Black.or.African.American.Alone']])
latino <- as.numeric(df[['Total.Population..Hispanic.or.Latino']])
non_white <- total_pop - white
dat <- data.frame(state, area, total_pop, white, black, latino, non_white)
dat <- dat[-which(dat$state=='Puerto Rico'),]
dat$state
write.csv(dat,row.names = F,'demo_2012.csv')


df <- read.csv('demo_2016.csv',stringsAsFactors = F)
df <- df[-c(1,3143:nrow(df)),]

area <- toupper(gsub(' County','',sapply(str_split(df$Qualifying.Name,', '),function(x)x[1])))
state <- sapply(str_split(df$Qualifying.Name,', '),function(x)x[2])
names(df)
total_pop <- as.numeric(df[['Total.Population']])
white <- as.numeric(df[['Total.Population..Not.Hispanic.or.Latino..White.Alone']])
black <- as.numeric(df[['Total.Population..Not.Hispanic.or.Latino..Black.or.African.American.Alone']])
latino <- as.numeric(df[['Total.Population..Hispanic.or.Latino']])
non_white <- total_pop - white
dat <- data.frame(state, area, total_pop, white, black, latino, non_white)
write.csv(dat,row.names = F,'demo_2016.csv')

df <- read.csv('all_county_returns_1992_2016.csv',stringsAsFactors = F)
dem1990 <- read.csv('demo_1990.csv',stringsAsFactors = F)
dem2000 <- read.csv('demo_2000.csv',stringsAsFactors = F)
dem2010 <- read.csv('demo_2010.csv',stringsAsFactors = F)
dem2012 <- read.csv('demo_2012.csv',stringsAsFactors = F)
dem2016 <- read.csv('demo_2016.csv',stringsAsFactors = F)

df1992 <- merge(df[df$year==1992,],dem1990,by=c('state','area'))
df1996 <- merge(df[df$year==1996,],dem1990,by=c('state','area'))
df2000 <- merge(df[df$year==2000,],dem2000,by=c('state','area'))
df2004 <- merge(df[df$year==2004,],dem2000,by=c('state','area'))
df2008 <- merge(df[df$year==2008,],dem2000,by=c('state','area'))
df2012 <- merge(df[df$year==2012,],dem2012,by=c('state','area'))
df2016 <- merge(df[df$year==2016,],dem2016,by=c('state','area'))
out <- rbind(df1992,df1996,df2000,df2004,df2008,df2012,df2016)
out$pct_white <- out$white/out$total_pop
out$pct_black <- out$black/out$total_pop
out$pct_latino <- out$latino/out$total_pop
out$pct_nonwhite <- out$non_white/out$total_pop
key <- data.frame(state.name,state.region)
out$region <- key$state.region[match(out$state,key$state.name)]

write.csv(out,row.names=F,'all_county_returns_1992_2016_demo.csv')


setwd('/Users/treny/Dropbox/git_election_data/')
out <- read.csv('all_county_returns_1992_2016_demo.csv',stringsAsFactors=F)
dat2016 <- out[out$year==2016,]
dat1992 <- out[out$year==1992,]
temp <- merge(dat2016,dat1992,by=c('state','area'))
names(temp)
temp$change_non_white <- ((temp$non_white.x - temp$non_white.y)/temp$non_white.y) * 100

temp$change_white <- ((temp$white.x - temp$white.y)/temp$white.y) * 100
temp$change_latino <- ((temp$latino.x - temp$latino.y)/temp$latino.y) * 100
temp$change_latino[which(temp$change_latino == Inf)] <- NA
temp$change_black <- ((temp$black.x - temp$black.y)/temp$black.y) * 100
temp$change_black[which(temp$change_black == Inf)] <- NA
names(temp)
out <- merge(out, temp[,c('state', 'area', 'change_white', 'change_non_white', 
                          'change_latino','change_black')],by=c('state','area'))

names(out)



out %>% 
  filter(year==2016 & change_latino < 2500) %>%
  ggplot() +
  geom_point(aes(change_latino,rep_votes),alpha=0.1) +
  stat_smooth(aes(change_latino,rep_votes),method='lm',color='red') +
  facet_wrap(~region + year,ncol=7) +
  labs(x='White Share of Population',y='Votes for Republican Pres Candidate')

library(broom)
res <- lm(rep_votes ~ change_non_white,data=out)
res <- broom::tidy(res)
res
ggplot(res,aes(term,estimate)) + 
  geom_point() + coord_flip() + 
  geom_segment(aes(x=term,xend=term,y=estimate-std.error,yend=estimate+std.error))



###
# merge county data with demos
###

setwd('/Users/treny/Dropbox/git_election_data/')
df <- read.csv('county_president_1968-2016.csv',stringsAsFactors=F)

demo_1990 <- read.csv('demo_1990.csv',stringsAsFactors = T)
names(demo_1990)[3:7] <- paste0(names(demo_1990)[3:7],'_1990')

demo_2000 <- read.csv('demo_2000.csv',stringsAsFactors = T)
names(demo_2000)[3:7] <- paste0(names(demo_2000)[3:7],'_2000')

demo_2010 <- read.csv('demo_2010.csv',stringsAsFactors = T)
names(demo_2010)[3:7] <- paste0(names(demo_2010)[3:7],'_2010')

demo_2012 <- read.csv('demo_2012.csv',stringsAsFactors = T)
names(demo_2012)[3:7] <- paste0(names(demo_2012)[3:7],'_2012')

demo_2016 <- read.csv('demo_2016.csv',stringsAsFactors = T)
names(demo_2016)[3:7] <- paste0(names(demo_2016)[3:7],'_2016')

m1 <- merge(df,demo_1990,by=c('state','area'))
m1 <- merge(m1,demo_2000,by=c('state','area'))
m1 <- merge(m1,demo_2010,by=c('state','area'))
m1 <- merge(m1,demo_2012,by=c('state','area'))
m2 <- merge(m1,demo_2016,by=c('state','area'))
m2$rep_votes[m2$year < 1992] <- m2$rep_votes[m2$year < 1992]/100

m2$non_white_2016 <- 1-(m2$white_2016/m2$total_pop_2016)
m2$non_white_2012 <- 1-(m2$white_2012/m2$total_pop_2012)
m2$non_white_2010 <- 1-(m2$white_2010/m2$total_pop_2010)
m2$non_white_2000 <- 1-(m2$white_2000/m2$total_pop_2000)
m2$non_white_1990 <- 1-(m2$white_1990/m2$total_pop_1990)

m2$nwhite_growth[m2$year==2000] <- ((m2$non_white_2000 - m2$non_white_1990)/m2$non_white_1990)*100
m2$nwhite_growth[m2$year==2004] <- ((m2$non_white_2000 - m2$non_white_1990)/m2$non_white_1990)*100
m2$nwhite_growth[m2$year==2010] <- ((m2$non_white_2010 - m2$non_white_2000)/m2$non_white_2000)*100
m2$nwhite_growth[m2$year==2012] <- ((m2$non_white_2012 - m2$non_white_2000)/m2$non_white_2000)*100
m2$nwhite_growth <- ((m2$non_white_2016 - m2$non_white_1990)/m2$non_white_1990)*100

write.csv(m2,row.names=F,'merged_county_election_with_demo.csv')

####
# merge primary data with demos
####

#2016 primary
setwd('/Users/treny/Dropbox/git_election_data/')
df <- read.csv('primary_results_2016.csv',stringsAsFactors=F)
df$area <- toupper(df$county)

demo_1990 <- read.csv('demo_1990.csv',stringsAsFactors = T)
names(demo_1990)[3:7] <- paste0(names(demo_1990)[3:7],'_1990')

demo_2000 <- read.csv('demo_2000.csv',stringsAsFactors = T)
names(demo_2000)[3:7] <- paste0(names(demo_2000)[3:7],'_2000')

demo_2012 <- read.csv('demo_2012.csv',stringsAsFactors = T)
names(demo_2012)[3:7] <- paste0(names(demo_2012)[3:7],'_2012')

demo_2016 <- read.csv('demo_2016.csv',stringsAsFactors = T)
names(demo_2016)[3:7] <- paste0(names(demo_2016)[3:7],'_2016')

m1 <- merge(df,demo_1990,by=c('state','area'))
m1 <- merge(m1,demo_2000,by=c('state','area'))
m1 <- merge(m1,demo_2012,by=c('state','area'))
m2 <- merge(m1,demo_2016,by=c('state','area'))
names(m2)
m2$non_white_2016 <- 1-(m2$white_2016/m2$total_pop_2016)
m2$non_white_1990 <- 1-(m2$white_1990/m2$total_pop_1990)
m2$non_white_change <- (m2$non_white_2016 - m2$non_white_1990)/m2$non_white_1990
