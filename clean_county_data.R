####
# Clean county data
####

setwd('/Users/treny/Dropbox/_projects/justin_gest/wwc/data')
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

write.csv(dat,row.names=F,'county_returns/pres_1992.csv')


setwd('/Users/treny/Dropbox/_projects/justin_gest/wwc/data/county_returns/')
files <- list.files()[-7]
dat <- read.csv(files[1],stringsAsFactors = F)
dat <- dat[,c('State','Area','TotalVotes','RepVotesTotalPercent','DemVotesTotalPercent','ThirdVotesTotalPercent','OtherVotesTotalPercent')]
names(dat) <- c('state','area','total_votes','rep_votes','dem_votes','third_votes','other_votes')
dat$year <- files[1]
head(dat)
for(i in 2:length(files)){
  temp <- read.csv(files[i],stringsAsFactors=F)
  temp <- temp[,c('State','Area','TotalVotes','RepVotesTotalPercent','DemVotesTotalPercent','ThirdVotesTotalPercent','OtherVotesTotalPercent')]
  names(temp) <- c('state','area','total_votes','rep_votes','dem_votes','third_votes','other_votes')
  temp$year <- files[i]
  dat <- rbind(dat,temp)
}
dat$total_votes <- as.numeric(gsub(',','',dat$total_votes))


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





