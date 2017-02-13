rm(list=ls())

setwd('/Users/treny/Dropbox/git_election_data')
df <- read.csv('gubernatorial/full_gub_data_1992-2015.csv',stringsAsFactors = F)
head(df)
demo <- read.csv('demo_2016.csv',stringsAsFactors = F)
demo$county <- demo$area

out <- merge(df,demo,by=c('state','county'))
write.csv(out,row.names=F,'')







out$pct_white <- out$white/out$total_pop

temp <- out %>%
  group_by(year) %>%
  filter(pct_white > 0.95) %>%
  summarise(rep=mean(pct_rep,na.rm=T),
            dem=mean(pct_dem,na.rm=T),
            n=n())

out$years <- NA
out$years[out$year %in% c(1992,1993,1994,1995)] <- 1992
out$years[out$year %in% c(1996:1999)] <- 1996
out$years[out$year %in% c(2000:2003)] <- 2000
out$years[out$year %in% c(2004:2007)] <- 2004
out$years[out$year %in% c(2008:2011)] <- 2008
out$years[out$year %in% c(2012:2015)] <- 2012


temp <- out %>%
  group_by(years) %>%
  filter(pct_white > 0.95) %>%
  summarise(rep=mean(pct_rep,na.rm=T),
            dem=mean(pct_dem,na.rm=T),
            n=n())
temp <- gather(temp,party,vote,2:3)
ggplot(temp,aes(years,vote,group=party,color=party)) + 
  geom_point() + geom_line() +
  scale_color_manual(values=c('red','blue')) +
  scale_x_continuous(breaks=c(1992,1996,2000,2004,2008,2012))

temp <- out %>%
  group_by(years) %>%
  summarise(rep=mean(pct_rep,na.rm=T),
            dem=mean(pct_dem,na.rm=T),
            n=n())
temp <- gather(temp,party,vote,2:3)
ggplot(temp,aes(years,vote,group=party,color=party)) + 
  geom_point() + geom_line() +
  scale_color_manual(values=c('blue','red')) +
  scale_x_continuous(breaks=c(1992,1996,2000,2004,2008,2012))

out$pct_nonwhite <- 1-out$pct_white
out$pct_latino <- out$latino/out$total_pop
ggplot(out,aes(pct_latino, pct_rep,size=total_pop)) + 
  geom_point() +
  stat_smooth(se=F,aes(weight=total_pop))
