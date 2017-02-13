#####
# Mapping and Plots
# County Election Data
# Tyler Reny
#####

rm(list=ls())
library(stringr)
library(tidyverse)

df <- read.csv('/Users/treny/Dropbox/git_election_data/merged_county_election_with_demo.csv',stringsAsFactors = F)

out <- df %>%
  group_by(year,state) %>%
  filter(year >= 1988, non_white_2016 < 0.05) %>%
  summarise(repub=mean(rep_votes,na.rm=T))
ggplot(out, aes(year,repub)) + 
  geom_point() +
  geom_line() + 
  geom_hline(yintercept = .50,linetype=2,color='red') +
  scale_x_continuous(breaks=c(1988,1992,1996,2000,2004,2008,2012,2016)) +
  facet_wrap(~state)


#### vote in places that grew from <2% Latino to 10% Latino
df$small_lat_change <- as.factor(ifelse(((df$non_white_2016 - df$non_white_1990)/df$non_white_1990)*100 > 100,'high','low'))
out <- df %>%
  group_by(year, state, small_lat_change) %>%
  filter(year >= 1988) %>%
  summarise(repub=mean(rep_votes,na.rm=T))
ggplot(out, aes(year,repub,group=small_lat_change,color=small_lat_change)) + 
  geom_point() +
  geom_line() + 
  geom_hline(yintercept = .50,linetype=2,color='red') +
  facet_wrap(~state)

  


ggplot(m2[m2$year > 1996,], aes(nwhite_growth,rep_votes)) + 
  geom_point() + 
  stat_smooth(method='lm') + facet_wrap(~year)









######
# create maps ----
######

library(tidyverse)
library(maps)

setwd('/Users/treny/Dropbox/git_election_data/')
#read in full data
out <- read.csv('all_county_returns_1992_2016_demo.csv') 
out$polyname <- paste(tolower(out$state),tolower(out$area),sep=',')

# subset to counties of interest
out$state_county <- paste(out$state,out$area,sep=' ')
#tosub <- out$state_county[which(out$pct_white > 0.95 & out$dem_votes > 0.50 & out$year == 1992)]
tosub <- out$state_county[which(out$pct_white > 0.95)]
temp <- out[out$state_county %in% tosub,]

#create mean line
line <- temp %>% 
  group_by(year) %>%
  summarise(Democrat=mean(dem_votes,na.rm=T),
            Republican= mean(rep_votes,na.rm=T))
line <- gather(line,Candidate,Percent,2:3)

#plot
ggplot(temp,aes(year,dem_votes)) + 
  geom_point(color='blue',alpha=.1) + 
  geom_point(aes(year,rep_votes),color='red',alpha=.1) + 
  scale_y_continuous(limits=c(0,1)) +
  geom_line(data=line,aes(x=year,y=Percent,color=Candidate)) +
  scale_color_manual(values = c('blue','red')) + 
  scale_x_continuous(breaks=c(1992,1996,2000,2004,2008,2012,2016)) +
  labs(x='',y='Pct Vote',
       title='County Level Vote Share',
       subtite='White Counties (95% or greater)')
       #subtitle='White Counties That Voted Clinton 1992')

#mapping
county.fips$color <- ifelse(county.fips$polyname %in% temp$polyname,"#C994C7","#F1EEF6")
map("county", col = county.fips$color, fill = TRUE, resolution = 0, 
    lty = 0, projection = "polyconic")
map("state", col = "black", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2, 
    projection = "polyconic")


library(RColorBrewer)
file <- 'op-ed-map.csv'
lat_pop <- read.csv(file,stringsAsFactors = F)

cols <- brewer.pal(n = 9,name='Blues')
fips <- county.fips[grep('illinois,',county.fips$polyname),]
for_col <- merge(fips,lat_pop,by='fips')
for_col$color <- cols[cut(for_col$pct_latino,9)]

pdf(width=4,height=7,file='illinois-map.pdf')
map("county",regions = 'illinois',col=for_col$color, fill = T, resolution = 0, 
    lty = 0, projection = "polyconic")
map("county",regions='illinois', col = "black", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2, 
    projection = "polyconic")
title("Illinois Latino Pop 2015 \n(by county)")
legend(x=62000, y=0, 
       legend=leglabs(round(for_col$pct_latino*100,2)), 
       fill=for_col$color, bty="n",
       x.intersp = .5, y.intersp = .5)



shp <- '/Users/treny/Downloads/cb_2015_us_county_500k'
dat <- readOGR(shp, layer ="cb_2015_us_county_500k", stringsAsFactors = F )
dat <- subset(dat, dat$STATEFP == 17)
names(dat)
dat@data$id <- rownames(dat@data)
dat@data$county <- dat@data$NAME
dat.points = tidy(dat, region='id')
df = plyr::join(dat.points, dat@data, by="id")
df$fips <- df$COUNTYFP
unique(df$fips)


library(stringr)
color <- for_col[c('fips','pct_latino')]
color$fips <- str_sub(color$fips,3,-1)
df <- merge(df,color,by='fips')

g.out <- ggplot() +
    geom_polygon(data = df, aes(x = long, y = lat, group = group,
        fill = pct_latino)) +
    geom_polygon(data = df, aes(x = long, y = lat, group = group),
        fill = NA, color = "black", size = 0.25) +
 coord_map() +
    scale_fill_distiller(palette = "Blues", labels = percent,
        breaks = pretty_breaks(n = 10), values = c(1,0)) +
    guides(fill = guide_legend(reverse = TRUE)) +
      theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
  ) +
    labs(title = "Percent Latino 2015",
         subtitle='by county',
         fill='')
ggsave(g.out,height=6,width=4,file='illinois_lat_pop.png')







