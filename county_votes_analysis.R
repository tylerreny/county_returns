#####
# Mapping and Plots
# County Election Data
# Tyler Reny
#####

library(tidyverse)
library(maps)

#read in full data
out <- read.csv('all_county_returns_1992_2016_demo.csv') 
out$polyname <- paste(tolower(out$state),tolower(out$area),sep=',')

# subset to counties of interest
out$state_county <- paste(out$state,out$area,sep=' ')
tosub <- out$state_county[which(out$pct_white > 0.90 & out$dem_votes > 0.50 & out$year == 1992)]
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
       subtitle='White Counties That Voted Clinton 1992')

#mapping
county.fips$color <- ifelse(county.fips$polyname %in% temp$polyname,"#C994C7","#F1EEF6")
map("county", col = county.fips$color, fill = TRUE, resolution = 0, 
    lty = 0, projection = "polyconic")
map("state", col = "black", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2, 
    projection = "polyconic")


