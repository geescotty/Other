### FGR Football Analysis ####
library(dplyr)
library(jsonlite)
library(ggplot2)
library(ggforce)
library(readxl)
library(ggimage)
library(tidyr)
library(purrr)
library(knitr)
library(kableExtra)

json_data <- fromJSON("3788759.json")
json_data$location<-as.character(json_data$location)
json_data$pass$end_location<-as.character(json_data$pass$end_location)

coordinates_split <- strsplit(json_data$location, ",")
coordinates_splitend <- strsplit(json_data$pass$end_location, ",")
clean_string <- function(s) {
  gsub("[^0-9.]", "", s)
}
cleaned_list <- lapply(coordinates_split, clean_string)
cleaned_listend <- lapply(coordinates_splitend, clean_string)

json_data$x<- sapply(cleaned_list, function(coord) {
  as.numeric(coord[1])
})
json_data$y <- sapply(cleaned_list, function(coord) {
  as.numeric(coord[2])
})

json_data$xend<- sapply(cleaned_listend, function(coord) {
  as.numeric(coord[1])
})
json_data$yend <- sapply(cleaned_listend, function(coord) {
  as.numeric(coord[2])
})
json_data$image<-ifelse(json_data$team$name=="Scotland",
                        "https://www.scottishfa.co.uk/media/1008/scottish-football-association-logo.png?mode=max&width=90","https://th.bing.com/th/id/R.4b2953952844cadf7da857b12af3aca9?rik=3hM0Q3KIBrIvMA&riu=http%3a%2f%2flogos-download.com%2fwp-content%2fuploads%2f2016%2f05%2fEngland_national_football_team_logo_crest.png&ehk=INYcvMf7goeZHy7pYTOLckHbaG1lp9iO9QF%2bhK9Jpc0%3d&risl=&pid=ImgRaw&r=0")

### passing plots for each side in attacking third 
passings<-subset(json_data,json_data$type$name=="Pass" & json_data$possession_team$name=="Scotland")
passinge<-subset(json_data,json_data$type$name=="Pass" & json_data$possession_team$name=="England")

ggplot(data=passings) +
  ylab("")+
  xlab("")+
  theme(panel.background = element_rect(fill = 'lime green'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_segment(aes(x=80,y=0,xend=120,yend=0),color="white",size=1.5)+
  geom_segment(aes(x=80,y=80,xend=120,yend=80),color="white",size=1.5)+
  geom_segment(aes(x=0,y=0,xend=0,yend=80),color="white",size=1.5)+
  geom_segment(aes(x=120,y=0,xend=120,yend=80),color="white",size=1.5)+
  geom_segment(aes(x=60,y=0,xend=60,yend=80),color="white",size=1.5)+ ### main fields lines
  geom_point(aes(x=60,y=40),col="white",size=2)+ ## centre point
  geom_point(aes(x=12,y=40),col="white",size=2)+ ## pen mark
  geom_point(aes(x=108,y=40),col="white",size=2)+ ## pen mark
  geom_segment(aes(x=-2,y=36,xend=-2,yend=44),color="white",size=1.5)+
  geom_segment(aes(x=-2,y=36,xend=0,yend=36),color="white",size=1.5)+
  geom_segment(aes(x=-2,y=44,xend=0,yend=44),color="white",size=1.5)+
  geom_segment(aes(x=122,y=36,xend=122,yend=44),color="white",size=1.5)+
  geom_segment(aes(x=122,y=36,xend=120,yend=36),color="white",size=1.5)+
  geom_segment(aes(x=122,y=44,xend=120,yend=44),color="white",size=1.5)+### goalposts
  geom_segment(aes(x=102,y=18,xend=102,yend=62),color="white",size=1.5)+
  geom_segment(aes(x=102,y=18,xend=120,yend=18),color="white",size=1.5)+
  geom_segment(aes(x=102,y=62,xend=120,yend=62),color="white",size=1.5)+
  geom_segment(aes(x=18,y=18,xend=18,yend=62),color="white",size=1.5)+
  geom_segment(aes(x=0,y=18,xend=18,yend=18),color="white",size=1.5)+  
  geom_segment(aes(x=0,y=62,xend=18,yend=62),color="white",size=1.5)+ 
  ### penalty area 
  geom_segment(aes(x=114,y=30,xend=114,yend=50),color="white",size=1.5)+
  geom_segment(aes(x=114,y=30,xend=120,yend=30),color="white",size=1.5)+
  geom_segment(aes(x=114,y=50,xend=120,yend=50),color="white",size=1.5)+
  geom_segment(aes(x=6,y=30,xend=6,yend=50),color="white",size=1.5)+
  geom_segment(aes(x=0,y=30,xend=6,yend=30),color="white",size=1.5)+  
  geom_segment(aes(x=0,y=50,xend=6,yend=50),color="white",size=1.5)+ ### gk box
  geom_circle(aes(x0 = 60, y0 = 40, r = 9.15), color = "white", fill = NA,size=1.5)+
  geom_segment(aes(x=x,y=y,xend=xend,yend=yend),size=1)+
  geom_point(aes(x=x,y=y),color="black")+
  geom_point(aes(x=xend,y=yend),color="red")+
  xlim(80,120)+
  coord_equal()+
  theme(legend.title  = element_blank(),axis.text = element_blank(), 
        axis.ticks = element_blank())

ggplot(data=passinge) +
  ylab("")+
  xlab("")+
  theme(panel.background = element_rect(fill = 'lime green'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_segment(aes(x=80,y=0,xend=120,yend=0),color="white",size=1.5)+
  geom_segment(aes(x=80,y=80,xend=120,yend=80),color="white",size=1.5)+
  geom_segment(aes(x=0,y=0,xend=0,yend=80),color="white",size=1.5)+
  geom_segment(aes(x=120,y=0,xend=120,yend=80),color="white",size=1.5)+
  geom_segment(aes(x=60,y=0,xend=60,yend=80),color="white",size=1.5)+ ### main fields lines
  geom_point(aes(x=60,y=40),col="white",size=2)+ ## centre point
  geom_point(aes(x=12,y=40),col="white",size=2)+ ## pen mark
  geom_point(aes(x=108,y=40),col="white",size=2)+ ## pen mark
  geom_segment(aes(x=-2,y=36,xend=-2,yend=44),color="white",size=1.5)+
  geom_segment(aes(x=-2,y=36,xend=0,yend=36),color="white",size=1.5)+
  geom_segment(aes(x=-2,y=44,xend=0,yend=44),color="white",size=1.5)+
  geom_segment(aes(x=122,y=36,xend=122,yend=44),color="white",size=1.5)+
  geom_segment(aes(x=122,y=36,xend=120,yend=36),color="white",size=1.5)+
  geom_segment(aes(x=122,y=44,xend=120,yend=44),color="white",size=1.5)+### goalposts
  geom_segment(aes(x=102,y=18,xend=102,yend=62),color="white",size=1.5)+
  geom_segment(aes(x=102,y=18,xend=120,yend=18),color="white",size=1.5)+
  geom_segment(aes(x=102,y=62,xend=120,yend=62),color="white",size=1.5)+
  geom_segment(aes(x=18,y=18,xend=18,yend=62),color="white",size=1.5)+
  geom_segment(aes(x=0,y=18,xend=18,yend=18),color="white",size=1.5)+  
  geom_segment(aes(x=0,y=62,xend=18,yend=62),color="white",size=1.5)+ 
  ### penalty area 
  geom_segment(aes(x=114,y=30,xend=114,yend=50),color="white",size=1.5)+
  geom_segment(aes(x=114,y=30,xend=120,yend=30),color="white",size=1.5)+
  geom_segment(aes(x=114,y=50,xend=120,yend=50),color="white",size=1.5)+
  geom_segment(aes(x=6,y=30,xend=6,yend=50),color="white",size=1.5)+
  geom_segment(aes(x=0,y=30,xend=6,yend=30),color="white",size=1.5)+  
  geom_segment(aes(x=0,y=50,xend=6,yend=50),color="white",size=1.5)+ ### gk box
  geom_circle(aes(x0 = 60, y0 = 40, r = 9.15), color = "white", fill = NA,size=1.5)+
  geom_segment(aes(x=x,y=y,xend=xend,yend=yend),size=1)+
  geom_point(aes(x=x,y=y),color="black")+
  geom_point(aes(x=xend,y=yend),color="red")+
  xlim(80,120)+
  coord_equal()+
  theme(legend.title  = element_blank(),axis.text = element_blank(), 
        axis.ticks = element_blank())

### shots by each side ###
shots<-subset(json_data,json_data$type$name=="Shot")

shotsscot<-subset(shots,shots$team$name=="Scotland")
shotsseng<-subset(shots,shots$team$name=="England")

ggplot(data=shotsscot) +
  xlab("")+
  ylab("")+
  theme(panel.background = element_rect(fill = 'lime green'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_segment(aes(y=60,x=0,yend=120,xend=0),color="white",size=1)+
  geom_segment(aes(y=60,x=80,yend=120,xend=80),color="white",size=1)+
  geom_segment(aes(y=0,x=0,yend=0,xend=80),color="white",size=1)+
  geom_segment(aes(y=120,x=0,yend=120,xend=80),color="white",size=1)+
  geom_segment(aes(y=60,x=0,yend=60,xend=80),color="white",size=1)+ ### main fields lines
  geom_point(aes(y=60,x=40),col="white",size=2)+ ## centre point
  geom_point(aes(y=12,x=40),col="white",size=2)+ ## pen mark
  geom_point(aes(y=108,x=40),col="white",size=2)+ ## pen mark
  geom_segment(aes(y=-2,x=36,yend=-2,xend=44),color="white",size=1)+
  geom_segment(aes(y=-2,x=36,yend=0,xend=36),color="white",size=1)+
  geom_segment(aes(y=-2,x=44,yend=0,xend=44),color="white",size=1)+
  geom_segment(aes(y=122,x=36,yend=122,xend=44),color="white",size=1)+
  geom_segment(aes(y=122,x=36,yend=120,xend=36),color="white",size=1)+
  geom_segment(aes(y=122,x=44,yend=120,xend=44),color="white",size=1)+### goalposts
  geom_segment(aes(y=102,x=18,yend=102,xend=62),color="white",size=1)+
  geom_segment(aes(y=102,x=18,yend=120,xend=18),color="white",size=1)+
  geom_segment(aes(y=102,x=62,yend=120,xend=62),color="white",size=1)+
  geom_segment(aes(y=18,x=18,yend=18,xend=62),color="white",size=1)+
  geom_segment(aes(y=0,x=18,yend=18,xend=18),color="white",size=1)+  
  geom_segment(aes(y=0,x=62,yend=18,xend=62),color="white",size=1)+ 
  ### penaltx area 
  geom_segment(aes(y=114,x=30,yend=114,xend=50),color="white",size=1)+
  geom_segment(aes(y=114,x=30,yend=120,xend=30),color="white",size=1)+
  geom_segment(aes(y=114,x=50,yend=120,xend=50),color="white",size=1)+
  geom_segment(aes(y=6,x=30,yend=6,xend=50),color="white",size=1)+
  geom_segment(aes(y=0,x=30,yend=6,xend=30),color="white",size=1)+  
  geom_segment(aes(y=0,x=50,yend=6,xend=50),color="white",size=1)+ ### gk box
  geom_arc(aes(y0 = 60, x0 = 40, r = 9.15,start=0,end=180),color = "white",size=1)+
  geom_point(aes(y=x,x=y,pch=as.factor(shotsscot$shot$outcome$name)),fill="navy",size=6)+
  scale_shape_manual(values = c(16, 17, 15)) +  # Adjust shape values as needed
  theme(legend.title  = element_blank(),axis.text = element_blank(), 
        axis.ticks = element_blank(),legend.text = element_text(size = 20))+
  ylim(60,125)+
  coord_equal()


ggplot(data=shotsseng) +
  xlab("")+
  ylab("")+
  theme(panel.background = element_rect(fill = 'lime green'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_segment(aes(y=60,x=0,yend=120,xend=0),color="white",size=1.5)+
  geom_segment(aes(y=60,x=80,yend=120,xend=80),color="white",size=1.5)+
  geom_segment(aes(y=0,x=0,yend=0,xend=80),color="white",size=1.5)+
  geom_segment(aes(y=120,x=0,yend=120,xend=80),color="white",size=1.5)+
  geom_segment(aes(y=60,x=0,yend=60,xend=80),color="white",size=1.5)+ ### main fields lines
  geom_point(aes(y=60,x=40),col="white",size=2)+ ## centre point
  geom_point(aes(y=12,x=40),col="white",size=2)+ ## pen mark
  geom_point(aes(y=108,x=40),col="white",size=2)+ ## pen mark
  geom_segment(aes(y=-2,x=36,yend=-2,xend=44),color="white",size=1.5)+
  geom_segment(aes(y=-2,x=36,yend=0,xend=36),color="white",size=1.5)+
  geom_segment(aes(y=-2,x=44,yend=0,xend=44),color="white",size=1.5)+
  geom_segment(aes(y=122,x=36,yend=122,xend=44),color="white",size=1.5)+
  geom_segment(aes(y=122,x=36,yend=120,xend=36),color="white",size=1.5)+
  geom_segment(aes(y=122,x=44,yend=120,xend=44),color="white",size=1.5)+### goalposts
  geom_segment(aes(y=102,x=18,yend=102,xend=62),color="white",size=1.5)+
  geom_segment(aes(y=102,x=18,yend=120,xend=18),color="white",size=1.5)+
  geom_segment(aes(y=102,x=62,yend=120,xend=62),color="white",size=1.5)+
  geom_segment(aes(y=18,x=18,yend=18,xend=62),color="white",size=1.5)+
  geom_segment(aes(y=0,x=18,yend=18,xend=18),color="white",size=1.5)+  
  geom_segment(aes(y=0,x=62,yend=18,xend=62),color="white",size=1.5)+ 
  ### penaltx area 
  geom_segment(aes(y=114,x=30,yend=114,xend=50),color="white",size=1.5)+
  geom_segment(aes(y=114,x=30,yend=120,xend=30),color="white",size=1.5)+
  geom_segment(aes(y=114,x=50,yend=120,xend=50),color="white",size=1.5)+
  geom_segment(aes(y=6,x=30,yend=6,xend=50),color="white",size=1.5)+
  geom_segment(aes(y=0,x=30,yend=6,xend=30),color="white",size=1.5)+  
  geom_segment(aes(y=0,x=50,yend=6,xend=50),color="white",size=1.5)+ ### gk box
  geom_arc(aes(y0 = 60, x0 = 40, r = 9.15,start=0,end=180),color = "white",size=1.5)+
  geom_point(aes(y=x,x=y,pch=as.factor(shotsseng$shot$outcome$name)),fill="navy",size=6)+
  #geom_image(aes(y=x,x=y,image = image), size = 0.15)+
  scale_shape_manual(values = c(16, 17, 15,18)) +  # Adjust shape values as needed
  theme(legend.title=element_blank(),axis.text = element_blank(), 
        axis.ticks = element_blank(),legend.text = element_text(size = 20))+
  ylim(60,122)+
  coord_equal()
### confirm xg  ##
scot<-subset(json_data,json_data$team$name=="Scotland")
eng<-subset(json_data,json_data$team$name=="England")

scot %>%
  dplyr::summarise(sum(scot$shot$statsbomb_xg,na.rm=TRUE))

eng %>%
  dplyr::summarise(sum(eng$shot$statsbomb_xg,na.rm=TRUE))


sum(shots$shot$statsbomb_xg)

#### xg dynamics ###
shotsscot <- shotsscot %>%
  arrange(minute) %>%
  mutate(cumulative_xg = cumsum(shot$statsbomb_xg),
         last_cumulative_xg = last(cumulative_xg))
shotsseng <- shotsseng %>%
  arrange(minute) %>%
  mutate(cumulative_xg = cumsum(shot$statsbomb_xg),
         last_cumulative_xg = last(cumulative_xg))
ggplot(shotsscot, aes(x = minute, y = cumulative_xg)) +
  geom_line(col="navy",size=1.5) +
  geom_segment(aes(x = max(minute), xend = 92, y = last_cumulative_xg, yend = last_cumulative_xg),
               color = "navy", size = 1.5)+
  geom_line(data=shotsseng, aes(x = minute, y = cumulative_xg),col="maroon",size=1.5)+
    geom_segment(data=shotsseng,aes(x = max(minute), xend = 92, y = last_cumulative_xg, yend = last_cumulative_xg),
                 color = "maroon", size = 1.5)+
  labs(x = "Minute",
       y = "Cumulative xG (Statsbomb)") +
  theme_minimal()+
  theme(legend.title=element_blank(),axis.text = element_text(size=20), 
        legend.text = element_text(size = 20),axis.title=element_text(size=20))+
  ylim(0,1)+
  xlim(0,92)
  

### timeline summary ###

number_line_data <- data.frame(start = c(0, 45, 45, 90), end = c(45, 0, 90, 45))

cards<-subset(json_data,json_data$type$name=="Foul Committed") ### 87th minute card found - 15th minute pen only in dataset but card given. 
cards2<-subset(json_data,json_data$type$name=="Foul Won")


ggplot(number_line_data, aes(x = start, xend = end, y = 0, yend = 0)) +
  geom_segment(arrow = arrow(type = "closed", length = unit(0.2, "cm"))) +
  xlim(-5, 95) + 
  ylim(-1,1)+# Adjust xlim to accommodate the range 0-90
  theme_void() +
  geom_image(aes(y=0.1,x=0,image = "https://www.scottishfa.co.uk/media/1008/scottish-football-association-logo.png?mode=max&width=90"), size = 0.05)+
  geom_image(aes(y=-0.1,x=0,image = "https://th.bing.com/th/id/R.4b2953952844cadf7da857b12af3aca9?rik=3hM0Q3KIBrIvMA&riu=http%3a%2f%2flogos-download.com%2fwp-content%2fuploads%2f2016%2f05%2fEngland_national_football_team_logo_crest.png&ehk=INYcvMf7goeZHy7pYTOLckHbaG1lp9iO9QF%2bhK9Jpc0%3d&risl=&pid=ImgRaw&r=0"), size = 0.05)+
  geom_image(aes(y=0.08,x=87,image = "https://th.bing.com/th/id/R.48ddca10e3a9fcfe2c003a046f3622c5?rik=9Th3UP3uOr8tqw&riu=http%3a%2f%2fupload.wikimedia.org%2fwikipedia%2fcommons%2fthumb%2fb%2fb1%2fYellow_card.svg%2f200px-Yellow_card.svg.png&ehk=yKXSaVD0Pichcel4kzTj8tkOdkmuf7Q352%2bPOrgYQ0Y%3d&risl=&pid=ImgRaw&r=0"), size = 0.03)+
  geom_image(aes(y=0.08,x=15,image = "https://th.bing.com/th/id/R.48ddca10e3a9fcfe2c003a046f3622c5?rik=9Th3UP3uOr8tqw&riu=http%3a%2f%2fupload.wikimedia.org%2fwikipedia%2fcommons%2fthumb%2fb%2fb1%2fYellow_card.svg%2f200px-Yellow_card.svg.png&ehk=yKXSaVD0Pichcel4kzTj8tkOdkmuf7Q352%2bPOrgYQ0Y%3d&risl=&pid=ImgRaw&r=0"), size = 0.03)+
  geom_text(aes(label = "Half Time \n45+1", x = 45, y = -0.08), color = "black")+
  geom_text(aes(label = "Full Time \n90+2", x = 90, y = -0.08), color = "black")+
  theme(
    plot.background = element_rect(fill = "#ADB9CA", color = NA),  # Set background color
    panel.background = element_rect(fill = "#ADB9CA", color = NA),  # Set panel background color
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank()  # Remove minor gridlines
  )
  
  

##### team and player data #####

teamdata<-read_excel("DataScientistTaskDataset.xlsx",sheet=1)
playerdata<-read_excel("DataScientistTaskDataset.xlsx",sheet=2)


# transpose and calculate relative values - scotland the reference side ##
ttdata<-t(teamdata)
colnames(ttdata)<-ttdata[1,]
ttdata<-ttdata[-c(1,98,99),]
ttdata<-as.data.frame(ttdata)
ttdata<-lapply(ttdata,as.numeric)
ttdata<-as.data.frame(ttdata)
ttdata$Relative<-ttdata[,1]-ttdata[,2]
rownames(ttdata)<-colnames(teamdata)[2:97]
ttdata$Variable<-rownames(ttdata)

### table of metrics ##
nice_table <- ttdata %>%
  kable("html") %>%
  kable_styling(full_width = FALSE)

# Print the nice table
print(nice_table)

##### OBV metrics ####
OBV <- ttdata[grepl("OBV", ttdata$Variable), ]

ggplot(OBV, aes(x = Variable, y = Relative, fill = Relative > 0)) +
geom_bar(stat = "identity", position = "identity", color = "black") +
  scale_fill_manual(values = c("maroon", "navy"), guide = FALSE) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  labs(x = "OBV Variables", y = "Relative OBV")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0))+
  geom_text(aes(label="Scotland Outperform",y=1,x=0,vjust=-1,hjust=1),size = 7.5)+
  geom_text(aes(label="England Outperform",y=-1,x=0),vjust=-1,hjust=0,size = 7.5)+
  geom_text(aes(label="Equal Performance",y=0.1,x=0,vjust=-1,hjust=0.75),size = 7.5)+
  coord_flip()+
  theme(axis.text = element_text(size = 20),axis.title = element_text(size = 20),
        plot.background = element_rect(fill = "grey", color = NA),  # Set background color
        panel.background = element_rect(fill = "grey", color = NA),)

### Defensive Metrics 

def <- ttdata[c(72,73,76), ]


ggplot(def, aes(x = Variable, y = Relative, fill = Relative > 0)) +
  geom_bar(stat = "identity", position = "identity", color = "black") +
  scale_fill_manual(values = c("maroon", "navy"), guide = FALSE) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  labs(x = "OBV Variables", y = "Relative OBV")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0))+
  coord_flip()


#### pulling other matches from the competition ###

process_json_file <- function(file_path) {
  # Read JSON data
  json_data <- fromJSON(file_path)
  
  # Convert location and end_location to character
  json_data$location <- as.character(json_data$location)
  json_data$pass$end_location <- as.character(json_data$pass$end_location)
  
  # Split coordinates
  coordinates_split <- strsplit(json_data$location, ",")
  coordinates_splitend <- strsplit(json_data$pass$end_location, ",")
  
  # Function to clean string
  clean_string <- function(s) {
    gsub("[^0-9.]", "", s)
  }
  
  # Clean and process coordinates
  cleaned_list <- lapply(coordinates_split, clean_string)
  cleaned_listend <- lapply(coordinates_splitend, clean_string)
  
  # Create new columns
  json_data$x <- sapply(cleaned_list, function(coord) as.numeric(coord[1]))
  json_data$y <- sapply(cleaned_list, function(coord) as.numeric(coord[2]))
  
  json_data$xend <- sapply(cleaned_listend, function(coord) as.numeric(coord[1]))
  json_data$yend <- sapply(cleaned_listend, function(coord) as.numeric(coord[2]))
  
  return(json_data)
}

# file paths
file_paths <- c("3795506.json", "3795221.json", "3795187.json","3794688.json","3788772.json","3788771.json","3788748.json","3788745.json","3788759.json")

# Process each file and store the results in a list
processed_data <- lapply(file_paths, process_json_file)

# combine the processed data into a single data frame
combined_data <- bind_rows(processed_data, .id = "file_id")

n<-c(1:9)
c<-c("ITAvENG","ENGvDEN","UKRvENG","ENGvGER","CZEvENG","CROvSCO","SCOvCZE","ENGvCRO","ENGvSCO")
d<-cbind(n,c)
d<-as.data.frame(d)
d$n<-as.numeric(d$n)
combined_data1 <-merge(combined_data,d,by.x="file_id",by.y="n")
### unnest some columns ###
data_unnested <- combined_data1 %>%
  unnest(cols = team, names_sep = "_")

data_unnested <- data_unnested %>%
  unnest(cols = shot, names_sep = "_")

shots$shot_type
shots<-subset(data_unnested,data_unnested$type$name=="Shot")
shots<-subset(shots,!shots$shot_type$name=="Penalty")

A<-shots %>%
  group_by(team_name,c) %>%
  dplyr::summarise(sum(shot_statsbomb_xg,na.rm=TRUE))

##
A$`sum(shot_statsbomb_xg, na.rm = TRUE)`
df_wide <- A %>%
  pivot_wider(names_from = team_name, values_from = `sum(shot_statsbomb_xg, na.rm = TRUE)`)


engxg <- df_wide %>%
  mutate(diff_column = England - rowSums(across(-c(c, England)),na.rm=TRUE))


scotxg <- df_wide %>%
  mutate(diff_column = Scotland - rowSums(across(-c(c, Scotland)),na.rm=TRUE))


#### 
scotxg_filtered <- scotxg %>%
  filter(!is.na(diff_column))

scotxg_filtered <- scotxg_filtered %>%
  mutate(c = factor(c, levels = c("SCOvCZE", "ENGvSCO", "CROvSCO")))

scotxg_filtered[3,10]<-(-0.06)## insert team data values 

ggplot(scotxg_filtered, aes(x =c , y = diff_column,fill=diff_column>0)) +
  geom_bar(stat = "identity", position = "identity", color = "black", na.rm = TRUE) +
  scale_fill_manual(values = c("red", "green"), guide = FALSE) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  labs(x = "Matches", y = "NP xG Difference")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0))+
  theme(axis.text = element_text(size = 20),axis.title = element_text(size = 20),panel.grid.major = element_blank())+
  ylim(-2,2)


engxg_filtered <- engxg %>%
  filter(!is.na(diff_column))

engxg_filtered <- engxg_filtered %>%
  mutate(c = factor(c, levels = c("ENGvCRO", "ENGvSCO", "CZEvENG","ENGvGER","UKRvENG","ENGvDEN","ITAvENG")))

engxg_filtered[5,10]<-(0.06) ## insert team data values

ggplot(engxg_filtered, aes(x =c , y = diff_column,fill=diff_column>0)) +
  geom_bar(stat = "identity", position = "identity", color = "black", na.rm = TRUE) +
  scale_fill_manual(values = c("red", "green"), guide = FALSE) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  labs(x = "Matches", y = "NP xG Difference")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0),panel.grid.major = element_blank())+
  theme(axis.text = element_text(size = 20),axis.title = element_text(size = 20))+
  ylim(-2,2)

#### player tables ###

col_info <- data.frame(
  Column_Name = names(playerdata),
  Column_Index = seq_along(playerdata)
)

# Print the column information
print(col_info)


easyplayer<-playerdata[,c(1,2,3,4,6,8,10,14,16,27,30,55,54,92,96,97)]


scot<-subset(easyplayer,easyplayer$Team=="Scotland")
eng<-subset(easyplayer,easyplayer$Team=="England")

### passing metrics ##
nice_table <- scot[,-2] %>%
  kable("html",escape = TRUE) %>%
  kable_styling(full_width = FALSE)
# Print the nice table
print(nice_table)


### passing metrics ##
nice_table <- eng[,-2] %>%
  kable("html",escape = TRUE) %>%
  kable_styling(full_width = FALSE)
# Print the nice table
print(nice_table)

opts <- options(knitr.kable.NA = "")


