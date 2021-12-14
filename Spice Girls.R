studio_album_tracks <- read.csv("studio_album_tracks.csv",stringsAsFactors = TRUE)

data <- subset(studio_album_tracks,select=c("danceability","track_name","key_name"))
data <- data %>% arrange(key_name,danceability)

data$key_name <- as.character(data$key_name)
data$key_name <- as.character(data$key_name)
empty_bar <- 10
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$key_name), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$key_name <- rep(levels(data$key_name), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(key_name)
data$id <- seq(1, nrow(data))

label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar    
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

p <- data %>% 
  ggplot(aes(x=as.factor(id), y=danceability*10, fill=key_name)) +      
  geom_bar(stat="identity", alpha=0.5) +
  ylim(-2,10) +
  labs(title="Danceability of Spice Girls Songs by Key")+
  labs(fill="Song Key")+
  theme_minimal() +
  theme(
    plot.title=element_text(hjust=0.5,vjust = -30),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank() 
  ) +
  coord_polar() + 
  scale_fill_viridis(discrete=TRUE)+
  geom_text(data=label_data, aes(x=id, y=danceability, label=track_name, hjust=hjust), 
            color="black", fontface="bold",alpha=0.6, size=3, angle= label_data$angle, inherit.aes = FALSE) 
p

