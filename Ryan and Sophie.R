#Load Packages


library(gganimate)
library(coronavirus)
library(scales)
library(dplyr)

#Load Full Data Set

df <- refresh_coronavirus_jhu()


# Filter the data for France and Sweden confirmed cases
df2 <- df %>% filter(location == "France" | location=="Sweden",
                     data_type == "cases_new") %>%
  select(date, location,cases = value) %>%
  arrange(date)

# Replace negative values with NA (France reported some negative days)

df2$cases <- replace(df3$cases,which(df3$cases<0),NA)

#Prepare for making the graph

point <- format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)

#Make the figure

f<- df2 %>% ggplot(aes(x=date, y=cases, group=location, color=location)) +
  geom_line(size=1.2)+scale_y_continuous(labels = point)+
  scale_x_date(date_labels = "%b/%y")+
  transition_reveal(date) +
  view_follow(fixed_y = TRUE)+
  labs(title="COVID Cases in France and Sweden",caption = "Date: {frame_along}",x='Date',y='Cases',color="Country")+
  scale_color_manual(values=c("#EF4135", "#004B87"))+
  ylim(0,100000)+
  theme(
    panel.background = element_rect(fill="#000000"),
    plot.background= element_rect(fill="#000000"),
    axis.title.x = element_text(color="#FFFFFF",size=14),
    axis.title.y=element_text(color="#FFFFFF",size=14),
    axis.text = element_text(size=12),
    plot.title=element_text(face="bold",hjust = 0.5,color="white"),
    plot.caption = element_text(face="bold",color="white",size=14)
  )

#Animate the Figure

animate(f,fps=2.5,width=900,height=600)
