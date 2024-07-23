#figure for FWI over time

climate <- read.csv("./Analysis/Climate-future-input-log.csv") %>%
  filter(Timestep > 180 & Timestep < 270) %>%
  group_by(Year) %>% 
  summarise(mean = mean(FWI))
  

ggplot(climate, aes(x = Year, y = mean)) + 
  geom_point(color = "steelblue") + 
  geom_smooth(color = "black") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  ylab("FWI")
