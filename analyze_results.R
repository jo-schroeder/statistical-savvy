setwd("/Users/schroejh/Documents/git/statistical-savvy/")

library(tidyverse)
library(ggpmisc)

coffee <- read.csv("/Users/schroejh/Downloads/how-many-cups-of-a-caffeinated-results.csv")
emails <- read.csv("/Users/schroejh/Downloads/how-many-emails-have-you-sent-today-results.csv")

r1 <- coffee %>% select(Response, Screen.name) %>% 
  mutate(type = "coffee")
r2 <- emails %>% select(Response, Screen.name) %>% 
  mutate(type = "emails")

responses <- r1 %>% left_join(r2, by = "Screen.name")

img0 <- responses %>% 
  ggplot(aes(x = Response.x, y = Response.y)) +
  geom_point(alpha = 0) +
  xlab("Number of cups of caffiene") + 
  ylab("Number of emails") +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("Is there a relationship between the number of cups of \ncaffiene and number of emails sent?") 
png("img0.png")
print(img0)
dev.off() 

img1 <- responses %>% 
  ggplot(aes(x = Response.x, y = Response.y)) +
      geom_point() +
      xlab("Number of cups of caffiene") + 
      ylab("Number of emails") +
      theme_minimal() +
      theme(legend.position = "none") +
      ggtitle("Is there a relationship between the number of cups of \ncaffiene and number of emails sent?") 
  png("img1.png")
  print(img1)
  dev.off() 


img2 <- responses %>% 
  ggplot(aes(x = Response.x, y = Response.y)) +
  geom_point() +
  xlab("Number of cups of caffiene") + 
  ylab("Number of emails") +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("Is there a relationship between the number of cups of \ncaffiene and number of emails sent?") + 
  stat_poly_line(color = "red", se = FALSE) 

png("img2.png")
print(img2)
dev.off() 


img3 <- responses %>% 
  ggplot(aes(x = Response.x, y = Response.y)) +
  geom_point() +
  xlab("Number of cups of caffiene") + 
  ylab("Number of emails") +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("Is there a relationship between the number of cups of \ncaffiene and number of emails sent?") + 
  stat_poly_line(color = "red", se = FALSE) +
  stat_poly_eq(aes(label =  paste(after_stat(eq.label), "*\" with \"*", 
                                  after_stat(rr.label), "*\", \"*", 
                                  after_stat(p.value.label), "*\"\"",
                                  sep = "")), size = 5)
png("img3.png")
print(img3)
dev.off() 


  