setwd("/Users/schroejh/Documents/git/statistical-savvy/")

library(tidyverse)
library(ggpmisc)

colnames <- c("Response","Via","Screen name","Registered participant","Created At") 

results <- read_csv("/Users/schroejh/Downloads/placeholder_results.csv") %>%
  filter(!is.na(placeholder)) %>%
  separate(col = placeholder, into = colnames, sep = ",") 

results <- data.frame(t(apply(results, 1, zoo::na.locf)))

results <- results %>% select(Response, Screen.name) %>%
  mutate(question = str_extract(Response, "[^_]+\\?|[^_]+\\...")) %>%
  fill(question, .direction = "down") %>%
  filter(Response != "Response",
         Response != question)
  
# create some synthetic observations
# code up the analysis of these things

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


  