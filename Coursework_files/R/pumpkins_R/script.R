library(tidyverse)

pumpkins <- read.csv("pumpkins_07.csv")
head(pumpkins)

pumpkins %>%
  select(weight_lbs, variety, country, state_prov, id) %>%
  arrange(desc(weight_lbs)) %>%
  head(5)

lbs_to_kg <- function(x){
  y <- x*0.45359237
  return(y)
  }

pumpkins$weight_kg <- lbs_to_kg(pumpkins$weight_lbs)

#ERROR:
#pumpkins$weight_class <- if (pumpkins$weight_lbs <480) {
#  print("light")
#} else if (pumkins$weight_lbs < 950) {
#  print("medium") 
#} else {
#  print("heavy")
#}

pumpkins$weight_class <- case_when(pumpkins$weight_lbs < 480 ~ 'light',
  pumpkins$weight_lbs < 950 ~ 'medium',
  TRUE ~ 'heavy')

ggplot(aes(x = est_weight, y = weight_lbs, colour = weight_class), data = pumpkins) +
  geom_point(size = 0.5) +
  geom_abline() +
  scale_color_manual(values = c("light" = "darkblue", "medium" = "pink", "heavy" = "purple")) +
  labs( 
        x = "Actual Weight (lbs)",
        y = "Estimated Weight (lbs)",
        colour = "Weight Class"
  ) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "grey", fill = NA),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour = "grey")
  )

#pumpkins %>%
#  filter(country == c('Canada', 'Italy', 'Germany'))
  
filtered_pumpkins <- (pumpkins %>%
                        filter(country %in% c("Germany", "Italy", "Canada")))

filtered_pumpkins <- data.frame(filtered_pumpkins)
write.csv(filtered_pumpkins, "filtered_pumpkins.csv")

summary <- filtered_pumpkins %>%
  group_by(country) %>%
  summarise(
    mean_weight = mean(weight_lbs),
    .groups = "drop"
  )

data.frame(summary)

n <- summary %>%
        filter(mean_weight == max(mean_weight)) %>%
        select(country)

#print("Country with the highest mean pumpkin weight:", n)

print(paste("Country with the highest mean pumpkin weight:", n))


summary_2 <- filtered_pumpkins %>%
  select(country, variety, weight_lbs) %>%
  group_by(variety, country) %>%
  summarise(
    mean_weight = mean(weight_lbs),
    .groups = "drop"
  )

data.frame(summary_2)

summary_2 %>%
  arrange(mean_weight)

n_2 <- summary_2 %>%
  filter(mean_weight == min(mean_weight)) %>%
  select(variety)

n_3 <- summary_2 %>%
  filter(mean_weight == min(mean_weight)) %>%
  select(country)

print(paste("Variety and country with the lowest mean pumpkin weight:", n_2,",", n_3))

ggplot(aes(x = country, y = weight_lbs), data = filtered_pumpkins) +
  geom_boxplot(fill = "lightblue3") +
  labs(x = "Country", y = "Weight (lbs)") +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 10, colour = "black"),
    axis.text.y = element_text(colour = "black", size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(colour = "grey90"),
  )

#ggplot(aes(x = country, y = weight_lbs), data = filtered_pumpkins) +
# geom_facet(fill = "lightblue3")

ggplot(filtered_pumpkins, aes(x = country, y = weight_lbs, fill=country)) +
  geom_boxplot() +
  facet_wrap(~variety) +
  scale_fill_manual(values=c("lightblue2", "pink3", "purple3"))+
  scale_alpha_manual() +
  theme(axis.text.x = element_blank()) +
  labs(x = "Country", y = "Weight (lbs)")








