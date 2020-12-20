library(tidyverse)
library(janitor)
library(parsedate)
library(ggforce)
library(jkmisc)
library(glue)
library(ggtext)
library(here)
 
covid_data <- here("data", "meqrjo57.csv") %>% 
  read_csv() %>% 
  clean_names() %>% 
  mutate(date = parse_date(category)) %>% 
  rename(seven_day_average = x7_day_average)

timeline <- tibble(date = c("October 6, 2020", "October 28, 2020", "November 2, 2020", "November 9, 2020", "November 16, 2020", "November 20, 2020", "December 7, 2020", "December 15, 2020"),
                   text = c("We are flattening the curve.", 
                            "We see the curve going down", 
                            "We’re seeing the numbers starting to go down, not to the level that any of us would like to see, but they are maintaining at a plateau.",
                            "As much as we're seeing the numbers go up, we still have the lowest numbers for any large jurisdiction",
                            "I just want to talk about this announcement. This is a massive, massive announcement. I don't know if people realize how big of an announcement this is",
                            "The situation is extremely serious and further action is required to avoid the worst case scenario.",
                            "Some of the individuals who do some of the data analysis have said they are seeing indications that the lockdown has had an effect",
                            "Because of the guidelines we put in, Dr. Williams has put in place and his team, we're seeing a plateau per se"),
                   speaker = c("Premier Doug Ford", "Premier Doug Ford", "Health Minister Christine Elliott", "Premier Doug Ford", "Premier Doug Ford", "Premier Doug Ford", "Dr. David Williams", "Premier Doug Ford")
                  ) %>% 
  mutate(date = parse_date(date))

labels <- inner_join(covid_data, timeline)

on_plot <- ggplot(covid_data, aes(x = date, y = seven_day_average)) +
  geom_path(color = "red", size = 1) +
  geom_point(data = labels, fill = "red", color = "white", size = 2, shape = 21) +
  geom_mark_circle(data = labels, aes(description = glue('"{text}"'), label = glue("{date} {speaker}:"), group = date), expand = unit(2, "mm"), label.family = c("Oswald", "Poppins"), label.buffer = unit(15, "mm"), con.size = 0.2) +
  geom_text(data = slice(labels, 1), aes(x = parse_date("2020-01-31"), y = 1800, label = "Ontario's second wave of COVID-19, in quotes and charts."), hjust = 0, family = "Oswald", size = 14, vjust = 1) +
  geom_text(data = slice(labels, 1), aes(x = parse_date("2020-01-31"), y = 1720, label = "The red line is the average daily number of new cases over the previous seven days"), hjust = 0, family = "Poppins", size = 6, vjust = 1) +
  labs(x = NULL,
       y = NULL, 
       caption = "**Data**: Ontario Ministry of Health | **Graphic:** @jakekaupp | **Original**: Mike Crawley") +
  scale_x_datetime(limits = parse_date(c("2020-01-31", "2021-03-01")), date_breaks = "1 month", date_labels = "%B") +
  theme_jk(subtitle_family = "Poppins",
           markdown = TRUE,
           grid = "Y") +
  theme(axis.text.x = element_markdown(color = "#cccccc", size = 14),
        axis.text.y = element_markdown(color = "#cccccc", size = 14))

ggsave(here("on_covid.png"), on_plot, dev = ragg::agg_png(), width = 26, height = 16)
