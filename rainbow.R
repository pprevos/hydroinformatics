# Rainbow chart demo code

# Minimal libraries
library(dplyr)
library(scales)
library(ggforce)

# Generate data
cases <- tibble(Topic = paste0(sample(LETTERS[1:26], 5), 
                               rep(paste0(sample(letters[1:26], 4), collapse = ""), 5)),
                Volume = sample(10:100, 5))

# Calculate data geometries
cases_dial <- cases %>% 
  mutate(fraction = Volume / sum(Volume),
         ymax = cumsum(fraction),
         ymin = lag(ymax, default = 0),         
         text_angle = 180 - ((ymin + ymax) / 2) * 180,
         text_angle = ifelse(text_angle > 90, text_angle - 180, text_angle)) %>% 
  mutate_at(vars(starts_with("y")), rescale, to = pi * c(-.5, .5), from = 0:1)

# Visualise
ggplot(cases_dial) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = .5, r = 1, start = ymin, end = ymax, fill = Topic), col = NA) + 
  #scale_fill_manual(name = NULL, values = palet) + 
  geom_text(aes(x = .75 * sin((ymax + ymin) / 2), 
                y = .75 * cos((ymax + ymin) / 2), 
                angle = text_angle,
                label = Volume), size = 5, col = "white") +
  coord_fixed() +
  theme_void() +
  theme_void(base_size = 10) +
  annotate("text", x = 0, y = .1, 
           label = prettyNum(sum(cases$Volume), big.mark = ","), 
           fontface = 2, size = 5, col = darkblue)

ggsave("rainbow.png", width = 3, height = 2)
