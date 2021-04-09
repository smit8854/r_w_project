# Load packages
library(dplyr)
library(ggplot2)
library(forcats)
library(ggridges)

# read in dataframe
rw_df <- read.csv(file = "./Project_1/r-w_VAS_all136.csv")
rw_df <- as_tibble(rw_df)


sinlge_child <- rw_df %>% 
  filter(subject == "001L" & word == "rain")

ggplot(sinlge_child) +
  aes(x = XResponse) +
  geom_dotplot()

sex <- rw_df %>% 
  filter(word == "rain") %>% 
  group_by(sex, subject) %>% 
  summarise(avg_rating = mean(XResponse))

ggplot(sex) +
  aes(x = avg_rating, fill = sex) +
  geom_density(alpha = 0.5)

r_words <- rw_df %>% 
  filter(target == "r") %>% 
  group_by(sex, subject) %>% 
  summarise(avg_rating = mean(XResponse))

ggplot(sex) +
  aes(x = avg_rating, fill = sex) +
  geom_density(alpha = 0.5) +
  facet_wrap(~sex)

# density plots using ggridge
density_facet <- rw_df %>% 
  filter(word%in%c("red","rock","ring") & subject %in% c("031L", "053L")) %>% 
  ggplot() +
  aes(x = XResponse, y = word, fill = stat(x)) +
  geom_density_ridges_gradient(alpha = 0.8, scale = 0.95, quantile_lines = TRUE) +
  scale_x_continuous(breaks = c(0, 250, 500, 750, 1000, 1250)) +
  scale_fill_viridis_c(name = "/r/ - /w/", option = "magma") +
  facet_wrap(~subject) +
  labs(x = "Visual Analog Scale Rating", y = "Target Word") +
  theme_classic() + theme(legend.position = "top")

density_facet

ggsave(density_facet, filename = "./Project_1/density_facet.png",
       height = 6, width = 6, dpi = 300)

# version 2.0
labels <- c("031L" = "Child A", 
            "053L" = "Child B")

density_facet2 <- rw_df %>% 
  filter(word%in%c("red","rock","ring") & subject %in% c("031L", "053L")) %>% 
  ggplot() +
  aes(x = XResponse, y = word, fill = stat(x)) +
  geom_density_ridges_gradient(alpha = 0.8, scale = 0.95, quantile_lines = TRUE) +
  scale_x_continuous(breaks = c(0, 250, 500, 750, 1000, 1250)) +
  scale_fill_viridis_c(name = "/r/ - /w/", option = "magma") +
  facet_wrap(~subject, labeller = as_labeller(labels)) +
  labs(x = "Visual Analog Scale Rating", y = "Target Word", title = "Distribution of Ratings") +
  theme_classic() + theme(legend.position = "bottom")
density_facet2

ggsave(density_facet2, filename = "./Project_1/density_facet2.png",
       height = 6, width = 6, dpi = 300)

# with individual points
density_facet2_indiv <- rw_df %>% 
  filter(word%in%c("red","rock","ring") & subject %in% c("031L", "053L")) %>% 
  ggplot() +
  aes(x = rating, y = word, fill = stat(x)) +
  geom_density_ridges_gradient(alpha = 0.8, scale = 0.85, quantile_lines = TRUE, 
                               jittered_points = TRUE, 
                               position = position_points_jitter(height = 0.01, yoffset = -0.08, seed = 666),
                               point_size = 2, point_alpha = 0.5) +
  scale_x_continuous(limits = c(-0.25, 1.25), breaks = c(0, 0.25, 0.5, 0.75, 1.0)) +
  scale_fill_viridis_c(name = "/r/ - /w/", option = "magma", labels = c(0, 0.25, 0.5, 0.75, 1.0)) +
  facet_wrap(~subject, labeller = as_labeller(labels)) +
  labs(x = "Visual Analog Scale", y = "Target Word", title = "Listener Ratings for Target Words", caption = "0 = /r/, 1 = /w/") +
  theme_classic() + theme(legend.position = "bottom")

density_facet2_indiv

ggsave(density_facet2_indiv, filename = "./Project_1/density_facet2_indiv.png",
       height = 6, width = 6, dpi = 300)

# un flip 
raincloud_avg_rw_unflip <- rw_df %>% 
  filter(subject %in% c("031L", "053L")) %>%
  group_by(subject, target, Subject) %>% 
  summarise(avg_rating = mean(rating)) %>%
  ggplot() +
  aes(x = avg_rating, y = subject, fill = stat(x)) +
  geom_density_ridges_gradient(alpha = 0.8, scale = 0.85, quantile_lines = TRUE, rel_min_height = 0.01,
                               jittered_points = TRUE, 
                               position = position_points_jitter(height = 0.01, yoffset = -0.08, seed = 666),
                               point_size = 2, point_alpha = 0.5) +
  facet_wrap(~target) +
  scale_y_discrete(labels = labels) +
  scale_fill_viridis_c(name = "/r/ - /w/", option = "magma") +
  labs(x = "Mean Listener Rating", y = "Child Speaker", title = "Distribution of Ratings") +
  theme_classic() + theme(legend.position = "bottom")
raincloud_avg_rw_unflip

ggsave(raincloud_avg_rw_unflip, filename = "./Project_1/raincloud_avg_rw_unflip.png",
       height = 4, width = 4, dpi = 300)

raincloud_rw_v2 <- rw_df %>% 
  filter(subject %in% c("031L", "053L")) %>%
  ggplot() +
  aes(x = rating, y = subject, fill = stat(x)) +
  geom_density_ridges_gradient(alpha = 0.8, scale = 0.85, quantile_lines = TRUE,
                               jittered_points = TRUE, 
                               position = position_points_jitter(height = 0.01, yoffset = -0.08, seed = 666),
                               point_size = 2, point_alpha = 0.5) +
  facet_wrap(~target) +
  scale_x_continuous(limits = c(-0.15, 1.15), breaks = c(0, 0.25, 0.5, 0.75, 1.0)) +
  scale_y_discrete(labels = labels) +
  scale_fill_viridis_c(name = "/r/ - /w/", option = "magma") +
  labs(x = "Visual Analog Scale", y = "Child Speaker", title = "Listener Ratings for All Words", caption = "0 = /r/, 1 = /w/") +
  theme_classic() + theme(legend.position = "bottom")
raincloud_rw_v2

ggsave(raincloud_rw_v2, filename = "./Project_1/raincloud_rw_v2.png",
       height = 5, width = 5, dpi = 300)

# facet by child and put target word on the y axis
rain_child <- rw_df %>% 
  filter(subject %in% c("031L", "053L")) %>%
  ggplot() +
  aes(x = rating, y = target, fill = stat(x)) +
  geom_density_ridges_gradient(alpha = 0.8, scale = 0.85, quantile_lines = TRUE,
                               jittered_points = TRUE, 
                               position = position_points_jitter(height = 0.01, yoffset = -0.08, seed = 666),
                               point_size = 2, point_alpha = 0.5) +
  facet_wrap(~subject, labeller =  as_labeller(labels)) +
  scale_x_continuous(limits = c(-0.15, 1.15), breaks = c(0, 0.25, 0.5, 0.75, 1.0)) +
  scale_fill_viridis_c(name = "/r/ - /w/", option = "magma", labels = c(0, 0.25, 0.5, 0.75, 1.0)) +
  labs(x = "Visual Analog Scale", y = "Target Phoneme", title = "Listener Ratings for All Words", caption = "0 = /r/, 1 = /w/") +
  theme_classic() + theme(legend.position = "bottom")
rain_child

ggsave(rain_child, filename = "./Project_1/rain_child.png",
       height = 5, width = 5, dpi = 300)

# density plot of one child for 3 different words
density_quantiles_1subject <- rw_df %>% 
  filter(word%in%c("red","rock","ring") & subject %in% c("053L")) %>% 
  ggplot() +
  aes(x = XResponse, y = word, fill = stat(x)) +
  geom_density_ridges_gradient(alpha = 0.8, scale = 0.95, quantile_lines = TRUE, rel_min_height = 0.01) +
  scale_x_continuous(breaks = c(0, 250, 500, 750, 1000, 1250)) +
  scale_fill_viridis_c(name = "/r/ - /w/", option = "magma") +
  labs(x = "Visual Analog Scale Rating", y = "Target Word", title = "Child 053L") +
  theme_classic() + theme(legend.position = "top")
density_quantiles_1subject
# 
# ggsave(density_quantiles_1subject, filename = "./Project_1/density_quantiles_1subject.png",
#        height = 4, width = 4, dpi = 300)


# collapse across phonemes to compare r vs w for these talkers
density_avg_rw_2subject <- rw_df %>% 
  filter(subject %in% c("031L", "053L")) %>%
  group_by(subject, target, Subject) %>% 
  summarise(avg_rating = mean(rating)) %>%
  ggplot() +
  aes(x = avg_rating, y = subject, fill = stat(x)) +
  geom_density_ridges_gradient(alpha = 0.8, scale = 0.95, quantile_lines = TRUE, rel_min_height = 0.01) +
  facet_wrap(~target) +
  scale_fill_viridis_c(name = "/r/ - /w/", option = "magma") +
  labs(x = "Mean Listener Rating", y = "Child Speaker") +
  theme_classic() + theme(legend.position = "top")
density_avg_rw_2subject

# ggsave(density_avg_rw_2subject, filename = "./Project_1/density_avg_rw_2subject.png",
#        height = 4, width = 4, dpi = 300)

# flip x-y axis and add indiv points
raincloud_avg_rw_2sub <- rw_df %>% 
  filter(subject %in% c("031L", "053L")) %>%
  group_by(subject, target, Subject) %>% 
  summarise(avg_rating = mean(rating)) %>%
  ggplot() +
  aes(x = avg_rating, y = subject, fill = stat(x)) +
  geom_density_ridges_gradient(alpha = 0.8, scale = 0.85, quantile_lines = TRUE, rel_min_height = 0.01,
                               jittered_points = TRUE, 
                               position = position_points_jitter(height = 0.01, yoffset = -0.08, seed = 666),
                               point_size = 1.5, point_alpha = 0.5) +
  coord_flip() +
  facet_wrap(~target) +
  scale_fill_viridis_c(name = "/r/ - /w/", option = "magma") +
  labs(x = "Mean Listener Rating", y = "Child Speaker", caption = "0 = /r/, 1 = /w/") +
  theme_classic() + theme(legend.position = "top")
raincloud_avg_rw_2sub

# ggsave(raincloud_avg_rw_2sub, filename = "./Project_1/raincloud_avg_rw_2sub.png",
#        height = 4, width = 4, dpi = 300)



