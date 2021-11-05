
# To-Do -------------------------------------------------------------------

# finalize the story
# finalize figures for the poster that tell the story
# then make those figures pretty :)
# run models to predict listener ratings from new f3f2 distance measures



# Load Packages -----------------------------------------------------------

library(readr)
library(tidyverse)


# Prep Workspace ----------------------------------------------------------

dir_data <- "C:\\Users\\Michael\\Documents\\GitHub\\r_w_project\\data"
setwd(dir_data)

vas <- read.csv(file = "VAS.csv")

acoustic <- read.csv(file = "acoustic.csv")

triple <- read.csv(file = "RW_triple_tracker.csv")

names(vas)[1] <- "ExperimentName"
names(vas)[2] <- "Version"

names(vas)[which(names(vas)=="Subject")] <- "Listener"


# Merge Files -------------------------------------------------------------

triple_formerge <- triple %>% 
  mutate(
    f3f2onset = f3_onset_med - f2_onset_med,
    f3f2qrtr = f3_qrtr_med - f2_qrtr_med,
    f3min = pmin(f3_onset_med, f3_qrtr_med, f3_midpt_med, f3_thrqrtr_med, f3_endpt_med),
    ChildWordKey = paste(str_pad(spkr,3,pad="0"),"L",age_mos,gender,session,"Test",testnumber,word, sep="")
  ) %>% 
  select(f3f2onset, f3f2qrtr,f3min, ChildWordKey)

rw.merge.part1 <- vas %>% 
  filter(include == "yes") %>% 
  merge(acoustic, by = c("ChildWordKey", "word"), all = FALSE)

head(rw.merge.part1)
dim(rw.merge.part1)

rw.merge.tt <- rw.merge.part1 %>% 
  filter(include == "yes") %>% 
  merge(triple_formerge, by = "ChildWordKey", all = FALSE) %>% 
  select(!c("IPA", "Letter", "Front.back", "Height"))

head(rw.merge.tt)
dim(rw.merge.tt)

# why do the num of observations not make sense? i.e. we lose rows from vas -> acoustic -> triple_tracker
# ... TBD

rw.merge.tt %>% 
  select(transc, tran, target) %>% 
  head(n = 20)

# Plots ! -----------------------------------------------------------------

names(rw.merge.tt)

ggplot(rw.merge.tt) +
  aes(x = f3f2onset, color = transc) +
  geom_density() +
  theme_bw()

# same plot but subet just r and w transcriptions
# keep
rw.merge.tt %>% 
  filter(transc == c("r", "w")) %>% 
  ggplot() +
  aes(x = f3f2onset, color = transc) +
  geom_density() +
  theme_bw()


# keep this and make pretty
ggplot(rw.merge.tt) +
  aes(x = rating, color = transc) +
  geom_density() +
  theme_bw()


rw.merge.tt %>% 
  filter(transc == c("rw", "r") & target == "r") %>% 
  ggplot() +
  aes(x = f3f2onset, color = transc) +
  geom_density() +
  theme_bw()

rw.merge.tt %>% 
  filter(transc == c("rw", "r") & target == "r") %>% 
  ggplot() +
  aes(x = rating, color = transc) +
  geom_density() +
  theme_bw()

#try and plot rating, f3f2 and target (fix later)
rw.merge.tt %>% 
  filter(transc == c("rw", "r") & target == "r") %>% 
  ggplot() +
  aes(y = f3f2onset, x = rating, fill = rating) +
  ggridges::geom_density_ridges_gradient() +
  theme_bw()
  
rw.merge.tt %>% 
  filter(target == "r" & transc == "r") %>% 
  ggplot() +
  aes(x = f3f2onset, y = rating) +
  geom_point() +
  geom_smooth() +
  theme_bw()

rw.merge.tt %>% 
  filter(target == "r" & transc == "rw") %>% 
  ggplot() +
  aes(x = f3f2qrtr, y = rating) +
  geom_point() +
  geom_smooth() +
  theme_bw()

# now with min F3 value
rw.merge.tt %>% 
  filter(target == "r" & transc == c("rw", "r")) %>% 
  ggplot() +
  aes(x = f3min, y = rating) +
  geom_point() +
  geom_smooth() +
  theme_bw()

rw.merge.tt %>% 
  filter(target == "r" & transc == c("rw", "r")) %>% 
  ggplot() +
  aes(x = f3min, color = transc) +
  geom_density() +
  theme_bw()

rw.merge.tt %>% 
  filter(target == "r" & transc == c("rw", "r")) %>% 
  ggplot() +
  aes(x = f3min, y = rating) +
  geom_hex() +
  scale_fill_continuous(type = "viridis") +
  labs(title = "rw & r transcriptions") +
  # geom_point() +
  # geom_smooth() +
  theme_bw()

rw.merge.tt %>% 
  filter(target == "r" & transc == c("rw", "r")) %>% 
  ggplot() +
  aes(x = f3f2onset, y = rating) +
  geom_hex() +
  scale_fill_continuous(type = "viridis") +
  labs(title = "rw & r transcriptions") +
  # geom_point() +
  # geom_smooth() +
  theme_bw()

rw.merge.tt %>% 
  filter(target == "r" & transc == c("w")) %>% 
  ggplot() +
  aes(x = f3f2onset, y = rating) +
  geom_point() +
  geom_smooth() +
  theme_bw()

rw.merge.tt %>% 
  filter(target == "r" & transc == c("w")) %>% 
  ggplot() +
  aes(x = f3f2onset, y = rating) +
  geom_point() +
  geom_smooth() +
  theme_bw()

rw.merge.tt %>% 
  filter(target == "r" & transc == c("rw")) %>% 
  ggplot() +
  aes(x = f3f2onset, y = rating) +
  geom_hex() +
  scale_fill_continuous(type = "viridis") +
  theme_bw()

rw.merge.tt %>% 
    filter(target == "r" & transc == c("w")) %>% 
    ggplot() +
    aes(x = f3f2onset, y = rating) +
    geom_point() +
    # scale_fill_continuous(type = "viridis")
  theme_bw()


# Models ------------------------------------------------------------------

#TBD

