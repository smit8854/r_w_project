
# To-Do -------------------------------------------------------------------

# finalize the story
# finalize figures for the poster that tell the story
# then make those figures pretty :)
# run models to predict listener ratings from new f3f2 distance measures



# Load Packages -----------------------------------------------------------

library(readr)
library(tidyverse)
library(stringr)
library(dplyr)


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
    f3f2midpt = f3_midpt_med - f2_midpt_med,
    f3f2thrgrtr = f3_thrqrtr_med - f2_thrqrtr_med,
    f3f2endpt = f3_endpt_med - f2_endpt_med,
    ChildWordKey = paste(str_pad(spkr,3,pad="0"),"L",age_mos,gender,session,"Test",testnumber,word, sep="")
  ) %>% 
  select(f3f2onset, f3f2qrtr, f3f2midpt, f3f2thrgrtr, f3f2endpt, f3_onset_med, f3_qrtr_med, f3_midpt_med, f3_thrqrtr_med, f3_endpt_med, ChildWordKey)

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

#F3-F2 at minimum median F3

rw.merge.tt = rw.merge.tt %>% mutate(
  f3minmed = pmin(f3_onset_med, f3_qrtr_med, f3_midpt_med, f3_thrqrtr_med, f3_endpt_med),
)

f3values = data.frame(rw.merge.tt$f3_onset_med, rw.merge.tt$f3_qrtr_med, rw.merge.tt$f3_midpt_med, rw.merge.tt$f3_thrqrtr_med, rw.merge.tt$f3_endpt_med)

rw.merge.tt.f3 = rw.merge.tt %>% mutate(
  f3mincol = names(f3values)[apply(f3values, MARGIN = 1, FUN = which.min)]
)
rw.merge.tt.f3 = rw.merge.tt.f3 %>% mutate(
  f3mincolshort = gsub("rw.merge.tt.", "", rw.merge.tt.f3$f3mincol)
)

table(rw.merge.tt.f3$f3mincolshort)
count(rw.merge.tt.f3)


#Based on these values, we can see that the highest number of minimun f3 values occurs at the onset. 
#However, the onset still only contains the minimum f3 values for 28% of the data
#Because target w, where f3 is not expected to be low at the onset, may be confusing this, we should look at the data sorted by target

table(rw.merge.tt.f3$f3mincolshort, rw.merge.tt.f3$target)

#looking at target r vs w does not change the data much, in fact, only 25% of the minimum f3 values occur at the onset. 
#This gives support for the idea that we should be finding the f3-f2 distance where f3 is at its minimum

rw.merge.tt.f3 = rw.merge.tt.f3 %>% 
  mutate(f3f2min = case_when(f3mincolshort <= "f3_onset_med" ~ f3f2onset,
                             f3mincolshort <= "f3_qrtr_med" ~ f3f2qrtr,
                             f3mincolshort <= "f3_midpt_med" ~ f3f2midpt, 
                             f3mincolshort <= "f3_thrqrtr_med" ~ f3f2thrgrtr,
                             f3mincolshort <= "f3_endpt_med" ~ f3f2endpt))

#yay! now we can look at graphs with the f3-f2 distance taken from the point where f3 is lowest and see if they look any different

#duplicated for comparison:
rw.merge.tt %>% 
  filter(transc == c("r", "w")) %>% 
  ggplot() +
  aes(x = f3f2onset, color = transc) +
  geom_density() +
  theme_bw()

#with f3f2min
rw.merge.tt.f3 %>% 
  filter(transc == c("r", "w")) %>% 
  ggplot() +
  aes(x = f3f2min, color = transc) +
  geom_density() +
  theme_bw()

#not too much of a difference!

#duplicated for comparison
rw.merge.tt %>% 
  filter(target == "r") %>% 
  ggplot() +
  aes(x = f3f2onset, y = rating) +
  geom_point() +
  geom_smooth() +
  theme_bw()


#with f3f2min
rw.merge.tt.f3 %>% 
  filter(target == "r") %>% 
  ggplot() +
  aes(x = f3f2min, y = rating) +
  geom_point() +
  geom_smooth() +
  theme_bw()

#for only target /r/ words transcribed as r and rw
rw.merge.tt.f3 %>% 
  filter(target == "r" & transc == c("r", "rw")) %>% 
  ggplot() +
  aes(x = f3f2min, y = rating) +
  geom_point() +
  geom_smooth() +
  theme_bw()

#also not very different

#looking at slopes of individual children to see if they individually show relationship between f3f2min and rating
#target /r/ sounds by child
rw.merge.tt.f3 %>%
  filter(target == "r" ) %>%
  ggplot(aes(x=f3f2min, 
             y=rating, 
             color=ChildID
             ))+
  geom_point(alpha = .01/10)+
  geom_smooth(method="lm",se = FALSE) +
    theme(legend.position = "none")

#accurate r sounds
#target /r/ transcribed as r and rw by child
rw.merge.tt.f3 %>%
  filter(target == "r" & transc == c("r", "rw")) %>%
  ggplot(aes(x=f3f2min, 
             y=rating, 
             color=ChildID))+
  geom_point(alpha = .01/10)+
  geom_smooth(method="lm",se = FALSE) +
  theme(legend.position = "none")

#target /r/ transcribed as w and wr by child
rw.merge.tt.f3 %>%
  filter(target == "r" & transc == c("w", "wr")) %>%
  ggplot(aes(x=f3f2min, 
             y=rating, 
             color=ChildID
  ))+
  geom_point(alpha = .01/10)+
  geom_smooth(method="lm",se = FALSE) +
  theme(legend.position = "none")

#all sounds by child
rw.merge.tt.f3 %>%
  ggplot(aes(x=f3f2min, 
             y=rating, 
             color=ChildID))+
  geom_point(alpha = .01/10)+
  geom_smooth(method="lm",se = FALSE) +
  theme(legend.position = "none")

#all sounds transcribed as r and rw by child
rw.merge.tt.f3 %>%
  filter(transc == c("r", "rw")) %>%
  ggplot(aes(x=f3f2min, 
             y=rating, 
             color=ChildID))+
  geom_point(alpha = .01/10)+
  geom_smooth(method="lm",se = FALSE) +
  theme(legend.position = "none")

#all sounds transcribed as w and wr by child
rw.merge.tt.f3 %>%
  filter(transc == c("w", "wr")) %>%
  ggplot(aes(x=f3f2min, 
             y=rating, 
             color=ChildID))+
  geom_point(alpha = .01/10)+
  geom_smooth(method="lm",se = FALSE) +
  theme(legend.position = "none")


#what does this mean?
#all sounds by listener
rw.merge.tt.f3 %>%
  ggplot(aes(x=f3f2min, 
             y=rating, color = as.character(Listener)))+
  geom_point(alpha = .01/10)+
  geom_smooth(method="lm",se = FALSE) +
  theme(legend.position = "none")

rw.merge.tt.f3 %>%
  filter(transc == "w") %>%
  ggplot(aes(x=f3f2min, 
             y=rating, color = as.character(Listener)))+
  geom_point(alpha = .01/10)+
  geom_smooth(method="lm",se = FALSE) +
  theme(legend.position = "none")

#If adult listeners demonstrate a consistent positive relationship between their ratings and f3f2min, what makes the child data so messy?
#Adults are rating off of f3-f2 cue


# Models ------------------------------------------------------------------

#TBD

