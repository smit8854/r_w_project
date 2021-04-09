vas=read.csv(file=file.choose())
# vas -> one record per rater per sound

acous=read.csv(file=file.choose())
# acous -> one record per child per sound

names(vas)[1]="ExperimentName"
names(acous)[1]="sound"

names(vas)
names(acous)

head(acous)
head(vas)

#standardize f3f2 per child
acous2 <- acous %>% 
  group_by(ChildID) %>% 
  mutate(f3f2norm = scale(f3f2))




#merged file

va=merge(vas,acous2,by=c("ChildWordKey","word"), all=TRUE)
dim(va)
sum(is.na(acous$ChildWordKey))
#whats goin on with the dims?
## there are missing child IDs???

library(tidyverse)
ggplot(data=va, aes(x=rating, color=transc))+geom_density()

ggplot(data=(va %>% filter(transc=="r")), aes(x=rating, color=word))+geom_density()
ggplot(data=(va %>% filter(transc=="rw")), aes(x=rating, color=word))+geom_density()
ggplot(data=(va %>% filter(transc=="wr")), aes(x=rating, color=word))+geom_density()
ggplot(data=(va %>% filter(transc=="w")), aes(x=rating, color=word))+geom_density()

va %>% 
  filter(!is.na(transc)) %>% 
  ggplot() +
  aes(x = rating, color = word) +
  geom_density() +
  facet_wrap(~transc)

#table of target-transc
x=unique(va %>% select(ChildWordKey, transc, target))
table(x$transc,x$target)

# it would be nice to have vowel variable when looking at transc-vas for rw

# when "error", look at vowel pattern by VAS rating. 

# relationship between f3, f2, and listener rating by transc
library(ggpubr)
p1=ggplot(data=(va %>% filter(transc=="w")), aes(y=f3f2, x=rating)) + 
  labs(title="w") + 
  geom_density_2d() + 
  scale_y_continuous(limits=c(500,3000))

p2=ggplot(data=(va %>% filter(transc=="r")), aes(y=f3f2, x=rating)) + 
  labs(title="r") + 
  geom_density_2d() + 
  scale_y_continuous(limits=c(500,3000))

p3=ggplot(data=(va %>% filter(transc=="rw")), aes(y=f3f2, x=rating)) + 
  labs(title="rw") + 
  geom_density_2d() + 
  scale_y_continuous(limits=c(500,3000))

p4=ggplot(data=(va %>% filter(transc=="wr")), aes(y=f3f2, x=rating)) + 
  labs(title="wr") + 
  geom_density_2d() + 
  scale_y_continuous(limits=c(500,3000))
ggarrange(p1,p2, p3, p4)

# plot contours of ratings that are strictly r and w to look at f3f2 diff
va %>% 
  filter(transc == "r" & rating < 0.125 | transc == "w" & rating > 0.875) %>% 
  ggplot() +
  aes(x = rating, y = f3f2) +
  geom_density_2d() +
  scale_y_continuous(limits=c(500,3000)) +
  facet_wrap(~transc, scales = "free")

va %>% 
  filter(transc == "r" & rating < 0.125 | transc == "w" & rating > 0.875) %>% 
  ggplot() +
  aes(x = f3f2, color = transc) +
  geom_density() +
  scale_x_continuous(limits=c(500,3000))


#f3
p1=ggplot(data=(va %>% filter(transc=="w")), aes(y=f3, x=rating)) + 
  labs(title="w") + 
  geom_density_2d() + 
  scale_y_continuous(limits=c(0,5000))
p2=ggplot(data=(va %>% filter(transc=="r")), aes(y=f3, x=rating)) + 
  labs(title="r") + 
  geom_density_2d() + 
  scale_y_continuous(limits=c(0,5000))
p3=ggplot(data=(va %>% filter(transc=="rw")), aes(y=f3, x=rating)) + 
  labs(title="rw") + 
  geom_density_2d() + 
  scale_y_continuous(limits=c(0,5000))
p4=ggplot(data=(va %>% filter(transc=="wr")), aes(y=f3, x=rating)) + 
  labs(title="wr") + 
  geom_density_2d() + 
  scale_y_continuous(limits=c(0,5000))
ggarrange(p1, p2, p3, p4)

ggplot(data=va, aes(x=f3, color=transc))+geom_density()
ggplot(data=va, aes(x=f2, color=transc))+geom_density()
ggplot(data=va, aes(x=f1, color=transc))+geom_density()
ggplot(data=va, aes(x=f3f2, color=transc))+geom_density()


ggplot(data=va, aes(y=f3f2, x=rating)) + 
  geom_density_2d() 
ggplot(data=va, aes(y=f3, x=rating)) + 
  geom_density_2d() 
ggplot(data=va, aes(y=f2, x=rating)) + 
  geom_density_2d() 
ggplot(data=va, aes(y=f1, x=rating)) + 
  geom_density_2d() 

#f2
p1=ggplot(data=(va %>% filter(transc=="w")), aes(y=f2, x=rating)) + 
  labs(title="w") + 
  geom_density_2d() + 
  scale_y_continuous(limits=c(1000,3000))
p2=ggplot(data=(va %>% filter(transc=="r")), aes(y=f2, x=rating)) + 
  labs(title="r") + 
  geom_density_2d() + 
  scale_y_continuous(limits=c(1000,3000))
p3=ggplot(data=(va %>% filter(transc=="rw")), aes(y=f2, x=rating)) + 
  labs(title="rw") + 
  geom_density_2d() + 
  scale_y_continuous(limits=c(1000,3000))
p4=ggplot(data=(va %>% filter(transc=="wr")), aes(y=f2, x=rating)) + 
  labs(title="wr") + 
  geom_density_2d() + 
  scale_y_continuous(limits=c(1000,3000))
ggarrange(p1, p2, p3, p4)


# Vowels -------------------------------------------------------------

# load vowel info
vowels <- read.csv("target_words_vowel_type.csv")

# merge vowel df with larger df
va_vowel <- merge(va,vowels, by="word", all=TRUE)
names(va_vowel)

# front back vowels
va_vowel %>% 
  filter(transc == "w" | transc == "r") %>% 
  ggplot() +
  aes(x = rating, color = Front.back) +
  geom_density() +
  facet_wrap(~target)

va_vowel %>% 
  filter(!is.na(transc)) %>% 
  ggplot() +
  aes(x = rating, color = Front.back) +
  geom_density() +
  facet_wrap(~transc)

# height
va_vowel %>% 
  filter(transc == "w" | transc == "r") %>% 
  ggplot() +
  aes(x = rating, color = Height) +
  geom_density() +
  facet_wrap(~target)

va_vowel %>% 
  filter(!is.na(transc)) %>% 
  ggplot() +
  aes(x = rating, color = Height) +
  geom_density() +
  facet_wrap(~transc)

# by vowel
va_vowel %>% 
  filter(transc == "w" | transc == "r") %>% 
  ggplot() +
  aes(x = rating, color = Letter) +
  geom_density() +
  facet_wrap(~target)

va_vowel %>% 
  filter(!is.na(transc)) %>% 
  ggplot() +
  aes(x = rating, color = Letter) +
  geom_density() +
  facet_wrap(~transc)

va_vowel %>% 
  filter(!is.na(target)) %>% 
  ggplot() +
  aes(x = rating) +
  geom_density() +
  facet_wrap(~target)


# April 9, 2021 -----------------------------------------------------------

## Goals -> 
### (1) standardize f3-f2 difference per child, 
#### it worked! yay! -> see lines 16-19

### (2) look at standardizations by child, by experiment
#### not sure what the hells going on. not a ton of differences.

### (3) look at raters' ratings by std f3-f2 per child
#### need to figure out which raters listened to which kid

### (4) look at raters' ratings by std f3-f2 across children
#### again, not a ton of differences

### (5) see if raters calibrated to childrens' f3-f2 ranges
#### ehhhh next time maybe


ggplot(
  data=(acous2 %>% filter(
    ChildID == unique(acous2$ChildID)[22], 
    tran == c("w", "r")
  )), 
  aes(x=f3f2norm, color=tran)
) + 
  geom_density()

ggplot(
  data=(va %>% filter(
    ChildID == unique(acous2$ChildID)[22],
    transc == c("r","w")
  )),
  aes(x=f3f2norm,y=rating)
) + 
  geom_density_2d()+
  scale_x_continuous(limits=c(-2,2))+
  scale_y_continuous(limits=c(0,1))+
  facet_wrap(~transc)

ggplot(
  data=(va %>% filter(
    Subject == unique(va$Subject)[42],
 #   ChildID == unique(acous2$ChildID)[4],
    transc == c("r","w")
  )),
  aes(x=f3f2norm,y=rating, color=transc)
) + 
  geom_density_2d()+
#  geom_point()+geom_jitter()+
#  geom_hex()+geom_hitter()+
  scale_x_continuous(limits=c(-2,2))+
  scale_y_continuous(limits=c(0,1))
## find a better viz  


### which kids did subject 5002 (index42) listen to?
va %>% filter(Subject==5002) %>% select(ChildID) %>% unique()
