vas=read.csv(file=file.choose())
acous=read.csv(file=file.choose())
names(vas)[1]="ExperimentName"
names(acous)[1]="sound"

names(vas)
names(acous)

head(acous)
head(vas)


#merged file

va=merge(vas,acous,by=c("ChildWordKey","word"), all=TRUE)
dim(va)
sum(is.na(acous$ChildWordKey))
#whats goin on with the dims?

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


