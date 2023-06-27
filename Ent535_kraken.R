#Jared Adam 
#final  project 
#last annotations made on 4/27/2023 and 8:33 am 
R.version

#####Packages####
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(gganimate)
library(gapminder)
library(gifski)
library(plyr)
library(glmmTMB)
library(emmeans)
library(car)
library(lme4)
library(vegan)
library(FSA)
library(multcomp)
library(multcompView)
library(RColorBrewer)

##### goals #####
#FIRST: What am i interested in learning? 
#1. I want to know if treatment (cover crop termination timing) influences predator abundance and total predation
#2. I want to compare these two variables across treatment where predator abundance is my independent variable
#BECAUSE: total predation is dependent on the number of predators present 
#I want to keep the predators separate rather than total predators b/c it is important to know who is there in what number 

##### data and wrangling/ cleaning #####
#sent prey and beans data from repos: 535 
#https://github.com/Jared-Adam-PSU/535
data = read.csv("https://raw.githubusercontent.com/Jared-Adam-PSU/535/main/SentPreyandPF.Beans.2022.csv?token=GHSAT0AAAAAACA4ZDN2AGF5AOF7R3MWNBHQZCFOA5A")

#need to create df for just predators of interest and less time stuff
#which predators? 
#Lycosid, Poecilus, Amara, Pterostichus, Chlaenius, Thomsidae, Stenolophus, Linyphiidae, Staphylinidae, Formicidae, Badister, Bemidion

new_data = data.frame(data$time_block, data$treatment, data$plot, data$SUM_Lycosid, data$SUM_Poecilus, data$SUM_.Amara, data$SUM_Pterostichus,
                      data$SUM_Chlaenius, data$SUM_Thomisidae, data$SUM_Stenolophus, data$SUM_Linyphiidae, data$SUM_Staphylinidae, data$SUM_Formicidae, data$SUM_Badister,
                      data$SUM_Bembidion,data$SUM_Cincidelidae, data$sum_pred_plot)

# *** clean_data will be the backbone for parts 1-2.5 ***   

#these names are also far too long to type out over and over
clean_data = plyr::rename(new_data,c("data.SUM_Lycosid" = "LYC",
                                     "data.SUM_Poecilus" = "POE",
                                     "data.SUM_.Amara" = "AMR",
                                     "data.SUM_Pterostichus" = "PTE",
                                     "data.SUM_Chlaenius" = "CHA",
                                     "data.SUM_Thomisidae" = "THM",
                                     "data.SUM_Stenolophus" = "STE",
                                     "data.SUM_Linyphiidae" = "LIN",
                                     "data.SUM_Staphylinidae" = "STA",
                                     "data.SUM_Formicidae" = "FOR",
                                     "data.SUM_Badister" = "BAD",
                                     "data.SUM_Bembidion" = "BEM",
                                     "data.sum_pred_plot" = "tot.pred",
                                     "data.time_block" = "timing",
                                     "data.SUM_Cincidelidae" = "CIN",
                                     "data.treatment" = "trt"))

##### PART 1: predation and predators #####
#statistical test? 
#I have two continuous variables I want to regress together. 
#these include the predator and counts  (x, independent) and the total predation (y, dependent)
#I am going to geom_point these jawns, but first, a test 


#the question is, which test would be most useful? 
#ANVOA wouldn't work because these variables are not independent of each other 
#MANCOVA would not work because I do not have another co-variate to use
#I could use cover crop biomass/ treatment, I think?
#that said, I only have ONE dependent variable, so mancova would not work 

#Lettuce see if I can pull it off

# hist(clean_data$cubrt_totpred)
# clean_data$cubrt_totpred <- nthroot(clean_data$tot.pred,5)
#the above did not work 


#generalized linear model 
#binding tot.pred and trt ~ as a function of the species 
#binomial gave the best fit of residuals 

#x trt
mlm1 = glm(cbind(tot.pred,trt) ~ LYC + POE + AMR + PTE + CHA + THM + STE + LIN + STE + FOR + BAD + BEM, data = clean_data, family = binomial)
hist(residuals(mlm1))
res <- resid(mlm1)
plot(fitted(mlm1,res))
summary(mlm1)
coef(mlm1)

#x timing 
mlm2 = glm(cbind(tot.pred,timing) ~ LYC + POE + AMR + PTE + CHA + THM + STE + LIN + STE + FOR + BAD + BEM, data = clean_data, family = binomial)
hist(residuals(mlm2))
res <- resid(mlm2)
plot(fitted(mlm2,res))
summary(mlm2)
coef(mlm2)

# #looking at each species with glmer
# # mlm3 = glmer(tot.pred ~ trt*timing + (1|LYC), data = clean_data, family = gaussian)
# summary(mlm3)
# hist(residuals(mlm3))


#4/26/2023

#looking at tot.pred by trt AND timing for two sp 
#did this work??? 
mldtest <- glm(cbind(tot.pred,timing*trt)~LYC, data = clean_data, family = binomial)
summary(mldtest)
hist(residuals(mldtest))
mldtest1 <- glm(cbind(tot.pred,timing*trt)~PTE, data = clean_data, family = binomial)
summary(mldtest1)
hist(residuals(mldtest1))
# mldtest1 <- glm(cbind(tot.pred,trt)~LYC, data = clean_data, family = binomial)
# summary(mldtest1)
# hist(residuals(mldtest1))

# #this glmer don't work. same output no matter which sp i put in.. why tho?? 
# mlm4 = glmer(tot.pred ~ trt*timing + (1|LYC), data = clean_data, family = poisson)
# summary(mlm4)
# hist(residuals(mlm4))
# plot(clean_data$LYC, clean_data$tot.pred)


#attempted glmer here with a loop 
#this would not work because it is not a fixed effects 

#hold the phone, this glmer is not working correctly 
# COLS <-list()
# time_fits <- list()
# trt_fits <- list()
# int_fits<- list()
# species.pred.glmers = list()
# for (i in 4:12){
#   # create new dataframe
#   column1 <- colnames(clean_data[i])
#   COLS[[i-3]]<-column1
#   newdf <- subset(clean_data,select=c("timing","trt","data.plot",'LYC' , 'POE' , 'AMR' , 'PTE' , 'CHA' , 'THM' , 'STE' , 'LIN' , 'STE' , 'FOR',
#                                       'BAD' , 'BEM','tot.pred', column1))
#   colnames(newdf)<-c("timing","trt","plotID",'LYC' , 'POE' , 'AMR' , 'PTE' , 'CHA' , 'THM' , 'STE' , 'LIN' , 'STE' , 'FOR',
#                      'BAD' , 'BEM','tot.pred',"column")
#   preds <- colnames(clean_data)[4:12]
#   
#   mlm5 = glmer(tot.pred ~ trt*timing + (1|PTE), data = clean_data, family = poisson)
# }
# summary(mlm5)
# # print(hist(residuals(mlm5)))
# # print(plot(fitted(mlm5),resid(mlm5)))
#   
#   aov1<-Anova(mdl,type=3)
#   
#   # extract p values
#   timing_pval<-aov1$`Pr(>Chisq)`[3]
#   trt_pval<-aov1$`Pr(>Chisq)`[2]
#   int_pval<-aov1$`Pr(>Chisq)`[4]
#   time_fits[[i-3]]<-timing_pval
#   trt_fits[[i-3]]<-trt_pval
#   int_fits[[i-3]]<-int_pval
#   
#   species_grouped <- newdf %>% dplyr::group_by(timing,trt) %>% dplyr::summarise(mean = mean(column), sd = sd(column), n=n())
#   species_grouped$se <- species_grouped$sd/sqrt(species_grouped$n)
#   
#   species.pred.glmers[[i-3]] <- kruskal.test(clean_data[[i]] ~ clean_data[[1]])



#data vis 
#starting with the regression of x=predators and y=predation by treatment(symbol)
#i want preds/trap on the x axis 

#want to create vector of the predators to put into ggplot? 
pred_for_plot = data.frame(PRED = c("LYC","POE" ,"AMR" , "PTE", "CHA","THM", "STE", "LIN","STA","FOR","BAD","BEM","CIN"))

# ggplot(data=clean_data, aes(x=4:16, y=tot.pred))+
#   geom_point()+
#   scale_y_continuous(n.breaks = NULL)

#sum the predator columns 
clean_data$total_predators <- rowSums(clean_data[,4:16])
clean_data$pred.prop <- clean_data$tot.pred/6

plot(clean_data$total_predators, clean_data$pred.prop)


### data vis ###

#showing gradient 
#this would look better if I had 1000+ data points

# ggplot(clean_data, aes(x = total_predators, y= pred.prop))+
#   geom_point(alpha = 1/3, size = 2.5)+
#   ylim(.5,1)+
#   labs(x = "Total Predators",
#        y = "Proportion Predated x/6",
#        title = "Proportion Predated x Total Predators")


#standard ggplot regression
#Proportion predated x total pred 

# ggplot(clean_data, aes(x = total_predators, y= pred.prop, color = factor(trt, 
#                        labels = c("Control","Green","Brown","GR-BR")), 
#                        shape = factor(timing, labels = c("V3", "V5", "R3"))),
#                        color = "black", pch=21, size = 5)+
#   geom_point(size = 2.5)+
#   scale_color_brewer(palette = "Dark2")+
#   ylim(.5,1)+
#   labs(x = "Total Predators",
#        y = "Proportion Predated x/6",
#        title = "Proportion Predated x Total Predators")+
#   labs(shape = "Timing", color = "Treatment")


# TRIAL RUN
# built this to see if I could actually make an animation: see final product below 
#gganimate 

# plot.test <- ggplot(clean_data, aes(x = total_predators, y= pred.prop, color = factor(trt, 
#   labels = c("Control","Green","Brown","GR-BR"))),
#   color = "black", pch=21, size = 5)+
#   geom_point(size = 2.5)+
#   scale_color_brewer(palette = "Dark2")+
#   ylim(.5,1)+
#   labs(x = "Total Predators",
#        y = "Proportion Predated x/6",
#        title = "Proportion Predated x Total Predators")+
#   labs(shape = "Timing", color = "Treatment")+
#   labs(title = "Timing: {frame_time}")+
#   transition_time(timing)+
#   ease_aes('linear')
# 
# animate(plot.test, duration = 10, fps = 5, width = 700, height = 400, renderer = gifski_renderer())


#Spice up your life ~ for presentation 
#gganimate 
plot1 <- ggplot(clean_data, aes(x = total_predators, y= pred.prop, color = factor(trt, 
                                                                                  labels = c("Control","Green","Brown","GR-BR"))), 
                color = "black", pch=21, size = 5)+
  geom_point(size = 6)+
  scale_color_brewer(palette = "Dark2")+
  ylim(.5,1)+
  labs(x = "Total Predators",
       y = "Proportion Predated x/6")+
  # title = "{frame_time}")+
  labs(title = "(Proportion Predated x Total Predators) x Time", caption = 'Timing: {frame_time}',shape = "Timing", color = "Treatment")+
  transition_time(timing)+
  ease_aes('linear')+
  theme(
    plot.caption = element_text(hjust = .95, vjust= 15, size = 45, 
                                color = "darkgray", face = "bold"),
    axis.line.x = element_line(color = 'black', linewidth =0.75, linetype='solid'),#adding the x and y axis lines 
    axis.line.y = element_line(color = 'black', linewidth =0.75, linetype='solid'),
    axis.ticks = element_line(linewidth = 1),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 20),
    legend.title = element_text(size=15),
    legend.text = element_text(size=15),
    title = element_text(size = 15)
  )

# geom_text(aes( label = as.factor(timing)), 
#            hjust=-2, vjust = -0.2, alpha = 0.2,  col = "gray", size = 20) +
#  transition_states(as.factor(timing), state_length = 0)+
animate(plot1, duration = 10, fps = 5, width = 700, height = 400, renderer = gifski_renderer()) #gifski here, couldn't get it to work without 
#here is the animation duration in seconds at 5 fps. plot width and height specified 
#caveat to gganimate: depending on animation specs, would take up to 20/30 seconds for each rendition of this 
#meaning, just to chnage the font size and view this, it would take 20 seconds to build the ggplot becuase of the animatio
#pro-tip : build your normal ggplot, then layer in the animation so you do not need to re-animate over and over 

anim_save('plot1.gif') #very easy to save
plot1 #calling the plot back to look 
#theme(legend.position = 'none')+
# geom_smooth(method = "lm")
# geom_point(aes(color = factor(trt)))+


######PART 2: Species/ treatment x timing: NMDS ALL data#####


#ALL code runs as of 4/21/2023


#this section will be using clean_data from part one

#now, PERMANOVA time 

#treatment as factor 
clean_data$trt = as.factor(clean_data$trt)

#create a species data matrix 
#getting rid of time, plot, trt and pred tot? 
species_new = clean_data[4:16]

#calculate the distances 
#bray = distance measurement, these are abundance
#vegan
dist = vegdist(species_new, "bray")

#run a permanova ##set.seed?? 
#999 = standard 
#dist = the new value I created above as a function of trt
permanova = adonis2(dist ~ trt, permutations = 999, method = "bray", data = clean_data)
permanova
#TRT is not significant here ~ there is no significance between treatment 
#do again for time 

permanova.timing = adonis2(dist ~ timing, permutations = 999, method = "bray", data = clean_data)
permanova.timing

#treatments are not sig, so just do time 

#let's visualize this 
#plot this jawn 

#STRESS
#tell how much variation the visualization is taking out
#low stress= good visualization of dist in community 

#Multidimensional scaling 
#WHY?: we want to be able to represent the distances among objects in a parsimonious and visual way 
#MDS can take yeeaarrrsss to run, but not here 
ord = metaMDS(species_new)

#made scores and then turn it into a df with treatment 
scrs = scores(ord, display = "sites")
scrs = cbind(as.data.frame(scrs), Treatment = clean_data$trt)

#let's try something new: I want SPECIES by trt and labeled by time? 
new.MDS = metaMDS(species_new, k=2)

#add this to all!!
new.MDS$stress
#lower stress = meta.mds fxn has k function with default k = 2
#constrains to 2 dimensions
#can increase dimensions and then show only 2 

#quick vis
plot(new.MDS, type ="t")

#scrs.new = as.data.frame(scores(new.MDS)) #this did not work?
scrs.new = scores(new.MDS, display = "sites") 
scrs.new = cbind(as.data.frame(scrs.new))
timing = clean_data$timing #I want to add timing, so creating a variable for timing
scrs.new$timing = timing #adding a column in the df for the timing variable I created 
head(scrs.new)

#I want species scores as a df
species.scores = as.data.frame(scores(new.MDS, "species"))
species.scores$species = rownames(species.scores) #creating new column in species.score for my species names 
head(species.scores) #looks how I want it to 

#NOW ggplot for species 

#I want to group them in a shape somehow
#geom_polygon !!!
#need to create an object to group them by...
#can do this by timing and eliminate those fucking floating numbers! they aint doing anyone any good anywho (say that out loud haha)
#buckle up
#chull seems like a thing to use
grp.1 = scrs.new[scrs.new$timing == "1",][chull(scrs.new[scrs.new$timing == "1",c("NMDS1", "NMDS2")]),]
grp.2 = scrs.new[scrs.new$timing == "2",][chull(scrs.new[scrs.new$timing == "2",c("NMDS1", "NMDS2")]),]
grp.3 = scrs.new[scrs.new$timing == "3",][chull(scrs.new[scrs.new$timing == "3",c("NMDS1", "NMDS2")]),]

#OK now I have these grouped
#let's HULL

hull.data = rbind(grp.1, grp.2, grp.3) #binding them 
hull.data$timing = as.factor(hull.data$timing) #need this to be a factor so I can manipulate the colors manually 
hull.data

#i think this worked.... let's throw in the plot 

#eliminate fill by treatment because it is not sig in the group 
#show me lines and polygon by time

ggplot()+
  geom_polygon(data=hull.data, aes(x=NMDS1, y=NMDS2, group=timing,fill = timing),alpha=0.3)+#adding the convex of the hulls 
  scale_fill_manual(values = c("red","black","blue"))+
  geom_segment(data = species.scores, aes(x=0, xend=NMDS1,y=0, yend=NMDS2), arrow =arrow(length = unit(0.25, "cm")),
               color = "grey10", lwd=0.3)+ #ading arrows to show species?
  ggrepel::geom_text_repel(data = species.scores, aes(x=NMDS1, y=NMDS2, label = species),cex = 5, direction ="both",
                           segment.size =0.25)+ #this allows me to get rid of geom_text for species names 
  # geom_text(data=species.scores, aes(x=NMDS1, y=NMDS2, label=species), alpha=0.5)+ #this is adding species label at size=0.5
  # geom_point(data=scrs.new, aes(x=NMDS1, y=NMDS2, shape=Treatment, color=Treatment),size=3)+ #add point markers by trt
  # scale_color_manual(values=c("1" = "red", "2" = "blue", "3" = "green","4" = "black"))+
  annotate(geom = "text", x=1.5, y=2.2, label ="Stress: 0.162")+
  coord_equal()+
  theme_bw()+
  labs(fill="Timing")+
  labs(title = "Predator abundance by timing")+
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

#this looks fucking sick.. 


##### PART 2.25 Species/ treatment x timing: NMDS time separated ######

#this section will feed off of the objects made in part 2
#will not be able to run this, without that
#I can use the grp.s I have already made 
#I need to make a new object/ df for the species at each time


#subsetting timing
#I want to look at NMDS of each timing, which means build new NMDS models for each timing 
#BUT I am not running a permanova for each because that is not needed information 

# timing 1 

clean_data_t1 = clean_data %>%
  filter(timing == "1")

#create a species data matrix 
#getting rid of time, plot, trt and pred tot? 
species_t1 = clean_data_t1[4:16]

T1.MDS = metaMDS(species_t1)
T1.MDS$stress
plot(T1.MDS, type ="t")

#scrs.new = as.data.frame(scores(new.MDS)) #this did not work?
T1.scrs = scores(T1.MDS, display = "sites")
T1.scrs = cbind(as.data.frame(T1.scrs))
timing = clean_data$timing [1:20] #I want to add timing, so creating a variable for timing
T1.scrs$timing = timing #adding a column in the df for the timing variable I created 
head(T1.scrs)

#I want species scores as a df
T1.species.scores = as.data.frame(scores(T1.MDS, "species"))
T1.species.scores$species = rownames(T1.species.scores) #creating new column in species.score for my species names 
head(T1.species.scores) #looks how I want it to 


ggplot()+
  geom_text(data=T1.species.scores, aes(x=NMDS1, y=NMDS2, label = NA), alpha=0.5)+ #this is adding species label at size=0.5
  geom_segment(data = T1.species.scores, aes(x=0, xend=NMDS1,y=0, yend=NMDS2), arrow =arrow(length = unit(0.25, "cm")),
               color = "grey10", lwd=0.3)+ #ading arrows to show species?
  ggrepel::geom_text_repel(data = T1.species.scores, aes(x=NMDS1, y=NMDS2, label = species),cex = 5, direction ="both",
                           segment.size =0.25)+ # geom_point(data=T1.scrs, aes(x=NMDS1, y=NMDS2, shape=Treatment, color=Treatment),size=3)+ #add point markers by trt
  # scale_color_manual(values=c("1" = "red", "2" = "blue", "3" = "green","4" = "black"))+
  annotate(geom = "text", x=1.3, y=2, label ="Stress: 0.148")+
  coord_equal()+
  theme_bw()+
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())+
  labs(title = "NMDS for Timing 1")


dev.off() #use this when graphs do not work 

#timing 2

clean_data_t2 = clean_data %>%
  filter(timing == "2")

species_t2 = clean_data_t2[4:16]

T2.MDS = metaMDS(species_t2)
T2.MDS$stress
plot(T2.MDS, type ="t")

#scrs.new = as.data.frame(scores(new.MDS)) #this did not work?
T2.scrs = scores(T2.MDS, display = "sites")
T2.scrs = cbind(as.data.frame(T2.scrs))
timing = clean_data$timing [21:40]  #I want to add timing, so creating a variable for timing
T2.scrs$timing = timing #adding a column in the df for the timing variable I created 
head(T2.scrs) #it prints as T=1, but the base plots are different, so perhaps a coding issue

#I want species scores as a df
T2.species.scores = as.data.frame(scores(T2.MDS, "species"))
T2.species.scores$species = rownames(T2.species.scores) #creating new column in species.score for my species names 
head(T2.species.scores) #looks how I want it to 

ggplot()+
  geom_text(data=T2.species.scores, aes(x=NMDS1, y=NMDS2, label=NA), alpha=0.5)+ #this is adding species label at size=0.5
  geom_segment(data = T2.species.scores, aes(x=0, xend=NMDS1,y=0, yend=NMDS2), arrow =arrow(length = unit(0.25, "cm")),
               color = "grey10", lwd=0.3)+ #ading arrows to show species?
  ggrepel::geom_text_repel(data = T2.species.scores, aes(x=NMDS1, y=NMDS2, label = species),cex = 5, direction ="both",
                           segment.size =0.25)+ # geom_point(data=T1.scrs, aes(x=NMDS1, y=NMDS2, shape=Treatment, color=Treatment),size=3)+ #add point markers by trt
  # geom_point(data=T2.scrs, aes(x=NMDS1, y=NMDS2, shape=Treatment, color=Treatment),size=3)+ #add point markers by trt
  # scale_color_manual(values=c("1" = "red", "2" = "blue", "3" = "green","4" = "black"))+
  annotate(geom = "text", x=0.4, y=2, label ="Stress: 0.122")+
  coord_equal()+
  theme_bw()+
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())+
  labs(title = "NMDS for Timing 2")



#timing 3
clean_data_t3 = clean_data %>%
  filter(timing == "3")

species_t3 = clean_data_t3[4:16]

T3.MDS = metaMDS(species_t3)
T3.MDS$stress
plot(T3.MDS, type ="t")

#scrs.new = as.data.frame(scores(new.MDS)) #this did not work?
T3.scrs = scores(T3.MDS, display = "sites")
T3.scrs = cbind(as.data.frame(T3.scrs))
timing = clean_data$timing [41:60] #I want to add timing, so creating a variable for timing
T3.scrs$timing = timing #adding a column in the df for the timing variable I created 
head(T2.scrs) #it prints as T=1, but the base plots are different, so perhaps a coding issue

#I want species scores as a df
T3.species.scores = as.data.frame(scores(T3.MDS, "species"))
T3.species.scores$species = rownames(T3.species.scores) #creating new column in species.score for my species names 
head(T3.species.scores) #looks how I want it to 

ggplot()+
  geom_text(data=T3.species.scores, aes(x=NMDS1, y=NMDS2, label=NA), alpha=0.5)+ #this is adding species label at size=0.5
  geom_segment(data = T3.species.scores, aes(x=0, xend=NMDS1,y=0, yend=NMDS2), arrow =arrow(length = unit(0.25, "cm")),
               color = "grey10", lwd=0.3)+ #ading arrows to show species?
  ggrepel::geom_text_repel(data = T3.species.scores, aes(x=NMDS1, y=NMDS2, label = species),cex = 5, direction ="both",
                           segment.size =0.25)+ # geom_point(data=T1.scrs, aes(x=NMDS1, y=NMDS2, shape=Treatment, color=Treatment),size=3)+ #add point markers by trt
  # geom_point(data=T3.scrs, aes(x=NMDS1, y=NMDS2, shape=Treatment, color=Treatment),size=3)+ #add point markers by trt
  # scale_color_manual(values=c("1" = "red", "2" = "blue", "3" = "green","4" = "black"))+
  annotate(geom = "text", x=1.5, y=2, label ="Stress: 0.114")+
  coord_equal()+
  theme_bw()+
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())+
  labs(title = "NMDS for Timing 3")



##### PART 2.5: FOR LOOP for glmer: abundance ~ timing #####

####

#this section was with the Kruskal-Wallis and Dunn's test whcih i took out. I can share the code if needed, but I used 
#lof transform again and almost left my OG ggplots (example code below log transform), but took them out for time's sake 

#i think I want to log the species for a better figure 
#log transform these jawns wih dplyr
#bringing in the ones with sig values ONLY
clean.data_log = clean_data[1:13]

#this was for boxplots, which I am no longer using... 
#I also deleted those ggplots and their code on accident and saved without realizing it... FAAAAKKKK. It was so noice. 
#SOSososoSOSOsOsosOsOsOooooooooo noice. SHOOOOOOOT. 
#no trouble. life goes on
#life is fiction, the truth is a lie, the heck with this code, let's get high 
#haha jk 
#but fr. that was slick
#TOYOYOYOYOYO i recovered them janws. hot damn 
#this whole event was about 2 minutes. what a rush. 
#leaving this script in here so you can read my venting process
clean.data_log = mutate(clean.data_log, LYC = log(LYC + 1))
clean.data_log = mutate(clean.data_log, POE = log(POE + 1))
clean.data_log = mutate(clean.data_log, AMR = log(AMR + 1))
clean.data_log = mutate(clean.data_log, CHA = log(CHA + 1))
clean.data_log = mutate(clean.data_log, PTE = log(PTE + 1))
clean.data_log = mutate(clean.data_log, THM = log(THM + 1))
clean.data_log = mutate(clean.data_log, STE = log(STE + 1))
clean.data_log = mutate(clean.data_log, LIN = log(LIN + 1))
clean.data_log = mutate(clean.data_log, STA = log(STA + 1))
clean.data_log = mutate(clean.data_log, FOR = log(FOR + 1))

clean.data_log$timing <- as.factor(clean.data_log$timing)
#for your leisure, this is the code I was referring to 
#LYC
ggplot(data = clean.data_log, aes(x=timing, y=LYC, fill=timing))+
  geom_boxplot(flatten = 2.5)+
  labs(x= "Timing",
       y="Abundance: log(x+1)",
       title = "Lycosidae x Timing")+
  scale_fill_brewer(palette = "Blues")+ #i fuck with this color palette, so simple, so elegant 
  theme( 
    panel.grid.major = element_line(size = 0.5, linetype = "solid", color = "lightgrey"),#major lines = grey 
    panel.grid.minor = element_line(size = 0.5, linetype = "solid", color = "lightgrey"),#minor lines = grey 
    panel.background = element_rect(fill = "white", color = "black", linewidth  = 0.5, linetype = 'solid'),
    legend.position = 'none',
    axis.text.x =element_text(size=12),
    axis.text.y = element_text(size=12))+
  scale_x_discrete(labels=c("1" = "V3", "2" = "V5", "3" = "R3"))

####



#GGPLOTS: FOR presentation I want to include lycosidae and pterostichus?
#I cannot get error bars on geom bar. I know why, but cannot seem to fix it
#the grouped data set is a difference size than newdf
#to get the error bars in, they must be the same size 
#I am either mentally tapped on this, or I am missing something simple because I cannot get this to work 
#either way, you  may need to run the whole loop and then run the ggplot code separately to get the correct figure. 
#sometimes it was weird for me when running loop and ggplot

clean_data$Block<-substr(clean_data$data.plot,1,1) #starting with the first number in plot (1,), add an associated value to the new 'block' column
COLS <-list() #column list for loop values
time_fits <- list() #time list for loop values 
trt_fits <- list() #trt list for loop values
int_fits<- list() #intercept list for loop values 
for (i in 4:4){
  # create new dataframe
  #7:7 here denotes I want to look from column 7 to column 7. This is where Pterostichus is
  #change to 4:16 to run glmer for all of the species columns 
  column <- colnames(clean_data[i])
  COLS[[i-3]]<-column #i is my 'pronoun', I am saying do not look at columns 1 through 3
  newdf <- subset(clean_data,select=c("timing","trt","Block","data.plot",column)) #new df with what I am. column is a new vector, so not quotes bc not from old df
  colnames(newdf)<-c("timing","trt","block","plotID","column") #naming new columns in order of the newdf creation string 
  
  # generalized linear mixed effects model 
  # why? my samples are not independent 
  # I visit each trap 3 times throughout the year 
  #this does not meet assumptions for anova OR kruskal-wallis 
  mdl<-glmer(column~trt*timing+(1+timing|plotID)+(1|block),data=newdf,family=poisson)
  #column is response 
  #trt*timing = term
  #timing and plotid and block = random effects
  # | separates expressions for design matrices from grouing factors 
  
  # print(hist(residuals(mdl)))
  # print(plot(fitted(mdl),resid(mdl)))
  
  aov1<-Anova(mdl,type=3)
  
  # extract p values
  timing_pval<-aov1$`Pr(>Chisq)`[3]
  trt_pval<-aov1$`Pr(>Chisq)`[2]
  int_pval<-aov1$`Pr(>Chisq)`[4]
  time_fits[[i-3]]<-timing_pval
  trt_fits[[i-3]]<-trt_pval
  int_fits[[i-3]]<-int_pval
  
  species_grouped <- newdf %>% dplyr::group_by(timing,trt) %>% dplyr::summarise(mean = mean(column), sd = sd(column), n=n())
  species_grouped$se <- species_grouped$sd/sqrt(species_grouped$n)
  # print(ggplot(species_grouped, aes(x=trt,y=mean, fill=timing))+
  #         geom_bar(aes(x=trt,y=mean, fill=timing), stat = "identity",position="dodge")+ggtitle(column))
  
  #this stopped working on 4/27/20223
  newdf$trt <- as.factor(newdf$trt)
  print(ggplot(newdf, aes(x=trt,y=column, fill=timing))+
          geom_bar(aes(x=trt,y=log(column+1), fill=timing),stat = "identity",position="dodge"))+
    labs(x= "Treatment", y="Abundance log(x+1)", title = "Lycosidae: Abundance x timing*treatment")+
    scale_fill_brewer(palette = "Dark2", labels=c("V3", "V5", "R3"),name = "Timing")+
    scale_x_discrete(limit=c("Control", "Brown", "Green", "GR-BR"))
  
  #geom_errorbar(aes(ymin = (species_grouped$mean-species_grouped$sd), ymax =(species_grouped$mean+species_grouped$sd)))
  # scale_y_continuous(breaks = seq(0,4, by=1))
  
  #neither of these works... 
  #
  #geom_errorbar(aes(x = trt,ymin = mean - se, ymax = mean + se),width=0.2)
  
  
  # FITS2[[i-3]]
}
data.frame("Species"=unlist(COLS), "TimingPval"=unlist(time_fits), "TRTPval"=unlist(trt_fits), "IntPval"=unlist(int_fits))

summary(mdl)


#####PART 3: sent prey alone #####

sent_data = read.csv("https://raw.githubusercontent.com/Jared-Adam-PSU/535/main/SentintelPrey.ALLtimes.2022.csv?token=GHSAT0AAAAAACA4ZDN2UO25TD753MIQYKGAZCFGROQ")

#models = begin 
sent_data$treatment<-as.factor(sent_data$treatment)

#subset data into V3, V5, and R3 because I want to look at predation per timing
predv3 = subset(sent_data, growth_stage == "V3")
predv5 = subset(sent_data, growth_stage == "V5")
predr3 = subset(sent_data, growth_stage == "R3")

#models for total predation
#using total predation as a function of my treatment as a factor 
#I want to look location by plot by row ->  (1|location/plotid/row)... I cannot define why I use '1|', but i think it because I want one variable/identifier there
#I use the location variable because it is how I organize my multi-state data so I do it here so my code works on those data as well
#I am only using PA data here
?glmmTMB
#Fit a generalized linear mixed model (GLMM) using Template Model Builder (TMB).

?emmeans
#Compute estimated marginal means (EMMs) for specified factors or factor combinations in a linear model; and optionally, 
#comparisons or contrasts among them. EMMs are also known as least-squares means.


#I spelled plotid as ploitid, need to fix that
predv3 = plyr::rename(predv3,c("ploitid" = "plotid"))
predv5 = plyr::rename(predv5,c("ploitid" = "plotid"))
predr3 = plyr::rename(predr3,c("ploitid" = "plotid"))



sent_data$plot_sample <- paste(sent_data$block,sent_data$sample,sep="_")
#1| format for intercept 

#making vector of timings to clean up df
time_am = c("am","am","am","am")
time_pm = c("pm","pm","pm","pm")

###

#I use this model over and over. ONLY annotating full explanantion once 

#this is a combined fixed and random effects model 
#follows lme4 syntax 
#binomial expressed as proportion 
#to.pred ~ by treatment + random effects of plotid and row 
# | for design matrices 
#data = pred v3 
#running a bionomial (family)

model1 = glmmTMB(to.predated ~ as.factor(treatment) + 
                   (1|plotid/row), data = predv3,
                 family = binomial)
model1.sum = summary(model1)

#want the emmeans of model1 with a pairwise comparison by treatment
#response = gets reults on the original scale (I dot not want any transformed information here)
emmeans_model1 <- emmeans(model1, pairwise ~ as.factor(treatment),type = "response")
#add the summary of emmeans to df to be used for my figures 
emmeans_model1_df<-summary(emmeans_model1$emmeans)

#vectors to rename my columns 
time = c("overall","overall","overall","overall")
v3 = c("v3","v3","v3", "v3")
emmeans_model1_df = cbind(emmeans_model1_df, v3 ) #cbind to bring in v3 vector
emmeans_model1_df = cbind(emmeans_model1_df, time) #cbind to bring in time vector
emmeans_model1_df = plyr::rename(emmeans_model1_df,c("v3" = "stage")) #renaming column 
emmeans_model1_df = plyr::rename(emmeans_model1_df,c("time" = "time")) #renaming columns 

test.m1 <-test(emmeans_model1)

###

##

model2 = glmmTMB(to.predated ~ as.factor(treatment) + 
                   (1|plotid/row), data = predv5,
                 family = binomial)

emmeans_model2 <- emmeans(model2, pairwise ~ as.factor(treatment),type = "response")
emmeans_model2_df <- summary(emmeans_model2$emmeans)

v5 = c("v5","v5","v5", "v5")
emmeans_model2_df = cbind(emmeans_model2_df, v5 ) 
emmeans_model2_df = cbind(emmeans_model2_df, time)
emmeans_model2_df = plyr::rename(emmeans_model2_df,c("v5" = "stage"))
emmeans_model2_df = plyr::rename(emmeans_model2_df,c("time" = "time"))

test(emmeans_model2)
##

model3 = glmmTMB(to.predated ~ as.factor(treatment) + 
                   (1|plotid/row), data = predr3,
                 family = binomial)

emmeans_model3 <- emmeans(model3, pairwise ~ as.factor(treatment),type = "response")
emmeans_model3_df <- summary(emmeans_model3$emmeans)

r3 = c("r3","r3","r3", "r3")
emmeans_model3_df = cbind(emmeans_model3_df, r3 ) 
emmeans_model3_df = cbind(emmeans_model3_df, time)
emmeans_model3_df = plyr::rename(emmeans_model3_df,c("r3" = "stage"))
emmeans_model3_df = plyr::rename(emmeans_model3_df,c("time" = "time"))

test(emmeans_model3)

#adding these to one df
emmeans_total= rbind(emmeans_model1_df, emmeans_model2_df, emmeans_model3_df)


#models for night predation
model4 = glmmTMB(n.pred ~ as.factor(treatment) + 
                   (1|plotid/row), data = predv3,
                 family = binomial)

emmeans_model4 <- emmeans(model4, pairwise ~ as.factor(treatment),type = "response")
emmeans_model4_df <-summary(emmeans_model4$emmeans)

emmeans_model4_df = cbind(emmeans_model4_df, v3 )
emmeans_model4_df = cbind(emmeans_model4_df, time_pm)
emmeans_model4_df = plyr::rename(emmeans_model4_df,c("v3" = "stage"))
emmeans_model4_df = plyr::rename(emmeans_model4_df,c("time_pm" = "time"))

test(emmeans_model4)
##

model5 = glmmTMB(n.pred ~ as.factor(treatment) + 
                   (1|plotid/row), data = predv5,
                 family = binomial)

emmeans_model5 <- emmeans(model5, pairwise ~ as.factor(treatment),type = "response")
emmeans_model5_df <- summary(emmeans_model5$emmeans)

emmeans_model5_df = cbind(emmeans_model5_df, v5 )
emmeans_model5_df = cbind(emmeans_model5_df, time_pm)
emmeans_model5_df = plyr::rename(emmeans_model5_df,c("v5" = "stage"))
emmeans_model5_df = plyr::rename(emmeans_model5_df,c("time_pm" = "time"))

test(emmeans_model5)
##

model6 = glmmTMB(n.pred ~ as.factor(treatment) + 
                   (1|plotid/row), data = predr3,
                 family = binomial)

emmeans_model6 <- emmeans(model6, pairwise ~ as.factor(treatment),type = "response")
emmeans_model6_df <- summary(emmeans_model6$emmeans)

emmeans_model6_df = cbind(emmeans_model6_df, r3 ) 
emmeans_model6_df = cbind(emmeans_model6_df, time_pm)
emmeans_model6_df = plyr::rename(emmeans_model6_df,c("r3" = "stage"))
emmeans_model6_df = plyr::rename(emmeans_model6_df,c("time_pm" = "time"))

test(emmeans_model6)

#one df for these models
emmeans_total_pm = rbind(emmeans_model4_df, emmeans_model5_df,emmeans_model6_df)


#models for day predation
model7 = glmmTMB(d.pred ~ as.factor(treatment) + 
                   (1|plotid/row), data = predv3,
                 family = binomial)

emmeans_model7 <- emmeans(model7, pairwise ~ as.factor(treatment),type = "response")
emmeans_model7_df <- summary(emmeans_model7$emmeans)

emmeans_model7_df = cbind(emmeans_model7_df, v3 )
emmeans_model7_df = cbind(emmeans_model7_df, time_am)
emmeans_model7_df = plyr::rename(emmeans_model7_df,c("v3" = "stage"))
emmeans_model7_df = plyr::rename(emmeans_model7_df,c("time_am" = "time"))

test(emmeans_model7)
##

model8 = glmmTMB(d.pred ~ as.factor(treatment) + 
                   (1|plotid/row), data = predv5,
                 family = binomial)

emmeans_model8 <- emmeans(model8, pairwise ~ as.factor(treatment),type = "response")
emmeans_model8_df <- summary(emmeans_model8$emmeans)

emmeans_model8_df = cbind(emmeans_model8_df, v5 )
emmeans_model8_df = cbind(emmeans_model8_df, time_am)
emmeans_model8_df = plyr::rename(emmeans_model8_df,c("v5" = "stage"))
emmeans_model8_df = plyr::rename(emmeans_model8_df,c("time_am" = "time"))

test(emmeans_model8)
##

model9 = glmmTMB(d.pred ~ as.factor(treatment) + 
                   (1|plotid/row), data = predr3,
                 family = binomial)

emmeans_model9 <- emmeans(model9, pairwise ~ as.factor(treatment),type = "response")
emmeans_model9_df <- summary(emmeans_model9$emmeans)

emmeans_model9_df = cbind(emmeans_model9_df, r3 )
emmeans_model9_df = cbind(emmeans_model9_df, time_am)
emmeans_model9_df = plyr::rename(emmeans_model9_df,c("r3" = "stage"))
emmeans_model9_df = plyr::rename(emmeans_model9_df,c("time_am" = "time"))

test(emmeans_model9)
##

#adding all emmeans outputs to df 
emmeans_total_am = rbind(emmeans_model7_df, emmeans_model8_df,emmeans_model9_df)

emmeans_all = rbind(emmeans_total, emmeans_total_pm, emmeans_total_am)

#bringing the df with just the overalls by growth stage down here 
emmeans_total= rbind(emmeans_model1_df, emmeans_model2_df, emmeans_model3_df)


# , labeller = stage
# factor(stage, levels = c('v3','v5','r3')
#   rm(stage)   
# stage = c("R3", "V3", "V5")
# jpeg('pre3.jpeg', units="in", width=11.5, height=6, res=300)
ggplot(emmeans_total, aes(x = treatment, y = prob))+
  geom_point(aes(x = treatment, y = prob), size = 3,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0, size = 1.5)+
  geom_errorbar(aes(x = treatment,ymin = lower.CL, ymax = upper.CL),
                color = "black", alpha = 1, width = 0, size = 0.5)+
  facet_grid(~factor(stage, levels = c('v3','v5','r3')))+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ggtitle("OVERALL")+
  theme(plot.title = element_text(size=10))+
  ylab("Predation probability")+
  xlab("")+
  scale_y_continuous(limits = c(0,1.1), breaks = c(0.25,0.5,0.75,1))+
  theme(axis.text.y=element_text(size=16),
        axis.text.x=element_text(size=15, angle=45, hjust = 1))+
  theme(strip.text.x = element_text(size = 10))+
  theme(text = element_text(size=18))+
  theme(axis.title.y=element_text(angle=90, vjust=4))+
  theme(axis.title.x=element_text(vjust=-4))+
  theme(plot.margin = unit(c(1,1,1,1), "cm"))+
  scale_x_discrete(labels=c("1" = "Control", "2" = "Brown", "3" = "Green", "4" = "GR-BR"))


ggplot(emmeans_total_am, aes(x = treatment, y = prob))+
  geom_point(aes(x = treatment, y = prob), size = 3,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0, size = 1.5)+
  geom_errorbar(aes(x = treatment,ymin = lower.CL, ymax = upper.CL),
                color = "black", alpha = 1, width = 0, size = 0.5)+
  facet_grid(~factor(stage, levels = c('v3','v5','r3')))+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ggtitle("AM")+
  theme(plot.title = element_text(size=10))+
  ylab("Predation probability")+
  xlab("")+
  scale_y_continuous(limits = c(0,1.1), breaks = c(0.25,0.5,0.75,1))+
  theme(axis.text.y=element_text(size=16),
        axis.text.x=element_text(size=15, angle=45, hjust = 1))+
  theme(strip.text.x = element_text(size = 10))+
  theme(text = element_text(size=18))+
  theme(axis.title.y=element_text(angle=90, vjust=4))+
  theme(axis.title.x=element_text(vjust=-4))+
  theme(plot.margin = unit(c(1,1,1,1), "cm"))+
  scale_x_discrete(labels=c("1" = "Control", "2" = "Brown", "3" = "Green", "4" = "GR-BR"))


ggplot(emmeans_total_pm, aes(x = treatment, y = prob))+
  geom_point(aes(x = treatment, y = prob), size = 3,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0, size = 1.5)+
  geom_errorbar(aes(x = treatment,ymin = lower.CL, ymax = upper.CL),
                color = "black", alpha = 1, width = 0, size = 0.5)+
  facet_grid(~factor(stage, levels = c('v3','v5','r3')))+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ggtitle("PM")+
  theme(plot.title = element_text(size=10))+
  ylab("Predation probability")+
  xlab("")+
  scale_y_continuous(limits = c(0,1.1), breaks = c(0.25,0.5,0.75,1))+
  theme(axis.text.y=element_text(size=16),
        axis.text.x=element_text(size=15, angle=45, hjust = 1))+
  theme(strip.text.x = element_text(size = 10))+
  theme(text = element_text(size=18))+
  theme(axis.title.y=element_text(angle=90, vjust=4))+
  theme(axis.title.x=element_text(vjust=-4))+
  theme(plot.margin = unit(c(1,1,1,1), "cm"))+
  scale_x_discrete(labels=c("1" = "Control", "2" = "Brown", "3" = "Green", "4" = "GR-BR"))
?labs
