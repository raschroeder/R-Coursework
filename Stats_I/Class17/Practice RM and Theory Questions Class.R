# Modern movie music is based on musical style first heard in Der Ring des
# Nibelungen, a four-part opera created by Ricard Wagner (written between 1853
# -1869).  Wagner created a particular type of musical theme, called a
# leitmotif, which were often used to convey the emotions or intentions of the
# characters, foreshadow events, or give the audience contextual information
# about locations.  Modern composers such as John Williams (Star Wars, Jaws),
# James Horner (Alien, Star Trek), and Howard Shore (Lord of the Rings) all have
# utilized Wagner's creation of leitmotifs to help create the conception of what
# we now blockbuster movies (Jaws in 1975 was the first blockbuster and you all
# now that leitmotif well). Other famous modern examples of these "leitmotif."
# are in Star Wars, such as the Darth Vader theme which can be heard throughout
# the original trilogy in different ways to indicate how the audience should
# feel about Darth Vader. From the first he appears on screen to when his
# leitmotif becomes enmashed with his son's leitmotif in his death scene, the
# music always conveys the story.

# While modern movie viewers are exceptionally adept at recognizing leitmotifs,
# they actually often misattribute the emotions that the experience to what they
# see on screen and not the music (Cohen, 2001). What is on screen may not
# actually impact the emotional impact of the music, but knowing the story may
# highten the emotional experience. However, listeners are able to pick up on
# the emotional meaning of the leitmotifs even when they are completely
# unfamiliar with the opera (Hacohen & Wagner, 1997). Therefore, the current
# study investigates how the emotional perception changes (using the circumplex
# model of emotion) as a function of knowing the story of the movie clip and
# either hearing, seeing, and seeing and hearing the clip. 30 participants were
# recruited to listen to 24 different leitmotifs that were moderately arousing,
# but had boring videos. For half of the clips participants were given a short
# summary of the context and in the other half they were not told anything about
# the context. In both the context and no-context condition, they either only
# heard, saw or saw and heard the clip. The motifs was counterbalanced across
# participants and trials order was randomized. After each leitmotif they were
# asked to rate how arousing it was on a 0-10 scale (with 10 = very arousing).

# References 

# Cohen, A. J. (2001). Music as a source of emotion in film. Music and emotion:
# Theory and research, 249-272.

# Hacohen, R.& Wagner, N. (1997). The Communicative Force of Wagner's
# Leitmotifs: Complementary Relationships Between Their Connotations and
# Denotations. Music Perception, 14/4, 445-476.

##############################################################################
#Your task is to analyze this data using a two-way RM ANOVA model and follow it
#up using ALL three routes. I want pretty publication graphs!

setwd("/Users/rachel/Box\ Sync/R\ Coursework/Class17")

MotifStudy<-read.csv("Practice.csv")

MotifStudy$Context<-factor(MotifStudy$Context, 
                        levels=c("No Context", "Context"),
                        labels=c("No Context", "Context"))
MotifStudy$Modality<-factor(MotifStudy$Modality, 
                         levels=c("Sound", "Sight", "Sound & Sight"),
                         labels=c("Sound", "Sight", "Sound & Sight"))


# Run a two-way RM ANOVA 
library(afex)
ANOVA.Motif<-aov_car(Arousal~ Context*Modality + Error(ID/Context*Modality), 
                 data = MotifStudy)
ANOVA.Motif



####################################
# Theory Review 1
#####################################
# Return the ANOVA is univariate format: summary(Model.1, return='univariate')
# Note: "SS_error of the Intercept" = SS_sub value from our ANOVA table
ANOVA.Motifextra<-aov_car(Arousal~ Context*Modality + Error(ID/Context*Modality), 
                          data = MotifStudy, return="univariate")
ANOVA.Motifextra

# BY hand draw the Venn-diagram of variance parsing


####################################
# Theory Review 2
#####################################
# Drop Modality from your RM ANOVA and predict how each term would change
# Hint go back to your pie chart
# Hint 2: Check the formulas carefully (remember the "weighting" in the 2-RM way?!)

# Run it in R to check yourself: Return the ANOVA is univariate format

ANOVA.NoModality<-aov_car(Arousal~ Context + Error(ID/Context), 
          data = MotifStudy, return="univariate")

ANOVA.NoModality

####################################
# Theory Review 3
#####################################
# If you reran the ANOVA as between subject 

ANOVA.between<-aov_car(Arousal~ Context*Modality + Error(BetweenID), 
                     data = MotifStudy)
ANOVA.between
summary(ANOVA.between, return="nice")

# 1) Redraw the Venn-diagram of variance parsing

# 2) Recalcuate the F values BY hand

# 3) Check yourself in R.
# Note I create the Subject term you need for ErrorTerm: BetweenID




####################################
# Theory Review 4
####################################
# Follow up the RM ANOVA following what you think is best
# Repeat the process using all three methods we covered in class
# Be able to explain when we would use each, how and why they differ 


##Modern
library(emmeans)
Context.by.Modality<-emmeans(ANOVA.Motif, ~Context|Modality)
test(pairs(Context.by.Modality), joint=TRUE)

Context.by.Modality

pairs(Context.by.Modality, adjust = 'tukey')


##MANOVA
Model.M<-aov_car(Modality~)