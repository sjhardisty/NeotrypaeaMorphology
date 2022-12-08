#Morphology

str(neomorphotools)
dim(neomorphtools) # dimensions (no. of rows/columns) of table-like objects
length(neomorphtools) # number of values in a vector or a list
levels(neomorphotools$Population) # list of populations
levels(neomorphotools$Taxon) # list of taxa 
colnames(neomorphotools) # list of column names (characters)
library(klaR)
library(psych)
library(MASS)
library(ggord)

library(devtools)
library(car)
library(plotly)

str(nonamorph)

##LDA Total##
set.seed(123)
ind <- sample(2, nrow(populationnonamorph),
              replace = TRUE,
              prob = c(0.90, 0.10))
training <- populationnonamorph[ind==1,]
testing <- populationnonamorph[ind==2,]
linear <- lda(Population~., training)
linear
ggord(linear, training$Species, ylim = c(-10, 10))

plot(linear)

plot(linear, aes(col = blue))

# Make predictions
predictions <- linear %>% predict(testing)

# Model accuracy
mean(predictions$class==testing$Population)

model <- lda(Population~., data = training)
model

#define data to plot
lda_plot <- cbind(training, predict(linear)$x)

#create plot
ggplot(linear, aes(LD1, LD2)) +
  geom_point(aes(color = Population))


fill_boxplots <- c("#FFE1FF", "#CDC8B1", "#79CDCD")
outline_boxplots <- c("#CDB5CD","#CDAF95", "#53868B")
fill_boxplots <- setNames(fill_boxplots, c("estCaliforniensis", "EstGigas", "Offshore"))

pal_scatter <- c("#CDAF95", "#53868B")
pal_scatter <- setNames(pal_scatter, c( "EstGigas", "Offshore"))


##CORNEAW.EW.CL##
#boxplot
CorneaW.EW.CL.boxplot <- ggplot(newdata, aes(x=Population, y=CorneaW.EW.CL, fill=Population)) + geom_boxplot(fill = fill_boxplots,colour= outline_boxplots)

ggplotly(CorneaW.EW.CL.boxplot)

#levene
leveneTest(CorneaW.EW.CL ~ Population, data = newdata)
#.3969#do not reject the null hypothesis that the variances are equal #good to go equal variance

#ANOVA
ANVOA.Cornea.EW.CL <- aov(CorneaW.EW.CL ~ Population, data = newdata)
summary(ANVOA.Cornea.EW.CL)
#(F(2,77) = 11.2, p = 5.39e^-05)

#post-hoc
TukeyHSD(ANVOA.Cornea.EW.CL, conf.level=.95) 

#EstGigas-EstCaliforniensis 0.0000401; Offshore-EstCaliforniensis 0.0682400; Offshore-EstGigas 0.0179958



##EyestalkLengthCarapaceLength.boxplot##

CorneaA.EW.CL.boxplot <- ggplot(completeneomeasurements, aes(x=Population, y=EyestalkLength, fill=Population)) + geom_boxplot(fill = fill_boxplots,colour= outline_boxplots)

ggplotly(CorneaA.EW.CL.boxplot)


##CorneaA.EW.CL.boxplot##

CorneaA.EW.CL.boxplot <- ggplot(newdata, aes(x=Population, y=CorneaA.EW.CL, fill=Population)) + geom_boxplot(fill = fill_boxplots,colour= outline_boxplots)

ggplotly(CorneaA.EW.CL.boxplot)


##ConcaveA.boxplot##

EyestalkA.boxplot <- ggplot(newdata, aes(x=Population, y=`Concave Angle`, fill=Population)) + geom_boxplot(fill = fill_boxplots,colour= outline_boxplots)

ggplotly(EyestalkA.boxplot)


##CorneaWidth.EW##
CorneaW.EW.boxplot <- ggplot(newdata, aes(x=Population, y=CorneaW.EW, fill=Population)) +geom_boxplot(fill = fill_boxplots,colour= outline_boxplots)

ggplotly(CorneaW.EW.boxplot)

##EyestalkLength##
EyestalkL.boxplot <- ggplot(newdata, aes(x=Population, y=EyestalkLength/CL, fill=Population)) +geom_boxplot(fill = fill_boxplots,colour= outline_boxplots)

ggplotly(EyestalkL.boxplot)


##WL2PollexL.CL##
WL2PollexL.cL.boxplot <- ggplot(newdata, aes(x=Population, y=WL2PollexL.CL, fill=Population)) +geom_boxplot(fill = fill_boxplots,colour= outline_boxplots)

ggplotly(WL2PollexL.cL.boxplot)



##WL2MerusW.CL##
WL2MerusW.cL.boxplot <- ggplot(newdata, aes(x=Population, y=MerusW.CL, fill=Population)) +geom_boxplot(fill = fill_boxplots,colour= outline_boxplots)

ggplotly(WL2MerusW.cL.boxplot)


##WL2PropodusL.CL##
WL2PropodusL.CL.boxplot <- ggplot(newdata, aes(x=Population, y=WL2PropodusL.CL, fill=Population)) +geom_boxplot(fill = fill_boxplots,colour= outline_boxplots)

ggplotly(WL2PropodusL.CL.boxplot)



##WL1PollexH.DactylH.CL##
WL1CarpusL.CL.boxplot <- ggplot(newdata, aes(x=Population, y=WL1CarpusH.CL, fill=Population)) +geom_boxplot(fill = fill_boxplots,colour= outline_boxplots)

ggplotly(WL1CarpusL.CL.boxplot)

##WL1PollexH.DactylH.CL##
WL1PollexH.DactylH.CL.boxplot <- ggplot(newdata, aes(x=Population, y=WL1PollexH.DactylH.CL, fill=Population)) +geom_boxplot(fill = fill_boxplots,colour= outline_boxplots)

ggplotly(WL1PollexH.DactylH.CL.boxplot)

##MiCPropodusL.CL##
MiCPropodusL.CL.boxplot <- ggplot(newdata, aes(x=Population, y=MiCPropodusL.CL, fill=Population)) +geom_boxplot(fill = fill_boxplots,colour= outline_boxplots)

ggplotly(MiCPropodusL.CL.boxplot)

##MiCPropodusH.CL##
MiCPropodusH.CL.boxplot <- ggplot(newdata, aes(x=Population, y=MiCPropodusH.CL, fill=Population)) +geom_boxplot(fill = fill_boxplots,colour= outline_boxplots)

ggplotly(MiCPropodusH.CL.boxplot)




##MaClawCleftWidth##
MaCCleftWidth.CL.boxplot <- ggplot(newdata, aes(x=Population, y=MaCCleftWidth.CL, fill=Population)) +geom_boxplot(fill = fill_boxplots,colour= outline_boxplots)

ggplotly(MaCCleftWidth.CL.boxplot)



##MaClawCleftWidth##
MaCCleftWidth.CL.boxplot <- ggplot(newdata, aes(x=Population, y=MaCCleftWidth.CL, fill=Population)) +geom_boxplot(fill = fill_boxplots,colour= outline_boxplots)

ggplotly(MaCCleftWidth.CL.boxplot)

##MaCGap.cL##
MaCGap.cL.boxplot <- ggplot(newdata, aes(x=Population, y=MaCGap.CL, fill=Population)) +geom_boxplot(fill = fill_boxplots,colour= outline_boxplots)

ggplotly(MaCGap.cL.boxplot)
leveneTest(MaCGap.CL ~ Population, data = datafinal)
#p was 0.2012 

dPollexDactylGap/CL


#levene
leveneTest(WL1PollexH.DactylH.CL ~ Population, data = newdata)
#0.6508#do not reject the null hypothesis that the variances are equal #good to go equal variance

ttest(WL1PollexH.DactylH.CL ~ Population, data = newdata)4



for (i in 7:78)
{
  summary(aov(i ~ Population, data = datafinal))
  
}


#ANOVA
AOV1  <- aov(EyestalkLength/CL ~ Population, data = datafinal)
summary(AOV1)

TukeyHSD(AOV1, conf.level=.95) 


#ANOVA
AOV2  <- aov(EyestalkW/CL ~ Population, data = datafinal)
summary(AOV2)

TukeyHSD(AOV2, conf.level=.95) 


#ANOVA
AOV3  <- aov(`Concave Angle` ~ Population, data = datafinal)
summary(AOV3)
TukeyHSD(AOV3, conf.level=.95) 


#ANOVA
AOV4  <- aov(`Eyestalk Gap`/CL ~ Population, data = datafinal)
summary(AOV4)
TukeyHSD(AOV4, conf.level=.95) 


#ANOVA
AOV5  <- aov(WL1PollexH.DactylH.CL ~ Population, data = datafinal)
summary(AOV1)
TukeyHSD(AOV3, conf.level=.95) 


#ANOVA
AOV6  <- aov(CorneaA.EW.CL ~ Population, data = datafinal)
summary(AOV6)
TukeyHSD(AOV6, conf.level=.95) 


#ANOVA
AOV7  <- aov(CorneaW.EW.CL ~ Population, data = datafinal)
summary(AOV7)
TukeyHSD(AOV7, conf.level=.95) 


#WalkingLeg
#ANOVA
AOV8  <- aov(WL1PropodusL.CL ~ Population, data = datafinal)
summary(AOV8)
TukeyHSD(AOV8, conf.level=.95) 


#ANOVA
AOV9 <- aov(WL1Gap.CL ~ Population, data = datafinal)
summary(AOV9)
TukeyHSD(AOV9, conf.level=.95) 

#ANOVA
AOV10 <- aov(WL1CarpusH.CL ~ Population, data = datafinal)
summary(AOV10)
TukeyHSD(AOV10, conf.level=.95) 


AOV11 <- aov(WL1PollexH.DactylH.CL ~ Population, data = datafinal)
summary(AOV11)
TukeyHSD(AOV11, conf.level=.95) 


AOV12 <- aov(WL1CarpusA ~ Population, data = datafinal)
summary(AOV12)
TukeyHSD(AOV12, conf.level=.95) 

AOV13 <- aov(WL1CarpusA ~ Population, data = datafinal)
summary(AOV12)
TukeyHSD(AOV12, conf.level=.95) 


AOV14 <- aov(MiCPropodusL.CL ~ Population, data = datafinal)
summary(AOV14)
TukeyHSD(AOV14, conf.level=.95) 

AOV15 <- aov(MiCPropodusH.CL ~ Population, data = datafinal)
summary(AOV15)
TukeyHSD(AOV15, conf.level=.95) 

AOV16 <- aov(MiCCarpusH.CL ~ Population, data = datafinal)
summary(AOV16)
TukeyHSD(AOV16, conf.level=.95) 

AOV17 <- aov(MiCCarpusL.CL ~ Population, data = datafinal)
summary(AOV17)
TukeyHSD(AOV17, conf.level=.95) 

AOV18 <- aov(MiCDactylH.CL ~ Population, data = datafinal)
summary(AOV18)
TukeyHSD(AOV18, conf.level=.95) 

AOV19 <- aov(MiCPollexH.CL ~ Population, data = datafinal)
summary(AOV19)
TukeyHSD(AOV19, conf.level=.95) 

AOV20 <- aov(MiCMerusH.CL ~ Population, data = datafinal)
summary(AOV20)
TukeyHSD(AOV20, conf.level=.95) 

AOV21 <- aov(PropodusH.CL ~ Population, data = datafinal)
summary(AOV21)
TukeyHSD(AOV21, conf.level=.95) 

AOV22 <- aov(WL2PollexL.CL ~ Population, data = datafinal)
summary(AOV22)
TukeyHSD(AOV22, conf.level=.95) 

AOV23 <- aov(WL2PollexW.CL ~ Population, data = datafinal)
summary(AOV23)
TukeyHSD(AOV23, conf.level=.95) 

AOV24 <- aov(WL2CarpusW.CL ~ Population, data = datafinal)
summary(AOV24)
TukeyHSD(AOV24, conf.level=.95) 

AOV25 <- aov(WL2CarpusL.CL ~ Population, data = datafinal)
summary(AOV25)
TukeyHSD(AOV25, conf.level=.95) 

AOV26 <- aov(MerusW.CL ~ Population, data = datafinal)
summary(AOV26)
TukeyHSD(AOV26, conf.level=.95) 

AOV27 <- aov(MaCPropodusL.CL ~ Population, data = datafinal)
summary(AOV27)
TukeyHSD(AOV27, conf.level=.95) 

AOV28 <- aov(MaCPropodusL.PropodusH ~ Population, data = datafinal)
summary(AOV28)
TukeyHSD(AOV28, conf.level=.95) 

AOV29 <- aov(MaCCleftWidth.CL ~ Population, data = datafinal)
summary(AOV29)
TukeyHSD(AOV29, conf.level=.95) 

AOV30 <- aov(MaCCarpusH.CL ~ Population, data = datafinal)
summary(AOV30)
TukeyHSD(AOV30, conf.level=.95) 

AOV31 <- aov(MaCCarpuL.CL ~ Population, data = datafinal)
summary(AOV31)
TukeyHSD(AOV31, conf.level=.95) 

AOV32 <- aov(MaCPollexH.CL ~ Population, data = datafinal)
summary(AOV32)
TukeyHSD(AOV32, conf.level=.95) 

AOV33 <- aov(MaCDactyl.CL ~ Population, data = datafinal)
summary(AOV33)
TukeyHSD(AOV33, conf.level=.95) 

AOV35 <- aov(`MaCPollexDactyl Gap`/CL ~ Population, data = datafinal)
summary(AOV35)
TukeyHSD(AOV35, conf.level=.95) 

#post-hoc
TukeyHSD(ANOVA.WL1PollexH.DactylH.CL, conf.level=.95) 
#all greater than .05



WL1PropodusL.CL.boxplot <- g
gplot(newdata, aes(x=Population, y=WL1PropodusL.CL, fill=Population)) +geom_boxplot(fill = fill_boxplots,colour= outline_boxplots)

ggplotly(WL1PropodusL.CL.boxplot)



#ANOVA
ANOVA.WL1PropodusL.CL  <- aov(WL1PropodusL.CL ~ Population, data = newdata)
summary(ANOVA.WL1PropodusL.CL)
#(F(2,38) = 0.462  , p = 0.634)

#post-hoc
TukeyHSD(ANOVA.WL1PropodusL.CL, conf.level=.95) 
#all greater than .05



##MajorClawCleftWidth##
MaCCleftWidthL.boxplot <- ggplot(newdata, aes(x=Population, y=MaCCleftWidth.CL, fill=Population)) +geom_boxplot(fill = fill_boxplots,colour= outline_boxplots)


ggplotly(MaCCleftWidthL.boxplot)



##MaCGap.CL##
MaCGap.CL.boxplot <- ggplot(newdata, aes(x=Population, y=MaCGap.CL, fill=Population)) +geom_boxplot(fill = fill_boxplots,colour= outline_boxplots)


ggplotly(MaCGap.CL.boxplot)





##CorneaWidthEWCLScatter

CorneaW.EW.CL.Scatter <- plot_ly(data = newdata_nocali, x = ~CL, y = ~CorneaW.EW, color = ~Population, colors = pal_scatter)%>%
  layout(plot_bgcolor='gray96',
         xaxis = list(
           title='Carapace Length (mm)', font = list(size = 20),
           ,
           zerolinecolor = '#ffff',
           zerolinewidth = 2,
           gridcolor = 'ffff'),
         yaxis = list(
           title='Cornea Width / Eyestalk Width',
           font = list(size = 20),
           zerolinecolor = '#ffff',
           zerolinewidth = 2,
           gridcolor = 'ffff'))

CorneaW.EW.CL.Scatter

##LDA
LDA_CorneaWEyestalkW <- lda(Population~CorneaW.EW, data = newdata_nocali)



#boxplot CorneaA.EW.CarapaceLength
boxplot(CorneaA.EW.CL~Population,data=newdata,cex.axis=1,cex.lab=1,range=1.5,col="grey", 
        xlab="Population",ylab="CorneaArea/EyestalkWidth/CarapaceLength", 
        pars=list(whisklty=1,boxwex=0.7)) 

#boxplot CorneaW.EW.CarapaceLength
boxplot(CorneaW.EW.CL~Population,data=newdata,cex.axis=1,cex.lab=1,range=1.5,col="grey", 
        xlab="Population",ylab="CorneaWidth.EW.CL", 
        pars=list(whisklty=1,boxwex=0.7)) 

#boxplot CorneaW.EW
boxplot(CorneaW.EW~Population,data=newdata,cex.axis=1,cex.lab=1,range=1.5,col="grey", 
        xlab="Population",ylab="CorneaWidth/EyestalkWidth", 
        pars=list(whisklty=1,boxwex=0.7)) 

#boxplot CorneaA
boxplot(CorneaA~Population,data=newdata,cex.axis=1,cex.lab=1,range=1.5,col="grey", 
        xlab="Population",ylab="CorneaArea", 
        pars=list(whisklty=1,boxwex=0.7)) 

#boxplot CorneaW
boxplot(CorneaW~Population,data=newdata,cex.axis=1,cex.lab=1,range=1.5,col="grey", 
        xlab="Population",ylab="CorneaWidth", 
        pars=list(whisklty=1,boxwex=0.7)) 


##Discriminatory Functions##


data<-read.morphodata('neomorphotools.txt') 
str(data) 
summary(data) 
newdata<-data
newdata<-na.omit(data) 
histchar(data) 

shapiroWilkTest(newdata)
shapiroWilkTest(newdatatransform)
histAll(newdata, folderName = "histograms")
qqnormAll(newdata, folderName = "qqnormPlots")
newdatatransform = transformCharacter(newdata, character = "EyestalkW", newName = "EyestalkW.sqrt",
                                      FUN = function(x) log10((100*max(x)+1)-x))




##example how to describe the statistics ## 
#A one-way ANOVA was conducted to examine the effects of exercise program on weight loss (measured in pounds).
#There was a statistically significant difference between the effects of the three programs on weight 
#loss (F(2, 87) = 30.83, p = 7.55e-11). Tukey’s HSD post hoc tests were carried out.