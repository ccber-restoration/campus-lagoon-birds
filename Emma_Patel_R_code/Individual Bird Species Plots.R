library(tidyverse)
library(ggplot2)
setwd("/Users/.../Desktop") #set location of csv
bird <- read.csv("bird_data.csv", header=TRUE,)
north <- bird[,7]
lagoon <- bird[,12]
west_lagoon <- bird[,18]
south_beach <- bird[,23]
lagoon_island <- bird[,28]
campus_point <- bird[,33]
east_margin <- bird[,38]
totals <- bird[,41]
date <- bird$Date
month <- bird$Month
year <- bird$Year
species <- bird$Species

#American Coot
aa <- subset(bird, Species == "American Coot")
mon <- factor(aa$Month, levels=month.name)
yr <- as.factor(aa$Year)
ggplot(aa, aes(x=mon, y=aa$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="American Coot")

#American Crow
ac <- subset(bird, Species == "American Crow")
mon <- factor(ac$Month, levels=month.name)
yr <- as.factor(ac$Year)
ggplot(ac, aes(x=mon, y=ac$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="American Crow")

#American Kestrel
ak <- subset(bird, Species == "American Kestrel")
mon <- factor(ak$Month, levels=month.name)
yr <- as.factor(ak$Year)
ggplot(ak, aes(x=mon, y=ak$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="American Kestrel")

#American Pipit
ap <- subset(bird, Species == "American Pipit")
mon <- factor(ap$Month, levels=month.name)
yr <- as.factor(ap$Year)
ggplot(ap, aes(x=mon, y=ap$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="American Pipit")

#American Wigeon
aw <- subset(bird, Species == "American Wigeon")
mon <- factor(aw$Month, levels=month.name)
yr <- as.factor(aw$Year)
ggplot(aw, aes(x=mon, y=aw$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="American Wigeon")

#Anna's Hummingbird
ah <- subset(bird, Species == "Anna's Hummingbird")
mon <- factor(ah$Month, levels=month.name)
yr <- as.factor(ah$Year)
ggplot(ah, aes(x=mon, y=ah$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Anna's Hummingbird")

#Beldings Savannah Sparrow
b <- subset(bird, Species == "Beldings Savannah Sparrow")
mon <- factor(b$Month, levels=month.name)
yr <- as.factor(b$Year)
ggplot(b, aes(x=mon, y=b$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Beldings Savannah Sparrow")

#Belted Kingfisher
b <- subset(bird, Species == "Belted Kingfisher")
mon <- factor(b$Month, levels=month.name)
yr <- as.factor(b$Year)
ggplot(b, aes(x=mon, y=b$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Belted Kingfisher")

#Bewick's Wren
b <- subset(bird, Species == "Bewick's Wren")
mon <- factor(b$Month, levels=month.name)
yr <- as.factor(b$Year)
ggplot(b, aes(x=mon, y=b$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Bewicks Wren")

#Black Phoebe
b <- subset(bird, Species == "Black Phoebe")
mon <- factor(b$Month, levels=month.name)
yr <- as.factor(b$Year)
ggplot(b, aes(x=mon, y=b$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Black Phoebe")

#Black Turnstone
b <- subset(bird, Species == "Black Turnstone")
mon <- factor(b$Month, levels=month.name)
yr <- as.factor(b$Year)
ggplot(b, aes(x=mon, y=b$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Black Turnstone")

#Black-bellied Plover
b <- subset(bird, Species == "Black-bellied Plover")
mon <- factor(b$Month, levels=month.name)
yr <- as.factor(b$Year)
ggplot(b, aes(x=mon, y=b$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Black Bellied Plover")

#Black-crowned Night Heron
b <- subset(bird, Species == "Black-crowned Night Heron")
mon <- factor(b$Month, levels=month.name)
yr <- as.factor(b$Year)
ggplot(b, aes(x=mon, y=b$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Black Crowned Night Heron")

#Black-necked Stilt
b <- subset(bird, Species == "Black-necked Stilt")
mon <- factor(b$Month, levels=month.name)
yr <- as.factor(b$Year)
ggplot(b, aes(x=mon, y=b$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Black Necked Stilt")

#Blue-gray Gnatcatcher
b <- subset(bird, Species == "Blue-gray Gnatcatcher")
mon <- factor(b$Month, levels=month.name)
yr <- as.factor(b$Year)
ggplot(b, aes(x=mon, y=b$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Blue Gray Gnatcatcher")

#Bonparte's Gull
b <- subset(bird, Species == "Bonparte's Gull")
mon <- factor(b$Month, levels=month.name)
yr <- as.factor(b$Year)
ggplot(b, aes(x=mon, y=b$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Bonpartes Gull")

#Brandt's Commorant
b <- subset(bird, Species == "Brandts Commorant")
mon <- factor(b$Month, levels=month.name)
yr <- as.factor(b$Year)
ggplot(b, aes(x=mon, y=b$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Brandts Commorant")

#Brown Pelican
b <- subset(bird, Species == "Brown Pelican")
mon <- factor(b$Month, levels=month.name)
yr <- as.factor(b$Year)
ggplot(b, aes(x=mon, y=b$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Brown Pelican")

#Brown-headed Cowbird
b <- subset(bird, Species == "Brown-headed Cowbird")
mon <- factor(b$Month, levels=month.name)
yr <- as.factor(b$Year)
ggplot(b, aes(x=mon, y=b$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Brown headed Cowbird")

#Bufflehead
b <- subset(bird, Species == "Bufflehead")
mon <- factor(b$Month, levels=month.name)
yr <- as.factor(b$Year)
ggplot(b, aes(x=mon, y=b$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Bufflehead")

#Bushtit
b <- subset(bird, Species == "Bushtit")
mon <- factor(b$Month, levels=month.name)
yr <- as.factor(b$Year)
ggplot(b, aes(x=mon, y=b$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Bushtit")

#CA Thrasher
c <- subset(bird, Species == "CA Thrasher")
mon <- factor(c$Month, levels=month.name)
yr <- as.factor(c$Year)
ggplot(c, aes(x=mon, y=c$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="California Thrasher")

#California Towhee
c <- subset(bird, Species == "California Towhee")
mon <- factor(c$Month, levels=month.name)
yr <- as.factor(c$Year)
ggplot(c, aes(x=mon, y=c$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="California Towhee")

#Cassin's Kingbird
c <- subset(bird, Species == "Cassin's Kingbird")
mon <- factor(c$Month, levels=month.name)
yr <- as.factor(c$Year)
ggplot(c, aes(x=mon, y=c$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Cassins Kingbird")

#Common Yellowthroat
c <- subset(bird, Species == "Common Yellowthroat")
mon <- factor(c$Month, levels=month.name)
yr <- as.factor(c$Year)
ggplot(c, aes(x=mon, y=c$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Common Yellowthroat")

#Double-crested Cormorant
d <- subset(bird, Species == "Double-crested Cormorant")
mon <- factor(d$Month, levels=month.name)
yr <- as.factor(d$Year)
ggplot(d, aes(x=mon, y=d$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Double Crested Cormorant")

#Eared Grebe
d <- subset(bird, Species == "Eared Grebe")
mon <- factor(d$Month, levels=month.name)
yr <- as.factor(d$Year)
ggplot(d, aes(x=mon, y=d$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Eared Grebe")

#European Starling
d <- subset(bird, Species == "European Starling")
mon <- factor(d$Month, levels=month.name)
yr <- as.factor(d$Year)
ggplot(d, aes(x=mon, y=d$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="European Starling")

#Golden-crowned Sparrow
d <- subset(bird, Species == "Golden-crowned Sparrow")
mon <- factor(d$Month, levels=month.name)
yr <- as.factor(d$Year)
ggplot(d, aes(x=mon, y=d$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Goldren Crowned Sparrow")

#Great Egret
d <- subset(bird, Species == "Great Egret")
mon <- factor(d$Month, levels=month.name)
yr <- as.factor(d$Year)
ggplot(d, aes(x=mon, y=d$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Great Egret")

#Greater Yellowlegs
d <- subset(bird, Species == "Greater Yellowlegs")
mon <- factor(d$Month, levels=month.name)
yr <- as.factor(d$Year)
ggplot(d, aes(x=mon, y=d$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Greater Yellowlegs")

#House Finch
d <- subset(bird, Species == "House Finch")
mon <- factor(d$Month, levels=month.name)
yr <- as.factor(d$Year)
ggplot(d, aes(x=mon, y=d$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="House Finch")

#House Wren
d <- subset(bird, Species == "House Wren")
mon <- factor(d$Month, levels=month.name)
yr <- as.factor(d$Year)
ggplot(d, aes(x=mon, y=d$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="House Wren")

#Killdeer
d <- subset(bird, Species == "Killdeer")
mon <- factor(d$Month, levels=month.name)
yr <- as.factor(d$Year)
ggplot(d, aes(x=mon, y=d$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Killdeer")

#Least Sandpiper
d <- subset(bird, Species == "Least Sandpiper")
mon <- factor(d$Month, levels=month.name)
yr <- as.factor(d$Year)
ggplot(d, aes(x=mon, y=d$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Least Sandpiper")

#Lesser Scaup
d <- subset(bird, Species == "Lesser Scaup")
mon <- factor(d$Month, levels=month.name)
yr <- as.factor(d$Year)
ggplot(d, aes(x=mon, y=d$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Lesser Scaup")

#Long-billed Dowitcher
d <- subset(bird, Species == "Long-billed Dowitcher")
mon <- factor(d$Month, levels=month.name)
yr <- as.factor(d$Year)
ggplot(d, aes(x=mon, y=d$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Long Billed Dowitcher")

#Mallard
mal <- subset(bird, Species == "Mallard")
mon <- factor(mal$Month, levels=month.name)
yr <- as.factor(mal$Year)
ggplot(mal, aes(x=mon, y=mal$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Mallard")

#Marbled Godwit
mal <- subset(bird, Species == "Marbled Godwit")
mon <- factor(mal$Month, levels=month.name)
yr <- as.factor(mal$Year)
ggplot(mal, aes(x=mon, y=mal$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Marbled Godwit")

#Northern Mockingbird
mal <- subset(bird, Species == "Northern Mockingbird")
mon <- factor(mal$Month, levels=month.name)
yr <- as.factor(mal$Year)
ggplot(mal, aes(x=mon, y=mal$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Northern Mockingbird")

#Orange-crowned Warbler
mal <- subset(bird, Species == "Orange-crowned Warbler")
mon <- factor(mal$Month, levels=month.name)
yr <- as.factor(mal$Year)
ggplot(mal, aes(x=mon, y=mal$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Orange Crowned Warbler")

#Pied-billed Grebe
mal <- subset(bird, Species == "Pied-billed Grebe")
mon <- factor(mal$Month, levels=month.name)
yr <- as.factor(mal$Year)
ggplot(mal, aes(x=mon, y=mal$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Pied Billed Grebe")

#Red-breasted Merganser
mal <- subset(bird, Species == "Red-breasted Merganser")
mon <- factor(mal$Month, levels=month.name)
yr <- as.factor(mal$Year)
ggplot(mal, aes(x=mon, y=mal$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Red Breasted Merganser")

#Red-tailed Hawk
mal <- subset(bird, Species == "Red-tailed Hawk")
mon <- factor(mal$Month, levels=month.name)
yr <- as.factor(mal$Year)
ggplot(mal, aes(x=mon, y=mal$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Red Tailed Hawk")

#Red-winged Blackbird
mal <- subset(bird, Species == "Red-winged Blackbird")
mon <- factor(mal$Month, levels=month.name)
yr <- as.factor(mal$Year)
ggplot(mal, aes(x=mon, y=mal$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Red Winged Blackbird")

#Redhead
mal <- subset(bird, Species == "Redhead")
mon <- factor(mal$Month, levels=month.name)
yr <- as.factor(mal$Year)
ggplot(mal, aes(x=mon, y=mal$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Redhead")

#Ring-billed Gull
mal <- subset(bird, Species == "Ring-billed Gull")
mon <- factor(mal$Month, levels=month.name)
yr <- as.factor(mal$Year)
ggplot(mal, aes(x=mon, y=mal$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Ring Billed Gull")

#Ruby-crowned Kinglet
mal <- subset(bird, Species == "Ruby-crowned Kinglet")
mon <- factor(mal$Month, levels=month.name)
yr <- as.factor(mal$Year)
ggplot(mal, aes(x=mon, y=mal$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Ruby Crowned Kinglet")

#Ruddy Duck
mal <- subset(bird, Species == "Ruddy Duck")
mon <- factor(mal$Month, levels=month.name)
yr <- as.factor(mal$Year)
ggplot(mal, aes(x=mon, y=mal$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Ruddy Duck")

#Sanderling
mal <- subset(bird, Species == "Sanderling")
mon <- factor(mal$Month, levels=month.name)
yr <- as.factor(mal$Year)
ggplot(mal, aes(x=mon, y=mal$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Sanderling")

#Say's Phoebe
mal <- subset(bird, Species == "Say's Phoebe")
mon <- factor(mal$Month, levels=month.name)
yr <- as.factor(mal$Year)
ggplot(mal, aes(x=mon, y=mal$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Says Phoebe")

#Semipalmated Plover
mal <- subset(bird, Species == "Semipalmated Plover")
mon <- factor(mal$Month, levels=month.name)
yr <- as.factor(mal$Year)
ggplot(mal, aes(x=mon, y=mal$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Semipalmated Plover")

#Snowy Egret
mal <- subset(bird, Species == "Snowy Egret")
mon <- factor(mal$Month, levels=month.name)
yr <- as.factor(mal$Year)
ggplot(mal, aes(x=mon, y=mal$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Snowy Egret")

#Song Sparrow
mal <- subset(bird, Species == "Song Sparrow")
mon <- factor(mal$Month, levels=month.name)
yr <- as.factor(mal$Year)
ggplot(mal, aes(x=mon, y=mal$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Song Sparrow")

#Townsend's Warbler
mal <- subset(bird, Species == "Townsend's Warbler")
mon <- factor(mal$Month, levels=month.name)
yr <- as.factor(mal$Year)
ggplot(mal, aes(x=mon, y=mal$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Townsend's Warbler")

#Western Grebe
mal <- subset(bird, Species == "Western Grebe")
mon <- factor(mal$Month, levels=month.name)
yr <- as.factor(mal$Year)
ggplot(mal, aes(x=mon, y=mal$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Western Grebe")

#Western Gull
mal <- subset(bird, Species == "Western Gull")
mon <- factor(mal$Month, levels=month.name)
yr <- as.factor(mal$Year)
ggplot(mal, aes(x=mon, y=mal$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Western Gull")

#Whimbrel
mal <- subset(bird, Species == "Whimbrel")
mon <- factor(mal$Month, levels=month.name)
yr <- as.factor(mal$Year)
ggplot(mal, aes(x=mon, y=mal$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Whimbrel")

#White-crowned Sparrow
mal <- subset(bird, Species == "White-crowned Sparrow")
mon <- factor(mal$Month, levels=month.name)
yr <- as.factor(mal$Year)
ggplot(mal, aes(x=mon, y=mal$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="White Crowned Sparrow")

#Willet
mal <- subset(bird, Species == "Willet")
mon <- factor(mal$Month, levels=month.name)
yr <- as.factor(mal$Year)
ggplot(mal, aes(x=mon, y=mal$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Willet")

#Yellow-rumped Warbler
mal <- subset(bird, Species == "Yellow-rumbed Warbler")
mon <- factor(mal$Month, levels=month.name)
yr <- as.factor(mal$Year)
ggplot(mal, aes(x=mon, y=mal$Lagoon.Management.Area.Totals))+
  geom_point(aes(color=yr))+
  labs(x="Month", y="Total Number", title="Yellow Rumped Warbler")

### Animated Plot of total species by month over the years
## ImageMagick must be installed on the machine through Homebrew or Linux for gganimate to work
#library(ggplot2)
#library(stats)
#install.packages("cowplot")  #gganimate dependency
#devtools::install_github("talgalili/installr")
#knitr::opts_chunk$set(message = FALSE, warning = FALSE, fig.show = "animate")
#devtools::install_github("dgrtwo/gganimate")
#library(cowplot)
#library(gganimate)
#month <- bird$Month
#year <- bird$Year
#plot <- ggplot(bird, aes(x=month, y=totals, frame=year))+
#  geom_point(aes(color=species))
#gganimate(plot) #animation of the plot