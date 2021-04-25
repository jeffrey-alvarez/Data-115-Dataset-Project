library(dplyr)

YearsOnly <- bandframe[bandframe$Year != "N/A",]
YearsOnly <- mutate(YearsOnly, Year = as.integer(Year))

death <- YearsOnly[grepl("Death",YearsOnly$Genre)&!grepl("Deathcore",YearsOnly$Genre),]
black <- YearsOnly[grepl("Black",YearsOnly$Genre)&!grepl("Blackened",YearsOnly$Genre),]
doom <- YearsOnly[grepl("Doom",YearsOnly$Genre)|grepl("Sludge",YearsOnly$Genre)|grepl("Stoner",YearsOnly$Genre),]
folk <- YearsOnly[grepl("Folk",YearsOnly$Genre)|grepl("Viking",YearsOnly$Genre)|grepl("Pagan",YearsOnly$Genre),]
gothic <- YearsOnly[grepl("Gothic",YearsOnly$Genre),]
grindcore <- YearsOnly[grepl("Grindcore",YearsOnly$Genre),]
heavy <- YearsOnly[grepl("Heavy",YearsOnly$Genre),]
metalcore <- YearsOnly[grepl("Metalcore",YearsOnly$Genre)|grepl("Deathcore",YearsOnly$Genre),]
power <- YearsOnly[grepl("Power",YearsOnly$Genre),]
prog <- YearsOnly[grepl("Progressive",YearsOnly$Genre),]
thrash <- YearsOnly[grepl("Thrash",YearsOnly$Genre),]

plot(aggregate(ID~Year,FUN=length,data=death)$Year,aggregate(ID~Year,FUN=length,data=death)$ID,type="l",col="red",main="Number of Metal Bands Formed per Year by Genre",xlab="Year",ylab="Number of Bands Formed")
lines(aggregate(ID~Year,FUN=length,data=black)$Year,aggregate(ID~Year,FUN=length,data=black)$ID,col="black")
lines(aggregate(ID~Year,FUN=length,data=doom)$Year,aggregate(ID~Year,FUN=length,data=doom)$ID,col="blue")
lines(aggregate(ID~Year,FUN=length,data=folk)$Year,aggregate(ID~Year,FUN=length,data=folk)$ID,col="green")
lines(aggregate(ID~Year,FUN=length,data=gothic)$Year,aggregate(ID~Year,FUN=length,data=gothic)$ID,col="magenta")
lines(aggregate(ID~Year,FUN=length,data=grindcore)$Year,aggregate(ID~Year,FUN=length,data=grindcore)$ID,col="orange")
lines(aggregate(ID~Year,FUN=length,data=heavy)$Year,aggregate(ID~Year,FUN=length,data=heavy)$ID,col="gold")
lines(aggregate(ID~Year,FUN=length,data=metalcore)$Year,aggregate(ID~Year,FUN=length,data=metalcore)$ID,col="darkmagenta")
lines(aggregate(ID~Year,FUN=length,data=power)$Year,aggregate(ID~Year,FUN=length,data=power)$ID,col="darkgreen")
lines(aggregate(ID~Year,FUN=length,data=prog)$Year,aggregate(ID~Year,FUN=length,data=prog)$ID,col="tan4")
lines(aggregate(ID~Year,FUN=length,data=thrash)$Year,aggregate(ID~Year,FUN=length,data=thrash)$ID,col="cyan")
legend("topleft",legend=c("Death","Black","Doom/Stoner/Sludge","Folk/Viking/Pagan","Gothic","Grindcore","Heavy","Metalcore/Deathcore","Power","Progressive","Thrash"),pch="l",col=c("red","black","blue","green","magenta","orange","gold","darkmagenta","darkgreen","tan4","cyan"),cex=0.8)


DefiniteStatus <- YearsOnly[YearsOnly$Status == "Active"|YearsOnly$Status == "Split-up",]
DefiniteStatus[DefiniteStatus$Status == "Active",] <- mutate(DefiniteStatus[DefiniteStatus$Status == "Active",],Status = 1)
DefiniteStatus[DefiniteStatus$Status == "Split-up",] <- mutate(DefiniteStatus[DefiniteStatus$Status == "Split-up",],Status = 0)
DefiniteStatus <- mutate(DefiniteStatus, Status = as.integer(Status))

logistic <- glm(Status~Year, data = DefiniteStatus, family = binomial)
new <- data.frame(Year=1962:2021)
new$Status = predict(logistic, newdata=new, type="response")
plot(Status~Year,data=DefiniteStatus,main="Likelihood of Bands to Break Up by Year")
lines(Status~Year, data=new, col="red")
legend("topleft",inset=0.1,legend=c("Chance of Band Being Active"),pch="l",col=c("red"))
