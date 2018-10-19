library(ggplot2)
library(plyr)
library(scales)

# Reading the tsv file
ufo<-read.delim("data/ufo_sightings.tsv",
	sep="\t",
	stringsAsFactors=FALSE,
	header=FALSE,
	na.strings="")

# Assigning the column names
names(ufo)<-c("DateOccurred", "DateReported",
	"Location", "ShortDescription",
	"Duration", "LongDescription")

summary(ufo)
head(ufo)

print("Cleaning date columns...\n")

# Filtering rows with valid date strings
good.rows<-ifelse(
	nchar(ufo$DateOccurred)!=8 | nchar(ufo$DateReported)!=8,
	FALSE,
	TRUE)

length(which(!good.rows)) # 731 [less than 1.18% data is bad]
ufo<-ufo[good.rows,]

# Converting the date columns from string to Date
ufo$DateOccurred<-as.Date(ufo$DateOccurred, format="%Y%m%d")
ufo$DateReported<-as.Date(ufo$DateReported, format="%Y%m%d")
head(ufo)

print("Cleaning location data...\n")
# Splitting location into city and state
get.location<-function(line) {
	split.location<-tryCatch(strsplit(line, ",")[[1]], error=function(e) return(c(NA, NA)))
	clean.location<-gsub("^ ", "", split.location)
	if(length(clean.location)>2) {
		return(c(NA, NA))
	} else {
		return(clean.location)
	}
}

city.state<-lapply(ufo$Location, get.location)
head(city.state)

# Making a location matrix with City x State as the dimensions
location.matrix<-do.call(rbind, city.state)
ufo<-transform(ufo,
	USCity=location.matrix[,1],
	USState=tolower(location.matrix[,2]),
	stringsAsFactors=FALSE)
head(ufo)

# Converting all the states to lower cases for consistency
us.states<-c("ak","al","ar","az","ca","co","ct","de","fl","ga","hi","ia","id","il",
	"in","ks","ky","la","ma","md","me","mi","mn","mo","ms","mt","nc","nd","ne","nh",
	"nj","nm","nv","ny","oh","ok","or","pa","ri","sc","sd","tn","tx","ut","va","vt",
	"wa","wi","wv","wy")
ufo$USState<-us.states[match(ufo$USState,us.states)]
ufo$USCity[is.na(ufo$USState)]<-NA

print("Printing US subset...\n")
# Filtering the records for US states only
ufo.us<-subset(ufo, !is.na(USState))
head(ufo.us)
summary(ufo.us$DateOccurred)

# Plotting the histogram
quick.hist<-ggplot(ufo.us, aes(x=DateOccurred)) +
	geom_histogram() +
	scale_x_date(breaks="50 years", labels=date_format('%Y'))
dir.create("images")

# changed the height and width to avoid overlapping of x-labels
ggsave(plot = quick.hist,
       filename = file.path("images", "quick_hist.pdf"),
       height = 12,
       width = 16)

# INFERENCE: majority of sightings occurred between 1960
# and 2010 therefore we create a subset of the dataframe


ufo.us<-subset(ufo.us, DateOccurred>=as.Date("1990-01-01"))
nrow(ufo.us)

new.quick.hist<-ggplot(ufo.us, aes(x=DateOccurred)) +
	geom_histogram() +
	scale_x_date(breaks="50 years", labels=date_format('%Y'))
dir.create("images")

# changed the height and width to avoid overlapping of x-labels
ggsave(plot = new.quick.hist,
       filename = file.path("images", "new_quick_hist.pdf"),
       height = 12,
       width = 16)

# Adding the YearMonth col. to investigate seasonality in date
ufo.us$YearMonth<-strftime(ufo.us$DateOccurred, format="%Y-%m")
head(ufo.us)

# Making a matrix of sighting frequency for a given month in a state
sightings.counts<-ddply(ufo.us,.(USState, YearMonth), nrow)
head(sightings.counts)

date.range<-seq.Date(from=as.Date(min(ufo.us$DateOccurred)), to=as.Date(max(ufo.us$DateOccurred)), by="month")
date.strings<-strftime(date.range, "%Y-%m")

length(date.strings) # 248 records
# INFERENCE: Records for zero sightings are missing

head(date.strings)

states.dates<-lapply(us.states, function(s) cbind(s, date.strings))
states.dates<-data.frame(do.call(rbind, states.dates), stringsAsFactors=FALSE)
head(states.dates)

# Now we have records for all the YearMonth
# records, zero sighting is labelled as `NA`
all.sightings<-merge(states.dates, sightings.counts, by.x=c("s", "date.strings"), by.y=c("USState", "YearMonth"), all=TRUE)
head(all.sightings)

# Assign meaningful names to the columns
# Replace `NA` with 0 in Sightings
# convert State and YearMonth to appropriate types
names(all.sightings)<-c("State", "YearMonth", "Sightings")
all.sightings$Sightings[is.na(all.sightings$Sightings)]<-0
# converting strings to `Date` objects
all.sightings$YearMonth<-as.Date(rep(date.range, length(us.states)))
# converitng states to categorical variables
all.sightings$State<-as.factor(toupper(all.sightings$State))

head(all.sightings)

state.plot<-ggplot(all.sightings, aes(x=YearMonth, y=Sightings)) +
	geom_line(aes(color="darkblue")) +
	facet_wrap(~State, nrow=10, ncol=5) +
	theme_bw() +
	scale_color_manual(values=c("darkblue"="darkblue"), guide="none") +
	scale_x_date(breaks="5 years", labels=date_format('%Y')) +
	xlab("Time") +
	ylab("Number of Sightings") +
	labs(title="No. of UFO sightings by Month-Year and U.S State (1990-2010)")

ggsave(plot=state.plot,
	file.path("images", "ufo_sightings.pdf"),
	width=14,
	height=8.5)
