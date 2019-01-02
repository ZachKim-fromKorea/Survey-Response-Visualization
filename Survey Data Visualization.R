####################### Survey Response Visualization ########################

##```{r setup_ bring in packages, include=FALSE}
## Changing R Repository
(r = getOption("repos")) # CRAN mirror
r["CRAN"] = "http://cran.seoul.go.kr" # If you live in Korea you can choose btw Seoul & Ulsan
options(repos = r)
rm(r)

## import packages
library(ggplot2)
attach(data)

##```{r vis_ create data}
##```{r function to draw pie chart}
pieChart <- function(data = data, question){
	response <- data[,question]
	counts <- as.data.frame(table(response))	# ggplot 안에는 data frame이 들어가야 한다. 
	barPlot <- ggplot(counts, aes(x="Response", y=Freq, fill=response))+
		geom_bar(stat="identity")
	Pie <- barPlot+ coord_polar("y", start=0)+
		labs(title=colnames(data)[question])+
		theme(legend.position = "bottom")
Pie
}

##```{r function to draw stacked barplot}
stackedBarPlot <- function(data = data, interested.question = vector){
	table.data <- data.frame("Response" = c(), "Question.No"=c())
	for(i in interested.question){
		interested.column <- i
		interested.data <- data[,interested.column]
		interim.data <- data.frame("Response" = data[,interested.column], "Question.No"=rep(paste("Question",interested.column,sep=" "),dim(data)[1]))
		table.data <- rbind(table.data, interim.data)
	}
	barPlot <- ggplot(table.data, aes(Question.No))
	barPlot + geom_bar(aes(fill=Response), position=position_stack(reverse=TRUE)) +
		coord_flip() + 
		theme(legend.position = "top")
}

##```{r calculate percentage for each choices}
## Calculate percentage for choices
choicePct <- function(data = data, interested.question = vector){
	for(i in interested.question){
	cat("----------------------------------------------------------------","\n",
	paste("n =", dim(data)[1],",","p =",length(unique(data[,i]))),"\n",
	paste("Question : ",colnames(data)[i]),"\n")
	choices <- unique(data[,i])
		for(j in 1:length(choices)){
			cat(paste("Percentage(%) of ",choices[j], "=", round(sum(data[,i]==choices[j])/dim(data)[1]*100,1),"(",sum(data[,i]==choices[j]),"responses)"),"\n")
		}
	}
}
