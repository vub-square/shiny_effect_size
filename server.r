# shiny functionality
# retrieve input when changed
# add test dependent input 
# show specification
# provide output using functions of susanne
source("susanne.r")

## Test ----

# means <- array(c(1:12,(1:12)*2), dim =c(2,3,4))
# nrOfFactors <- 3
# cellSD <- 6

# results <- effectsMultiway(nrOfFactors, means, cellSD)

# means2 <- matrix(1:6, nrow = 2, ncol = 3)
# cellSD <- 2
# correlation <- 0.3

# results2 <- effectsRepeatedMeasures(means2, cellSD, correlation)

library(reshape2)
library(ggplot2)
server <- function(input, output) {

	###############
	# proces input

	# process input: test, within and between # levels
	getInDesign <- reactive({
		.design <- list()
		if(input$test=="two way anova"){
			validate(
				need(input$betweenmw, 'specify between group number factor levels'),
				need(input$withinmw, 'specify within group number factor levels')
			)
			.design <- list(test=input$test,wthn=as.numeric(input$withinmw),btwn=as.numeric(input$betweenmw))
		}
		if(input$test=="repeated measures anova"){
			validate(
				need(input$betweenrm, 'specify between group number factor levels'),
				need(input$withinrm, 'specify within group number factor levels')
			)
			.design <- list(test=input$test,wthn=as.numeric(input$withinrm),btwn=as.numeric(input$betweenrm))
		}
		.design
	})
	# process input: sd, cor (rm), interaction (T/F)
	getInSpecs <- reactive({
		.specs <- list()
		inDesign <- getInDesign()
		if(inDesign$test=="two way anova"){
			validate(
				need(input$stddev, 'specify pooled standard deviation'),
				need(input$xabmw, 'interaction required or not ?')
			)
		}
		if(inDesign$test=="repeated measures anova"){
			validate(
				need(input$stddev, 'specify pooled standard deviation'),
				need(input$xabrm, 'interaction required or not ?'),
				need(input$corrrm, 'specify intra-unit correlation')
			)
		}
		.means <- matrix(NA,ncol=inDesign$wthn,nrow=inDesign$btwn)
		.wlevels <- inDesign$wthn
		.blevels <- inDesign$btwn
		
		# Debugging: Print the levels and check their ranges
		#print(paste0("Within levels: ", .wlevels, ", Between levels: ", .blevels))
		
		for(j in seq_len(.wlevels)){
			for(i in seq_len(.blevels)){
			  input_value <- eval(parse(text = paste0("input$range", i, "_", j)))
			  
			  if (!is.null(input_value) && nzchar(input_value)) {
			    .means[i, j] <- as.numeric(input_value)
			  } else {
			    # Handle missing or invalid inputs
			    validate(
			      need(FALSE, paste0("Please provide input for range ", i, "_", j))
			    )
			  }
			  
				#.means[i,j] <- as.numeric(eval(parse(text=paste0("input$range",i,"_",j))))
		}}
		if(inDesign$test=="two way anova"){
			.specs <- list(means=.means,sd=as.numeric(input$stddev),xab=input$xabmw)
		}
		if(inDesign$test=="repeated measures anova"){
			.specs <- list(means=.means,sd=as.numeric(input$stddev),cor=as.numeric(input$corrrm),xab=input$xabrm)
		}
		.specs
	})
	
	#################
	# generate output

	# matrix of means (intermediate output)
	output$matrixMeans <- renderUI({
		inDesign <- getInDesign()
		lapply(seq(inDesign$wthn), function(j){
			column(width=2,
				wellPanel(			
					lapply(seq(inDesign$btwn),function(i){
						if(inDesign$test=="repeated measures anova") lbl <- paste0("b",i,"_w",j)
						if(inDesign$test=="two way anova") lbl <- paste0("a",i,"_b",j)
						textInput(inputId = paste0("range",i,"_",j),label=lbl,value=0) 
					})
				)				
			)
		})
	})
	# visualization of matrix of means
	output$plot <- renderPlot({
		inSpecs <- getInSpecs()
		.tmp <- data.frame(btw=seq(nrow(inSpecs$means)),inSpecs$means)
		names(.tmp)[-1] <- paste('t',seq(ncol(inSpecs$means)))
		.tmp <- melt(.tmp,id='btw')
		ggplot(.tmp,aes(y=value,x=variable,group=btw,color=factor(btw))) + geom_line() + theme(legend.position = "none")
	})
	# add comments
	output$comments <- renderPrint({
		cat("effects for use in sample size calculation")
	})
	# table of effect sizes (final output)
	output$tableEffects <- renderTable({
		inDesign <- getInDesign()
		inSpecs <- getInSpecs()
		if(inDesign$test=="repeated measures anova"){
			.tmp <- effectsRepeatedMeasures(inSpecs$means, inSpecs$sd, inSpecs$cor, inSpecs$xab)
			.tab <- data.frame(effect=rownames(.tmp),.tmp)
			names(.tab) <- c('effect','f','eta2','df')
		}
		if(inDesign$test=="two way anova"){
			.ttmp <- effectsMultiway(2,inSpecs$means, inSpecs$sd, inSpecs$xab)
			if(inSpecs$xab=="yes"){
				.ttmp <- rbind(data.frame(.ttmp$FmainEffects,.ttmp$mainEffects),c(.ttmp$FinteractionEffects,.ttmp$interactionEffects,.ttmp$interactionDf))
				# .ttmp <- data.frame(.ttmp)
				row.names(.ttmp) <- c("A","B","AxB")
			}
			if(inSpecs$xab=="no"){
				.ttmp <- data.frame(.ttmp$FmainEffects,.ttmp$mainEffects)
				row.names(.ttmp) <- c("A","B")
			}
			# names(.ttmp) <- c("effect","df")
			.tmp <- .ttmp
			names(.tmp) <- c("f","eta2","df")
			.tab <- data.frame(effect=dimnames(.tmp)[[1]],.tmp)
		}
		.tab <- data.frame(lapply(data.frame(.tab[,1],round(.tab[,2],4),round(.tab[,3],4),round(.tab[,4],0)),as.character),stringsAsFactors=FALSE)
		names(.tab) <- c('effect','f','eta2','df')
		.tab
	})
	
	################
	# dynamic input: within and between # levels, sd, cor, interaction (T/F)
	output$betweenrm <- renderUI({
		if(input$test=='repeated measures anova'){
			numericInput("betweenrm", "between: # levels", value = 2, min=1)
		}
	})
	output$betweenmw <- renderUI({
		if(input$test=='two way anova'){
			numericInput("betweenmw", "a: # levels", value = 2, min=1)
		}
	})
	output$withinrm <- renderUI({
		if(input$test=='repeated measures anova'){
			numericInput("withinrm", "within: # levels", value = 3, min=1)
		}
	})
	output$withinmw <- renderUI({
		if(input$test=='two way anova'){
			numericInput("withinmw", "b: # levels", value = 3, min=1)
		}
	})
	output$xrm <- renderUI({
		if(input$test=='repeated measures anova'){
			selectInput("xabrm", "interaction", choices = c("yes","no"))
		}
	})
	output$xmw <- renderUI({
		if(input$test=='two way anova'){
			selectInput("xabmw", "interaction", choices = c("yes","no"))
		}
	})
	output$stddevrm <- renderUI({
		if(input$test=='repeated measures anova'){
			textInput('stddev','sd','1')	
		}
	})
	output$corrrm <- renderUI({
		if(input$test=='repeated measures anova'){
			textInput('corrrm','cor','0')	
		}
	})
	output$stddevmw <- renderUI({
		if(input$test=='two way anova'){
			textInput('stddev','sd','1')		
		}
	})
	
}
