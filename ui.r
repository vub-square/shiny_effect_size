# shiny in and output
# input in sidebar, including action button
# output in main panel, including information and plot
ui <- fluidPage(

	titlePanel("Retrieving Effect Sizes"),

	sidebarLayout(

		sidebarPanel(
			fluidRow(
				selectInput("test", "test type:", choices = c("repeated measures anova","two way anova"))
			),
			fluidRow(
				column(3,
				uiOutput('betweenrm')
				),
				column(3,
				uiOutput('withinrm')
				),
				column(2,
				uiOutput('xrm')
				),
				column(2,
				uiOutput('corrrm')
				),
				column(2,
				uiOutput('stddevrm')
				)
			),
			fluidRow(
				column(3,
				uiOutput('betweenmw')
				),
				column(3,
				uiOutput('withinmw')
				),
				column(2,
				uiOutput('xmw')
				),
				column(2),
				column(2,
				uiOutput('stddevmw')
				)
			),
			fluidRow(
				plotOutput('plot')
			),			
			#tags$a(href="manual.pdf", "open introduction in new window", target="new"),
			h5("Susanne Blotwijk: effect size calculation"),
			h6("https://www.icds.be  (Wilfried Cools: shiny suit)"),
			width=4
		),

		mainPanel(
			fluidRow(
				tableOutput("matrixMeans")
			),
			fluidRow(
				tableOutput('tableEffects'),
				uiOutput('comments')
			),
			width=8
		)

	)
)