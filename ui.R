# Define UI for application that draws a histogram
load('borlist.rdata')
shinyUI(fluidPage(
  
  # Application title
  titlePanel("London House Price Estimator"),
  

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      
      selectInput("borough", label = h3("Choose Borough"), 
                  choices = list('Bromley' = 5, 'Richmond Upon Thames' = 27, 'Hillingdon' = 16,
                                 'Havering' = 15, 'Kingston upon Thames' = 21, 'Sutton' = 29,
                                'Hounslow'  = 17, 'Merton' = 24, 'Wandsworth' = 31,
                                 "Croydon" = 8, 'Lambeth' = 22, 'Southwark' = 28,
                                'Lewisham' = 23, 'Greenwich' = 11, 'Ealing' = 9,
                                'Hammersmith and Fulham' = 13, 'Brent' = 4, 'Harrow' = 18,
                                'Barnet' = 2, 'Islington' = 19, 'Hackney' = 12, 'Newham' = 25,
                                'Barking and Dagenham' = 1, 'Haringey' = 14, 'Enfield' = 10,
                                'Waltham Forest' = 26, 'Redbridge' = 26, 'Bexley' = 3,
                                'Kensington and Chelsea' = 20, 'Westminster' = 32, 'Camden' = 6, 'City of London' = 7,
                                'Tower Hamelets' = 30
                                ),
                                 selected = 2),
      
      selectInput("prop_type", label = h3("Choose building type"), 
                  choices = list ('Flat' = 9, 'Detached house' = 7, 'End terrace house' = 4,
                                  'Maisonette' = 12, 'Semi-detached House' = 16,
                  'Terraced house' = 19, 'Studio' = 17),
                  selected = 1),

      
      sliderInput("dist2stn",
                  "Distance in meters from train/tube station:",
                  min = 1,
                  max = 2500,
                  value = 30),

    
    sliderInput("num_bedrooms",
                "Number of bedrooms",
                min = 0,
                max = 5,
                value = 1),
  
  sliderInput("num_bathrooms",
                "Number of bathrooms:",
                min = 0,
                max = 5,
                value = 1),

sliderInput("num_recepts",
            "Number of Reception Rooms (living room, dining room, etc.):",
            min = 0,
            max = 5,
            value = 1),



submitButton("Submit"),

htmlOutput('inVar2')
    ),



# Show a plot of the generated distribution
mainPanel(

h2(textOutput("text3"), align = "center"),
p("This shiny app uses random forest gradient boosting to provide predictions on 
    the selling price of a house in London. Enter details on the left and press submit and 
  the model will provide a approximate selling price. Built from around 9800 examples from Zoopla's 
  property information API with freely avaliable London borough and train station location data."),

  HTML('<a href="http://www.zoopla.co.uk/"><img src="http://www.zoopla.co.uk/static/images/mashery/powered-by-zoopla-150x73.png" width="150" height="73" title="Property information powered by Zoopla" alt="Property information powered by Zoopla" border="0"></a>')
)
  )
))




