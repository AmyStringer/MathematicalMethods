#### Application for Mathematical Methods #### 
## Created by Amy Stringer 
## There is a README for this file, please 
## go and read this before attempting to edit 
## anything within this app.R file. 

## When editing, please make sure the indenting is consistent 


library(shiny)
library(LaplacesDemon)  # for dbern
library(tidyverse)      # for everything else 
library(TeachingDemos)  # for dice() 
library(shinythemes)    # for the theme of the app 
library(plotly)         # for interactive plotting environments 
library(DT)

#### Helper Funtions #### 

# helper function for the lower boundary on the 95% confidence interval
# for proportions 
propConf_lower <- function(p, n){
  # This is the formula given in the cambridge textbook 
  p - 1.96*sqrt((p*(1-p))/n)
}

# helper function for the upper boundary on the 95% confidence interval 
# for proportions 
propConf_upper <- function(p, n){
  # This is the formula given in the cambridge textbook 
  p + 1.96*sqrt((p*(1-p))/n)
}

# I wanted to create a function for the general form of the logarithm
logFun <- function(x, parms){
  a <- parms[1]
  k <- parms[2]
  h <- parms[3]
  v <- parms[4]
  b <- parms[5]
  pm <- parms[6]
  
  y <- pm*a*log(k*(x + h), base = b) + v
  
  return(y)
  
}

# same principle here for the exponential function 
# this is not entirely necessary but it does make things easier later on 
expFun <- function(x, parms){
  a <- parms[1]
  k <- parms[2]
  h <- parms[3]
  v <- parms[4]
  b <- parms[5]
  pm <- parms[6]
  
  y <- pm * b^(x/a-v)/k - h
  
  return(y) 
}


## For the pi exercise 
# plotting a circle is not entirely trivial 
# there MUST be a better way to do this 
circleDat <- data.frame(x = seq(0, 1, length.out = 5000), 
                        y = seq(0, 1, length.out = 5000))

CirclePlot <- ggplot(data = dat, aes(x = x, y = y)) + 
  # geom_rect for the outer square
  geom_rect(xmin = 0, xmax = 1, ymin = 0, ymax = 1, 
            color = "black", alpha = 0.01, fill = "grey80")+
  # draw out the circle using annotate 
  annotate("path",
           x=xc+r*cos(seq(0,2*pi,length.out=100)),
           y=yc+r*sin(seq(0,2*pi,length.out=100))) + 
  ylim(c(0, 1)) + 
  xlim(c(0, 1)) +
  coord_equal() +
  theme_bw()+
  # labels and scale are not relevant here, so remove them 
  theme(legend.position = "none", 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank()) +
  labs(x = "", y = "")

#### Virtual Reef Diver (VRD) data #### 
# this is the VRD data, I extracted 100 images worth of annotations 
# this is mostly a demonstration dataset
# the dataset contains - a couple of ID variables, classification, region, 
# sub_region, reef_name, lng, lat, year 
df <- read_csv("VRData.csv")

# this uses the dataset to compute the coverage at the classification level 
# for the entire population 
pop <- df %>% 
  group_by(Classification) %>% 
  mutate(Proportion = round(n()/nrow(df), 3)) %>%
  slice(1)

#### Build the Application User Interface #### 
# using theme flatly as it most closely fits to the VRD logo colouring
ui <- fluidPage(theme = shinythemes::shinytheme("flatly"),
    #### Preamble code for the UI #### 
    # MathJax allows us to use LaTeX code to produce maths 
    withMathJax(), 
    # Shiny doesn't use $<math>$ for inclusion of inline maths 
    # this updates that 
    tags$div(HTML("<script type='text/x-mathjax-config' >
            MathJax.Hub.Config({
            tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
            });
            </script >
            ")), 
    # hide error messages and warnings within the app itself
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    #### Visual Stuff #### 
    navbarPage(title = "Mathematical Methods Unit 4", 
    #### welcome ####          
               tabPanel(title = "Welcome", 
                        # include the virtual reef diver logo, this should be contained in the www folder
                        img(src = "VDR-Logo-HiRes-01.png", height = 300, width = 300, style = "float:right"),
                        # always using h1 for the main headings
                        h1("Mathematical Methods Unit 4"), 
                        # h5 is used for the standard/paragraph text 
                        h5("This app exists as a tool for students and teachers following the senior secondary Australian Curriculum for Mathematics. It was designed to assist in the explanation and understanding of the topics covered in Unit 4 of the Mathematical Methods course. "),
                        h5("This unit is made up of three major topics: "), 
                        h5("Topic 1: The Logarithm function"), 
                        h5("Topic 2: Continuous Random Variables and the Normal Distribution"), 
                        h5("Topic 3: Interval Estimates for Proportions"),
                        h5("Each topic has either a dedicated welcome page containing background and a glossary of terms or detailed instructions on how to interact with the app. "),
                        # h3 for intermediate headers 
                        h3("Topic 1"), 
                        h5("In this tab you will investigate the logarithmic function and it's inverse. Given is the general form for the logarithmic function and some sliders that allow you to adjust the function parameters and observe how it changes. "),
                        h3("Topic 2"),
                        h5("In this window, you will find three tabs each demonstrating concepts surrounding random variables, both discrete and continuous. "),
                        h5("You will be given detailed explanations of the Normal, Bernoulli and Binomial distributions, as well as plots of other common distributions that you may come across in the future. The final activity for this topic is a dice roll experiment which illustrates some properties of distributions and expected values of those distributions. "),
                        h3("Topic 3"),
                        h5("Topic 3 makes use of the Virtual Reef Diver $($VRD$)$ project. More information on this is given in in the associated topic welcome page. Here you will see three tabs; the first of which is a random sampling exercise which allows us to estimate the value to $\\pi$, the second use data from VRD to illustrate the long terms behaviour of sample proportions, and the final tab illustrates confidence intervals for proportions."),
                        # add a horizontal rule before adding credits/footnotes 
                        tags$hr(), 
                        # use of h6 for footnote text 
                        h6("Created by Amy Stringer as part of a QUT ARC Centre for Excellence in Mathematics and Statistics Initiative. For any enquiries regarding this app, or bug fixes, please email ", tags$a(href = "mailto:virtualreefdiver@qut.edu.au", "Virtual Reef Diver")), 
                        h6("Some activities seen here have been adapted from ", tags$a(href = "https://seeing-theory.brown.edu/frequentist-inference/index.html", "Seeing theory"))
                        ), 
    #### topic 1 ####  
               tabPanel(title = "Topic 1", 
                        
                        tabsetPanel(
                            tabPanel(title = "Logarithmic Functions",
                                
                                sidebarLayout(
                                  sidebarPanel(
                                    h3("The Logarithm Function"),
                                    uiOutput(outputId = "T1Info"),
                                    h5("By playing with the inputs below, we can see how the graph changes when these parameters are modified."),
                                    radioButtons(inputId = "T1Base", label = "Select a common base $(b)$", 
                                                 choices = c("e" = exp(1), "2" = 2, "10" = 10)),
                                    radioButtons(inputId = "T1pm", label = "Plus or Minus", 
                                                 choices = c("+"=1,"-"=-1)),
                                    # +/- 10 chosen as arbitrary min and max, it doesn't need to be a big change to illustrate the point 
                                    sliderInput(inputId = "T1h", label = "Horizontal Translation $(h)$", 
                                                min = -10, max = 10, value = 0), 
                                    sliderInput(inputId = "T1v", label = "Vertical Translation $(v)$", 
                                                min = -10, max = 10, value = 0), 
                                    # min and max here also chosen arbitrarily - just enough to illustrate a change
                                    sliderInput(inputId = "T1k", label = "Horizontal Stretch $(k)$", 
                                                min = 0.1, max = 3, value = 1), 
                                    sliderInput(inputId = "T1a", label = "Vertical Stretch $(a)$", 
                                                min = 0.1, max = 3, value = 1)
                                    
                                  ), 
                                  mainPanel(
                                    
                                    h3("Characteristics of the Log Function"), 
                                    h5("Here you can see three separte lines, one in black, one in blue and another in red."),
                                    h5("In black we have the log function whose genereal equation is given the left. "), 
                                    h5("In red, we have the vertical asymptote of this function. This is a value which the log function approaches, but never reaches."), 
                                    h5("And, finally, in blue we have the inverse of the log function. You can easily see from this graph that the inverse is the same function, only mirrored around the line $y = x$.  "), 
                                    h5("This should illustrate the relationship between log and exponential, where in general,"),
                                    # inline display mathematics, equiv to $$ $$ in LaTeX
                                    h5("$$ y = \\log_b x \\rightarrow  b^y = x $$ "),
                                    plotOutput(outputId = "T1LogPlot")
                                    
                                  )
                                )
                            )
                        )
              #### topic 2 ####          
                        ), 
               tabPanel(title = "Topic 2", 
                        withMathJax(), 
                        tabsetPanel(
                            tabPanel("Welcome",
                                     # main heading 
                                     h1("Continuous Random Variables and the Normal Distribution"), 
                                     h5("In this section of the application there are three tabs for your perusal. The first gives descriptions and plots of the distributions studied throughout this course. The second shows the probability distributions of other common distributions in statistics. Finally, the third tab contains a dice roll experiment. Here you will see how the expected value of a sample of dice rolls approaches the theoretical expected value of a single dice.  "), 
                                     # sub heading 
                                     h3("Glossary"), 
                                     # tags$b allows you to bold text 
                                     h5(tags$b("Probability Mass Function $(PMF)$"), "- The probability distribution for a discrete random variable consists of all the values that the random variable can take, together with the probability of each of these values. The probability distribution of a discrete random variable $X$ is described by a function $p(x) = Pr(X = x)$. This is called the probability mass function."), 
                                     h5(tags$b("Probability Density Function $(PDF)$"), "- The function $f$ whose graph models the histogram as the number of intervals is increased is called the probability density function."), 
                                     h5(tags$b("Discrete"), "- A discrete random variable is one that can take only a countable number of values."), 
                                     h5(tags$b("Continuous"), "- A continuous random variable is one that can take any value in an interval of the real number line, and is usually $($but not always$)$ generated by measuring."), 
                                     h5(tags$b("Random variable"), "- A random variable is a function that assigns a number to each outcome in the sample space $\\varepsilon$"), 
                                     h5(tags$b("Probability"), "- the extent to which an event is likely to occur, measured by the ratio of the favourable cases to the whole number of cases possible."), 
                                     h5(tags$b("Expected Value"), "- the mean of a random variable. The expected value of a discrete random variable $X$ is determined by summing the products of each value of $X$ and the probability that $X$ takes that value."), 
                                     h5(tags$b("Variance"), "- The variance of a random variable $X$ is a measure of the spread of the probability distribution about its mean or expected value $\\mu$.")
                                     ), 
                            tabPanel(title = "Random Variables",
                                     
                                     sidebarLayout(
                                         
                                         sidebarPanel(
                                           # select a distribution to investigate 
                                           # these three are the only ones I saw mentioned specifically the cambridge textbook 
                                           selectInput(
                                             inputId = "T2Dist", 
                                             label = "Select Distibution", 
                                             choices = c("Binomial Distribution", 
                                                         "Bernoulli Distribution", 
                                                         "Normal Distribution")),
                                             uiOutput(outputId = "T2Options")
                                         ),
                                         mainPanel(
                                             
                                             # based on the selected distribution, the header, text and formula will change 
                                             uiOutput(outputId = "T2header"),
                                             textOutput(outputId = "T2distInf"), 
                                             # I'm not sure this is necessary anymore now that mathjax is working 
                                             # but it works just fine either way 
                                             uiOutput(outputId = "T2DistFormula"),
                                             # using grid aspects inside the main panel
                                             # I want two plots to appear side by side in a row 
                                             fluidRow(
                                                column(width = 6, 
                                                    plotlyOutput(outputId = "T2DistPDF") 
                                                ), 
                                                column(width = 6, 
                                                    plotlyOutput(outputId = "T2DistCDF")
                                                )
                                             )
                                             
                                         )
                                     )
                                
                            ), 
                            tabPanel(title = "Examples of Other Distributions", 
                                     h3("Discrete Distributions"),
                                     # in this tab I want to use a grid layout 
                                     # this is done through a combination of fluidRow and column
                                     # first row is the discrete distributions 
                                     fluidRow(
                                       column(width = 4, 
                                              plotlyOutput(outputId = "T2Geometric"), 
                                              ), 
                                       column(width = 4, 
                                              plotlyOutput(outputId = "T2NegBin")
                                              ), 
                                       column(width = 4, 
                                              plotlyOutput(outputId = "T2Poisson")
                                              )
                                     ),
                                     h3("Continuous Distributions"), 
                                     # second row is continuous distributions 
                                     fluidRow(
                                       column(width = 4,
                                         plotlyOutput(outputId = "T2Uniform")
                                       ), 
                                       column(width = 4,
                                         plotlyOutput(outputId = "T2Exponential")
                                       ), 
                                       column(width = 4,
                                         plotlyOutput(outputId = "T2Gamma")
                                       )
                                     )
                                     ), 
                            tabPanel(title = "Dice Roll Experiment", 
                                     sidebarLayout(
                                       sidebarPanel(
                                         # h3 for a mid level heading 
                                         h3("The Dice Roll Experiment"), 
                                         # h5 for paragraph text 
                                         h5("Below you will see two buttons, one to roll a die once, and one to roll the same die 100 times. "), 
                                         h5("What we should see as we roll more and more times, is that eventually the expected value of our sample distribution (our rolls) should approach the theoretical expected value for all die rolls."),
                                         h5("The theoretical expected value is displayed in red in the graph to the right, and your sample expected value after each new roll is shown in black. "), 
                                         # these buttons will be cumulative, roll once is really "roll another 1"
                                         # and roll many is really "roll another 100" 
                                         actionButton(inputId = "RollOnce", label = "Roll Once"), 
                                         actionButton(inputId = "RollMany", label = "Roll 100 Times")
                                       ), 
                                       mainPanel(
                                         # the tab looked a little plain so i included a stock image of a dice 
                                         img(src = "dice.jpg", height = 300, width = 262, style = "float:left"),
                                         plotOutput(outputId = "ExpValPlot")
                                       )
                                     )
                                
                            )
                        )
                        ), 
    #### topic 3 ####  
               tabPanel(title = "Topic 3",  
                        
                        tabsetPanel(
                            tabPanel("Welcome", 
                                # main heading 
                                h1("Random Sampling and Proportion Statistics"), 
                                # sub heading 
                                h3("Virtual Reef Diver"),
                                # add the virtual reef diver logo 
                                img(src = "VDR-Logo-HiRes-01.png", height = 245, width = 245, style = "float:left"),
                                # paragraph text 
                                h5("The Virtual Reef Diver is a project run through QUT that provides a new way of monitoring the Great Barrier Reef by harnessing the power of citizen science. Monitoring of the reef is done by collecting images and determining what makes up each image. "),
                                h5("This is done by overlaying 15 points on the image, and classifying each point as one of hard coral, soft coral, water, sand, algae, other, or unknown. With large numbers of images, these classifications can be used to provide a picture of the reef in terms of it's contents, and doing this over many years allows scientists and reef personel to observe changes in reef coverage."),
                                h5("The Virtual Reef Diver initiative aids in this effort by providing a platform for citizens to participate in image collection and annotation. "),
                                h5("For this activity we have pulled 100 images and their corresponding annotations. The proportion of points belonging to a particular classification for all 100 images will be considered our ground truth, or population, proportion.  "),
                                # sub heading 
                                h3("Glossary"),
                                h5("A ", tags$b("population"), "is the set of all eligible members of a group which we intend to study. In this activity, when we refer to the population we are referencing the 100 images."),
                                h5("A ", tags$b("sample"), "is a subset of the population which we select in order to make inferences about the population. Generalising from the sample to the population will not be useful unless the sample is representative of the population."),
                                h5("An ", tags$b("interval estimate"), "for the population proportion $p$ is called a ", tags$b("confidence interval")),
                                # sub heading 
                                h3("Relevant Equations"),
                                h5("$$ \\text{Population Proportion, }p = \\frac{\\text{Number in Population with Attribute}}{\\text{Population Size}}  $$"), 
                                h5("$$ \\text{Sample Proportion, }\\hat{p} = \\frac{\\text{Number in Sample with Attribute}}{\\text{Sample Size}} $$"), 
                                h5("where, "),
                                h5("$p$ is a ", tags$b("Population Parameter"),"and is constant."),
                                h5("$\\hat{p}$ is a ", tags$b("Sample Statistic"), "and is not constant as it varies from sample to sample.")
                            ), 
                            tabPanel(title = "Random Sampling",
                                
                                     sidebarLayout(
                                       sidebarPanel(
                                         h5("We know that the exact area of the circle cannot be computed without the value of pi, so instead, we sample points inside the square. Some of these will fall inside the circle, giving us an approximation for the area. In this situation, we can let $A_{circle} \\approx S_{circle} = \\text{No. of points inside the circle}$ and $A_{square} \\approx S_{square} = \\text{No. of points inside the square}$ so that $$\\pi \\approx 4\\frac{S_{circle}}{S_{square}} $$  "),
                                         # use buttons to drop samples onto the plot 
                                         # we can build up slowly by dropping only 100 samples at a time 
                                         # or speed things up by dropping in lots of 1000 
                                         actionButton(inputId = "T3Drop100", label = "Drop 100 Samples"), 
                                         actionButton(inputId = "T3Drop1000", label = "Drop 1000 Samples"), 
                                         h5("The more we sample, the closer we should get to the true value of $\\pi$"),
                                         h5("The same is true for population samples. The more points we drop, or the more subjects we sample, the closer we get to the size of the population, and therefore we get closer to our ground truth.")
                                       ), 
                                       mainPanel(
                                         # sub heading 
                                         h3("Estimating Pi through the use of Random Sampling"), 
                                         # paragraph text
                                         h5("There are many ways of finding the value of $\\pi$. Here we will attempt to approach the value using random sampling. This activity is based on the premise that $\\pi$ can be expressed as a ratio of areas between a cicle of radius $r$ and a square of side length $2r$. "),
                                         h5("The area of a circle, and the area of a square are given by the following: $$ A_{circle} = \\pi r^2, \\quad A_{square} = 4r^2 $$"),
                                         h5("Dividing these gives: $$ \\frac{A_{circle}}{A_{square}} = \\frac{\\pi r^2}{4r^2} = \\frac{\\pi}{4} $$ "),
                                         h5("Rearranging this for $\\pi$ gives us a relationship between the areas that represents the exact value of $\\pi$, $$ \\pi = 4\\frac{A_{circle}}{A_{square}} $$"),
                                         # adding a horizontal line to keep text and outputs nice and separate 
                                         tags$hr(),
                                         # this is uioutput because it uses mathjax for the variable names. Though I do believe this could be avoided as mathjax was loaded in the first few lines 
                                         uiOutput("T3ValPi"),
                                         # this doesn't need to be in a fluid row as it is just one plot, but this was done in attempt to fix an earlier bug. Personally, I like that it makes things clear.
                                         fluidRow(
                                           column(width = 12, 
                                                    plotlyOutput(outputId = "T3PiEst", height = "100%")
                                                  )
                                         )
                                         
                                       )
                                     )    
                                
                            ), 
                            tabPanel(title = "Sample Proportions",
                                  sidebarLayout(
                                    sidebarPanel(
                                      # need to select a classification - I've chosen the main three 
                                      radioButtons(inputId = "T3classif2", label = "Select a Classification to Investigate", 
                                                   choices = c("Hard Coral", "Soft Coral", "Algae")),
                                      # need to select the number of images to sample from the total of 100 images that we extracted
                                      sliderInput(inputId = "T3nimg2", label = "Select the number of images to sample", 
                                                  min = 1, max = length(unique(df$MediaID))-1, value = 5), 
                                      # here we also want buttons that allow user to build up slowly, or to more quickly demonstrate a point 
                                      # so we either sample 10 or 100 times and each time we sample this will add the the samples already taken
                                      actionButton(inputId = "T3Add10", label = "Sample 10 times"), 
                                      actionButton(inputId = "T3Add100", label = "Sample 100 times")
                                    ), 
                                    mainPanel(
                                      # sub heading 
                                      h3("Long term behaviour of sampling"),
                                      # paragraph text 
                                      h5("In this activity, you must select the number of images you would like to sample, and then how many times you would like to sample. The app then computes the proportion of your chosen classification for each sample, and plots it on a histogram."),
                                      h5("As more samples are added, regardless of the number of images sampled at any one time, you should notice that the histogram becomes more and more normally distributed. "),
                                      plotOutput(outputId = "T3SampleHist")
                                    )
                                  )
                            ), 
                            tabPanel(title = "Confidence Intervals for Proportions",
                                     sidebarLayout(
                                       sidebarPanel(
                                         # again we want the user to select a classification out of the main three 
                                         radioButtons(inputId = "T3classif", label = "Choose a Classification to Investigate", 
                                                      choices = c("Hard Coral", "Soft Coral", "Algae")), 
                                         # and also how many images to select in each sample 
                                         sliderInput(inputId = "T3nimg", label = "How many images would you like to sample?", 
                                                     min = 1, max = length(unique(df$MediaID))-1, value = 5), 
                                         # and buttons to build things up slowly or more quickly 
                                         actionButton(inputId = "T3samp1", label = "Sample once"), 
                                         actionButton(inputId = "T3sampmany", label = "Sample 10 times")
                                       ), 
                                       mainPanel(
                                         # sub heading 
                                         h3("Confidence Intervals"),
                                         # paragraph text 
                                         h5("The formula for the 95% confidence interval of a proportion is given by $$\\left( \\hat{p} -1.96\\sqrt{\\frac{\\hat{p}(1-\\hat{p})}{n}}, \\hat{p} + 1.96\\sqrt{\\frac{\\hat{p}(1-\\hat{p})}{n}} \\right)$$"),
                                         h5("Where $\\hat{p}$ is the sample proportion and $n$ is size of the sample from which $\\hat{p}$ was calculated."),
                                         h5("In this activity you select the classification of interest, the number of images to sample and then you may sample 1 at a time or 10 at a time. The app then computes the sample proportion and the confidence interval for each sample and plots them. "),
                                         h5("Also plotted is the true proportion for the full 100 images. Each point and interval is then coloured according to whether or not the interval contains the population proportion. In general, due to this being a 95% interval, you would expect 95% of intervals to contain the true population proportion. "),
                                         plotOutput(outputId = "T3ConfIntPlot")
                                       )
                                     )
                                     
                                
                            )
                        )
                        
                        )
  
               
               )
    
)
#### server #### 
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #### Topic 1 #### 
    # there's definitely a better way to do this, perhaps just using inline maths 
    # but like earlier, this works 
    output$T1Info <- renderUI({
      withMathJax("The General Form of the Logarithm Function is: ", 
                  "$$y = a \\log_b(k(x+h)) + v$$", 
                  "where",
                  "$a$ is the vertical stretch, $k$ is the horizontal stretch, $h$ is the horizontal translation, $v$ is the vertical translation and $b$ is the base." )
    })
    
    output$T1LogPlot <- renderPlot({
      # build a dataframe to plot with, this simply stores the x values and then we will use stat_function
      dat <- data.frame(x = seq(from = -10, to = 10, by = 0.001))
      # logFun was written at the top of this script under the helper functions heading 
      ggplot(data = dat, aes(x = x)) + 
        stat_function(fun = logFun, 
                      args = list(
                        parms = c(input$T1a,
                                  input$T1k,
                                  input$T1h, 
                                  input$T1v, 
                                  as.numeric(input$T1Base),
                                  as.numeric(input$T1pm))
                      ), 
                      aes(color = "Log"),
                      n = 10001) +
        # expFun was also written at the beginning 
        stat_function(fun = expFun, 
                      args = list(
                        parms = c(input$T1a,
                                  input$T1k,
                                  input$T1h, 
                                  input$T1v, 
                                  as.numeric(input$T1Base),
                                  as.numeric(input$T1pm))
                      ), 
                      aes(color = "Inverse"), 
                      n = 1001) +
        geom_vline(xintercept = -input$T1h, color = "red", linetype = "dashed") + 
        theme_bw() +
        ylim(c(-10, 10)) +
        scale_color_manual(values = c("Log" = "black", 
                                      # this rgb colour is the colour that matches the blue in the theme 
                                      "Inverse" = rgb(24, 188, 156, 
                                                      maxColorValue = 255)))
      
      
    
    })
    
    
    #### Topic 2 #### 
    
    output$T2Options <- renderUI({
        if (input$T2Dist == "Binomial Distribution"){
            tagList(
                # the binomial function requires an input of size and prob 
                sliderInput("T2BinomSize", "Select the number of trials", 
                            min = 0, max = 50, value = 5, 
                            round = TRUE), 
                sliderInput("T2BinomProb", "Choose a probability of success", 
                            min = 0, max = 1, value = 0.5)
            )
        } else if (input$T2Dist == "Bernoulli Distribution"){
            tagList(
                # the bernoulli function requires a prob input 
                sliderInput("T2BernProb", "Select Probability of Success", 
                            min = 0, max = 1, value = 0.5)
            )
        } else if (input$T2Dist == "Normal Distribution"){
            tagList(
                # dnorm requires a mean and standard deviation 
                sliderInput("T2NormMean", "Adjust the mean", 
                            min = -10, max = 10, value = 0), 
                sliderInput("T2NormSD", "Adjust the Standard Deviation", 
                            min = 0.5, max = 10, value = 1)
            )
        }
    })
    
    # this is the output that simply updates the heading in the main panel based on the dist input 
    output$T2header <- renderUI({
        if (input$T2Dist == "Binomial Distribution"){
            tagList(
                h3("The Binomial Distribution")
            ) 
        } else if (input$T2Dist == "Bernoulli Distribution"){
            tagList(
                h3("The Bernoulli Distribution")
            ) 
        } else if (input$T2Dist == "Normal Distribution"){
            tagList(
                h3("The Normal Distribution")
            )
        }
        
    })
    
    # this is the output which updates the text based on the dist input 
    output$T2distInf <- renderText({
        if (input$T2Dist == "Binomial Distribution"){
            paste(
                "The Binomial distribution is a discrete distribution and hence the graph you see below represents the probability mass function. ", 
                "The Binomial distribution is parameterised by the number of trials, n, and the probability of success, p. If a random variable is distributed according to a binomial distribution, ", 
                "the probability mass function can show you the probability of obtaining x successes from N trials. "
            )
        } else if (input$T2Dist == "Bernoulli Distribution"){
            paste(
                "The Bernoulli distribution is a discrete distribution and hence the graph you see below represents the probability mass function. ", 
                "The Bernoulli distribution is a special case of the Binomial Distribution where N = 1 and is parameterised by the probability of success only.",
                "This distribution is not incredibly useful on its own, but is useful in constructing more complex distributions. "
            )
        } else if (input$T2Dist == "Normal Distribution"){
            paste(
                "The Normal Distribution is a very useful continuous distribution, and is typically used to represent random variables that should should be clustered around some mean value, such as errors, running time, heights, weights etc.", 
                "It is often referred to as the 'Bell Curve' due to its shape or the 'Gaussian Distribution'. "
            )
        }
    })
    
    # this updates the formula components based on the dist input. This may possibly be included in the above output 
    output$T2DistFormula <- renderUI({
        if (input$T2Dist == "Binomial Distribution"){
          
            withMathJax("The formula for this probability mass function is: ", 
                        "$$ \\text{Pr}(X = x) =  {n \\choose x}p^x (1-p)^{n-x} \\quad \\text{where} \\quad {n \\choose x} = \\frac{n!}{x!(n - x)!}$$", 
                        "And if X is a random variable distributed binomially, we write, ", 
                        "$$ X \\sim \\text{Binomial}(n, p) $$", 
                        "The Expected Value of a Binomial Distribution is given in terms of its parameters as: ", 
                        "$$ E(X) = np $$ ", 
                        "and its Variance is given by", 
                        "$$Var(X) = np(1-p) $$")
          
        } else if (input$T2Dist == "Bernoulli Distribution"){
          
            withMathJax("The formula for this probability mass function is:", 
                        "$$\\text{Pr}(X = x) = \\begin{cases} 1-p & x = 0 \\\\ p & x = 1 \\end{cases} $$", 
                        "And if X is a random variable distributed according to a Bernoulli distribution, we write:", 
                        "$$ X \\sim \\text{Bernoulli}(p) $$", 
                        "Since the Bernoulli distribution is simply a Binomial with n=1, the Expected Value is given by:", 
                        "$$ E(X) = p $$", 
                        "and the Variance", 
                        "$$ Var(X) = p(1-p) $$")
            
        } else if (input$T2Dist == "Normal Distribution"){
          
            withMathJax("The Normal Distribution is parameterised by its mean, $\\mu$, and variance, $\\sigma^2$.", 
                        "The equation for the Normal probability density function is: ", 
                        "$$ f(x) = \\frac{1}{\\sqrt{2\\pi \\sigma^2}} \\exp\\left(\\frac{-(x - \\mu)^2}{2\\sigma^2}\\right) $$", 
                        "If $X$ is a normally distributed, continuous random variable, we write ", 
                        "$$ X \\sim N(\\mu, \\sigma^2)$$", 
                        "The Expected Value of the Normal, or Gaussian, distribution is given by: ", 
                        "$$ E(X) = \\mu $$", 
                        "and its Variance is given by:", 
                        "$$ Var(X) = \\sigma ^2$$")
          
        }
    })
    
    
    # for the plotting component of this tab we will use a reactive dataset that updates the results based on the input distribution 
    InputDat <- reactive({
        if (input$T2Dist == "Binomial Distribution"){
            # define the x values 
            x1 <- 0:input$T2BinomSize
            # use the built in function to computes corresponding y values from the  binomial distribution 
            # this uses the inputs specified 
            dat <- data.frame(x = x1, 
                              y = dbinom(x1, 
                                         size = input$T2BinomSize, 
                                         prob = input$T2BinomProb)) %>% 
                # cumsum is for the cumulative plot 
                mutate(z = cumsum(y))
        } else if (input$T2Dist == "Bernoulli Distribution"){
            # x in this instance can only be 0 or 1 
            x1 <- 0:1
            # randomly select from a bernoulli distribution 
            dat <- data.frame(x = x1, 
                              y = dbern(x1, prob = input$T2BernProb)) %>% 
                # compute the cumulative sum for the cumulative plot
                mutate(z = cumsum(y))
        } else if (input$T2Dist == "Normal Distribution") {
            # select an x range to cover the possible range of the inputs 
            x1 <- seq(from = -20, to = 20, by = 0.01)
            # sample from the normal 
            dat <- data.frame(x = x1, 
                              y = dnorm(x1, 
                                        mean = input$T2NormMean, 
                                        sd = input$T2NormSD)) %>% 
                # same as above 
                mutate(z = cumsum(y))
        }
    })
    
    output$T2DistPDF <- renderPlotly({
        # use the reactive dataset InputDat() 
        dat <- InputDat()
        
        if (input$T2Dist == "Binomial Distribution" || input$T2Dist == "Bernoulli Distribution"){
            P1 <- ggplot(data = dat, aes(x = x, y = y)) + 
                geom_bar(stat = "identity", 
                         # there's that theme colour agin 
                         fill = rgb(24, 188, 156, 
                                    maxColorValue = 255)) +
                labs(title = "Probability Mass Function") + 
                theme_bw()
        } else if (input$T2Dist == "Normal Distribution"){
            # this is a pretty standard ggplot line plot 
            P1 <- ggplot(data = dat, aes(x = x, y = y)) +
                geom_line() + 
                ylim(c(0, 1)) +
                labs(title = "Probability Density Function") + 
                theme_bw() 
        }
        ggplotly(P1)
    })
    
    output$T2DistCDF <- renderPlotly({
        dat <- InputDat()
        if (input$T2Dist == "Binomial Distribution" || input$T2Dist == "Bernoulli Distribution"){
          # i used geom_step to get the segmented effect for the discrete variables 
          P1 <- ggplot(data = dat, aes(x = x, y = z)) + 
              geom_step() + 
              labs(title = "Cumulative Mass Function") + 
              theme_bw()
        } else if (input$T2Dist == "Normal Distribution"){
          # since this is continuous, geom_path was used 
          P1 <- ggplot(data = dat, aes(x = x, y = z)) + 
            geom_path() +
            labs(title = "Cumulative Density Function") +
            theme_bw()
        }
        ggplotly(P1)
    })
    ## starting here we are looking at the "other distributions" tab
    # geometric distribution 
    output$T2Geometric <- renderPlotly({
      dat <- data.frame(x = seq(0, 10, by = 1))
      # making use of the built in dgeom function 
      dat$y <- dgeom(dat$x, prob = 0.3)
      
      P1 <- ggplot(dat, aes(x = x, y = y)) + 
        geom_bar(stat = "identity", 
                 # there's the colour again 
                 fill = rgb(24, 188, 156, 
                            maxColorValue = 255)) +
        theme_bw() +
        labs(title = "Geometric Distribution")
      
      ggplotly(P1)
    })
    
    # negative binomial distribution 
    output$T2NegBin <- renderPlotly({
      dat <- data.frame(x = seq(0, 40, by = 1))
      
      # built in function 
      dat$y <- dnbinom(dat$x, size = 5, prob = 0.3)
      
      P1 <- ggplot(data = dat, aes(x = x, y = y)) +
        geom_bar(stat = "identity", 
                 # theme colour
                 fill = rgb(24, 188, 156, 
                            maxColorValue = 255)) + 
        theme_bw() + 
        labs(title = "Negative Binomial")
      
      ggplotly(P1)
    })
    
    # Poisson distribution 
    output$T2Poisson <- renderPlotly({
      dat <- data.frame(x = seq(0, 10, by = 1)) 
      # built in function 
      dat$y <- dpois(dat$x, lambda = 3)
      
      P1 <- ggplot(data = dat, aes(x = x, y = y)) + 
        geom_bar(stat = "identity", 
                 # theme colour 
                 fill = rgb(24, 188, 156, 
                            maxColorValue = 255)) + 
        theme_bw() +
        labs(title = "Poisson Distribution")
      
      ggplotly(P1)
      
    })
    
    # uniform distribution 
    output$T2Uniform <- renderPlotly({
      
      dat <- data.frame(x = seq(0, 4, by = 0.1))
      
      # built in function 
      dat$y <- dunif(dat$x, min = 0, max = 4)
      
      P1 <- ggplot(data = dat, aes(x = x, y = y)) + 
        geom_line() + 
        theme_bw() +
        labs(title = "Uniform Distribution")
      
      ggplotly(P1)
    })
    
    # exponential distribution 
    output$T2Exponential <- renderPlotly({
      
      dat <- data.frame(x = seq(0, 5, 0.1)) 
      
      dat$y <- dexp(dat$x, rate = 1.5)
      
      P1 <- ggplot(data = dat, aes(x = x, y = y)) + 
        geom_line() + 
        theme_bw() +
        labs(title = "Exponential Distribution")
      
      ggplotly(P1)
      
    })
    
    # gamma distribution 
    output$T2Gamma <- renderPlotly({
      
      dat <- data.frame(x = seq(0, 20, 0.1))
      
      # built in function 
      dat$y <- dgamma(dat$x, shape = 3.0, scale = 2.0)
      
      P1 <- ggplot(dat, aes(x = x, y = y)) + 
        geom_line() + 
        theme_bw() +
        labs(title = "Gamma Distribution")
      
      ggplotly(P1)
      
    })
    
    # reactive values used to built a reactive dataframe 
    # initialise the dataframe as an empty data frame 
    diceDat <- reactiveValues(data = data.frame()) 
    
    # observe event allows for you to execute certain code after a button push or input change 
    observeEvent(input$RollOnce, {
      # dice is a cool function i found that simulates a dice roll 
      # but it returns the results in some kind of data frame with a column called Red
      thing <- dice(rolls = 1, ndice = 1, sides = 6, plot.it = FALSE)
      # add a counter column that keeps track of the number of rolls 
      n <- nrow(diceDat$data)+1
      temp <- data.frame(n = n, roll = thing$Red)
      # update the diceDat to include the new roll information 
      diceDat$data <<- rbind(diceDat$data, temp) 
    })
    
    # this is effectively the same as above, but changing rolls to 100 in the dice function 
    observeEvent(input$RollMany, {
      thing <- dice(rolls = 100, ndice = 1, sides = 6, plot.it = FALSE)
      n <- (nrow(diceDat$data)+1):(nrow(diceDat$data) + 100)
      temp <- data.frame(n = n, roll = thing$Red) 
      
      # update the diceDat
      diceDat$data <<- rbind(diceDat$data, temp)
    })
    
    # this puts the reactive dataset to use and plots the expected value of the rolls within 
    output$ExpValPlot <- renderPlot({
      # first we need to compute the expected value after each roll 
      dat <- diceDat$data %>% 
        mutate(ExpVal = cumsum(roll)/n)
      
      ggplot(data = dat, aes(x = n, y = ExpVal)) + 
        geom_line() + 
        # include the true expected value in red 
        geom_hline(yintercept = 3.5, color = "red") + 
        theme_bw() + 
        labs(x = "Roll Number", 
             y = "Expected Value")
    })
    
    
    #### Topic 3 #### 
    
    # this generate a reactive dataset that will hold the randomly sampled points 
    T3PiDots <- reactiveValues(data = data.frame())
    
    # this will be the reactive plot. It is initialised with the empty square and circle that is constructed 
    # at the beginning under helper functions 
    T3PiPlotReact <- reactiveValues(data = CirclePlot)
    
    # we want both the dataset and the plot to change when we select one of the buttons 
    # so use observe event 
    observeEvent(input$T3Drop100, {
      # the sampling of points is done by simply selecting a combination of x and y points from the circle dat dataset 
      # generate in the helper functions section 
      samp_x <- sample_n(select(circleDat, x), 100)
      samp_y <- sample_n(select(circleDat, y), 100)
      samp <- bind_cols(samp_x, samp_y)
      
      samp <- samp %>%
        # this computes a distance and compares it to the distance to the edge of the circle 
        mutate(d = 0.5^2 - ((0.5-x)^2 + (0.5-y)^2), 
               # and then we explicitly test whether or not a point is inside or outside 
               Inside = ifelse(d>=0, TRUE, FALSE))
      
      # update the PiDots data frame 
      T3PiDots$data <<- bind_rows(T3PiDots$data, samp)
      
      # update the plot to use the current dataset 
      T3PiPlotReact$data <<- T3PiPlotReact$data +
        geom_point(data = T3PiDots$data, aes(x = x, y = y, color = Inside)) 
    })
    
    # this is the same as the above step only we have 1000 samples rather then 100 
    observeEvent(input$T3Drop1000, {
      samp_x <- sample_n(select(circleDat, x), 1000)
      samp_y <- sample_n(select(circleDat, y), 1000)
      samp <- bind_cols(samp_x, samp_y)
      
      samp <- samp %>% 
        mutate(d = 0.5^2 - ((0.5-x)^2 + (0.5-y)^2), 
               Inside = ifelse(d>=0, TRUE, FALSE))
      
      T3PiDots$data <<- bind_rows(T3PiDots$data, samp)
      
      T3PiPlotReact$data <<- T3PiPlotReact$data +
        geom_point(data = T3PiDots$data, aes(x = x, y = y, color = Inside)) +
        scale_color_manual(values = c("TRUE" = rgb(24, 188, 156, 
                                                  maxColorValue = 255), 
                                     "FALSE" = "salmon"))
    })
    
    output$T3ValPi <- renderUI({
      
      # compute the approxiamte value for Pi 
      S_circ <- sum(T3PiDots$data$Inside)  # how many are in the circle?
      S_squ <- nrow(T3PiDots$data)         # how many points are there in total?
      PiEst <- 4*S_circ/S_squ              # compute the ratio 
      # then put it all together in a nice statement 
      withMathJax("$S_{circle} = $", S_circ, "$, \\quad S_{square} = $", S_squ, "$, \\quad \\pi \\approx $", PiEst)
      
    })
    
    # this just includes the plot we generated earlier 
    output$T3PiEst <- renderPlotly({
      ggplotly(T3PiPlotReact$data)
    })
    
    # setting up a new reactive dataset for the next part of the app 
    T3Samples <- reactiveValues(data = data.frame())
    
    # we want the observe event to trigger computation of either of the classification or number of images are changed 
    # so we can make a reactive list 
    toListen2 <- reactive({
      list(input$T3classif2, input$T3nimg2)
    })
    
    # here we observe the reactive list and what this does is reset the dataframe 
    observeEvent(toListen2(), {
      # reset data frame when class or nimg is changed 
      T3Samples$data <<- data.frame()
    })
    
    # next we want our dataset to update once we press a button 
    observeEvent(input$T3Add10, {
      # initialise samples as an empty dataframe so that we can use bind_rows
      samples <- data.frame()
      
      # we want 10 samples, so we use a for loop 
      for(i in 1:10){
        # sample the images
        samp <- sample_n(distinct(df, MediaID), input$T3nimg)
        
        # filter the original dataframe so that we only see those mediaIDs present in our sample 
        sampled <- df %>% 
          filter(MediaID %in% samp$MediaID) %>% 
          # add a counter 
          mutate(n = n()) %>% 
          # compute the proprtion by counting the number in each group and dividing by the total 
          group_by(Classification) %>% 
          mutate(p = round(n()/n, 2)) %>% 
          # we only need one proportion for each classification 
          slice(1) %>%
          # compute the conf int by using the helper functions 
          mutate(lwr = propConf_lower(p, n), 
                 upr = propConf_upper(p, n)) %>% 
          # we only care about the classification chosen by the user 
          filter(Classification == input$T3classif2)
        
        # add it to the mega dataframe 
        samples <- bind_rows(samples, sampled)
      }
      
      # update T3Samples with all ten sample means and confidence intervals 
      T3Samples$data <<- bind_rows(T3Samples$data, samples)
    })
    
    # this is all the same as above except that we loop 100 times 
    observeEvent(input$T3Add100, {
      samples <- data.frame()
      
      for(i in 1:100){
        samp <- sample_n(distinct(df, MediaID), input$T3nimg)
        
        sampled <- df %>% 
          filter(MediaID %in% samp$MediaID) %>% 
          mutate(n = n()) %>% 
          group_by(Classification) %>% 
          mutate(p = round(n()/n, 2)) %>% 
          slice(1) %>%
          mutate(lwr = propConf_lower(p, n), 
                 upr = propConf_upper(p, n)) %>% 
          filter(Classification == input$T3classif2)
        
        samples <- bind_rows(samples, sampled)
      }
      
      T3Samples$data <<- bind_rows(T3Samples$data, samples)
    })
    
    # use the data generated using the above code, and plot a histogram of the sample proportions 
    output$T3SampleHist <- renderPlot({
      ggplot(T3Samples$data, aes(x = p)) + 
        geom_histogram(color = "black", 
                       # there's that colour again 
                       fill = rgb(24, 188, 156, 
                                  maxColorValue = 255)) + 
        theme_bw() 
    })
    
    ## the confidence interval tab 
    # this set up is very similar to the previous tab 
    T3Data <- reactiveValues(data = data.frame())
    
    # we have our dataset reset once the classification or number of imgs sampled is changed by the user 
    toListen <- reactive({
      list(input$T3classif, input$T3nimg)
    })
    
    observeEvent(toListen(), {
      # reset the plot when the classification of number of images changes 
      T3Data$data <<- data.frame()
    })
    
    # samp1 is the button that adds one sample 
    observeEvent(input$T3samp1, {
      # as before, sample from the media Id col 
      samp <- sample_n(distinct(df, MediaID), input$T3nimg)
      
      # compute all the proportions, and confidence intervals and then select out the relevant info 
      sampled <- df %>% 
        filter(MediaID %in% samp$MediaID) %>% 
        mutate(n = n()) %>% 
        group_by(Classification) %>% 
        mutate(p = round(n()/n, 2)) %>% 
        slice(1) %>%
        mutate(lwr = propConf_lower(p, n), 
               upr = propConf_upper(p, n)) %>% 
        filter(Classification == input$T3classif)
      
      # bind to the main dataframe 
      T3Data$data <<- rbind(T3Data$data, sampled)
    })
    
    # sampmany samples ten lots of images 
    observeEvent(input$T3sampmany, {
      # initialise
      samples <- data.frame()
      
      # we want ten samples with all the relevant info, so use a for loop 
      for(i in 1:10){
        samp <- sample_n(distinct(df, MediaID), input$T3nimg)
        
        # compute all the things as above 
        sampled <- df %>% 
          filter(MediaID %in% samp$MediaID) %>% 
          mutate(n = n()) %>% 
          group_by(Classification) %>% 
          mutate(p = round(n()/n, 2)) %>% 
          slice(1) %>%
          mutate(lwr = propConf_lower(p, n), 
                 upr = propConf_upper(p, n)) %>% 
          filter(Classification == input$T3classif)
        
        samples <- bind_rows(samples, sampled)
      }
      # bind to original data 
      T3Data$data <<- bind_rows(T3Data$data, samples)
      
    })
    
    # this is where we build the conf int plot that updates with each button press 
    output$T3ConfIntPlot <- renderPlot({
      # make sure we have a value for the population proportion for comparison 
      # this is taken directly from the original data, and all proportions were computed in the helper function section 
      popP <- filter(pop, Classification == input$T3classif)$Proportion[1]
      
      # add a rows or samples column that numbers the samples 
      T3Data$data <- T3Data$data %>% 
        mutate(rows = 1:nrow(.)) %>% 
        # we want a  binary variable that tells us whether popP is inside our interval 
        mutate(Contained = (popP>lwr)&(popP<upr)) 
      
      # here some spicy ggplot code 
      ggplot(T3Data$data, aes(x = rows, color = Contained)) + 
        # geom_pointrange gives us a point with an interval, handy for conf int 
        geom_pointrange(aes(y = p, 
                            ymin = lwr, 
                            ymax = upr)) +
        # just a straight line to represent the population value 
        geom_hline(yintercept = popP, 
                   color= "red", 
                   linetype = "dashed") +
        theme_bw() +
        # we want the intervals to move up the page rather than across 
        coord_flip() + 
        # we don't need the numbers on the y axis as its clear that each new interval is a new sample
        theme(axis.text.y = element_blank()) +
        labs(x = "", y = "Proportion") +
        # theme colour for TRUE 
        scale_color_manual(values = c(
          "TRUE" = rgb(24, 188, 156, 
                       maxColorValue = 255), 
          "FALSE" = "salmon"
        ))
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
