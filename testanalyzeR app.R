#########LIBRARIES#######################################################
library(tidyverse)
library(dplyr)
library(tidyr)
library(psych)
library(openxlsx)
library(DT)
library(stringr)
library(mirt)
library(psychometric)
library(ltm)
library(corrplot)
library(CTT)
library(ShinyItemAnalysis)
library(bslib)

#######UTILS###########################################################################
tema <- bs_theme( bg= "white",fg = "navy ", primary = "tomato",
  base_font = font_google("Space Mono"),
  code_font = font_google("Space Mono")
)

#GÜÇLÜK AYIRTEDİCİLİK FONKSİYONLARI:
labelDifficulties<-function (itemDifficulties){
  difficultycommemnt <- c()
  for (difficulty in 1:length(itemDifficulties)){
    if(itemDifficulties[difficulty] <= 0.2){difficultycommemnt[difficulty]<- "DIFFICULT" 
    }else if(itemDifficulties[difficulty] > 0.2 & itemDifficulties[difficulty] < 0.8){difficultycommemnt[difficulty]<- " MEDIOCRE"
    }else{difficultycommemnt[difficulty]<- "     EASY"
    }
  }
  return(difficultycommemnt)
}


percent_al<-function(df){
  x<- vector("numeric", length=length(df))
  for (i in 1:length(df))
    x[i]<-round(df[i]/sum(df),3)
  
  return(x)
}

labelDiscriminations<- function(itemDiscriminations){
  discriminationcomment<-c()
  for (discrimination in 1:length(itemDiscriminations)) {
    if(itemDiscriminations[discrimination] <= 0 | is.na(itemDiscriminations[discrimination] )  ){discriminationcomment[discrimination]<-      "  DISCARD"
    }else if(itemDiscriminations[discrimination] <= 0.2 & itemDiscriminations[discrimination] > 0){ discriminationcomment[discrimination]<-   "   REVISE"
    }else if(itemDiscriminations[discrimination] <= 0.3 & itemDiscriminations[discrimination] > 0.2){ discriminationcomment[discrimination]<- "   MEDIUM"
    }else if(itemDiscriminations[discrimination] <= 0.4 & itemDiscriminations[discrimination] > 0.3){ discriminationcomment[discrimination]<- "     GOOD"
    }else if(itemDiscriminations[discrimination] > 0.4){ discriminationcomment[discrimination]<- "VERY GOOD"
    }
  }
  return(discriminationcomment)
}


main_function<- function ( datapath,  
                           a, b, c, d, 
                           x, y, z, w, 
                           requested_output=c(1:7),
                           p_table=NULL){
  
  #N of items
  part1<-a
  if (b != 0){part2<-b}else{part2<-1}
  if (c != 0){part3<-c}else{part3<-1} 
  if (d != 0){part4<-d}else{part4<-1}
  #max Converted scores
  max_score_part1<-x
  if (y != 0){max_score_part2<-y}else{max_score_part2<-1}
  if (z != 0){max_score_part3<-z}else{max_score_part3<-1}
  if (w != 0){max_score_part4<-w}else{max_score_part4<-1}
  part5 <- 1#input$part5
  part6 <- 1#input$part6
  
  tbl <- read.delim(datapath, header = FALSE, dec = " ")
  
  data <- str_split_fixed(tbl[, 1], "N", 2)
  data <- as.data.frame(data[,-1])
  colnames(data) <- "v1"
  
  #türkçe karakterleri dönüştür:
  data$v1 <- gsub('<dd>', 'I', data$v1)
  data$v1 <- gsub('<d6>', 'O', data$v1)
  data$v1 <- gsub('<de>', 'S', data$v1)
  data$v1 <- gsub('<d0>', 'G', data$v1)
  data$v1 <- gsub('<c7>', 'C', data$v1)
  data$v1 <- gsub('<dc>', 'U', data$v1)
  
  #(custom) N of chars in each section on the optical form
  data <- data %>%
    separate(v1, into = c("st_id", "colm"), sep = 15)
  data <- data %>%
    separate(colm, into = c("part_1", "colm"), sep = 50)
  data <- data %>%
    separate(colm, into = c("part_2", "colm"), sep = 50)
  data <- data %>%
    separate(colm, into = c("part_3", "colm"), sep = 50)
  data <- data %>%
    separate(colm, into = c("part_4", "colm"), sep = 50)
  data <- data %>%
    separate(colm, into = c("part_5", "colm"), sep = 50)
  data <- data %>%
    separate(colm, into = c("part_6", "colm"), sep = 50)
  data <- data %>%
    separate(colm, into = c("st_name", "colm"), sep = 14)
  data <- data %>%
    separate(colm, into = c("st_surname", "colm"), sep = 14)
  data <- data %>%
    separate(colm, into = c("section", "colm"), sep = 6)
  data <- data %>%
    separate(colm, into = c("room_desk_id"), sep = 4)
  
  
  
  data <- data %>%
    separate(part_1, into = c("part_1"), sep = part1)
  data <- data %>%
    separate(part_2, into = c("part_2"), sep = part2)
  data <- data %>%
    separate(part_3, into = c("part_3"), sep = part3)
  data <- data %>%
    separate(part_4, into = c("part_4"), sep = part4)
  data <- data %>%
    separate(part_5, into = c("part_5"), sep = part5)
  data <- data %>%
    separate(part_6, into = c("part_6"), sep = part6)
  
  #section adı ile burada ilgilen:
  data$section <- gsub(" ", "", data$section)
  
  #response set şekil ver:
  data$part_1 <- str_split_fixed(data$part_1, "", part1)
  data$part_2 <- str_split_fixed(data$part_2, "", part2)
  data$part_3 <- str_split_fixed(data$part_3, "", part3)
  data$part_4 <- str_split_fixed(data$part_4, "", part4)
  data$part_5 <- str_split_fixed(data$part_5, "", part5)
  data$part_6 <- str_split_fixed(data$part_6, "", part6)
  
  
  if(requested_output == 1){ data<-data }else{
    AK<-data[1,]
    data<-subset(data, data$st_id     !=  "                " 
                 & data$st_name      !=  "              " 
                 & data$st_surname      !=  "              " )
    data<-rbind(AK, data)
    row.names(data)<- 1:nrow(data)
  }
  
  
  if (part4 == 1 & part3 != 1) {
    respo <- as.data.frame(cbind(data$part_1[, 1:part1],
                                 data$part_2[, 1:part2],
                                 data$part_3[, 1:part3]))
  }else if(part3 == 1 & part4 == 1){
    respo <- as.data.frame(cbind(data$part_1[, 1:part1],
                                 data$part_2[, 1:part2]))
  }else{
    respo <- as.data.frame(cbind(data$part_1[, 1:part1],
                                 data$part_2[, 1:part2],
                                 data$part_3[, 1:part3],
                                 data$part_4[, 1:part4]))
  }
  
  
  
  unscored_respo <- as.matrix(sapply(respo,
                                     function(functiondata)
                                       as.numeric(
                                         gsub("A", 1,
                                              gsub("B", 2,
                                                   gsub("C", 3,
                                                        gsub("D", 4,
                                                             gsub("E", 5, functiondata)
                                                        )))
                                         ))))
  
  anskey <- as.matrix(unscored_respo[1, 1:ncol(unscored_respo)])
  scored_respo <- key2binary(unscored_respo, anskey, score_missing = TRUE)
  colnames(scored_respo) <- paste0("item", 1:ncol(scored_respo))
  
  data_for_distractorAnal <- as.matrix(drop_na(as.data.frame(unscored_respo)))
  ans_for_distractorAnal <- as.character(anskey)
  
  results <- as.data.frame(scored_respo)
  if (part4 == 1 & part3 != 1) {
    results$total_part_1 <- rowSums(results[, 1:part1])
    results$total_part_2 <- rowSums(results[, (part1 + 1):(part1 + part2)])
    results$total_part_3 <- rowSums(results[, (1 + part1 + part2):(part1 + part2 + part3)])
    
    results$conv_part_1 <- (results$total_part_1 * max_score_part1) / part1
    results$conv_part_2 <- (results$total_part_2 * max_score_part2) / part2
    results$conv_part_3 <- (results$total_part_3 * max_score_part3) / part3
    # requested_output 6: 1/3
    sumstats <- rbind(
      summary(results$conv_part_1),
      summary(results$conv_part_2),
      summary(results$conv_part_3)
    )
    rownames(sumstats) <- c("Part-1:", "Part-2:", "Part-3:")
  }else if(part3 == 1 & part4 == 1){
    results$total_part_1 <- rowSums(results[, 1:part1])
    results$total_part_2 <- rowSums(results[, (part1 + 1):(part1 + part2)])
    
    results$conv_part_1 <- (results$total_part_1 * max_score_part1) / part1
    results$conv_part_2 <- (results$total_part_2 * max_score_part2) / part2
    # requested_output 6: 2/3
    sumstats <- rbind(
      summary(results$conv_part_1),
      summary(results$conv_part_2)
    )
    rownames(sumstats) <- c("Part-1:", "Part-2:")
  }else{
    results$total_part_1 <- rowSums(results[, 1:part1])
    results$total_part_2 <- rowSums(results[, (part1 + 1):(part1 + part2)])
    results$total_part_3 <- rowSums(results[, (1 + part1 + part2):(part1 + part2 + part3)])
    results$total_part_4 <- rowSums(results[, (1 + part1 + part2 + part3):(part1 + part2 + part3 + part4)])
    
    results$conv_part_1 <- (results$total_part_1 * max_score_part1) / part1
    results$conv_part_2 <- (results$total_part_2 * max_score_part2) / part2
    results$conv_part_3 <- (results$total_part_3 * max_score_part3) / part3
    results$conv_part_4 <- (results$total_part_4 * max_score_part4) / part4
    # requested_output 6: 3/3
    sumstats <- rbind(
      summary(results$conv_part_1),
      summary(results$conv_part_2),
      summary(results$conv_part_3),
      summary(results$conv_part_4)
    )
    rownames(sumstats) <- c("Part-1:", "Part-2:", "Part-3:", "Part-4:")
  }
  
  
  #requested_output 1 :EXAM TAKER REPORT:
  exam_results <- as.data.frame(cbind(
    data$st_id,
    data$st_name,
    data$st_surname,
    data$section,
    respo,
    results
  ))
  
  if (part4 == 1 & part3 != 1) {
    st_resp_itemnames <-
      paste0("st_resp@item", 1:(part1 + part2 + part3 ))
    dicho_itemnames <- paste0("item", 1:(part1 + part2 + part3 ))
    
    colnames(exam_results) <-
      c(
        "st_id",
        "st_name",
        "st_fam.name",
        "section",
        st_resp_itemnames,
        dicho_itemnames,
        "Truescore@part1",
        "Truescore@part2",
        "Truescore@part3",
        "conv_sco@part1",
        "conv_sco@part2",
        "conv_sco@part3"
      )
  }else if(part3 == 1 & part4 == 1){
    st_resp_itemnames <-
      paste0("st_resp@item", 1:(part1 + part2))
    dicho_itemnames <- paste0("item", 1:(part1 + part2))
    
    colnames(exam_results) <-
      c(
        "st_id",
        "st_name",
        "st_fam.name",
        "section",
        st_resp_itemnames,
        dicho_itemnames,
        "Truescore@part1",
        "Truescore@part2",
        "conv_sco@part1",
        "conv_sco@part2"
      )
  }else{
    st_resp_itemnames <-
      paste0("st_resp@item", 1:(part1 + part2 + part3 + part4))
    dicho_itemnames <- paste0("item", 1:(part1 + part2 + part3 + part4))
    
    colnames(exam_results) <-
      c(
        "st_id",
        "st_name",
        "st_fam.name",
        "section",
        st_resp_itemnames,
        dicho_itemnames,
        "Truescore@part1",
        "Truescore@part2",
        "Truescore@part3",
        "Truescore@part4",
        "conv_sco@part1",
        "conv_sco@part2",
        "conv_sco@part3",
        "conv_sco@part4"
      )
  }
  #requested_output 2: ITEM LEVEL ANALYSIS:
  scored_respo<-scored_respo[ rowSums(scored_respo[,-1]) > 0, ]
  #item_stats<- ItemAnalysis(scored_respo)
  difficulties<-round(colMeans(scored_respo),2)
  discriminations<-round(item.total(scored_respo)$Item.Total,2)
  
  DISC.LABELS<-labelDiscriminations(discriminations)
  DIFF.LABELS<-labelDifficulties(difficulties)
  
  
  item_stats<- cbind(difficulties, discriminations, DIFF.LABELS, DISC.LABELS )
  colnames(item_stats)<- c("difficulty index", "discrimination index", "difficulty comment", "discrimination comment")
  
  
  
  #requested_output 7:TEST LEVEL ANALYSIS:
  mytable<-table( DIFF.LABELS, DISC.LABELS)
  ....TOTAL<-colSums(mytable)
  ..PERCENT<-round(percent_al(....TOTAL)*100)
  mytable<-rbind(mytable,....TOTAL, ..PERCENT)
  #...TOTAL<-rowSums(mytable)
  # mytable<-cbind(mytable  , ...TOTAL     )
  
  
  #requested_output 3:TEST LEVEL ANALYSIS:
  #güvenirlik katsatısı
  alpha_index<-cronbach.alpha(scored_respo)
  
  #requested_output 4:OPTION LEVEL ANALYSIS:
  if (p_table=="Counts"){p_table<-FALSE } else{ p_table<-TRUE}
  option_level_stats <- ShinyItemAnalysis::DistractorAnalysis(data_for_distractorAnal, 
                                                              ans_for_distractorAnal, 
                                                              p.table  = p_table, num.groups=3)
  
  
  
  if(requested_output == 1){ return(exam_results)}    
  if(requested_output == 2){ return(item_stats)}
  if(requested_output == 3){ return(alpha_index)}
  if(requested_output == 4){ return(option_level_stats )}
  if(requested_output == 5){ return(as.data.frame(scored_respo))}
  if(requested_output == 6){ return(as.data.frame(sumstats))}
  if(requested_output == 7){ return(mytable)}
}

##########USER INTERFACE################################################################
#shiny::runGitHub("dreamRs/fresh", subdir = "inst/examples/create")
ui <- fluidPage(
  theme = tema,

  

                sidebarLayout(
                  sidebarPanel(
                    width = 3,
                    tags$a(href="https://www.etu.edu.tr/en/bolum/department-of-foreign-languages/duyuru/-", 
                           tags$img(src='tobb_etu__logo_tr.png',
                                    align="middle",    
                                    height = "49px", 
                                    style='padding-top: 1px')),
                    
                    tags$hr(),
                    fileInput('file1', 'Upload your .dat file'),
                    tags$b(
                      "Please enter the number of items and maximum possible score for each part below."
                    ),
                    tags$br(),
                    tags$br(),
                    tags$p(
                      tags$b(
                        "If there are LESS THAN FOUR parts, please enter ZERO for both cells."
                      )
                    ),
                    hr(),
                    splitLayout(
                      numericInput(
                        inputId = 'part1',
                        label = 'Part 1',
                        value = 10
                      ),
                      numericInput(
                        inputId = 'max_score_part1',
                        label = 'Max:',
                        value = 100
                      )
                    ),
                    splitLayout(
                      numericInput(
                        inputId = 'part2',
                        label = 'Part 2',
                        value = 10
                      ),
                      numericInput(
                        inputId = 'max_score_part2',
                        label = 'Max:',
                        value = 100
                      )
                    ),
                    splitLayout(
                      numericInput(
                        inputId = 'part3',
                        label = 'Part 3',
                        value = 10
                      ),
                      numericInput(
                        inputId = 'max_score_part3',
                        label = 'Max:',
                        value = 100
                      )
                    ),
                    splitLayout(
                      numericInput(
                        inputId = 'part4',
                        label = 'Part 4',
                        value = 10
                      ),
                      numericInput(
                        inputId = 'max_score_part4',
                        label = 'Max:',
                        value = 100
                      )
                    ),
                    hr(),
                  ),
                  
                  mainPanel(
                    titlePanel(' Test Analyze-R '),
                    tags$p(
                      "This is an application developed specifically for obtaining test scores and for conducting elementary item analysis on test data 
                          which were scanned directly by optical mark recognition machines at TOBB ETÜ-DFL.
                          With this application, you can easily read .dat files, conduct item analysis, and generate detailed reports
                          to measure student performance and identify areas of improvement. 
                           This is a reactive application - 
                          meaning; you do not need to click anywhere to get the output.
                          So, for optimum performance, enter your input into the cells (on the left) before you upload any files. 
                          Then, wait for a few seconds. Reload the page any time you upload a new file.", 
                      tags$b("The only accepted file format is .dat.")),
                    tags$p("You can download",tags$a(href="https://drive.google.com/file/d/1hVEldl2mKDec1NIcRhTKq00v_zPNp_mJ/view?usp=share_link",
                                                     "a sample .dat file"), "in order to try the app (Keep the default input values for the sample)."),
                    hr(),
                    tabsetPanel(
                      tabPanel(
                        "Student Scores",
                        hr(),
                        tags$br(),
                        dataTableOutput(outputId = 'table.output')
                      ),
                      tabPanel(
                        "Test Statistics",
                        hr(),
                        tags$br(),
                        tags$b("DESCRIPTIVE STATISTICS"),
                        tags$p(
                          "Descriptive statistics gives us an insight to our test and sample.
                        We investigate descriptives depending on the data's distribution."
                        ),
                        tags$b(" RELIABILITY"),
                        tags$p(
                          "Cronbach's α is an estimate of the internal consistency of a test. 
                          The α index is between 0 and 1. The higher the better for educational tests."
                        ),
                        tags$b("VALIDITY"),
                        tags$p("Test Analyze-R provides two types of output currently:"),
                        tags$p(tags$b(" Scree Plot ")),
                        tags$p(
                          "A scree plot might give us a clue about the factors of the test.
                        In the screeplot, each dot/triangle located above the red line might indicate a factor.
                        If there are any items with zero variance (meaning: all students gave the same response to an item),
                        a scree plot will NOT be calculated. In that case, you might also use a correlation heat map (below) as a validity check."
                        ),
                        tags$b("Correlation Heat Map"),
                        tags$p(
                          "A correlation heat map displays the correlation between item pairs.
                                 The color of circles indicates in which way the items are correlated-
                                 a blue color means possitive correlation and a red color means negative correlation.
                                 The diogonal will always be dark blue. Other than that, we do not want red or dark blue dots.
                                 Instead, LIGHT BLUE DOTS are expected output in this plot."
                        ),
                        hr(),
                        verbatimTextOutput(outputId = "summary_stats"),
                        hr(),
                        verbatimTextOutput(outputId = "c.alpha"),
                        hr(),
                        plotOutput(
                          outputId = "scree_plot",
                          width = "100%",
                          height = "400px"
                        ),
                        tags$br(),
                        plotOutput(
                          outputId = "cor_plot",
                          width = "100%",
                          height = "400px"
                        ),
                        
                      ),
                      tabPanel(
                        "Item Statistics",
                        hr(),
                        tags$br(),
                        tags$b("ITEM ANALYSIS"),
                        tags$p(
                          "Difficulty of an item is simply:  N of individuals with correct response / N of total group. That's why it is also called mean of the item."),
                        tags$p(
                          "Discrimination of an item, on the other hand, can be calculated with three different methods. 
                          They are listed below and we use the second one in this app."),
                        tags$p(tags$b("1."), "Correlation between the item and total score."),
                        tags$p(tags$b("2."), "Correlation between the item and total score withhout the item."),
                        tags$p(tags$b("3."), "The difference in item score in the upper and lower third of the respondents (Upper-Lower Index, ULI)."),
                        dataTableOutput(outputId = 'item.anal')
                      ),
                      tabPanel(
                        "Distractor Statistics",
                        hr(),
                        tags$br(),
                        tags$b("DISTRACTOR ANALYSIS"),
                        tags$p(
                          "In this report, you will see a table for each item. 
                          In the columns, the test takers are divided into three subgroups according to their total scores."),
                        tags$p(tags$b("Group 1"), "is the test takers with the lowest scores."),
                        tags$p(tags$b("Group 3"), "is the test takers with the highest scores."),
                        tags$p("In the rows, you will see numbers referring to options. 1-5 means A-E.
                               If you don't see an option, it means it is not selected by any test takers."),
                        radioButtons(inputId = "p_table", label = "Choose to swap:", 
                                     choices = c("Counts", "Percentages"), selected = "Counts"),
                    
                        
                         verbatimTextOutput(outputId = "distractor.anal")
                      #  dataTableOutput(outputId = "distractor.anal")
                      ),
                      tabPanel(
                        "Item Writer Report",
                        hr(),
                        tags$br(),
                        verbatimTextOutput(outputId = "commenttable")
                        #  dataTableOutput(outputId = "commenttable")
                      )
                    )
                    
                  )
                ))
##########SERVER####################################################################
server <- function(input, output) {
  options(shiny.sanitize.errors = TRUE)
  #exam results:
  output$table.output <- renderDataTable({    
    if (is.null(input$file1))    return(NULL)
    exam_results <- main_function(
      input$file1$datapath,
      input$part1,
      input$part2,
      input$part3,
      input$part4,
      input$max_score_part1,
      input$max_score_part2,
      input$max_score_part3,
      input$max_score_part4,
      1,
      p_table = input$p_table
    )
    datatable(
      data = exam_results,
      caption = "First select 'show all rows'. Then click on 'Excel' to download the results. ",
      extensions = c('Buttons'),
      options = list(
        dom = 'Bfrtip',
        buttons = c('pageLength',  'excel'),
        pagelength = 5,
        lengthMenu = list(c(5, 25, 50, 100,-1),
                          c('5', '25', '50', '100', 'All'))
      )
    )
  })
  #scree plot:
  output$scree_plot <- renderPlot({
    if (is.null(input$file1))
      return(NULL)
    scored_respo <- main_function(
      datapath = input$file1$datapath,
      input$part1,
      input$part2,
      input$part3,
      input$part4,
      input$max_score_part1,
      input$max_score_part2,
      input$max_score_part3,
      input$max_score_part4,
      5,
      p_table = input$p_table
    )
    
    scree_pl <- fa.parallel(scored_respo, fa = "pc")
  })
  #correlation matrix:
  output$cor_plot <- renderPlot({
    if (is.null(input$file1))
      return(NULL)
    scored_respo <- main_function(
      datapath = input$file1$datapath,
      input$part1,
      input$part2,
      input$part3,
      input$part4,
      input$max_score_part1,
      input$max_score_part2,
      input$max_score_part3,
      input$max_score_part4,
      5,
      p_table = input$p_table
    )
    
    item_cor <- corrplot(round(cor(scored_respo), 1),
                         method = "circle",
                         title="Correlations Heat Map",
                         na.label ="#",
                         na.label.col = "tomato",
                         diag=T)
  })
  #item analysis:
  output$item.anal <- renderDataTable({    
    if (is.null(input$file1))
    return(NULL)
    item_stats <- main_function(
      datapath = input$file1$datapath,
      input$part1,
      input$part2,
      input$part3,
      input$part4,
      input$max_score_part1,
      input$max_score_part2,
      input$max_score_part3,
      input$max_score_part4,
      2,
      p_table = input$p_table
    )
    datatable(
      data = item_stats,
      caption = "First select 'show all rows'. Then click on 'Excel' to download the results. ",
      extensions = c('Buttons'),
      options = list(
        dom = 'Bfrtip',
        buttons = c('pageLength',  'excel'),
        pagelength = 5,
        lengthMenu = list(c(5, 10, 20, 40, 70,-1),
                          c('5', '10', '20', '40', '70', 'All'))
      )
    )
  })
  #reliability index:
  output$c.alpha <- renderPrint({
    validate(need(
      try(input$file1 != "")
      , "A reliability index will appear here when data is uploaded."
    ))
    alpha_index <- main_function(
      datapath = input$file1$datapath,
      input$part1,
      input$part2,
      input$part3,
      input$part4,
      input$max_score_part1,
      input$max_score_part2,
      input$max_score_part3,
      input$max_score_part4,
      3,
      p_table = input$p_table
    )
    print(alpha_index)
  })
  #distractors:
  output$distractor.anal <- renderPrint({
    validate(need(try(input$file1 != "")
                  , "Please upload your data to see the output"))
    option_level_stats <-
      main_function(
        datapath = input$file1$datapath,
        input$part1,
        input$part2,
        input$part3,
        input$part4,
        input$max_score_part1,
        input$max_score_part2,
        input$max_score_part3,
        input$max_score_part4,
        4,
        p_table = input$p_table
      )
    print(option_level_stats)
  })
  # summary_stats:
  output$summary_stats <- renderPrint({
    validate(need(
      try(input$file1 != "")
      , "Summary statistics will appear here when data is uploaded."
    ))
    
    sumstats <- main_function(
      datapath = input$file1$datapath,
      input$part1,
      input$part2,
      input$part3,
      input$part4,
      input$max_score_part1,
      input$max_score_part2,
      input$max_score_part3,
      input$max_score_part4,
      6,
      p_table = input$p_table
    )
    print(sumstats)
  })
  
  # discrimination and difficulty comment table
    output$commenttable <- renderPrint({
    validate(need(try(input$file1 != "")
                  , "Please upload your data to see the output"))
    mycommenttable <-
      main_function(
        datapath = input$file1$datapath,
        input$part1,
        input$part2,
        input$part3,
        input$part4,
        input$max_score_part1,
        input$max_score_part2,
        input$max_score_part3,
        input$max_score_part4,
        7,
        p_table = input$p_table
      )
    print(mycommenttable)
  })
}
######################OUTPUT#######################################################
shinyApp(ui = ui, server = server)
