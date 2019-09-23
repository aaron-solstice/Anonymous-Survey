
# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  dashboardHeader(title = "Anonymous Survey Dashboard", titleWidth = 300),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Survey Overview", tabName = "overview_tab", icon = icon("briefcase")),
      menuItem("Summary", tabName = "summary_tab", icon = icon("list-alt")),
      menuItem("Results", tabName = "results_tab", icon = icon("chart-bar"),
               menuSubItem("ENPS Results", tabName = "results_sub_enps"),
               menuSubItem("Overall 5-pt Distribution", tabName = "results_sub_overall_distribution"),
               menuSubItem("Areas of Company Focus", tabName = "results_sub_focus_areas"),
               menuSubItem("Distribution of Average Scores", tabName = "results_sub_avg_score_dist"),
               menuSubItem("Open Ends Current Wave", tabName = "results_sub_open_ends_current"),
               # menuSubItem("Open Ends Over Time", tabName = "results_sub_open_ends_over_time"),
               menuSubItem("All Question Distributions", tabName = "results_sub_ind_questions"),
               menuSubItem("Explore Individual Question", tabName = "results_sub_ind_questions_exp")
               ),
      menuItem("Growth/Improvement", tabName = "growth_improvement_tab", icon = icon("dashboard"))
    )
    
  ),
  dashboardBody(
    tags$style("html, body {overflow: visible !important;"),
    
    
    tabItems(
      tabItem(tabName = "overview_tab",
                h2("Solstice's Anonymous Employee Survey is comprised of:"),
                br(),
                infoBoxOutput("info_completed_surveys"),
                infoBoxOutput("info_5pt_scale_count"),
                infoBoxOutput("info_10pt_scale_count"),
                infoBoxOutput("info_OE_count"),
                infoBoxOutput("info_total_q_count"),
                infoBoxOutput("info_number_of_waves"),
                selectInput(inputId = "wave_date_overview", choices = wave_dates, label = "Choose Survey Date", width = "50%", multiple = F, selected = current_wave_date)
                
              
      ),
      
      tabItem(tabName = "summary_tab",
              h2("Summary of Findings"),
              fluidRow(
                box(
              h4(shiny::uiOutput(outputId = "summary_txt")) )
              )
      )
      ,tabItem(tabName = "results_tab",
              h2("Results"),
              fluidRow(
                box(
                   )
              )
      )
      ,tabItem(tabName = "results_sub_enps",
               h2("ENPS"),
               selectInput(inputId = "wave_date_enps", choices = wave_dates, label = "Choose Survey Date", width = "25%", multiple = F, selected = current_wave_date),
               fluidRow(
                box(
                  plotlyOutput(outputId = "enps_out"),
                  hr(),
                  plotlyOutput(outputId = "enps_over_time_out")),
                box(title = "ENPS Norms", tableOutput(outputId = "enps_scale_norms")),
                box(h4("ENPS question and how the score is calculated: ON A SCALE OF 1 TO 10 (with 10 being best), how likely are you to recommend this company to a colleague or a friend?
                        (1) Percent of top two score categories (9s and 10s)
                       (2) Percent of Bottom six score categories (1 thru 6)
                       (3)  Subtract."
                )),
                box(h4("March 18, 2019"),  h4("Results suggest that Solstice is performing in the 'Excellent' category. Very few companies can claim an ENPS at this level. "), br(),
                    h4("September 17, 2019"),  h4(
p("ENPS is somewhat of a volatile measure, because scores can move from one category to another to produce a 2-point swing in either direction. 
So, in that sense it is quite sensitive to a hange in sentiment."),

p("While our ENPS scores is still in a healthy range, relative to known norms, the challenges and stresses
we have faced working together in the first year did result in a significant drop in our
ENPS score. Promoter ratings of 9 + 10 on the scale have shifted down to either neutral (7 + 8)
 or detractor levels (1 thru 6). Whereas in the past survey we had fewer than 7% of our ratings
below neutral, now we have 22% of our scores there.") ),
                    
                    background = "light-blue")
               )
      ),
      tabItem(tabName = "results_sub_overall_distribution",
              h2("5-pt Distribution"),
              selectInput(inputId = "wave_date_5p_dist", choices = wave_dates, label = "Choose Survey Date(s)", width = "25%", multiple = T, selected = current_wave_date),
                box(
                  plotlyOutput(outputId = "current_overall_gg"),
                  hr(),
                  plotlyOutput(outputId = "score_distribution_over_time_pltly")),
              box(h4("March 18, 2019"),h4("Most responses were either a 4 or a 5. There is only one grouping of 
                                                 responses which means employees generally agree that their experiences is very positive. "), br(),
                  h4("September 17, 2019"), h4("The results from the 82 individual questions mirrors (and reveals) the change in the ENPS score,
with an overall downward shift in the distribution of scores. Across the board there was a
                                               decrease in the number of top scores (fives and fours), and sharp increase in negative ratings
                                               (ones, twos), as well as neutral ratings (threes). Note, importantly, in spite of the downward shift, the
majority of scores are above a 3, 70% being a 4 or higher.
                                               "), background = "light-blue")
              
              
      ),
      tabItem(tabName = "growth_improvement_tab",
              h2("Steps We Are Taking To Improve"),
              fluidRow(
                box(h4(HTML("March 18, 2019 <br>
                    <ul><li>Additional training for managers.</li>
                   <li>Improve interdepartmental communication</li>
                   <li>Get more movie projects in the pipeline</li>
                   <li>Continue to survey Solstice employees, listen, and respond to formal and informal feedback</li></ul>
                             
              September 17, 2019, <br>
                              <ul><li>At this point Senior Management will focus on paying more attention to the issues raised here,
                            and is already thinking of solutions to address them.</li>
                              <li>One concrete step is that we’ve identified a management consulting company who will be
                            coming in to train anyone in management to do a better job.</li>
                              <li>Actual steps will be taken.</li></ul>"
                ) )
                  
                )
                
                

              )
      ),
      tabItem(tabName = "results_sub_focus_areas",
              

              h2("We have 8 categories of questions on the Solstice experience."),
              selectInput(inputId = "wave_sub_focus_areas", choices = wave_dates, label = "Choose Survey Date(s)", width = "25%", multiple = T, selected = current_wave_date),
              box(
                tableOutput("current_8_categories"),
                hr(),
                plotlyOutput(outputId = "categories_table_over_time_gg")
              
              ),
              box(h4("March 18, 2019"),  h4("Solstice scored well in every category. `A Place Where Management is Trusted` scored the lowest of all, and will be an area of focus while we grow as a company. 
                                                 "), br(),
                  h4("September 17, 2019"), h4("When we examine the areas that decreased the most, our assessment of what it’s like to work
at Solstice is that it’s harder to trust management, that we don’t feel as psychologically safe to
                                               take risks and operate independently. Many therefore feel they are not as productive as they
                                               could be. And we see a corresponding drop in the feeling that Solstice is a good place to work.
                                               "),
                  background = "light-blue")
              ),
      
      tabItem(tabName = "results_sub_avg_score_dist",
              h2("Distribution of Average Scores Across All 5-pt Questions"),
              selectInput(inputId = "wave_sub_avg_score_dist", choices = wave_dates, label = "Choose Survey Date(s)", width = "25%", multiple = T, selected = current_wave_date),
              box(plotlyOutput(outputId = "dist_across_respondents_5pt_plty") ),
              box(h4("March 18, 2019"),  h4("The vast majority of average scores are above 4.0, and none below 3.5. We see, on a per employee basis, a unified and positive experience that all Solstice employees share. 
                                                 "), br(),
                  h4("September 17, 2019"), h4("Given the decreasing scores across the board, and especially in specific areas, we wanted to
check the disparity of scores for individuals. We can see that while there’s a general downward
                                               shift from the very high scores from the last wave, we don’t see signs of bimodality (extremely
                                               low scores and extremely high scores).
                                                 "),
                  background = "light-blue")
              
              
    
      ),
      tabItem(tabName = "results_sub_open_ends_current",
              h2("Open End Results"),
              selectInput(inputId = "wave_sub_open_ends_current", choices = wave_dates, label = "Choose Survey Date(s)", width = "25%", multiple = F, selected = current_wave_date),
              box (plotlyOutput(outputId = "current_oe_ggpltly"),  width = 10 ),
              box(h4("March 18, 2019"),  h4("The vast majority of average scores are above 4.0, and none below 3.5. We see, on a per employee basis, a unified and positive experience that all Solstice employees share. 
                                                 "), 
                 h4("September 17, 2019"),  
h4(
p("Turning to the open ends to gain insight into what is working well and what can improve, we
see that the company is great at communication, collaboration, and then speed, transparency,
                                                      and providing a positive environment for employees."),
                                                    p("However, areas where we can improve mention stop the persistent micromanaging, they want
more flexible work hours, and there were several people mentioning wanting to bring their
                                                      dogs to work."),
                                                    p("A note for managers: people appreciate being included as much as possible, in decisions, brain-
storming sessions, in understanding department and company challenges, possible strategies,
                                                      and allowing them to contribute solutions. Next to that we love being authentically praised and
                                                      recognized for our contributions. As much as people want inclusion, they dislike being
                                                      micromanaged, and they dislike feeling like they or others are disrespected by their managers."),
                                                    p("The best things we can do to make employees feel more valued is to trust them with more
responsibility, while encouraging them through positive feedback and constructive criticism.") ) ,
                  background = "light-blue", width = 10)
              
              
              
      ),
     
      # tabItem(tabName = "results_sub_open_ends_over_time",
      #         h2("Open End Results Over Time"),
      #         
      #         plotlyOutput(outputId = "overtime_oe_ggpltly", height = "1000px") 
      # 
      #         
      #         
      # ),
      tabItem(tabName = "results_sub_ind_questions",
              h2("Individual Question Distributions"),
              selectInput(inputId = "wave_date_ind_questions", choices = wave_dates, label = "Choose Survey Date", width = "25%", multiple = F, selected = current_wave_date),
              plotlyOutput(outputId = "by_question_distribution_ggpltly", height = "1000px") 

              
              
      ),
      tabItem(tabName = "results_sub_ind_questions_exp",
              h2("Explore Individual Questions"),
              
              selectInput(inputId = "question_text_str", choices = question_txt_li, label = "Choose Question", width = "50%" ),
              selectInput(inputId = "wave_date_ind_questions_exp", choices = wave_dates, label = "Choose Survey Date", width = "50%", multiple = T, selected = current_wave_date),
              plotlyOutput(outputId = "indiv_question_distributions") 
              
              
              
      )
      
      
      


    )
    
    
    
  )
)
