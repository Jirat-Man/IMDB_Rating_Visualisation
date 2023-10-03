library(ggplot2)
library(readxl)
library(tidyverse)
library(conflicted)
library(reshape2)
library(plyr)
library(dplyr)
library(plotly)
library(gganimate)
library(gifski)
library("viridis")
library(mice)
library("grDevices")
library(GGally)
library(fmsb)
library(ggradar)
library(shiny)
library(quantmod)


############################### Loading data ###################################
imdb1000 <- read.csv("C:/Users/Ga401/Downloads/
Visualisation_Project
                    /imdb_top_1000.csv", header=TRUE)

### checking data ###
View(imdb1000)
str(imdb1000)
glimpse(imdb1000)

############################## Data Cleaning ###################################
#### changing datatypes ####
imdb1000$Released_Year <- as.integer(imdb1000$Released_Year)
imdb1000$Runtime <- as.integer(gsub(" min","", imdb1000$Runtime))
imdb1000$Meta_score <- as.integer(imdb1000$Meta_score)
imdb1000$No_of_Votes <- as.integer(imdb1000$No_of_Votes)
imdb1000$Gross <- as.integer(gsub("," , "" , imdb1000$Gross))

#### imputing missing data #####

# checking for NAs in all columns 
colSums(is.na(imdb1000))

# Impute Released_Year
imdb1000$Series_Title[is.na(imdb1000$Released_Year)]  # Apollo 13 
imdb1000$Released_Year[is.na(imdb1000$Released_Year)] <- 1995 # from Google search

# Impute Meta_score
Meta_score_new <- imdb1000[c("Runtime","IMDB_Rating", # make new dataframe with numerical data
                             "Released_Year","No_of_Votes","Meta_score")]
imputed_ms <- mice(Meta_score_new, m=1, maxit = 50, method = 'pmm', seed = 100) # impute using predictive mean matching
summary(imputed_ms)
imputed_ms$imp
colnames(imputed_ms$imp$Meta_score) <- 'Meta_score'

ggplot(imputed_ms$imp$Meta_score, aes(x=Meta_score)) + geom_bar()
ggplot(complete(imputed_ms), aes(x=Meta_score)) + geom_bar()
ggplot(imdb1000, aes(x=Meta_score)) + geom_bar()

imdb1000$Meta_score <- complete(imputed_ms)$Meta_score

# Impute Gross
Gross_new <- imdb1000[c("Runtime","IMDB_Rating","Released_Year","No_of_Votes","Gross")]
imputed_gross <- mice(Gross_new, m=1, maxit = 50, method = 'pmm', seed = 100)
summary(imputed_gross)
imputed_gross$imp$Gross
colnames(imputed_gross$imp$Gross) <- 'gross'

ggplot(imdb1000, aes(x = Gross, y=..density.., )) +  
  geom_histogram(binwidth=1000000,color='grey', fill="grey") +
  geom_density(alpha=.2, fill="#FF6666")

ggplot(imputed_gross$imp$Gross, aes(x = gross, y=..density.., )) +  
  geom_histogram(binwidth=1000000,color='grey', fill="grey") +
  geom_density(alpha=.2, fill="#FF6666")


imdb1000$Gross <- complete(imputed_gross)$Gross

# Changing certificates with same meaning to same label
    # R <- A, R, TV-MA
    # Approved <- Approved, Passed
    # G <- G, U
    # PG <- GP, PG, TV-PG
    # UA <- UA, U/A

imdb1000["Certificate"][imdb1000["Certificate"] == "A" | imdb1000["Certificate"] == "TV-MA"] <- "R"
imdb1000["Certificate"][imdb1000["Certificate"] == "Passed"] <- "Approved"
imdb1000["Certificate"][imdb1000["Certificate"] == "U"] <- "G"
imdb1000["Certificate"][imdb1000["Certificate"] == "GP" | imdb1000["Certificate"] == "TV-PG"] <- "PG"
imdb1000["Certificate"][imdb1000["Certificate"] == "U/A"] <- "UA"


# Separating genres in each row and combining all of them into a list
# Also created a list of their associated released year (both have same length 2541)
    # eg. Row1: 1988 "Drama, Romance" 
    #     Row2: 2000 "Action, Crime"  
    #     --> result: Genre_sep = c("Drama", "Romance", "Action", "Crime") 
    #                 Year_sep = c(1988, 1988, 2000, 2000)

Genre_list <- as.vector(imdb1000$Genre) # save 'Genre' column as a list
Genre_sep<- list() # creating an empty list
Year_sep <- list()
Star1_sep <- list()
Star2_sep <- list()
Star3_sep <- list()
Star4_sep <- list()

k <- 1
for (i in 1:length(Genre_list)) {  # looping through all rows
  lst <- as.list(strsplit(Genre_list[i], ", ")[[1]]) # splitting str when encounter ", "
  j <- 1
  while (j <= length(lst)) { # looping through alls genre in each row
    Genre_sep[[k]] <- lst[j] # appending to the initially empty list
    Year_sep[[k]] <- as.vector(imdb1000$Released_Year)[i]
    Star1_sep[[k]] <- as.vector(imdb1000$Star1)[i]
    Star2_sep[[k]] <- as.vector(imdb1000$Star2)[i]
    Star3_sep[[k]] <- as.vector(imdb1000$Star3)[i]
    Star4_sep[[k]] <- as.vector(imdb1000$Star4)[i]
    
    j <- j+1
    k <- k+1
  }
}
Star1_sep

Genres_year = data.frame(unlist(Year_sep),unlist(Genre_sep),unlist(Star1_sep),unlist(Star2_sep),unlist(Star3_sep),unlist(Star4_sep)) # combine 2 lists to make df
names(Genres_year) = c("Year","Genre","Star1","Star2","Star3","Star4") # naming columns

Group_gnryr <- Genres_year %>% group_by(Year,Genre) %>%  # group data by year followed by genre 
  summarise(total_count=n(), # and count number of each genre in each year
            .groups = 'drop') %>%   
  as.data.frame()

Group_gnryr <- Group_gnryr %>% group_by(Year) %>%
  mutate(percentage = total_count/sum(total_count)) %>%
  mutate(ypos = cumsum(percentage)- 0.5*percentage ) %>%
  as.data.frame()

Group_gnryr

Group_gnrstr1 <- Genres_year %>% 
  group_by(Genre, Star1) %>%   
  dplyr::summarise(total_count=n(), 
            .groups = 'drop') %>%   
  as.data.frame() %>%
  group_by(Genre)

Group_gnrstr2 <- Genres_year %>% 
  group_by(Genre, Star2) %>%   
  dplyr::summarise(total_count=n(), 
            .groups = 'drop') %>%   
  as.data.frame() %>%
  group_by(Genre)

Group_gnrstr3 <- Genres_year %>% 
  group_by(Genre, Star3) %>%   
  dplyr::summarise(total_count=n(), 
            .groups = 'drop') %>%   
  as.data.frame() %>%
  group_by(Genre)

Group_gnrstr4 <- Genres_year %>% 
  group_by(Genre, Star4) %>%   
  dplyr::summarise(total_count=n(), 
            .groups = 'drop') %>%   
  as.data.frame() %>%
  group_by(Genre)
  
Group_gnrstr1 <- Group_gnrstr1[order(Group_gnrstr1$Genre, -Group_gnrstr1$total_count),]
Group_gnrstr2 <- Group_gnrstr2[order(Group_gnrstr2$Genre, -Group_gnrstr2$total_count),]
Group_gnrstr4

##################### Exploratory visualization (initial questions) ###############

# plots
corr_df <- imdb1000[c("Released_Year", "Certificate","Runtime","IMDB_Rating","Meta_score","Gross")]
png("Correlation.png", width = 2200, height = 1400, res = 225)
ggpairs(corr_df)
dev.off

png("Genre_count.png", width = 1800, height = 1200, res = 225)
ggplot(Genres_year, aes(x=Genre)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(title = "Counts of Movie Genres in Top 1000 IMDB",
       caption = "Movies are associated with up to 3 different genres, 
       this plot represents all associations for each genre") +
  xlab("Count") + ylab("Genre")
dev.off()

png("Certificate_count.png", width = 1800, height = 1200, res = 225)
ggplot(imdb1000, aes(x=Certificate)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(title = "Counts of Movie Certificates in Top 1000 IMDB") +
  xlab("Count") + ylab("Certificate")
dev.off()


png("Certificate_IMDB.png", width = 1800, height = 1200, res = 225)
ggplot(imdb1000, aes(x = Certificate, y = IMDB_Rating)) +
  geom_violin(fill="#69b3a2", trim=FALSE) +
  geom_boxplot(width=0.1) +
  labs(title = "IMDB Ratings for Different Movie Certificates") +
  ylab("IMDB Rating") + xlab("Certificate") 
dev.off()


ggplot(imdb1000, aes(x=Meta_score)) + geom_bar()

boxplot(imdb1000$Meta_score)


ggplot(imdb1000, aes(x = Gross, y=..count.., )) +  
  geom_histogram(binwidth=10000000,color="white", fill="grey")

ggplot(imdb1000, aes(x = IMDB_Rating, y = Meta_score)) +  
  geom_jitter() +
  geom_smooth(method = loess) 

png("Count_ReleasedYear.png", width = 1400, height = 1200, res = 225)
ggplot(imdb1000, aes(x=Released_Year)) +
  geom_bar()+
  labs(title = "Number of Movies Released each Year") +
  ylab("Count") + xlab("Year")
dev.off()

ggplot(imdb1000, aes(x = Released_Year, y = Gross)) +  geom_jitter() +
  geom_smooth(method = loess) 

ggplot(imdb1000, aes(x = IMDB_Rating, 
                     y = Gross, 
                     size = Runtime)) +  
      geom_jitter(alpha= 0.5)

ggplot(imdb1000, aes(x = IMDB_Rating, 
                     y = Gross))+  
  geom_jitter(alpha= 0.5)

imdb1000 %>%
  plyr::mutate( bin=cut_width(IMDB_Rating, width=0.25, boundary=7.0) ) %>%
  ggplot( aes(x=bin, y=Gross) ) +
  geom_boxplot(fill="#69b3a2") +
  xlab("IMDB Rating")

ggplot(imdb1000, aes(x = Runtime, 
                     y = Gross))+  
  geom_jitter(alpha= 0.5)

png("Density_runtime.png", width = 1400, height = 1200, res = 225)
ggplot(imdb1000, aes(x = Runtime, y=..density.., )) +  
  geom_histogram(binwidth=5,color="white", fill="grey") +
  geom_vline(aes(xintercept=mean(Runtime)),
               color="blue", linetype="dashed", size=1) +
  geom_density(alpha=.2, fill="#FF6666") +
  labs(title = "Density Plot of Movie Runtimes") +
  ylab("Density") + xlab("Runtime (min)")
dev.off()


############################# Advanced plots ###################################
####### Load new datasets to get info of budget ##########
#### Dataset 2 #####
Bdgt1 <- read.csv("~/Downloads/TheNumbersMovieBudgetData.csv", header=TRUE)
Bdgt1$budget
Bdgt1$budget <- gsub("[[:punct:]]" , "" , Bdgt1$budget)
Bdgt1$budget <- as.integer(str_trim(Bdgt1$budget,c("left")))
View(Bdgt1)

# Left join data keeping top 1000 IMDB movies
imdb_bdgt1 <- left_join(imdb1000, Bdgt1, by = join_by("Series_Title" == "title","Released_Year"=='year'))

# NAs
colSums(is.na(imdb_bdgt1))

#### Dataset 3 ####
Bdgt2 <- read.csv("~/Downloads/movies-budget.txt", header=TRUE)

imdb_bdgt2 <- left_join(imdb_bdgt1, Bdgt2, by = join_by("Series_Title" == "name",
                                                        "Released_Year"=='year'))
imdb_bdgt2$budget.x <- ifelse(is.na(imdb_bdgt2$budget.x), 
                              imdb_bdgt2$budget.y, imdb_bdgt2$budget.x)
colSums(is.na(imdb_bdgt2))
select(imdb_bdgt2, budget.x, budget.y)

#### Dataset 4 ####
Bdgt3 <- read.csv("~/Downloads/movie_profit.txt", header=TRUE)

class(Bdgt3$release_date)
Bdgt3$year <- as.integer(gsub(".*/","",Bdgt3$release_date))

imdb_bdgt3 <- left_join(imdb_bdgt2, Bdgt3, by = join_by("Series_Title" == "movie","Released_Year"=='year'))
colSums(is.na(imdb_bdgt3))
imdb_bdgt3$budget.x <- ifelse(is.na(imdb_bdgt3$budget.x), 
                              imdb_bdgt3$production_budget, imdb_bdgt3$budget.x)
colSums(is.na(imdb_bdgt3))

#### Dataset 5 ####
Bdgt4 <- read.csv("~/Downloads/IMDB Top 250 Movies.csv", header=TRUE)

imdb_bdgt4 <- left_join(imdb_bdgt3, Bdgt4, by = join_by("Series_Title" == "name","Released_Year"=='year'))
colSums(is.na(imdb_bdgt4))
imdb_bdgt4$budget.x <- ifelse(is.na(imdb_bdgt4$budget.x), 
                              imdb_bdgt4$budget, imdb_bdgt4$budget.x)
colSums(is.na(imdb_bdgt4))

#### Dataset 6 ####
Bdgt5 <- read.csv("~/Downloads/movies_metadata.csv", header=TRUE)

Bdgt5["budget"][Bdgt5["budget"] == 0] <- NA
Bdgt5$year <- as.integer(gsub("-.*","",Bdgt5$release_date))

imdb_bdgt5 <- left_join(imdb_bdgt4, Bdgt5, by = join_by("Series_Title" == "original_title","Released_Year"=='year'))
colSums(is.na(imdb_bdgt5))
imdb_bdgt5$budget.x <- ifelse(is.na(imdb_bdgt5$budget.x), 
                              imdb_bdgt5$budget.y.y, imdb_bdgt5$budget.x)
colSums(is.na(imdb_bdgt5))

imdb_bdgt<-imdb_bdgt5[, which(colMeans(!is.na(imdb_bdgt5)) > 0.65)]
colSums(is.na(imdb_bdgt))

str(imdb_bdgt)
colnames(imdb_bdgt)[17] <-'Budget'
imdb_bdgt$Budget
which(is.na(as.numeric(imdb_bdgt$Budget)) != is.na(imdb_bdgt$Budget))
imdb_bdgt$Budget <- as.integer(imdb_bdgt$Budget) # NAs introduced from "Not Available" text

 #### Histogram of Gross #####
annotations <- data.frame(
  x = c(min(imdb1000$Gross,na.rm = T), round(mean(imdb1000$Gross,na.rm = T),0), max(imdb1000$Gross,na.rm = T)),
  y = c(300, 40, 10),
  label = c("Min:", "Mean:", "Max:")
) # creating labels for min max mean

ggplot(imdb1000, aes(x=Gross)) + 
  geom_histogram(bins=60,color="white", fill="grey") +
  geom_text(data = annotations, aes(x = x, y = y, label = paste(label, x)), size = 2, fontface = "bold")

str(annotations) # checking the dataframe content

imdb1000 %>%
  # bin Gross into bin size of 
  mutate(Gross_cut = cut(Gross,seq(1200,9.38e8,20000000))) %>% 
  #Count and mean of 
  group_by(Gross_cut) %>% 
  summarise(
    n = n(),
    IMDB = mean(IMDB_Rating), 
    Meta = mean(Meta_score)
  ) %>% 
  ggplot(aes(x=Gross_cut)) +
  #scatter plot 
  geom_jitter(aes(y = n, color=IMDB))



#### Interctive plot of stars popularity ####
  # final plot is a bar chart of the top 10 most appeared star as a star1 - star4 in this dataset 

# make 1D list of star1, star2, star3, star4
Stars_list <- c(as.vector(imdb1000$Star1), as.vector(imdb1000$Star2), 
                as.vector(imdb1000$Star3), as.vector(imdb1000$Star4))
Star_table <- sort(table(Stars_list), decreasing = TRUE)[1:10] # table top 10 most appeared actors and the frequency
Star_names <- names(sort(table(Stars_list), decreasing = TRUE)[1:10]) # the names of those 10 actors
Stars_df <- as.data.frame(table(imdb1000$Star1)[Star_names]) # create dataframe of those stars as a star1
colnames(Stars_df)[1] <- "Star_Names"
colnames(Stars_df)[2] <- "Star1_freq"
Star2_freq <- as.vector(table(imdb1000$Star2)[Star_names]) # list of those stars as a star2
Star3_freq <- as.vector(table(imdb1000$Star3)[Star_names]) # list of those stars as a star3
Star4_freq <- as.vector(table(imdb1000$Star4)[Star_names]) # list of those stars as a star4
Stars_df <- cbind(Stars_df, Star2_freq, Star3_freq,Star4_freq) # create 3 new columns in Stars_df from these 3 lists
Stars_df

cbp1 <- c("yellow", "green","firebrick1", "maroon1", "darkorchid1",
          "royalblue1", "orange")

#plot
Star_fig <- plot_ly(Stars_df, x = ~Star_Names, y = ~Star1_freq, type = 'bar', name = 'Star 1',marker = list(color = 'royalblue')) 
Star_fig <- Star_fig %>% add_trace(y = ~Star2_freq, name = 'Star 2', marker =  list(color = "#FF6666"))
Star_fig <- Star_fig %>% add_trace(y = ~Star3_freq, name = 'Star 3',marker =  list(color ="#00CC66"))
Star_fig <- Star_fig %>% add_trace(y = ~Star4_freq, name = 'Star 4',marker = list(color = "#FFFF66"))
Star_fig <- Star_fig %>% plotly::layout(title = list(text = paste0('Top 10 Most Appeared Stars in IMDB Top 1000',
                                                                   '<br>',
                                                                   '<sup>',
                                                                   'Counts grouped by role rank',
                                                                   '</sup>'), size = 20),
                                        yaxis = list(title = 'Count'),
                                        xaxis = list(title = 'Star Names'),
                                        barmode = 'stack', 
                                        plot_bgcolor = "#e5ecf6")
m <- list(l = 0, r = 0, b = 0, t = 80, pad = 4)
Star_fig <- Star_fig %>% plotly::layout(autosize = F, width = 500, height = 400, margin = m)

Star_fig # warning message due to plot omitting NAs

#### How does the popularity of genres evolved through the years? ####

options(gganimate.dev_args = list(width = 9, height = 9, units = 'in', res=320))

bar1 <- ggplot(Group_gnryr, aes(x = "", y = percentage, fill=Genre)) + 
  geom_bar(stat = 'identity', color='white') +
  coord_polar("y", start = 0)+
  #ylim(-1, 28) +
  transition_states(Year, transition_length = 1,state_length=30) +
  labs(title = 'Counts of Movies in Different Genres', 
       subtitle = "Year: {closest_state}", y = 'Counts') +
  theme(legend.position = "right") +
  geom_label(aes(y=1-ypos, x=1.5, label = paste(Genre, '\n Counts:' ,total_count)), size=2.5)


animate(bar1, end_pause = 10, fps = 1, renderer = gifski_renderer())


############ Budget and IMDB Rating #################

imdb_bdgt %>%
  plyr::mutate( bin=cut_width(IMDB_Rating, width=0.25, boundary=7.0) ) %>%
  ggplot( aes(x=bin, y=Budget) ) +
  geom_boxplot(fill="#69b3a2") +
  xlab("IMDB Rating")

budgetIMDB_stats <- imdb_bdgt %>%    # Get mean & standard deviation by year
  group_by(IMDB_Rating) %>%
  summarise_at(vars(Budget),
               list(Budget_mean = mean,
                    sd = sd),
               na.rm = TRUE)%>% 
  as.data.frame()

b <- ggplot(budgetIMDB_stats,    # plot with means & standard deviation
       aes(x = IMDB_Rating,
           y = Budget_mean)) + 
  geom_errorbar(aes(ymin = Budget_mean - sd,
                    ymax = Budget_mean + sd)) +
  geom_point() +
  geom_smooth()

ggplotly(b)


############ How does Gross relate with production cost #################

p <- imdb_bdgt %>% drop_na(Budget) %>% ggplot(aes(x=Budget, y=Gross)) +
  #stat_binhex(bins=50) +
  geom_jitter(aes(size=IMDB_Rating, color=Runtime),fill="gray",alpha=0.5 ) +
  geom_smooth(method = 'lm', formula = y~x) +
  labs(title = "Top 10 Directors with the Most Movies",
       subtitle = "with Average Gross and Ratings",
       color='Runtime',
       size ='IMDB Rating') +
  scale_color_continuous(name = "My Legend Title") +
  scale_size_continuous(name = "My Size Scale")
p

ggplotly(p, source = "select", tooltip = c("Gross",'IMDB_Rating',"Runtime"),dynamicTicks = TRUE)



ui <- fluidPage(
  plotlyOutput("gross")
)

server <- function(input, output, session) {
  output$gross <- renderPlotly({
    # set up plot
    p1 <- imdb_bdgt %>% drop_na(Budget) %>%
                       ggplot(aes(x = Budget, 
                                  y = Gross,
                                  key = Series_Title)) +
      geom_jitter(aes(size = IMDB_Rating,
                      color = Runtime),
                  alpha=0.6, stat='identity', position='identity')  +
      labs(title = paste0("Relationship between Gross and Budget",
                        '<br>', 
                        '<sup>',
                        'with movie\'s title appearing when clicked, and size and color indicating IMDB rating and runtime respectively',
                        '</sup>'),
           color='Runtime',
           size ='IMDB Rating') 
    
    
    # get clicked point
    click_data <- event_data("plotly_click", source = "select")
    # if a point has been clicked, add a label to the plot
    if(!is.null(click_data)) {
      label_data <- data.frame(x = click_data[["x"]],
                               y = click_data[["y"]],
                               label = paste0(click_data[["key"]], 
                                              " (", imdb_bdgt$Released_Year[imdb_bdgt$Series_Title == click_data[["key"]]], ")"),
                               stringsAsFactors = FALSE)
      p1 <- p1 + 
        geom_text(data = label_data,
                  aes(x = x, y = y, label = label),
                  inherit.aes = F, nudge_x = 0.25) 
    }
    # return the plot
    gp <- ggplotly(p1, source = "select", tooltip = c("Budget","Gross",'IMDB_Rating',"Runtime"), dynamicTicks = TRUE) 
    gp
  })
}

shinyApp(ui, server)


######### Were Gross and Budget aftected by the State of the Economy? ###########

GDP_Growth <- read_excel("Downloads/statistic_id996758_annual-gdp-growth-for-the-united-states-1930-2021.xlsx", 
                          sheet = "Data", col_names = FALSE, skip = 4) 

# Clean GDP_Growth
str(GDP_Growth)
colnames(GDP_Growth) <- c('Year','Growth','Unit')
GDP_Growth$Year <- as.integer(GDP_Growth$Year)
GDP_Growth$Color <-  ifelse(GDP_Growth$Growth<0, 
                             "#dc3545", '#28a745')

# Yearly average of gross and budget dataframe
gross_budget_avg <- imdb_bdgt %>%    # Get mean & standard deviation by year
  group_by(Released_Year) %>%
  summarise_at(vars(Gross:Budget),
               mean, na.rm = TRUE) %>% 
  as.data.frame()

colnames(gross_budget_avg)[1] <- 'Year' 

# merge dataframe 
final_gross_df <- merge(gross_budget_avg,GDP_Growth,by='Year')

# Button Gross and Budget Timeline
high_annotations <- list(
  x=final_gross_df$Year[final_gross_df$Gross == max(final_gross_df$Gross)], 
  y=max(final_gross_df$Gross),
  xref='x', yref='y',
  text=paste0('Gross High: $',signif(max(final_gross_df$Gross),4)),
  ax=0, ay=-40
)

high_annotations_1 <- list(
  x=na.omit(final_gross_df$Year[final_gross_df$Budget == max(final_gross_df$Budget, na.rm = TRUE)]), 
  y=max(final_gross_df$Budget, na.rm = TRUE),
  xref='x', yref='y',
  text=paste0('Budget High: $',signif(max(final_gross_df$Budget, na.rm = TRUE),4)),
  ax=0, ay=-40
)

low_annotations <- list(
  x=final_gross_df$Year[final_gross_df$Gross == min(final_gross_df$Gross)], 
  y=min(final_gross_df$Gross),
  xref='x', yref='y',
  text=paste0('Gross Low: $',signif(min(final_gross_df$Gross),4)),
  ax=0, ay=50
)

low_annotations_1 <- list(
  x=na.omit(final_gross_df$Year[final_gross_df$Budget == min(final_gross_df$Budget, na.rm = TRUE)]), 
  y=min(final_gross_df$Budget, na.rm = TRUE),
  xref='x', yref='y',
  text=paste0('Budget Low: $',signif(min(final_gross_df$Budget, na.rm = TRUE),4)),
  ax=0, ay=30
)

# updatemenus component
updatemenus <- list(
  list(
    active = -1,
    type= 'buttons',
    buttons = list(
      list(
        label = "High",
        method = "update",
        args = list(list(visible = c(TRUE, TRUE)),
                    list(title = "High",
                         annotations = list(c(), high_annotations, high_annotations_1)))),
      list(
        label = "Low",
        method = "update",
        args = list(list(visible = c(TRUE, TRUE)),
                    list(title = "Low",
                         annotations = list(low_annotations, low_annotations_1 ,c() )))),
      list(
        label = "Both",
        method = "update",
        args = list(list(visible = c(TRUE, TRUE)),
                    list(title = "Both",
                         annotations = list(low_annotations,low_annotations_1 , high_annotations, high_annotations_1 )))),
      list(
        label = "Reset",
        method = "update",
        args = list(list(visible = c(TRUE, TRUE)),
                    list(title = "Average Annual Gross and Budget with USA Economic Growth 1930-2020 ",
                         annotations = list(c(), c())))))
  )
)

ayL <- list(
  tickfont = list(color = "black"),
  title = "Gross and Budget Value $",
  range = c((max(final_gross_df$Gross)+40e6) / (max(final_gross_df$Growth)+40) * (min(final_gross_df$Growth)-5), 
            max(final_gross_df$Gross)+40e6),
  gridcolor = 'ffff'
  )

ayR <- list(
  tickfont = list(color = "black"),
  overlaying = "y",
  side = "right",
  title = "USA Economic Growth %",
  range = c(min(final_gross_df$Growth)-5, max(final_gross_df$Growth)+40),
  rangemode = "tozero")


fig <- plot_ly(final_gross_df) 
fig <- fig %>% add_lines(x=~Year, y=~Gross, name="Average Gross",
                         line=list(color="royalblue"),type = 'scatter',mode='line',yaxis = 'y1') 
fig <- fig %>% add_lines(x=~Year, y=~Budget, name="Average Budget",
                         line=list(color="red"),type = 'scatter',mode='line',yaxis = 'y1') 
fig <- fig %>% add_bars(x=~Year, y=~Growth, marker = list(color = ~Color), 
                        name="GDP Growth",yaxis = 'y2', opacity = 0.6)
fig <- fig %>% plotly::layout(title = "Average Annual Gross and Budget with USA Economic Growth 1930-2020", showlegend=T,
                              yaxis2 = ayR,
                              xaxis=list(title="Year", gridcolor = 'ffff'),
                              yaxis= ayL,
                              updatemenus=updatemenus,
                              plot_bgcolor = "#e5ecf6"
                              )
fig


####### Top ten highest grossing directors #########

Dir_table <- sort(table(as.vector(imdb1000$Director)), decreasing = TRUE)[1:10]
Dir_name <- as.vector(names(Dir_table))
Dir_df <- imdb1000[imdb1000$Director %in% Dir_name,]
Dir_mean_df <- Dir_df %>% 
  group_by(Director) %>% 
  dplyr::summarise(mean_gross=mean(Gross),
            mean_imdb= mean(IMDB_Rating),
            .groups = 'drop') %>%
  as.data.frame()

Dir_mean_df

Dir_final_df <- inner_join(Dir_mean_df,as.data.frame(Dir_table), by=join_by("Director"=="Var1"))
Dir_final_df <- Dir_final_df %>% dplyr::arrange(desc(Freq), dplyr::desc(mean_imdb))
View(Dir_final_df)

png("Dot_Top10_Director.png", width = 1300, height=1000, res=220)
ggplot(Dir_final_df,aes(x = factor(Director, level=Dir_name), y = Freq)) +
  geom_segment(aes(xend = Director, yend = 0), colour = "grey50") +
  geom_point(aes(color = mean_imdb, size = mean_gross)) +
  scale_fill_viridis_c(option = "magma") +
  coord_flip() +
  ylim(0, 15) +
  xlab("Director") +
  ylab("Number of Movies") +
  labs(title = "Top 10 Directors with the Most Movies",
       subtitle = "with Average Gross and Ratings",
       color='Average\nIMDB Rating',
       size ='Average Gross')
dev.off()

######### Movies IMDB Rating for each Certificate Separated by Top 6 most common language ############

ggplot(imdb_bdgt, aes(x = Certificate, y = IMDB_Rating)) +
  geom_boxplot(fill="#69b3a2") +
  facet_wrap( ~original_language)

ggplot(imdb_bdgt, aes(x = original_language, y = IMDB_Rating)) +
  geom_violin(fill="#69b3a2", trim=F) +
  geom_boxplot(width = .1, fill = "black", outlier.colour = NA)

imdb_bdgt$IMDB_Rating[imdb_bdgt$original_language == 'bs']


ggplot(imdb_bdgt, aes(x = Released_Year, fill = as.factor(original_language))) +
  geom_histogram() +
  #geom_vline(data = class_mean, aes(xintercept = Mean), linetype = "dashed") +
  facet_wrap(~ original_language, scales = "free")

lang_count <- imdb_bdgt %>%
  group_by(Released_Year, original_language) %>%
  summarise(Count = n(),
            .groups = 'drop') %>% 
  as.data.frame()

lang_count

area <- ggplot(lang_count, aes(x = Released_Year, y = Count, fill = original_language)) +
  geom_col()

ggplotly(area)


Top_stars1_genre <- Group_gnrstr1[is.element(Group_gnrstr1$Star1, Star_names),]
Top_stars1_genre <- dcast(Top_stars1_genre, Star1~Genre)
colnames(Top_stars1_genre)[1] <- "Star"
Top_stars1_genre <- Top_stars1_genre %>% slice(match(Star_names, Star))
Top_stars1_genre[is.na(Top_stars1_genre)] <- 0
Top_stars1_genre <- rbind(rep(12,18) , rep(0,18) , Top_stars1_genre)

Top_stars2_genre <- Group_gnrstr2[is.element(Group_gnrstr2$Star2, Star_names),]
Top_stars2_genre <- dcast(Top_stars2_genre, Star2~Genre)
colnames(Top_stars2_genre)[1] <- "Star"
Top_stars2_genre <- Top_stars2_genre %>% slice(match(Star_names, Star))


Top_stars3_genre <- Group_gnrstr3[is.element(Group_gnrstr3$Star3, Star_names),]
Top_stars3_genre <- dcast(Top_stars3_genre, Star3~Genre)
diff3 = setdiff(Top_stars2_genre$Star, Top_stars3_genre$Star)
for(i in 1:length(diff3)){
  Top_stars3_genre[nrow(Top_stars3_genre) + 1,] <- c(diff3[i], rep(0,length(colnames(Top_stars3_genre))-1))
}
colnames(Top_stars3_genre)[1] <- "Star"
Top_stars3_genre <- Top_stars3_genre %>% slice(match(Star_names, Star))
#Top_stars3_genre <- Top_stars3_genre[order(Top_stars3_genre$Star), ]

Top_stars4_genre <- Group_gnrstr4[is.element(Group_gnrstr4$Star4, Star_names),]
Top_stars4_genre <- dcast(Top_stars4_genre, Star4~Genre)
diff4 = setdiff(Top_stars2_genre$Star, Top_stars4_genre$Star)
for(i in 1:length(diff4)){
  Top_stars4_genre[nrow(Top_stars4_genre) + 1,] <- c(diff4[i], rep(0,length(colnames(Top_stars4_genre))-1))
}
colnames(Top_stars4_genre)[1] <- "Star"
Top_stars4_genre <- Top_stars4_genre %>% slice(match(Star_names, Star))

total <- rbind.fill(Top_stars1_genre, Top_stars2_genre, Top_stars3_genre, Top_stars4_genre)
total[is.na(total)] <- 0
total[,2:18] <- sapply(total[,2:18],as.numeric)
total


titles <- as.vector(Top_stars1_genre$Star[c(-1,-2)])
titles

png("Actors_Genres.png", width = 2200, height = 1398, res = 225)
op <- par(mar = c(0, 1, 1, 1), oma=c(0, 0, 4, 0))
par(mfrow = c(3,4))

for(i in 1:8){
  radarchart(total[c(1,2,i+2,i+12,i+22,i+32),-1],
             axistype = 1,
             caxislabels = c(0, 3, 6, 9, 12), calcex = 0.8,
             axislabcol = "darkgray", vlcex =0.7,
             pfcol = scales::alpha(c("#00AFBB", "#E7B800", "#00cc00", "#FC4E07"), 0.3),
             plty = 2, cglty = 1, cglcol = "gray",
             pcol = scales::alpha(c("#00AFBB", "#E7B800","#00cc00", "#FC4E07"), 0.6)
             , plwd = 2,  title = titles[i]
  )
  
}

plot(1, type = "n", axes=FALSE, xlab="", ylab="")

for(i in 9:10){
  radarchart(total[c(1,2,i+2,i+12,i+22,i+32),-1],
             axistype = 1,
             caxislabels = c(0, 3, 6, 9, 12), calcex = 0.8,
             axislabcol = "darkgray", vlcex =0.7,
             pfcol = scales::alpha(c("#00AFBB", "#E7B800", "#00cc00", "#FC4E07"), 0.3),
             plty = 2, cglty = 1, cglcol = "gray",
             pcol = scales::alpha(c("#00AFBB", "#E7B800","#00cc00", "#FC4E07"), 0.6)
             , plwd = 2,  title = titles[i]
  )
  
}

plot(1, type = "n", axes=FALSE, xlab="", ylab="")

legend(
  "center", legend = c("Star 1", "Star 2","Star 3", "Star 4"), horiz = F,
  bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800", "#00cc00", "#FC4E07"),
  text.col = "black", cex = 0.9, pt.cex = 1.5, title = "Role Rank"
)
mtext("Top 10 Main Actors and Their Movies Genres", line=0.5, side=3, outer=TRUE, cex=1.5)
par(op)

dev.off()

############ References ###############
# imdb1000 dataset: https://www.kaggle.com/datasets/harshitshankhdhar/imdb-dataset-of-top-1000-movies-and-tv-shows
# Mice: https://www.analyticsvidhya.com/blog/2016/03/tutorial-powerful-packages-imputing-missing-values/
# Bdgt2: https://sta210-s22.github.io/website/ae/ae-0-movies.html
# Bdgt5: https://www.kaggle.com/datasets/rounakbanik/the-movies-dataset?resource=download&select=movies_metadata.csv
# GDP: https://fred.stlouisfed.org/series/GDP
# Shiny reference: https://stackoverflow.com/questions/49454652/add-custom-data-label-in-ggplotly-scatterplot

