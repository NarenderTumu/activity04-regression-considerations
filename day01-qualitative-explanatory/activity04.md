Activity 4
================

# Day 1 - Activity 4

## Task 2: Load the necessary packages

``` r
library(tidyverse)
library(tidymodels)
library(GGally)
library(ggfortify)
```

## Task 3: Load the data

``` r
evals<-read_tsv("https://www.openintro.org/data/tab-delimited/evals.txt",show_col_types = FALSE)
```

1.  This is an experimental study for me because the data was collected
    from students to test the hypothesis whether beauty leads directly
    to affect the score evaluations. We can answer the question if it
    phrased as whether or not beauty affects the score evaluations.

``` r
plot1<-evals%>%ggplot(aes(x=score))+
  geom_histogram(fill="red",color="black",bins = 30)+
  labs(title = "Distribution of Score")+
  theme(plot.title = element_text(hjust = 0.5))
plot1
```

![](activity04_files/figure-gfm/Distribution%20of%20Score-1.png)<!-- -->

2.  The distribution of the variable “Score” appears to be Left-skewed.
    It indicates that the majority of students gave the professors a
    more than average rating. It is not what I expected; I expected it
    to be slightly right-skewed.

3.  I chose the variables gender and bty avg to show their relationship.
    I chose a box plot to visualize their relationship, and the below
    visualization shows that female professors have a slightly higher
    average beauty score than male professors.

``` r
plot2<-evals%>%ggplot(aes(x=gender,y=bty_avg,fill=gender))+
  geom_boxplot()+
  labs(title = "Relationship between Gender and Beauty Average",
       x="Gender of the Professor",
       y="Beauty average score")+
  theme(plot.title = element_text(hjust = 0.5))

plot2
```

![](activity04_files/figure-gfm/Plot2-1.png)<!-- -->

## Task 4: Pairwise relationships

``` r
evals_bty<-select(evals,c("bty_f1lower","bty_f1upper","bty_f2upper","bty_m1lower","bty_m1upper","bty_m2upper","bty_avg"))

evals_ggpairs<-evals_bty %>% ggpairs()

evals_ggpairs
```

![](activity04_files/figure-gfm/ggpairs-1.png)<!-- -->

4.  

-   bty\_avg has a higher correlation with rest of the beauty variables
    and positively linear relation with bty\_m1upper

-   bty\_f1lower has the highest correlation with bty\_avg and
    bty\_m1upper

-   bty\_f1upper has the highest correlation with bty\_avg and
    bty\_m1upper

-   bty\_f2upper has positive linear relationship with bty\_avg

-   bty\_m1lower has positive linear relationship with bty\_avg

-   bty\_m1upper has positive linear relationship with bty\_avg

-   bty\_m2upper has positive linear relationship with bty\_avg

5.  I don’t think it makes sense to include all the variables in the
    model, because it would be a complex model with many variables.

6.  I think including bty\_avg variable in the model will be a good
    decision since it has a positive linear relationship with all other
    beauty variables.

## Task 5: Multiple linear regression: one quantitative predictor, one qualitative predictor

``` r
m_bty_gen <- lm(score ~ bty_avg+gender, data = evals)
tidy(m_bty_gen)
```

    ## # A tibble: 3 × 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)   3.75      0.0847     44.3  6.23e-168
    ## 2 bty_avg       0.0742    0.0163      4.56 6.48e-  6
    ## 3 gendermale    0.172     0.0502      3.43 6.52e-  4

``` r
# Creating the diagnostic plots using the function plot() from ggfortify package 
par(mfrow = c(2, 2))
plot(m_bty_gen)
```

![](activity04_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

7.  From the above diagnostic plots we can see that

    -   The linear relationship assumption is met very well as the
        residual vs fitted plot does not follow any particular patter
        and spread randomly
    -   The normality assumption has met satisfactorily as the residuals
        follow along the line in q-q plot
    -   The constant variance assumption also met reasonably as the
        residuals spread is equal along the horizantal line

8.  The bty\_avg looks like a significant predictor of the score. The
    addition of gender has changed the parameter estimate of bty\_avg
    from 3.88 to 3.75.

9.  

$$
  \begin{aligned}
\widehat{\texttt{score}} &= \hat{\beta}_0 + \hat{\beta}_1 \times \texttt{bty\\_avg} + \hat{\beta}_2 \times (1) \\
&= \hat{\beta}_0 + \hat{\beta}_1 \times \texttt{bty\\_avg}+ \hat{\beta}_2
\end{aligned}
$$

10. For two genders received the same beauty avg score, The male
    professors are likely to receive a higher course evaluation score.

``` r
m_bty_rank<-lm(score ~ bty_avg+rank, data = evals)
tidy(m_bty_rank)
```

    ## # A tibble: 4 × 5
    ##   term             estimate std.error statistic   p.value
    ##   <chr>               <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)        3.98      0.0908     43.9  2.92e-166
    ## 2 bty_avg            0.0678    0.0165      4.10 4.92e-  5
    ## 3 ranktenure track  -0.161     0.0740     -2.17 3.03e-  2
    ## 4 ranktenured       -0.126     0.0627     -2.01 4.45e-  2

11. R appears to handle categorical variables with more than 2 levels by
    removing one level from the total numer of levels. Here since there
    are 3 levels, R is using only 2 of them by removing teaching.
