MP2 - Final
================
Zoe Meers, Natasha Jadavji, Christine Yee
3/20/2017

These maps from the FEC data show the total contributions that would help a candidate in the 2012 election be elected grouped by party identification and state residency.

``` r
#filter candidates by year
candidates2012<-filter(candidates, cand_election_yr=="2012")
#filter contributions by individual contributions to HELP a candidate be elected
contributions2012<-filter(contributions, transaction_type=="24E")
#filter negative contributions
contributions2012<-filter(contributions2012, transaction_amt==gsub('-[A-z ]*', '', transaction_amt))
contributions2012<-filter(contributions2012, transaction_amt==gsub('0', '', transaction_amt))
```

``` r
#filter transactions for 2012 election only.
#2010 midterms were helf on the 11/02/10, so transactions on or after this date are most likely for 2012
contributions2012<- filter(contributions, transaction_dt>="11022010")
#filters for transaction dates before the 2012 election. We don't want contributions for the 2014 midterms.
contributions2012<- filter(contributions, transaction_dt<"11052012")
```

``` r
#Finds contributions from 2012 election and sums transaction amount by candidate ID
sum_cand<-contributions2012 %>%
  group_by(cand_id) %>%
  summarise(transaction_amt=sum(transaction_amt))
```

``` r
#Joins candidate DF with transaction DF by candidate ID
party_id <- candidates2012 %>%
  inner_join(sum_cand, by = ("cand_id"))
```

``` r
#Splits contributions and candidates by party identification
#democrats2012 - candidates
democrats2012<- filter(party_id, cand_party_affiliation=="DEM")
#repubs2012 - candidates
republicans2012 <- filter(party_id, cand_party_affiliation=="REP")
```

``` r
# sum of transaction amt by state - democrats
sum_dem <- democrats2012 %>% 
  group_by(cand_office_state) %>% 
  summarise(transaction_amt = sum(transaction_amt)) 
#sum of transaction amt by state - republicans
sum_rep <- republicans2012 %>% 
  group_by(cand_office_state) %>% 
  summarise(transaction_amt = sum(transaction_amt))
```

``` r
#rename cand_office_state variable to state
sum_dem <- rename(sum_dem, "State"=cand_office_state)
sum_rep <- rename(sum_rep, "State"=cand_office_state)
```

``` r
#cut US, PR, AS, GU, DC, MP, VI from States
sum_dem <- sum_dem[-c(3,8,12,41,47,50), ]
sum_rep <- sum_rep[-c(11,45), ]
```

``` r
#Loading map data 
all_states <- map_data("state")
all_states <- rename(all_states, "State"=region)

#join states together using left join
MyMap_dem <- left_join(all_states, sum_dem, by ="State")
MyMap_rep <- left_join(all_states, sum_rep, by="State")
```

``` r
#MAPS
options(scipen=999) #variables should not be written in scientific notation
#basic map of the United States, black borders, grey fill
states_map <- ggplot(data = all_states, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")
#Get rid of ggplot background.
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
  )
#Republican map
Rep_Map <- states_map + 
      geom_polygon(data = MyMap_rep, aes(fill = transaction_amt)) +
     geom_polygon(color = "white", fill = NA) +
    theme_bw() +
      ditch_the_axes + 
  scale_fill_gradient(low="pink", high="red2") + 
  ggtitle("Republican Contributions in the Continguous US - 2012 Election") + theme(plot.title = element_text(size = rel(1))) + theme(legend.title = element_text(face = "bold")) + theme(title = element_text(face = "bold")) + guides(fill=guide_legend(title="Contributions ($)")) + theme(plot.title = element_text(size = rel(1.2)))
Rep_Map
```

![](MP2_Final_files/figure-markdown_github/unnamed-chunk-13-1.png)

``` r
#Democrats map 
Dem_Map <- states_map + 
      geom_polygon(data = MyMap_dem, aes(fill = transaction_amt), color = "white") +
      geom_polygon(color = "white", fill = NA) + theme_bw() + scale_fill_continuous(low="lightblue", high="blue2") +
      ditch_the_axes + ggtitle("Democrat Contributions in the Continguous US - 2012 Election") + theme(plot.title = element_text(size = rel(1.2))) + theme(legend.title = element_text(face = "bold")) + theme(title = element_text(face = "bold")) + guides(fill=guide_legend(title="Contributions ($)"))

Dem_Map
```

![](MP2_Final_files/figure-markdown_github/unnamed-chunk-13-2.png)
