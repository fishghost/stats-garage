# Stats explorer for the Overwatch League
View the published app on [my website](https://fishg.host/shiny/garage/).

This is an R Shiny app to allow the community to explore the data provided by the [Overwatch League Stats Lab](https://overwatchleague.com/en-us/statslab). While the official website provides an easy way to browse some of the most popular stats, **Stats Garage** was built as a way to dig down into all the granular stats in the csv's provided by the league. 

The app lets you filter down to specific matches or groups of matches across all four years. Or filter matches by a specific team. And allows you to compare players across a single stat or in a 2D plot to check the relation between certain stats. 

The app can be slow when updating and could benefit from some refactoring around the reactive variables. Especially with debouncing. 
