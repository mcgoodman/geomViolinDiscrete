This small package implements new `stat_` and `geom_` functions for `ggplot2`, which display centered, vertically-stacked histograms which serve as a discrete analogue to violin plots. 

Simulating, for example, some Poisson-distributed data:

```R
devtools::install_github("mcgoodman/geomViolinDiscrete")

library("ggplot2")
library("geomViolinDiscrete")

set.seed(100)

data <- tibble::tibble(
  x = sample(letters[1:4], 400, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1)), 
  y = rpois(400, as.numeric(as.factor(x)))
)
```

Violin plots can distort the properties of these data unless we use a small bandwidth, which anyway looks a little silly:

![](https://raw.githubusercontent.com/mcgoodman/geomViolinDiscrete/main/img/violin.png)

Instead, `geom_violin_discrete` can plot the discrete distribution of these data across x-values, scaling the width of the widest bar to be the same between observations:

```R
data |> ggplot(aes(x, y)) + geom_violin_discrete()
# equivalently: data |> ggplot(aes(x, y)) + geom_rect(stat = "ycount")
```

![](https://raw.githubusercontent.com/mcgoodman/geomViolinDiscrete/main/img/violin_discrete.png)

Or, the bars can be scaled to show the discrepancy in sample sizes between categories:

```R
data |> ggplot(aes(x, y)) + geom_violin_discrete(scale = "count")
```

![](https://raw.githubusercontent.com/mcgoodman/geomViolinDiscrete/main/img/violin_discrete_count.png)

Other geoms, such as boxplots, can be overlaid:

```R
data |> 
  ggplot(aes(x, y)) + 
  geom_violin_discrete(fill = "grey80") + 
  geom_boxplot(width = 0.1, outlier.color = NA)
```

![](https://raw.githubusercontent.com/mcgoodman/geomViolinDiscrete/main/img/violin_discrete_boxplot.png)

The bar height and maximum bar width can be tweaked with the "height" and "width" parameters:

```R
data |> 
  ggplot(aes(x, y)) + 
  geom_violin_discrete(width = 0.5, height = 1)
```

![](https://raw.githubusercontent.com/mcgoodman/geomViolinDiscrete/main/img/violin_discrete_adjusted.png)

