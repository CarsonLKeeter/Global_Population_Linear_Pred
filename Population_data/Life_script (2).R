#Libraries used

library(ggplot2)
library(tidyverse)
library(gridExtra)
library(magicfor)
library(cowplot)
library(scales)

#Filtering dataset for continents

options(
  scipen = 999
)

df <- gapminder %>%
  select(
    continent,
    year,
    lifeExp,
    pop,
    country
    )

#Linear Trend for known dates

exp_plot <- ggplot(
  df, 
  aes(
    x = year,
    y = lifeExp
    )
  ) +
  geom_jitter(
    stat = "identity",
    alpha = .3,
    aes(
      color = continent,
      size = pop
    )
  ) +
  geom_smooth(
    method = lm,
    formula = y ~ x,
    data = df, 
    aes(
      x = year,
      y = lifeExp,
      color = continent
    )
  ) + 
  labs( 
    title = "Global Life Expentency Trend (1950 - 2010)",
    x = "Year",
    y = "Life Expentancy (years)"
    ) + 
  guides(
    color = guide_legend(
      title = "Continent"
    ),
    size = guide_legend(
      title = "Population"
    )
  ) +
  scale_size_continuous(
    labels = comma
    )

#Frequency of Life Expentancy plot  

freq <- ggplot(
  df,
  aes(
    lifeExp,
    fill = continent,
    stat(
      count
      )
    )
  ) +
  geom_density(
    data = df,
    aes(
      lifeExp
    ),
    alpha = .75,
    position = "stack"
  ) + 
  labs( 
    title = "Global Life Expentency Density (1950 - 2010)",
    x = "Life Expentancy (years)",
    y = "Frequency (%)"
  ) + 
  guides(
    fill = guide_legend(
      title = "Continent"
    )
  )

################################Africa

af <- df %>%
  filter(
    continent == "Africa"
  )

af_fit <- lm(
  lifeExp ~ year,
  data = af
)

af_coef <- coef(
  summary(
    af_fit
  )
)

af_coef <- af_coef[,1]

##############################Americas

am <- df %>%
  filter(
    continent == "Americas"
  )

am_fit <- lm(
  lifeExp ~ year,
  data = am
)

am_coef <- coef(
  summary(
    am_fit
  )
)

am_coef <- am_coef[,1]

#############################Asia

as <- df %>%
  filter(
    continent == "Asia"
  )

as_fit <- lm(
  lifeExp ~ year,
  data = as
)

as_coef <- coef(
  summary(
    as_fit
  )
)

as_coef <- as_coef[,1]

###########################Europe

eu <- df %>%
  filter(
    continent == "Europe"
  )

eu_fit <- lm(
  lifeExp ~ year,
  data = eu
)

eu_coef <- coef(
  summary(
    eu_fit
  )
)

eu_coef <- eu_coef[,1]

#########################Oceania

oc <- df %>%
  filter(
    continent == "Oceania"
  )

oc_fit <- lm(
  lifeExp ~ year,
  data = oc
)

oc_coef <- coef(
  summary(
    oc_fit
    )
  )

oc_coef <- oc_coef[,1]


###FUNctions

##Africa

magic_for(
  silent = TRUE
  )

af_func <- for(year in 1950:2050){
    
    pred <- af_coef[1] + af_coef[2]*year
    
    continent <- "Africa"
    
    put(
      year, 
      pred, 
      continent
      )
  }

af_pred <- magic_result_as_dataframe(
  
)

#Americas

magic_for(
  silent = TRUE
)

am_func <- for(year in 1950:2050){
  
  pred <- am_coef[1] + am_coef[2]*year
  
  continent <- "Americas"
  
  put(
    year, 
    pred, 
    continent
  )
}

am_pred <- magic_result_as_dataframe(
  
)

#Asia

magic_for(
  silent = TRUE
)

as_func <- for(year in 1950:2050){
  
  pred <- as_coef[1] + as_coef[2]*year
  
  continent <- "Asia"
  
  put(
    year, 
    pred, 
    continent
  )
}

as_pred <- magic_result_as_dataframe(
  
)

#Europe

magic_for(
  silent = TRUE
)

eu_func <- for(year in 1950:2050){
  
  pred <- eu_coef[1] + eu_coef[2]*year
  
  continent <- "Europe"
  
  put(
    year, 
    pred, 
    continent
  )
}

eu_pred <- magic_result_as_dataframe(
  
)

#Oceania

magic_for(
  silent = TRUE
)

oc_func <- for(year in 1950:2050){
  
  pred <- oc_coef[1] + oc_coef[2]*year
  
  continent <- "Oceania"
  
  put(
    year, 
    pred,
    continent
  )
}

oc_pred <- magic_result_as_dataframe(

)

#Dataframe for predictions 

total_pred <- as.data.frame(
  rbind(
    af_pred,
    am_pred,
    as_pred,
    eu_pred,
    oc_pred
  )
)

total_pred <- total_pred[
  -c(1)
]

##New plots for prediction 

pred_plot <- ggplot(
  total_pred,
  aes(
    year,
    pred,
    color = continent
  )
) +
  geom_smooth(
  data = total_pred,
  aes(
    year,
    pred
  ),
  method = "lm",
  formula = y ~ x,
  size = 1.5
) +
  labs( 
    title = "Global Life Expentency Prediction (1950 - 2050)",
    x = "Year",
    y = "Life Expentancy (years)"
  ) + 
  guides(
    color = guide_legend(
      title = "Continent"
    )
  ) 

## Code for Future Density Estimation 

den_est <- df %>%
  select(
    continent,
    year,
    pop
  )

#Africa

af_den <- den_est %>%
  filter(
    continent == "Africa"
  )

af_den_fit <- lm(
  pop ~ year,
  data = af_den
)

af_den_coef <- coef(
  summary(
    af_den_fit
  )
)

af_den_coef <- af_den_coef[,1]

magic_for(
  silent = TRUE
)

af_den_func <- for(year in 1950:2050){
  
  pred <- af_den_coef[1] + af_den_coef[2]*year
  
  continent <- "Africa"
  
  put(
    year, 
    pred, 
    continent
  )
}

af_den_pred <- magic_result_as_dataframe(
  
)

#Americas 

am_den <- den_est %>%
  filter(
    continent == "Americas"
  )

am_den_fit <- lm(
  pop ~ year,
  data = am_den
)

am_den_coef <- coef(
  summary(
    am_den_fit
  )
)

am_den_coef <- am_den_coef[,1]

magic_for(
  silent = TRUE
)

am_den_func <- for(year in 1950:2050){
  
  pred <- am_den_coef[1] + am_den_coef[2]*year
  
  continent <- "Americas"
  
  put(
    year, 
    pred, 
    continent
  )
}

am_den_pred <- magic_result_as_dataframe(
  
)

#Asia

as_den <- den_est %>%
  filter(
    continent == "Asia"
  )

as_den_fit <- lm(
  pop ~ year,
  data = as_den
)

as_den_coef <- coef(
  summary(
    as_den_fit
  )
)

as_den_coef <- as_den_coef[,1]

magic_for(
  silent = TRUE
)

as_den_func <- for(year in 1950:2050){
  
  pred <- as_den_coef[1] + as_den_coef[2]*year
  
  continent <- "Asia"
  
  put(
    year, 
    pred, 
    continent
  )
}

as_den_pred <- magic_result_as_dataframe(
  
)

#Europe

eu_den <- den_est %>%
  filter(
    continent == "Europe"
  )

eu_den_fit <- lm(
  pop ~ year,
  data = eu_den
)

eu_den_coef <- coef(
  summary(
    eu_den_fit
  )
)

eu_den_coef <- eu_den_coef[,1]

magic_for(
  silent = TRUE
)

eu_den_func <- for(year in 1950:2050){
  
  pred <- eu_den_coef[1] + eu_den_coef[2]*year
  
  continent <- "Europe"
  
  put(
    year, 
    pred, 
    continent
  )
}

eu_den_pred <- magic_result_as_dataframe(
  
)

#Oceania

oc_den <- den_est %>%
  filter(
    continent == "Oceania"
  )

oc_den_fit <- lm(
  pop ~ year,
  data = oc_den
)

oc_den_coef <- coef(
  summary(
    oc_den_fit
  )
)

oc_den_coef <- oc_den_coef[,1]

magic_for(
  silent = TRUE
)

oc_den_func <- for(year in 1950:2050){
  
  pred <- oc_den_coef[1] + oc_den_coef[2]*year
  
  continent <- "Oceania"
  
  put(
    year, 
    pred, 
    continent
  )
}

oc_den_pred <- magic_result_as_dataframe(
  
)

#Combine estimation density 

total_den <- rbind(
  af_den_pred,
  am_den_pred,
  as_den_pred,
  eu_den_pred,
  oc_den_pred
)

total_den <- total_den[
  -c(1)
  ]


#Plot for pop est

den_est_plot <- ggplot(
  total_den,
  aes(
    x = year,
    y = pred,
    color = continent
  )
) + 
  geom_line(
    aes(
      x = year,
      y = pred
    ),
    size = 1.5
  ) + 
  labs( 
    title = "Global Population Prediction (1950 - 2050)",
    caption = "*all trends are linear",
    x = "Year",
    y = "Population"
  ) + 
  guides(
    color = guide_legend(
      title = "Continent"
    )
  ) +
  scale_y_continuous(
    labels = comma
  ) 
  
#Arrange Grids

all_plot <- plot_grid(
  exp_plot,
  freq,
  den_est_plot,
  pred_plot,
  nrow = 2,
  ncol = 2
) 

save_plot(
  filename = "Global_Population.png",
  plot =  all_plot, 
  nrow = 2,
  ncol = 2,
  base_aspect_ratio = 1.3
  )


