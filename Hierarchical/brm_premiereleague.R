library(magrittr)
library(tidyverse)
library(brms)
select <- dplyr::select

soccer_df <- read_csv(
  file = "premiereleague.csv",
  col_names = c('Home','H', 'A', 'Away')
) %>%
  mutate(Game = row_number()) %>%
  select(Game, everything()) %>%
  pivot_longer(
    cols = c(H,A),
    names_to  = "Side",
    values_to = "Score"
  ) %>%
  mutate(
    At_home = as.integer(Side=="H"),
    Attack  = ifelse(At_home,Home,Away),
    Defend  = ifelse(!At_home,Home,Away) #,
    # def     = -1
  ) %>%
  select(Game, Score, Attack, Defend, At_home) %>% #, def ) %>%
  mutate(across(c(Attack,Defend),as.factor))

soccer_df

#mod1_formula <- Score ~ 0 + At_home + def + (1 | Attack) + (0 + def | Defend)
mod1_formula <- Score ~ At_home + (1 | Attack) + (1 | Defend)
priors_default <- get_prior(
  formula = mod1_formula,
  family  = poisson(link = "log"),
  data    = soccer_df
)
priors_default
priors0 <- set_prior( "student_t(3,0,0.1)", class = "b", lb = 0 ) +
  set_prior( "student_t(3,0,0.2)", class = "Intercept" ) +
  set_prior( "student_t(3,0,0.3)", class = "sd" )
priors0

brm_prior_only <- brm(
  formula = mod1_formula,
  family  = poisson(link = "log"),
  data    = soccer_df,
  prior   = priors0,
  sample_prior = "only",
  chains = 1L,
  warmup = 300,
  iter   = 3300
)
summary(brm_prior_only,priors = TRUE)

pp_check(brm_prior_only, type = "hist")
pp_check(brm_prior_only, type = "stat",stat = "median")


# brm_post <- brm(
#   formula = mod1_formula,
#   family  = poisson(link = "log"),
#   data    = soccer_df,
#   prior   = priors0,
#   sample_prior = "no",
#   chains = 4L,
#   cores  = 4L,
#   warmup = 1000,
#   iter   = 2000
# )

brm_post <- update(
  brm_prior_only,
  sample_prior = "no",
  chains = 4L,
  cores  = 4L,
  warmup = 1000,
  iter   = 2000
)
summary(brm_post,priors = TRUE)

pp_check(brm_post, type = "hist")
pp_check(brm_post, type = "stat",stat = "median")

coef(brm_post) %>%
{
  tibble(
    Team   = str_wrap( dimnames(.$Attack)[[1]],13),
    Attack = .$Attack[,"Estimate","Intercept"],
    xmin   = .$Attack[,"Q2.5","Intercept"],
    xmax   = .$Attack[,"Q97.5","Intercept"],
    Defend = -(.$Defend[,"Estimate","Intercept"]),
    ymax   = -(.$Defend[,"Q2.5","Intercept"]),
    ymin   = -(.$Defend[,"Q97.5","Intercept"])
  )
} %>%
  {
    ggplot(.,aes(x=Attack,y=Defend)) +
      geom_errorbar(aes(ymin=ymin,ymax=ymax), width = 0.025,color="gray") +
      geom_errorbarh(aes(xmin=xmin,xmax=xmax), height = 0.025,color="gray") +
      geom_point(color="blue", alpha=0.8,size=3) +
      geom_text(aes(label=Team),check_overlap = TRUE,
                hjust="outward",vjust="outward") +
      labs(title = "Team Strengths")
  } %>%
  print()
# rm(brm_post,brm_prior_only)
# gc()
