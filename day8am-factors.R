library(tidyverse)

glimpse(gss_cat)
# levels() pulls out the categories available in a factor
levels(gss_cat$rincome)

# A barplot *with* the levels of the factor set
ggplot(
    data = gss_cat,
    mapping = aes(x = rincome)
) + geom_bar() + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 90))
# The order of the categories comes from the order of the levels

# What if we strip away the levels?
gss_cat |> 
    mutate(
        # as.character() strips away the levels 
        rincome = as.character(rincome)
    ) |> 
    # Plot it
    ggplot(
    mapping = aes(x = rincome)
    ) + geom_bar() + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 90))
# Without the levels, the axis is sorted "alphabetically"

# Reordering 
relig_summary <- gss_cat |> 
    summarize(
        tvhours = mean(tvhours, na.rm = TRUE), 
        .by = relig
    )
relig_summary
ggplot(
    data = relig_summary, 
    mapping = aes(x = tvhours, y = relig)
) + geom_point()
# our categorical axis is ordered by the levels

#fct_reorder() reorders a factor by the values in another column 
ggplot(
    data = relig_summary, 
    mapping = aes(x = tvhours, y = fct_reorder(relig, tvhours)) #reorder religion by the values of tvhours 
    ) + geom_point()

# Collapsing factor levels 
gss_cat |> 
    count(partyid)
# 10 party affiliation categories - let's collapse down to four, R, I, D, O (other)
gss_cat |> 
    mutate(
        partyid = fct_collapse(
            partyid,
            "R" = c("Strong republican", "Not str republican"), 
            "I" = c("Independent", "Ind,near rep", "Ind,near dem"),
            "D" = c("Not str democrat", "Strong democrat"),
            "O" = c("No answer", "Don't know", "Other party")
        )
    ) |> 
    count(partyid)
