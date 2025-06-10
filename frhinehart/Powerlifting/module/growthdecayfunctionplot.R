growth_decay_curve1 <- function(x, a, b) {
  a * x * exp(-b * x)}


library(ggplot2)


# given b

#b1 = 0.005
x=seq(0,300,by=0.1)
a=c(5, 10, 20, 25)
b=0.005

datx = rep(x,4)
daty = c(growth_decay_curve1(x,a[1],b),growth_decay_curve1(x,a[2],b),growth_decay_curve1(x,a[3],b), growth_decay_curve1(x,a[4],b))
data=rep(a,each=length(x))
dat = cbind(x=datx,y=daty,a=data)

ggplot(dat,aes(x=x,y=y,color=as.factor(a))) + 
  geom_smooth(method="loess",aes(color=as.factor(a)),se=FALSE) 


# b2 = 0.01
x=seq(0,300,by=0.1)
a=c(5, 10, 20, 25)
b=0.01

datx = rep(x,4)
daty = c(growth_decay_curve1(x,a[1],b),growth_decay_curve1(x,a[2],b),growth_decay_curve1(x,a[3],b), growth_decay_curve1(x,a[4],b))
data=rep(a,each=length(x))
dat = cbind(x=datx,y=daty,a=data)

ggplot(dat,aes(x=x,y=y,color=as.factor(a))) + 
  geom_smooth(method="loess",aes(color=as.factor(a)),se=FALSE) 


# b3=0.025
x=seq(0,300,by=0.1)
a=c(5, 10, 20, 25)
b=0.025

datx = rep(x,4)
daty = c(growth_decay_curve1(x,a[1],b),growth_decay_curve1(x,a[2],b),growth_decay_curve1(x,a[3],b), growth_decay_curve1(x,a[4],b))
data=rep(a,each=length(x))
dat = cbind(x=datx,y=daty,a=data)

ggplot(dat,aes(x=x,y=y,color=as.factor(a))) + 
  geom_smooth(method="loess",aes(color=as.factor(a)),se=FALSE) 


#b4=0.04
x=seq(0,300,by=0.1)
a=c(5, 10, 20, 25)
b=0.04

datx = rep(x,4)
daty = c(growth_decay_curve1(x,a[1],b),growth_decay_curve1(x,a[2],b),growth_decay_curve1(x,a[3],b), growth_decay_curve1(x,a[4],b))
data=rep(a,each=length(x))
dat = cbind(x=datx,y=daty,a=data)

ggplot(dat,aes(x=x,y=y,color=as.factor(a))) + 
  geom_smooth(method="loess",aes(color=as.factor(a)),se=FALSE) 



################################
# tidying up

library(dplyr)

growth_decay_curve1 <- function(x, a, b) {
  a * x * exp(-b * x)
}

# Define inputs
x_vals <- seq(0, 300, by = 0.1)
a_vals <- c(5, 10, 20, 25)
b_vals <- c(0.005, 0.01, 0.025, 0.04)

# Create the combined data frame
combined_df <- expand.grid(x = x_vals, a = a_vals, b = b_vals) %>%
  mutate(y = growth_decay_curve1(x, a, b),
         a = as.factor(a),
         b = as.factor(b))

# Preview the result
head(combined_df)


ggplot(combined_df, aes(x = x, y = y, color = a)) + 
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ b, scales = "free_y", 
             labeller = purrr::partial(label_both, sep = " = ")) +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")  # Make facet labels bold
  ) +
  labs(color = "a")




