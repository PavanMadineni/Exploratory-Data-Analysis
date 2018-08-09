df = read.table(file.choose(),header = T)
movie_budgets =df
movie_budgets$log_budget = log10(movie_budgets$budget)

#Here we can play with the hyperparameters like Span, Degree of fit and Cutnumber on Year or Length
library(ggplot2)
ggplot(movie_budgets, aes(y=log_budget, x= year)) + geom_point() +
  geom_smooth(method.args= list(degree=1),span=0.35)+
  facet_grid(~cut_number(length,n=4))


ggplot(movie_budgets, aes(y=log_budget, x= length)) + geom_point() +
  geom_smooth(method.args= list(degree=1),span=0.25)+
  facet_grid(~cut_number(year,n=3)) +
  geom_vline(data = movie_budgets,aes(xintercept = 150))


length.bend = function(x) {
  return((x - 150) * (x < 150))
}

library(MASS)
movie_budgets.rlm = rlm(log_budget ~ year + length.bend(length), data = movie_budgets,
                 psi = psi.bisquare)

movie_budgets.grid = expand.grid(year = c(1906, 1985, 2000, 2005), length = c(50,150,250,350))
                                                                             
movie_budgets.grid.predict = predict(movie_budgets.rlm, newdata = movie_budgets.grid)


ggplot(data.frame(movie_budgets.grid, fit = as.vector(movie_budgets.grid.predict)), aes(x = length,y = fit)) + 
  geom_line() + facet_grid(~year)

#Q2
ggplot(data.frame(movie_budgets.grid, fit = as.vector(movie_budgets.grid.predict)), aes(x = length,y = fit, group = year, color = factor(year)))+ 
 geom_line()

library(broom)
movie_budgets.rlm.au = augment(movie_budgets.rlm) 
var(movie_budgets.rlm.au$.fitted)/var(movie_budgets.rlm.au$.fitted +movie_budgets.rlm.au$.resid )


#Q3

mb.grid = expand.grid(year = seq(1906,2005,1), length = seq(25,400,5))
movie.budgets.rlm = rlm(log_budget ~ year * length, data = movie_budgets,
                         psi = psi.bisquare)
mb_pr = predict(movie.budgets.rlm, newdata = mb.grid)  
mb_plot_df = data.frame(mb.grid, fit = as.vector(mb_pr))

ggplot(mb_plot_df,aes(x = length, y = year, fill = fit)) + geom_raster() +
  scale_fill_distiller(palette = "RdYlBu")

ggplot(mb_plot_df,aes(x = length, y = year, z = fit)) + geom_raster(aes(fill = fit))+
  scale_fill_distiller(palette = "RdYlBu") + geom_contour()

ggplot(mb_plot_df,aes(x = length, y = year, z = fit)) + 
  geom_contour(binwidth =3,aes(color = ..level..))





library(lattice)
cloud(fit ~ year * length, data = mb_plot_df)

wireframe(fit ~ length*year, data =mb_plot_df )

cloud(log_budget ~ year * length, data = movie_budgets)

#install.packages('TeachingDemos')
library(TeachingDemos)
#rotate.wireframe(fit ~ length*year,data =mb_plot_df, screen = list(z = -32, x = -60, y = 0),drape = TRUE)
                                                                 
wireframe(fit ~ length*year,data =mb_plot_df, screen = list(z = -32, x = -60, y = 0),drape = TRUE)

rotate.wireframe(fit ~ length.bend(length) * year, data = mb_plot_df, screen = list(z = 40, x = -60, y = -48), drape = TRUE)

# 

# #Q3 bending length
# 
# mb.grid = expand.grid(year = seq(1906,2005,1), length = seq(25,400,5))
# movie.budgets.rlm = rlm(log_budget ~ year * length, data = movie_budgets,
#                         psi = psi.bisquare)
# mb_pr = predict(movie.budgets.rlm, newdata = mb.grid)  
# mb_plot_df = data.frame(mb.grid, fit = as.vector(mb_pr))
# 
# ggplot(mb_plot_df,aes(x = length, y = year, fill = fit)) + geom_raster() +
#   scale_fill_distiller(palette = "RdYlBu")
# 
# ggplot(mb_plot_df,aes(x = length, y = year, z = fit)) + geom_raster(aes(fill = fit))+
#   scale_fill_distiller(palette = "RdYlBu") + geom_contour()
# 
# ggplot(mb_plot_df,aes(x = length, y = year, z = fit)) + 
#   geom_contour(binwidth =3,aes(color = ..level..))
# 
# 
# 
# 
# 
# library(lattice)
# cloud(fit ~ year * length, data = mb_plot_df)
# 
# wireframe(fit ~ length*year, data =mb_plot_df )
# 
# #install.packages('TeachingDemos')
# library(TeachingDemos)
# #rotate.wireframe(fit ~ length*year,data =mb_plot_df, screen = list(z = -32, x = -60, y = 0),drape = TRUE)
# 
# wireframe(fit ~ length*year,data =mb_plot_df, screen = list(z = -32, x = -60, y = 0),drape = TRUE)
