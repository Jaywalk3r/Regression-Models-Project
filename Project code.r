
# You work for Motor Trend, a magazine about the automobile industry. Looking at
#	a data set of a collection of cars, they are interested in exploring the
#	relationship between a set of variables and miles per gallon (MPG)
#	(outcome). They are particularly interested in the following two questions:
#
# 		“Is an automatic or manual transmission better for MPG”
#
#		"Quantify the MPG difference between automatic and manual transmissions"
#

options( warn = -1)
library( ggplot2)
options( warn = 0)
library( gridExtra)
library( leaps)

data( mtcars)


Data = mtcars

Data$am[ Data$am == 1] = "man"

Data$am[ Data$am == 0] = "auto"

Data$vs[ Data$vs == 0] = "V"

Data$vs[ Data$vs == 1] = "straight"

#rename( Data, trans = am, config = vs)

names( Data) = c( "mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "config", "trans", "gear", "carb")

Data$cyl = factor( Data$cyl)

Data$trans = factor( Data$trans)

Data$gear = factor( Data$gear)

Data$config = factor( Data$config)

Data$carb = factor( Data$carb)

FULL.model = lm( mpg ~ ., Data)

summary( FULL.model)

MIN.model.trans = lm( mpg ~ trans, Data)
summary( MIN.model.trans)

MIN.model.disp = lm( mpg ~ disp, Data)
summary( MIN.model.disp)

MIN.model.hp = lm( mpg ~ hp, Data)
summary( MIN.model.hp)

MIN.model.wt = lm( mpg ~ wt, Data)
summary( MIN.model.wt)








# boxplot( c( list( Data$mpg), split( Data$mpg, list( Data$trans, Data$cyl))))

model2 = lm( mpg ~ trans + cyl + disp + gear, Data)

model3 = lm( mpg ~ trans + cyl + disp, Data)



plot.wt = ggplot( Data, aes( x = wt, y = mpg, colour = trans, label = rownames( Data)))

plot.wt = plot.wt + geom_point( size = 1)




plot.hp = ggplot( Data, aes( x = hp, y = mpg, colour = trans, label = rownames( Data)))

plot.hp = plot.hp + geom_point( size = 1)



plot.disp = ggplot( Data, aes( x = disp, y = mpg, colour = trans, label = rownames( Data)))

plot.disp = plot.disp + geom_point( size = 1)




plot.cyl = ggplot( Data, aes( x = cyl, y = mpg, colour = trans, label = rownames( Data)))

plot.cyl = plot.cyl + geom_point( size = 1)


dev.new()

grid.arrange( plot.wt, plot.hp, plot.disp, plot.cyl, nrow = 2, ncol = 2, top = "Figure 2: MPG vs. Wt, HP, Disp, and Cyl")





plot.hp.disp = ggplot( Data, aes( x = disp, y = hp, colour = trans, label = rownames( Data)))

plot.hp.disp = plot.hp.disp + geom_point( size = 1)


plot.hp.cyl = ggplot( Data, aes( x = cyl, y = hp, colour = trans, label = rownames( Data)))

plot.hp.cyl = plot.hp.cyl + geom_point( size = 1)


plot.hp.wt = ggplot( Data, aes( x = wt, y = hp, colour = trans, label = rownames( Data)))

plot.hp.wt = plot.hp.wt + geom_point( size = 1)


plot.disp.cyl = ggplot( Data, aes( x = cyl, y = disp, colour = trans, label = rownames( Data)))

plot.disp.cyl = plot.disp.cyl + geom_point( size = 1)


dev.new()

grid.arrange( plot.hp.disp, plot.hp.cyl, plot.hp.wt, plot.disp.cyl, nrow = 2, ncol = 2, top = "Figure 1: HP vs. Disp, Cyl, and Wt; Disp vs. Cyl.")










g = ggplot( Data, aes( x = hp / wt, y = mpg, colour = trans))

g = g + geom_point( size = 3, colour = "black") + geom_point( size = 2)

g



model4 = lm( mpg ~ trans + I( sqrt( hp / wt)), Data)

model5 = lm( mpg ~ trans + I( hp / wt), Data)

DATA = Data[ -28,]
model5b = lm( mpg ~ trans + I( hp / wt), DATA)

model4b = lm( mpg ~ trans + I( sqrt( hp / wt)), DATA)

model6 = lm( mpg ~ trans + I( hp / wt) + drat, Data)



g.model = lm( wt ~ disp, Data)
slope = coef( g.model)[ 2]
intercept = coef( g.model)[ 1]

g = ggplot( Data, aes( x = disp, y = wt, colour = trans, label = rownames( Data)))

g = g + geom_point( size = 1, colour = "black") + geom_text(size=3)

g = g + geom_abline( slope = slope, intercept = intercept)

g



g.model = lm( wt ~ hp, Data)
slope = coef( g.model)[ 2]
intercept = coef( g.model)[ 1]

g.model.a = lm( wt ~ hp, Data[ Data$trans == "auto",])
g.model.m = lm( wt ~ hp, Data[ Data$trans == "man",])
intercept.a = coef( g.model.a)[ 1]
intercept.m = coef( g.model.m)[ 1]
slope.a = coef( g.model.a)[ 2]
slope.m = coef( g.model.m)[ 2]

g = ggplot( Data, aes( x = hp, y = wt, colour = trans, label = rownames( Data)))

g = g + geom_point( size = 1, colour = "black") + geom_text(size=3)

g = g + geom_abline( slope = slope, intercept = intercept)

g = g + geom_abline( slope = slope.a, intercept = intercept.a)

g = g + geom_abline( slope = slope.m, intercept = intercept.m)

g


g.model = lm( mpg ~ wt, Data)
slope = coef( g.model)[ 2]
intercept = coef( g.model)[ 1]
g.model.a = lm( mpg ~ wt, Data[ Data$trans == "auto",])
g.model.m = lm( mpg ~ wt, Data[ Data$trans == "man",])
intercept.a = coef( g.model.a)[ 1]
intercept.m = coef( g.model.m)[ 1]
slope.a = coef( g.model.a)[ 2]
slope.m = coef( g.model.m)[ 2]

g = ggplot( Data, aes( x = wt, y = mpg, colour = trans, label = rownames( Data)))

g = g + geom_point( size = 2, colour = "black") + geom_point( size = 1)

g = g + geom_abline( slope = slope, intercept = intercept)

g = g + geom_abline( slope = slope.a, intercept = intercept.a)

g = g + geom_abline( slope = slope.m, intercept = intercept.m)

g


g.model = lm( mpg ~ I( hp / wt), Data)
g.model.a = lm( mpg ~ I( hp / wt), Data[ Data$trans == "auto",])
g.model.m = lm( mpg ~ I( hp / wt), Data[ Data$trans == "man",])
slope = coef( g.model)[ 2]
slope.a = coef( g.model.a)[ 2]
slope.m = coef( g.model.m)[ 2]
intercept = coef( g.model)[ 1]
intercept.a = coef( g.model.a)[ 1]
intercept.m = coef( g.model.m)[ 1]

g = ggplot( Data, aes( x = I( hp / wt), y = mpg, colour = trans, label = rownames( Data)))

g = g + geom_point( size = 2, colour = "black") + geom_point( size = 1) + geom_text(size=3)

g = g + geom_abline( slope = slope, intercept = intercept)

g = g + geom_abline( slope = slope.a, intercept = intercept.a)

g = g + geom_abline( slope = slope.m, intercept = intercept.m)

g

g.model = lm( mpg ~ I( hp / wt), DATA)
g.model.a = lm( mpg ~ I( hp / wt), DATA[ Data$trans == "auto",])
g.model.m = lm( mpg ~ I( hp / wt), DATA[ Data$trans == "man",])
slope = coef( g.model)[ 2]
slope.a = coef( g.model.a)[ 2]
slope.m = coef( g.model.m)[ 2]
intercept = coef( g.model)[ 1]
intercept.a = coef( g.model.a)[ 1]
intercept.m = coef( g.model.m)[ 1]

g = ggplot( DATA, aes( x = I( hp / wt), y = mpg, colour = trans, label = rownames( DATA)))

g = g + geom_point( size = 1, colour = "black") + geom_text(size=3)

g = g + geom_abline( slope = slope, intercept = intercept)

g = g + geom_abline( slope = slope.a, intercept = intercept.a)

g = g + geom_abline( slope = slope.m, intercept = intercept.m)

g



dataset = Data[Data$hp <= 230,]
submodel = lm( mpg ~ wt + hp + trans, dataset)
model2 = lm( mpg ~ trans + cyl, dataset)
model3 = lm( mpg ~ trans + cyl + wt + hp + hp:wt, dataset)
model4 = lm( mpg ~ wt + hp + trans + cyl, dataset)
model5 = lm( mpg ~ trans + wt + hp + hp:wt, dataset)
model6 = lm( mpg ~ trans + hp:wt, dataset)
model7 = lm( mpg ~ trans + cyl + hp:wt, dataset)
model8 = lm( mpg ~ cyl + hp:wt, dataset)
model11 = lm( mpg ~ trans + hp + + drat + wt, dataset)


par( mfrow = c( 2, 2))

Model1 = lm( mpg ~ trans + cyl + wt, Data)
par( mfrow = c( 2, 2))
plot( Model1, main = "mpg ~ trans + cyl + wt Adj R2 = 0.81")
sumMod1 = summary( Model1)

model9 = lm( mpg ~ trans + cyl + hp, Data)
par( mfrow = c( 2, 2))
plot( model9, main = "mpg ~ trans + cyl + hp  Adj R2 = 0.80")
summod9 = summary( model9)

model.trans.cyl.hp.wt = lm( mpg ~ trans + cyl + hp + wt, Data)
par( mfrow = c( 2, 2))
sumMod2 = summary( model.trans.cyl.hp.wt)
plot( model.trans.cyl.hp.wt, main = "mpg ~ trans + cyl + hp + wt  Adj R2 = 0.84")

Model10 = lm( mpg ~ trans + disp + hp + wt, Data)
par( mfrow = c( 2, 2))
plot( Model10, main = "mpg ~ trans + disp + hp + wt  Adj R2 = 0.82")
sumMod10 = summary( Model10)

Model11 = lm( mpg ~ trans + cyl + disp + hp + wt, Data)
par( mfrow = c( 2, 2))
plot( Model11, main = "mpg ~ trans + cyly + disp + hp + wt  Adj R2 = 0.83")
sumMod11 = summary( Model11)

model.sm = lm( mpg ~ trans + hp + I( hp / wt), data.sm[ -(16:17),])
par( mfrow = c( 2, 2))
plot( model.sm)
summary.sm = summary( model.sm)



#Model1 = lm( mpg ~ trans + cyl + 

full.model = lm( mpg ~ trans + cyl + disp + hp + drat + wt + gear + carb + I( hp / wt), Data)

full.model2 = lm( mpg ~ trans + cyl + disp + hp + drat + wt + gear + carb + I( hp / wt), dataset)


mat.data = Data[ , c( 1,2,3,4,5,6,9,10,11)]

out = regsubsets( mpg ~ trans + cyl + disp + hp + drat + wt + gear + carb + I( hp / wt), data = Data, force.in =  c( 1), nvmax = 15)


model.cyl.hp.wt = lm( mpg ~ cyl + hp + wt, Data)
model.disp.hp.wt = lm( mpg ~ disp + hp + wt, Data)
model.cyl.disp.hp.wt = lm( mpg ~ cyl + disp + hp + wt, Data)
model.wt.cyl = lm( mpg ~ wt + cyl, Data)
model.wt.cyl.drat = lm( mpg ~ wt + cyl + drat, Data)

summary( model.cyl.hp.wt)
summary( model.disp.hp.wt)
summary( model.cyl.disp.hp.wt)
summary( model.wt.cyl)

anova( model.trans.cyl.hp.wt, model.cyl.hp.wt, model.wt.cyl)

