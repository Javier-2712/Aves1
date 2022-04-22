# Instalar dos paquetes para llamar el archivo xlsx
install.packages("readxl")
install.packages("Rcpp")

library(readxl)
library(Rcpp)

# lectura de la base de datos Aves
aves <- read_excel("datos.xlsx")
aves

# Estructura ...
str(aves)

# Lectura ...
# 
aves[,c(8:16)]

# nombres de las variables ...
# ...
names(aves[,c(8:16)])

# Estadiscos básicos ...
summary(aves[,c(8:16)])

# ----------------
# FIGURAS EXPLORATORIAS BÁSICAS EN R
#biblioteca ...
library(lattice)

# Gráfica por pares
pairs(aves[,8:16])

x11()
pairs(log10(aves[,c(8:16)]))


# Coplot
# Grafica de tres variables, ...
# ...
with(aves,coplot(Comisura~Longitud.total|Cuerda.Alar))

with(aves,coplot(Comisura~Longitud.total|Cuerda.Alar,
                 panel=function(x,y,col,pch)
                         panel.smooth(x,y,span=1)))


# Uso del xyplot
xyplot(Comisura~Longitud.total|Dieta,data=aves)
xyplot(Comisura~Longitud.total|Familia1,data=aves)

names(aves)
# ...
# ...
plot(Culmen.Total~Culmen.Expuesto,data=aves)
identify(aves$Culmen.Expuesto,aves$Culmen.Total)

# Recta de la relación ...
abline(lm(Culmen.Total~Culmen.Expuesto,data=aves))
help(xyplot)

#----------------------------------
# Gráfica para ...
library(ellipse)
plotcorr(cor(aves[,8:16]))

x11()
xc=aves[,c(8:16)]
corr.d <- cor(aves[,c(8:16)])
ord <- order(corr.d[1,])
xc <- corr.d[ord, ord]
colors <- c("#A50F15","#DE2D26","#FB6A4A","#FCAE91","#FEE5D9","pink",
            "#EFF3FF","#BDD7E7","#6BAED6","#3182BD","#08519C")
plotcorr(xc, col = colors[5*xc + 6])


# relaciones multiples
library(lattice)

pairs((aves[,c(8:16)]),panel=function(x,y)
{abline(lsfit(x,y)$coef,col=3)
        lines(lowess(x,y),lty=2,col=2)
        points(x,y)})


pairs((aves[,c(7:9,12:15)]),panel=function(x,y)
{abline(lsfit(x,y)$coef,lwd=1,col=3)
        lines(lowess(x,y),lty=2,lwd=1,col=2)
        points(x,y, col=aves$Dieta,cex=0.7,pch=2)})


#Colocar colores ...
xyplot(Cuerda.Alar~Longitud.total,group=Dieta,
       auto.key=T,data=aves)

# Otra forma de rotular para 
plot(Cuerda.Alar~Longitud.total,
     col=as.integer(Dieta),data=aves)              

legend(50,200,legend=levels(aves$Dieta),
       pch=19,col=1:5,cex=0.8)

# curva de ajuste ...       
abline(lm(Cuerda.Alar~Longitud.total,data=aves),lty=2)

# Resumen de la ...        
summary(lm(Cuerda.Alar~Longitud.total,data=aves))

#----------------------------------------------------------------
# Cajas y bigotes (exigen varios datos o replicas en c/variable)
boxplot(Longitud.total~Dieta,data=aves,notch=TRUE,
        xlab="Dieta",ylab="Longitud Total (mm)",
        col=c("#A50F15","#DE2D26","#FB6A4A","#FCAE91","#FEE5D9"), cex.lab=1.3)

demo("colors")

#-------------------------------------------------------------------
# Otra formas ...
bwplot(Longitud.total~Dieta,data=aves,notch=TRUE)
bwplot(Cuerda.Alar~Dieta|Familia1,notch=TRUE,data=aves)

# histograma 
histogram(~Longitud.total,data=aves)

#densityplot
densityplot(~Longitud.total,data=aves)      
densityplot(~Longitud.total|Dieta,data=aves)


#---------------------------------------------
#ANOVA 1 VIA (Longitud Total vs. Dieta)



# Normalidad


# Hogeneidad


# Kruskal-Wallis


# ANOVA y K-W detectan diferencias
# Normalidad = no se cumple
# Homogeneidad = se cumple

# Comparación múltiple de medianas




# Variables transformadas



