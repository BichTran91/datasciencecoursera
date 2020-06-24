add2<-function(x,y){x+y}

above10<-function(x){
        use<-x>10
        x[use]
}

above<-function(x,n=5){
        use<-x>n
        x[use]
}

colm<-function(y, removeNA=T){
        nc<-ncol(y)
        means<-numeric(nc)
        for (i in 1:nc) {
                means[i]<-mean(y[,i],na.rm=removeNA)
        }
        means
}

power<-function(n){
        pow<-function(x){
                x^n
        }
        pow
}

cube<-power(3)
square<-power(2)
quad<-power(4)

y<-10
f<-function(x){
        y<-2
        y^2+g(x)
}
g<-function(x){
        x*y
}

NegLogLik<-function(data,fixed=c(F,F)){
        params<-fixed
        function(p){
                params[!fixed]<-p
                mu<-params[1]
                sigma<-params[2]
                a<- -0.5*length(data)*log(2*pi*sigma^2)
                b<- -0.5*sum((data-mu)^2)/(sigma^2)
                -(a+b)
        }
}

nLL<-NegLogLik(normals,c(F,2))
optimize(nLL,c(-1,3))$minimum

nLL<-NegLogLik(normals,c(1,F))
optimize(nLL,c(1e-6,10))$minimum

x<-as.Date("1970-01-01")

unclass(as.Date("1991-06-15")) #how many days since 1970-01-01

x<-Sys.time() #current time

x<-as.POSIXlt(x)
names(unclass(p))

datestring<-c("June 23, 2020 21:05")
y<-strptime(datestring,"%B %d, %Y %H:%M")

y<-strptime("23 June 2020 21:15:20", "%d &B %Y %H:%M:%S")

x<-as.POSIXlt("1991-06-15 21:00:00")
y<-as.POSIXlt("1987-08-16 00:00:00")
dif<-x-y


cube<-function(x,n){
        x^3
}
cube(3)

x<-1:10
if(x>5) {
        x<-0
}

x<-5
y<- if(x<3){
        NA
}else{
        10
}

f<-function(x){
        g<-function(y){
                y+z
        }
        z<-4
        x+g(x)
}
z<-10
f(3)

x<-list(2,"a","b",TRUE)
x[[2]]

x<-1:4; y<-2
x+y

d<-read.csv(choose.files())

sub0<-subset(d,is.na(Ozone),select=Ozone)

sub1<-subset(d,!is.na(Ozone),select=Ozone)
sub1
mean(sub1$Ozone)

sub2<-subset(d,Ozone>31&Temp>90)
mean(sub2$Solar.R)

sub3<-subset(d,Month==6)
mean(sub3$Temp)

sub4<-subset(d,Month==5&!is.na(Ozone))
sub4
max(sub4$Ozone)

x<-1:4
y<-2:3
x+y
class(x+y)