#Author: Ravi Kumar

#Generation of random sample:

#Direct methods:
#refer page 247 of the book casella and berger , section 5.6.1

#the samples are generated in Y,Y2,Y3
# mean can be printed by typing mean_Y,mean_Y2,mean_Y3
#variance can be printed by typing var_Y,var_Y2,var_Y3
#where Y=Exponential (lambda=2); Y2= gamma(alpha =4, beta=2); Y3= Normal(0,1)


#generation of uniform random variables:
#https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Uniform.html
no_of_samples=20000;
lambda=2;
vec = runif(no_of_samples);

Y=NULL;
#==============================================================
#for Exponential distribution with lambda=2
# the distribution has been taken as: f(x)=(1/lambda)*exp(-x/lambda) with mean =2 and var=4
Y=-(lambda)*log(1-vec);

#to verify if our algorithm is correct for generating exponential RV ,
#we calculate mean and varaince first

mean_Y=sum(Y)/no_of_samples;

dummy_var1=0;
for (i in 1:no_of_samples)
  dummy_var1=dummy_var1+(Y[i]-mean_Y)^2;

var_Y= dummy_var1/(no_of_samples-1);

hist(Y,breaks=80,prob=TRUE)
curve((1/lambda)*exp(-x/lambda), 0, 20,add=TRUE,col="green")

#tips on graph plotting in R: see help in graph text


#================================================================
# for gamma distribution with even no of degree of freedom
#gamma(alpha,beta)

vec1 = runif(no_of_samples);
vec2 = runif(no_of_samples);
vec3 = runif(no_of_samples);
alpha= 4;beta=2;

Y2 <- -(beta)*(log(vec*vec1*vec2*vec3));
mean_Y2=sum(Y2)/no_of_samples;
  
dummy_var2=0;
for (i in 1:no_of_samples)
  dummy_var2=dummy_var2+(Y2[i]-mean_Y2)^2;

var_Y2= dummy_var2/(no_of_samples-1);

hist(Y2,breaks=80,prob=TRUE)
curve((x^(alpha-1))*exp(-x/beta)/(factorial(alpha-1)*(beta^alpha)), 0, 20,add=TRUE,col="red")

#===============================================================
# BOX MUELLER algorithm for samples from normal distribution
# X and Y are N(0,1)

R=sqrt(-2*log(vec));
theta= 2*pi*vec2;
X3= R * cos(theta);
Y3= R* sin(theta);

mean_Y3=sum(Y3)/no_of_samples;

dummy_var3=0;
for (i in 1:no_of_samples)
  dummy_var3=dummy_var3+(Y3[i]-mean_Y3)^2;

var_Y3= dummy_var3/(no_of_samples-1);

hist(Y3,breaks=80,prob=TRUE)
curve(exp(-(x^2)/2)/sqrt(2*pi), -5, 5,add=TRUE,col="red")

# some more hints:
#For Histogram in R(varying the number of bins, density etc.)
#see link:
#http://www.r-bloggers.com/basics-of-histograms/
  
