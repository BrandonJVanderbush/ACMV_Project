ACMV_Model <- function(x,z,y){
#This function takes in two variables and one string, the alpha value (x), the beta value (z), and the string relates to the hypothesis being tested using the model (y)
S = 9500
#This is the total number of initial susceptibles in the population
I = 500
#Total number of initial infected

N = 10000
#Initial number of all plants in the population

t=0
#Time steps start at zero
tmax=250
#Model will be run until time 250
dt=1
#dt equals the timestep of each iteration of the equation and for the model equals one day
cS=c(S)
#A variable that can store number of susceptibles over each iteration of the equation
cI=c(I)
#A variable that can store number of Infected over each iteration of the equation
ct=c(0)
#A variable that can store the time over each iteration of the equation
cN=c(N)
#A variable that can store number of the total population over each iteration of the equation
beta= z
#Infectivity Rate, 
alpha = x
#Alpha is death rate of infected plants due to two different conditions.
#The two conditions is increased death due to the infection and death due to plant pulling by farmers, if model is measuring it.
delta=0.0001
#Natural death rate of all plants whether they are infected or not

while(t<tmax){
  #This is a while loop that will run until the time t < max time,which is 250
  dS= (-delta*S - beta*S*I/N)*dt
  #This equation models the change in the number of susceptibles per time step
  dI = (beta*S*I/N - (alpha+delta)*I)*dt
  #This equation models the change in the number of infected per time step
  
  S=S+dS
  #Calculates the new number of susceptibles based off the previous number of susceptibles and the change in the number of susceptibles calculated through above equation
  I=I+dI
  #Calculates the new number of infected based off the previous population of infected and the change in infected
  t=t+dt
  #Calculates the time using previous time and the change in time which is 1
  N = S+I
  #Calculates the new total population using the new numbers for the population of infected and susceptible
  cS=c(cS,S)
  cI=c(cI,I)
  cN=c(cN,N)
  ct=c(ct,t)
  #The lines above create variables that allow for the storing of above values for insertion into a data frame
}


result=data.frame(ct,cS,cI,cN)
#Creates a data frame using the calculated values 
print(head(result))
#Lists the first 5 lines from the data frame inluding the initial values
print(tail(result))
#Lists the last 5 lines of the data frame

plot(result$ct,result$cS,type="l",xlab="Time",ylab="Number of Individuals", ylim=c(0,10000),xlim=c(0,270),lwd=1,main=paste("SI Model of ACMV", y, collapse = " "),col="blue")
#Sets up the initial plot that can be used to visualize the data frame and changes in the populations.
#Adds the susceptible line, which is blue
lines(result$ct,result$cI,type="l", col="red", lwd=1)
#Adds the infected line, which is red
lines(result$ct,result$cN,type="l", col="green",lwd=1)
#Adds the total population at each time step
Yield <- result[251,2] + (0.3* result[251,3])
#Calculates a total crop yield at the end of the 250 day season using the final population of susceptible and infected
print(paste("Total Yield of Cassava Crop:", round(Yield,digits = 0) , collapse = " ")) 
#Prints out the total yield of Cassava crop calculated above, while rounding it to a whole number
}


