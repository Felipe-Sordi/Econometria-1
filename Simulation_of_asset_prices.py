#Importing Libraries

import numpy as np
import matplotlib.pyplot as plt

# Geometric Brownian Motion (without drift)

    #Setting Parameters

mu = 0 #drift
sigma = 1.5 #vol
P0 = 27.1 #initial price
T = 1 #total time horizon
N = 10000 #number of timesteps

    #Time increments

dt = T / N #size of each time step
t = np.linspace(0, T, N+1)

    #Price array

P = np.zeros(N+1)
P[0] = P0

    #Simulating the price Path

for i in range(1,  N+1):
    dW = np.random.normal(0, np.sqrt(dt))
    P[i] = P[i-1] * (1 +  dt * sigma * dW) 

    #Plot 

plt.plot(t, P, color = "green")
plt.xlabel("Time")
plt.ylabel("Price")
plt.title("Geometric Brownian Motion without drift")
plt.show()




# Geometric Brownian Motion

    #Setting Parameters

mu = 1.76 #drift - it represents the average rate of return of the asset
sigma = 1.5 #vol 
P0 = 27.1 #initial price
T = 1 #time horizon - represents the total time horizon of the simulation (e.g., 1y)
N = 10000 #number of timesteps - the number of time steps in the simulation (T will be divided into 1000 intervals)

    #Time increments

dt = T / N #size of each time step
t = np.linspace(0, T, N + 1) #creating an array t of N+1 evenly spaced time points from 0 to T

    #Price Array
P = np.zeros(N+1) #creating an array P of zeros with N + 1 elements, where each element represents the price of the asset at each time step
P[0] = P0

    #Simulating the price path

for i in range(1, N+1):
    dW = np.random.normal(0, np.sqrt(dt))
    P[i] = P[i-1]*(1+ mu * dt * sigma * dW)

    #Plot
plt.plot(t, P, color = "#F6511D")
plt.xlabel("Time")
plt.ylabel("Price")
plt.title("Geometric Brownian Motion with drift")
plt.show()

# Mean-Reverting Process

    #Setting Parameters 
mu = 1.76
sigma = 5.9
P0 = 27.1 
T = 4
N = 30000
P_bar = 30

    #Time increments

dt = T/N
t = np.linspace(0, T, N+1)

    #Price array
P = np.zeros(N+1)
P[0] = P0

    #Simulating  the Price Path

for i in range(1, N+1):
    dW = np.random.normal(0, np.sqrt(dt))
    P[i] = P[i-1] + mu*(P_bar -  P[i-1])*dt + sigma*P[i-1]*dW


    #Plot 

plt.plot(t, P, color='blue') 
plt.xlabel('Time')
plt.ylabel('Price')
plt.title('Asset Price Simulation with Mean-Reverting')
plt.show()





# Geometric Brownian Motion (with stochastic volatility)

    #Setting Parameters

mu = 1.76
sigma = 5.9
P0 = 27.1 
T = 4
N = 30000

    #Time increments

dt = T / N 
t = np.linspace(0, T, N+1)

    #Price array

P = np.zeros(N+1)
P[0] = P0

    #Simulating the price path

for i in range(1, N+1):
    if i == 1:
        dP_t = 0  # no previous change for the first step
    else:
        dP_t = P[i-1] - P[i-2]
        mean = dP_t / P[i-2] if P[i-2] != 0 else 0
        dW = np.random.normal(mean, np.sqrt(dt))
    
    P[i] = P[i-1] + mu * P[i-1] * dt + sigma * P[i-1] * dW


    #Plot

plt.plot(t, P, color='blue') 
plt.xlabel('Time')
plt.ylabel('Price')
plt.title('Asset Price Simulation with Conditional Drift and Volatility')
plt.show()
