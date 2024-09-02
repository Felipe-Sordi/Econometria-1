import numpy as np
import matplotlib.pyplot as plt

# Setting Parameters
mu = 0.10
sigma = 0.25
P0 = 50  # Initial stock price in R$
T = 1  # Time horizon in years
N = 1000  # Number of time steps
M = 100  # Number of simulations (reduced to 100 for clear visualization of paths)

# Time increments
dt = T / N
t = np.linspace(0, T, N+1)

# Array to store all simulated paths
paths = np.zeros((M, N+1))

# Monte Carlo Simulation
for m in range(M):
    P = np.zeros(N+1)
    P[0] = P0
    for i in range(1, N+1):
        dW = np.random.normal(0, np.sqrt(dt))
        P[i] = P[i-1] + mu * P[i-1] * dt + sigma * P[i-1] * dW
    paths[m, :] = P  # Store the entire path

# Plotting all paths
plt.figure(figsize=(12, 6))
for m in range(M):
    plt.plot(t, paths[m, :], lw=0.5, alpha=0.7)  # Thinner lines for clarity

plt.xlabel('Time (years)')
plt.ylabel('Stock Price (R$)')
plt.title('Monte Carlo Simulation of Stock Price Paths')
plt.grid(True)
plt.show()

# Expected price after 1 year
final_prices = paths[:, -1]
expected_price = np.mean(final_prices)


# Output the expected price
print(f"The expected price after 1 year is: R$ {expected_price:.2f}")

# 1) Probability that the price of the stock will be above R$ 70 after one year
final_prices = paths[:, -1]
prob_above_70 = np.mean(final_prices > 70)
print(f"1) Probability that the stock price will be above R$ 70 after one year: {prob_above_70:.4f}")

# 2) Probability that the price of the stock will be above R$ 70 at some point in the next year
prob_above_70_anytime = np.mean(np.max(paths, axis=1) > 70)
print(f"2) Probability that the stock price will be above R$ 70 at some point in the next year: {prob_above_70_anytime:.4f}")

# 3) Probability that the stock will be above R$ 70 after one year and at some point below R$ 50
prob_above_70_below_50 = np.mean((final_prices > 70) & (np.min(paths, axis=1) < 50))
print(f"3) Probability that the stock will be above R$ 70 after one year and below R$ 50 at some point: {prob_above_70_below_50:.4f}")

# 4) Probability that the stock will be above R$ 70 after one year and below R$ 50 during the first six months
prob_above_70_below_50_6months = np.mean((final_prices > 70) & (np.min(paths[:, :t_six_months+1], axis=1) < 50))
print(f"4) Probability that the stock will be above R$ 70 after one year and below R$ 50 in the first six months: {prob_above_70_below_50_6months:.4f}")

# 5) Probability that the stock crosses the R$ 70 threshold from below three times in one year
cross_70_from_below = np.sum(paths[:, :-1] < 70, axis=1) * np.sum(paths[:, 1:] > 70, axis=1)
prob_cross_70_three_times = np.mean(cross_70_from_below == 3)
print(f"5) Probability that the stock crosses R$ 70 three times from below: {prob_cross_70_three_times:.4f}")
