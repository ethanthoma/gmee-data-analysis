import pandas as pd
from sklearn.linear_model import LinearRegression

df = pd.read_csv("./data/computed.csv")

y = df['Accuracy']

X = df[['Plausibility', 'Coverage']]
model = LinearRegression()
model.fit(X, y)
r_squared = model.score(X, y)
print(f"R-squared value: {r_squared}")

X = df[['Plausibility', 'Complexity']]
model = LinearRegression()
model.fit(X, y)
r_squared = model.score(X, y)
print(f"R-squared value: {r_squared}")

