### Econ 26610 Homework 2 ###

# Import packages
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from sklearn.linear_model import LinearRegression
from statsmodels.nonparametric.smoothers_lowess import lowess

# Load datasets
df_pre = pd.read_csv("/Users/jacksonvanvooren/Downloads/data/data_pre.csv")
df_post = pd.read_csv("/Users/jacksonvanvooren/Downloads/data/data_post.csv")
df_funding = pd.read_csv("/Users/jacksonvanvooren/Downloads/data/data_fundingcenters.csv")

#  Add policy period
df_pre["policyperiod"] = "pre"
df_post["policyperiod"] = "post"

# ---------------------------------------------------------------------------------------------- #
# 2

# Merge data for combined sample
df_merged = pd.concat([df_pre, df_post])

# Heating column
print(df_merged["heating"].value_counts(normalize=True))
# Mean age
print("Mean age:", df_merged["age"].mean())
# Mean land area
print("Mean land area:", df_merged["landarea"].mean())

# ---------------------------------------------------------------------------------------------- #
# 3
# Pre policy
# Create indicators
df_pre = pd.get_dummies(df_pre, columns=["year"], prefix="year")
df_pre["centralac"] = df_pre["centralac"].apply(lambda x: 1 if x == 1 else 0)
df_pre = pd.get_dummies(df_pre, columns=["exterior"], prefix="exterior")
df_pre = pd.get_dummies(df_pre, columns=["heating"], prefix="heating")
df_pre = pd.get_dummies(df_pre, columns=["condition"], prefix="condition")

X = df_pre[["year_1993", "year_1994", "year_1995", "year_1996",
            "year_1997", "centralac", "exterior_Brick", "exterior_Vinyl", 
            "heating_Central", "heating_Hot Water", "livingarea", "age", 
            "landarea", "condition_Average", "condition_Poor", "condition_Very Poor", 
            "bath"]]
y = df_pre["lpricesqft"]
reg_pre = LinearRegression().fit(X, y)
pre_coefficients = np.append(reg_pre.coef_, reg_pre.intercept_)
print(pre_coefficients)

# Post policy
# Create indicators
df_post = pd.get_dummies(df_post, columns=["year"], prefix="year")
df_post["centralac"] = df_post["centralac"].apply(lambda x: 1 if x == 1 else 0)
df_post = pd.get_dummies(df_post, columns=["exterior"], prefix="exterior")
df_post = pd.get_dummies(df_post, columns=["heating"], prefix="heating")
df_post = pd.get_dummies(df_post, columns=["condition"], prefix="condition")

X = df_post[["year_1999", "year_2000", "year_2001", "year_2002",
            "year_2003", "centralac", "exterior_Brick", "exterior_Vinyl", 
            "heating_Central", "heating_Hot Water", "livingarea", "age", 
            "landarea", "condition_Average", "condition_Poor", "condition_Very Poor", 
            "bath"]]
y = df_post["lpricesqft"]
reg_post = LinearRegression().fit(X, y)
post_coefficients = np.append(reg_post.coef_, reg_post.intercept_)
print(post_coefficients)

# ---------------------------------------------------------------------------------------------- #
# 5
# Cleaning data to only include funding center neighborhoods
df_merged_cleaned = df_merged.dropna(subset=['neighborhood'])

funding_neighborhoods = df_funding['neighborhood'].unique()
df_merged_cleaned = df_merged_cleaned[df_merged_cleaned['neighborhood'].isin(funding_neighborhoods)]

def calculate_euclidean(geo_x_1, geo_x_2, geo_y_1, geo_y_2):
    """Calulcates standard Euclidean distance between two locations"""
    return np.sqrt((geo_x_2 - geo_x_1)**2 + (geo_y_2 - geo_y_1)**2)

unique_neighborhoods = df_merged_cleaned['neighborhood'].unique()
all_distances = []

for neigh in unique_neighborhoods:
    df_homes_neigh = df_merged_cleaned[df_merged_cleaned['neighborhood'] == neigh]
    df_funding_neigh = df_funding[df_funding['neighborhood'] == neigh]
    
    neighborhood_distances = []
    for _, home_row in df_homes_neigh.iterrows():
        home_distances = []

        for _, funding_row in df_funding_neigh.iterrows():
            distance = calculate_euclidean(home_row["geo_x"], funding_row["geo_x"],
                                           home_row["geo_y"], funding_row["geo_y"])
            home_distances.append(distance)

        min_distance = min(home_distances)
        neighborhood_distances.append(min_distance)
    
    print(f"{neigh} average: {np.mean(neighborhood_distances)}")
    all_distances.extend(neighborhood_distances)

    # Plot the histogram
    bins = np.arange(min(neighborhood_distances), max(neighborhood_distances) + 50, 50)
    plt.hist(neighborhood_distances, bins=bins, edgecolor='black')
    plt.xlabel('Distance')
    plt.ylabel('Frequency')
    plt.title(f'Closest Distances for {neigh} (bin width 50)')
    plt.show()

df_merged_cleaned['min_distance'] = all_distances
print('Total average', np.mean(df_merged_cleaned['min_distance']))

# ---------------------------------------------------------------------------------------------- #
# 6
# Set up figure
plt.figure(figsize=(12, 10))
neighborhoods = df_merged_cleaned['neighborhood'].unique()

# LOWESS smoothing for each neighborhood by policy period
for i, neigh in enumerate(neighborhoods, start=1):
    df_neigh = df_merged_cleaned[df_merged_cleaned['neighborhood'] == neigh]
    ax = plt.subplot(2, 2, i)
    
    for period in ['pre', 'post']:
        df_period = df_neigh[df_neigh['policyperiod'] == period]
        smoothed = lowess(df_period['lpricesqft'], df_period['min_distance'])

        plt.plot(smoothed[:, 0], smoothed[:, 1], color='blue' if period == 'pre' else 'red',
                 label=f'{period.capitalize()}-Policy')

    plt.title(f'Lowess Smoothing: Pre vs Post Policy in {neigh}')
    plt.xlabel('Min Distance')
    plt.ylabel('Log Price per Sqft')
    plt.xlim(0, 1500)
    plt.legend()
    plt.grid(True)
