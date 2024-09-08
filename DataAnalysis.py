#!/usr/bin/env python
# coding: utf-8

# # AMPM

# In[ ]:


import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np


# In[23]:


file_path = 'AMPM.csv'
df = pd.read_csv(file_path)

df.head()



df['TOTAL_MILK_YIELD'] = df['MILK_YIELDAM'] + df['MILK_YIELDAMPM']


df['Pam'] = (df['ACT_PROAM'] / 100) * df['MILK_YIELDAM']
df['Ppm'] = (df['ACT_PROAMPM'] / 100) * df['MILK_YIELDAMPM']
df['Total_Pro']=((df['Pam']+df['Ppm'])/(df['MILK_YIELDAM']+df['MILK_YIELDAMPM']))*100


df['Fam'] = (df['ACT_FATAM'] / 100) * df['MILK_YIELDAM']
df['Fpm'] = (df['ACT_FATAMPM'] / 100) * df['MILK_YIELDAMPM']
df['Total_fat']=((df['Fam']+df['Fpm'])/(df['MILK_YIELDAM']+df['MILK_YIELDAMPM']))*100


df['CALVING_DATEAM'] = pd.to_datetime(df['CALVING_DATEAM'])
df['MILK_WEIGHING_DTMAM'] = pd.to_datetime(df['MILK_WEIGHING_DTMAM'])
df['MILK_WEIGHING_DTMAMPM'] = pd.to_datetime(df['MILK_WEIGHING_DTMAMPM'])

# Calculating lactation period and milk interval in minutes
df['LACTATION_PERIOD'] = (df['MILK_WEIGHING_DTMAM'] - df['CALVING_DATEAM']).dt.days
df['MILK_INTERVAL'] = ( df['MILK_WEIGHING_DTMAMPM']-df['MILK_WEIGHING_DTMAM'] ).dt.total_seconds() / 3600



remove = [
    'PRED_FATAM', 'DIFF_FATAM', 'PC_DIFF_FATAM', 'PRED_PROAM', 'DIFF_PROAM', 'PC_DIFF_PROAM',
    'PRED_FATAMPM', 'DIFF_FATAMPM', 'PC_DIFF_FATAMPM', 'PRED_PROAMPM', 'DIFF_PROAMPM', 'PC_DIFF_PROAMPM'
]

df = df.drop(columns=remove)

df = df[(df['MILK_INTERVAL'] >= 6) & (df['MILK_INTERVAL'] <= 12)]


# In[ ]:





# In[18]:


Final_ampm = 'Dataset_ampm.csv'
df.to_csv(Final_ampm, index=False)


# # PMAM

# In[24]:


file_path = 'PMAM.csv'
dfs = pd.read_csv(file_path)

dfs.head()


dfs['TOTAL_MILK_YIELD'] = dfs['MILK_YIELDPM'] + dfs['MILK_YIELDPMAM']


dfs['Ppm'] = (dfs['ACT_PROPM'] / 100) * dfs['MILK_YIELDPM']
dfs['Pam'] = (dfs['ACT_PROPMAM'] / 100) * dfs['MILK_YIELDPMAM']
dfs['Total_Pro']=((dfs['Pam']+dfs['Ppm'])/(dfs['MILK_YIELDPM']+dfs['MILK_YIELDPMAM']))*100


dfs['Fpm'] = (dfs['ACT_FATPM'] / 100) * dfs['MILK_YIELDPM']
dfs['Fam'] = (dfs['ACT_FATPMAM'] / 100) * dfs['MILK_YIELDPMAM']
dfs['Total_fat']=((dfs['Fam']+dfs['Fpm'])/(dfs['MILK_YIELDPM']+dfs['MILK_YIELDPMAM']))*100


dfs['CALVING_DATEPM'] = pd.to_datetime(dfs['CALVING_DATEPM'])
dfs['MILK_WEIGHING_DTMPM'] = pd.to_datetime(dfs['MILK_WEIGHING_DTMPM'])
dfs['MILK_WEIGHING_DTMPMAM'] = pd.to_datetime(dfs['MILK_WEIGHING_DTMPMAM'])

# Calculating lactation period and milk interval in minutes
dfs['LACTATION_PERIOD'] = (dfs['MILK_WEIGHING_DTMPM'] - dfs['CALVING_DATEPM']).dt.days
dfs['MILK_INTERVAL'] = ( dfs['MILK_WEIGHING_DTMPMAM']-dfs['MILK_WEIGHING_DTMPM'] ).dt.total_seconds() / 3600



remove = [
    'PRED_FATPM', 'DIFF_FATPM', 'PC_DIFF_FATPM', 'PRED_PROPM', 'DIFF_PROPM', 'PC_DIFF_PROPM',
    'PRED_FATPMAM', 'DIFF_FATPMAM', 'PC_DIFF_FATPMAM', 'PRED_PROPMAM', 'DIFF_PROPMAM', 'PC_DIFF_PROPMAM'
]

dfs = dfs.drop(columns=remove)


dfs = dfs[(dfs['MILK_INTERVAL'] >= 12) & (dfs['MILK_INTERVAL'] <= 16)]


# In[20]:


Final_ampm = 'Dataset_pmam1.csv'
dfs.to_csv(Final_ampm, index=False)


# # AMPM-Analysing

# In[50]:


df.head()


# In[51]:


df.shape


# In[31]:


df.info()


# In[32]:


df.nunique()


# In[86]:


df['MILK_WEIGHING_DTMAM'] = pd.to_datetime(df['MILK_WEIGHING_DTMAM'])

df['YEAR'] = df['MILK_WEIGHING_DTMAM'].dt.year
df['MONTH'] = df['MILK_WEIGHING_DTMAM'].dt.month

# Group by year and month and calculate the total number of milkings done each month
mmc = df.groupby(['YEAR', 'MONTH']).size().reset_index(name='TOTAL_MILKINGS')

smmc = mmc.sort_values(by=['YEAR', 'MONTH','TOTAL_MILKINGS'])

smmc


# In[87]:


df['MILK_WEIGHING_DTMAM'] = pd.to_datetime(df['MILK_WEIGHING_DTMAM'])

df_2020_2022 = df[df['MILK_WEIGHING_DTMAM'].dt.year.isin([2020, 2021, 2022])]
df_2023 = df[df['MILK_WEIGHING_DTMAM'].dt.year == 2023]


unique_tags_2020_2022 = df_2020_2022['TAGAM'].nunique()
unique_herds_2020_2022 = df_2020_2022['HERDAM'].nunique()

unique_tags_2023 = df_2023['TAGAM'].nunique()
unique_herds_2023 = df_2023['HERDAM'].nunique()

# Find common herds and tags between the two splits
common_herds = len(set(df_2020_2022['HERDAM']).intersection(set(df_2023['HERDAM'])))
common_tags = len(set(df_2020_2022['TAGAM']).intersection(set(df_2023['TAGAM'])))

(df_2020_2022.shape,df_2023.shape,unique_tags_2020_2022, unique_herds_2020_2022, unique_tags_2023, unique_herds_2023, common_herds, common_tags)



# In[41]:


df['HERDAM'].value_counts()


# In[88]:


years = smmc['YEAR'].unique()


plt.figure(figsize=(15, 10))

for i, year in enumerate(years, 1):
    plt.subplot(2, 2, i)
    data =smmc[smmc['YEAR'] == year]
    plt.bar(data['MONTH'], data['TOTAL_MILKINGS'], edgecolor='black')
    plt.xlabel('Month')
    plt.ylabel('Total Milkings')
    plt.title(f'Total Milkings in {year}')
    plt.xticks(ticks=range(1, 13))
    plt.grid(True)

plt.tight_layout()
plt.show()


# In[52]:


plt.figure(figsize=(20, 8))

# Distribution for TOTAL_MILK_YIELD
plt.subplot(1, 3, 1)
sns.histplot(df['TOTAL_MILK_YIELD'], bins=30, kde=False, color='skyblue', edgecolor='black')
plt.title('Distribution of 24hr MILK YIELD')
plt.xlabel('24hr MILK YIELD (kg)')
plt.ylabel('Frequency')

# Distribution for TOTAL_PRO
plt.subplot(1, 3, 2)
sns.histplot(df['Total_Pro'], bins=30, kde=False, color='lightgreen', edgecolor='black')
plt.title('Distribution of 24hr PROTEIN')
plt.xlabel('24hr PROTEIN (%)')
plt.ylabel('Frequency')

# Distribution for TOTAL_FAT
plt.subplot(1, 3, 3)
sns.histplot(df['Total_fat'], bins=30, kde=False, color='lightcoral', edgecolor='black')
plt.title('Distribution of 24hr FAT')
plt.xlabel('24hr FAT (%)')
plt.ylabel('Frequency')

plt.tight_layout()
plt.show()


# In[95]:


# Calculate quantiles
quantiles_milk_yield_am = np.percentile(df['MILK_YIELDAM'], np.arange(0, 101, 1))
quantiles_milk_yield_pm = np.percentile(df['MILK_YIELDAMPM'], np.arange(0, 101, 1))

quantiles_fat_am = np.percentile(df['ACT_FATAM'], np.arange(0, 101, 1))
quantiles_fat_pm = np.percentile(df['ACT_FATAMPM'], np.arange(0, 101, 1))

quantiles_pro_am = np.percentile(df['ACT_PROAM'], np.arange(0, 101, 1))
quantiles_pro_pm = np.percentile(df['ACT_PROAMPM'], np.arange(0, 101, 1))

# Plot quantiles
fig, axs = plt.subplots(1, 3, figsize=(24, 8))

# Milk Yield Plot
axs[0].plot(quantiles_milk_yield_am, label='MILK_YIELD-AM', marker='s')
axs[0].plot(quantiles_milk_yield_pm, label='MILK_YIELD-PM', marker='^')
axs[0].set_xlabel('Frequency (Percentile-%)')
axs[0].set_ylabel('Milk Yield (kg)')
axs[0].set_title('Quantile Plot for Milk Yields')
axs[0].legend()
axs[0].grid(True)

# Fat Plot
axs[1].plot(quantiles_fat_am, label='FAT-AM', marker='s')
axs[1].plot(quantiles_fat_pm, label='FAT-PM', marker='^')
axs[1].set_xlabel('Frequency (Percentile-%)')
axs[1].set_ylabel('Fat (%)')
axs[1].set_title('Quantile Plot for Fat')
axs[1].legend()
axs[1].grid(True)

# Protein Plot
axs[2].plot(quantiles_pro_am, label='PRO-AM', marker='s')
axs[2].plot(quantiles_pro_pm, label='PRO-PM', marker='^')
axs[2].set_xlabel('Frequency (Percentile-%)')
axs[2].set_ylabel('Protein (%)')
axs[2].set_title('Quantile Plot for Protein')
axs[2].legend()
axs[2].grid(True)

# Adjust layout
plt.tight_layout()
plt.show()


# In[56]:


#Calculate the lactation period in weeks
df['Lactation_Weeks'] = df['LACTATION_PERIOD'] / 7

# Plot the histogram
plt.figure(figsize=(12, 6))
plt.hist(df['Lactation_Weeks'], bins=20, edgecolor='black')
plt.xlabel('Lactation Period (Weeks)')
plt.ylabel('Total Number of milkings')
plt.title('Histogram of Lactation Period in Weeks vs Total Number of Milkings')
plt.grid(True)
plt.show()


# In[57]:


parity_distribution = df['PARITYAM'].value_counts().sort_index()

plt.figure(figsize=(10, 6))
parity_distribution.plot(kind='bar', edgecolor='black')
plt.xlabel('Parity')
plt.ylabel('Frequency')
plt.title('Distribution of Parity')
plt.grid(True)
plt.show()


# In[16]:


try:
    df['Parity_Group'] = pd.cut(df['PARITYAM'], bins=[0, 1, 2, df['PARITYAM'].max()], labels=['1', '2', '3+'])
    print("Parity_Group column created successfully.")
except KeyError as e:
    print(f"Error: {e}. Please ensure 'PARITYAM' column exists in the dataframe.")
    raise

# Step 2: Check if Parity_Group column exists in the dataframe
if 'Parity_Group' in df.columns:
    print("Parity_Group column is available for plotting.")
else:
    print("Parity_Group column is not found. Please check your dataframe.")
    raise KeyError("Parity_Group column not found.")

# Step 3: Create subplots and generate the boxplots
fig, axes = plt.subplots(1, 3, figsize=(18, 6))

# Boxplot for total milk yield
df.boxplot(column='TOTAL_MILK_YIELD', by='Parity_Group', grid=False, ax=axes[0])
axes[0].set_xlabel('Parity Group')
axes[0].set_ylabel('24hr Milk Yield (Kg)')
axes[0].set_title('24hr Milk Yield by Parity Group')

# Boxplot for total fat
df.boxplot(column='Total_fat', by='Parity_Group', grid=False, ax=axes[1])
axes[1].set_xlabel('Parity Group')
axes[1].set_ylabel('24hr Fat (%)')
axes[1].set_title('24hr Fat by Parity Group')

# Boxplot for total protein
df.boxplot(column='Total_Pro', by='Parity_Group', grid=False, ax=axes[2])
axes[2].set_xlabel('Parity Group')
axes[2].set_ylabel('24hr Protein (%)')
axes[2].set_title('24hr Protein by Parity Group')

# Adjust layout and show plot
plt.suptitle('')
plt.tight_layout()
plt.show()


# In[58]:


# Plotting the graphs
plt.figure(figsize=(18, 18))

# Scatter plot for Milk Interval vs Total Milk Yield
plt.subplot(3, 1, 1)
sns.scatterplot(x='MILK_INTERVAL', y='TOTAL_MILK_YIELD', data=df, edgecolor=None,alpha=0.03)
plt.title('Milk Interval vs 24hr Milk Yield')
plt.xlabel('Milk Interval')
plt.ylabel('24hr Milk Yield')

# Scatter plot for Milk Interval vs Total Fat
plt.subplot(3, 1, 2)
sns.scatterplot(x='MILK_INTERVAL', y='Total_fat', data=df, edgecolor=None,alpha=0.03)
plt.title('Milk Interval vs 24hr Fat')
plt.xlabel('Milk Interval ')
plt.ylabel('24hr Fat')

# Scatter plot for Milk Interval vs Total Protein
plt.subplot(3, 1, 3)
sns.scatterplot(x='MILK_INTERVAL', y='Total_Pro', data=df, edgecolor=None,alpha=0.03)
plt.title('Milk Interval vs 24hr Protein')
plt.xlabel('Milk Interval')
plt.ylabel('24hr Protein')

plt.tight_layout()
plt.show()


# In[146]:


herd_sizes = df['HERDAM'].value_counts()

small = herd_sizes.quantile(0.33)
large = herd_sizes.quantile(0.66)

def herd_size(herd):
    size = herd_sizes[herd]
    if size <= small:
        return 'Small Herd'
    elif size <= large:
        return 'Medium Herd'
    else:
        return 'Large Herd'

df['HS_GROUP'] = df['HERDAM'].map(herd_size)

plt.figure(figsize=(10, 6))
sns.scatterplot(data=df, x='MILK_INTERVAL', y='TOTAL_MILK_YIELD', hue='HS_GROUP', alpha=0.05)
plt.title('Scatter Plot of Milk Interval vs Total Milk Yield')
plt.xlabel('Milk Interval')
plt.ylabel('Total Milk Yield')
plt.legend(title='Herd Size Group')
plt.grid(True)
plt.show()

plt.figure(figsize=(10, 6))
sns.scatterplot(data=df, x='MILK_INTERVAL', y='Total_fat', hue='HS_GROUP', alpha=0.05)
plt.title('Scatter Plot of Milk Interval vs Total Fat')
plt.xlabel('Milk Interval')
plt.ylabel('Total Fat')
plt.legend(title='Herd Size Group')
plt.grid(True)
plt.show()

plt.figure(figsize=(10, 6))
sns.scatterplot(data=df, x='MILK_INTERVAL', y='Total_Pro', hue='HS_GROUP', alpha=0.05)
plt.title('Scatter Plot of Milk Interval vs Total Pro')
plt.xlabel('Milk Interval')
plt.ylabel('Total Pro')
plt.legend(title='Herd Size Group')
plt.grid(True)
plt.show()


# # PMAM-Analysis

# In[76]:


dfs.head()


# In[77]:


dfs.shape


# In[61]:


dfs.info()


# In[78]:


dfs.nunique()


# In[11]:


dfs['HERDPM'].value_counts()


# In[ ]:





# In[79]:


dfs['MILK_WEIGHING_DTMPM'] = pd.to_datetime(dfs['MILK_WEIGHING_DTMPM'])

dfs_2020_2022 = dfs[dfs['MILK_WEIGHING_DTMPM'].dt.year.isin([2020, 2021, 2022])]
dfs_2023 = dfs[dfs['MILK_WEIGHING_DTMPM'].dt.year == 2023]


unique_tags_2020_2022 = dfs_2020_2022['TAGPM'].nunique()
unique_herds_2020_2022 = dfs_2020_2022['HERDPM'].nunique()

unique_tags_2023 = dfs_2023['TAGPM'].nunique()
unique_herds_2023 = dfs_2023['HERDPM'].nunique()

# Find common herds and tags between the two splits
common_herds = len(set(dfs_2020_2022['HERDPM']).intersection(set(dfs_2023['HERDPM'])))
common_tags = len(set(dfs_2020_2022['TAGPM']).intersection(set(dfs_2023['TAGPM'])))

(dfs_2020_2022.shape,dfs_2023.shape,unique_tags_2020_2022, unique_herds_2020_2022, unique_tags_2023, unique_herds_2023, common_herds, common_tags)



# In[81]:


dfs['MILK_WEIGHING_DTMPM'] = pd.to_datetime(dfs['MILK_WEIGHING_DTMPM'])

dfs['YEAR'] = dfs['MILK_WEIGHING_DTMPM'].dt.year
dfs['MONTH'] = dfs['MILK_WEIGHING_DTMPM'].dt.month

# Group by year and month and calculate the total number of milkings done each month
mmc = dfs.groupby(['YEAR', 'MONTH']).size().reset_index(name='TOTAL_MILKINGS')

smmc = mmc.sort_values(by=['YEAR', 'MONTH','TOTAL_MILKINGS'])

years = smmc['YEAR'].unique()


plt.figure(figsize=(15, 10))

for i, year in enumerate(years, 1):
    plt.subplot(2, 2, i)
    data =smmc[smmc['YEAR'] == year]
    plt.bar(data['MONTH'], data['TOTAL_MILKINGS'], edgecolor='black')
    plt.xlabel('Month')
    plt.ylabel('Total No of Milkings')
    plt.title(f'Total Milkings in {year}')
    plt.xticks(ticks=range(1, 13))
    plt.grid(True)

plt.tight_layout()
plt.show()


# In[82]:


plt.figure(figsize=(20, 8))

# Distribution for TOTAL_MILK_YIELD
plt.subplot(1, 3, 1)
sns.histplot(dfs['TOTAL_MILK_YIELD'], bins=30, kde=False, color='skyblue', edgecolor='black')
plt.title('Distribution of 24hr MILK YIELD')
plt.xlabel('24hr MILK YIELD (kg)')
plt.ylabel('Frequency')

# Distribution for TOTAL_PRO
plt.subplot(1, 3, 2)
sns.histplot(dfs['Total_Pro'], bins=30, kde=False, color='lightgreen', edgecolor='black')
plt.title('Distribution of 24hr PROTEIN')
plt.xlabel('24hr PROTEIN (%)')
plt.ylabel('Frequency')

# Distribution for TOTAL_FAT
plt.subplot(1, 3, 3)
sns.histplot(dfs['Total_fat'], bins=30, kde=False, color='lightcoral', edgecolor='black')
plt.title('Distribution of 24hr FAT')
plt.xlabel('24hr FAT (%)')
plt.ylabel('Frequency')

plt.tight_layout()
plt.show()


# In[98]:


# Calculate quantiles
quantiles_milk_yield_pm = np.percentile(dfs['MILK_YIELDPM'], np.arange(0, 101, 1))
quantiles_milk_yield_am = np.percentile(dfs['MILK_YIELDPMAM'], np.arange(0, 101, 1))

quantiles_fat_pm = np.percentile(dfs['ACT_FATPM'], np.arange(0, 101, 1))
quantiles_fat_am = np.percentile(dfs['ACT_FATPMAM'], np.arange(0, 101, 1))

quantiles_pro_pm = np.percentile(dfs['ACT_PROPM'], np.arange(0, 101, 1))
quantiles_pro_am = np.percentile(dfs['ACT_PROPMAM'], np.arange(0, 101, 1))

# Plot quantiles
fig, axs = plt.subplots(1, 3, figsize=(24, 8))

# Milk Yield Plot
axs[0].plot(quantiles_milk_yield_pm, label='MILK_YIELD-PM', marker='s')
axs[0].plot(quantiles_milk_yield_am, label='MILK_YIELD-AM', marker='^')
axs[0].set_xlabel('Frequency (Percentile-%)')
axs[0].set_ylabel('Milk Yield (kg)')
axs[0].set_title('Quantile Plot for Milk Yields')
axs[0].legend()
axs[0].grid(True)

# Fat Plot
axs[1].plot(quantiles_fat_pm, label='FAT-PM', marker='s')
axs[1].plot(quantiles_fat_am, label='FAT-AM', marker='^')
axs[1].set_xlabel('Frequency (Percentile-%)')
axs[1].set_ylabel('Fat (%)')
axs[1].set_title('Quantile Plot for Fat')
axs[1].legend()
axs[1].grid(True)

# Protein Plot
axs[2].plot(quantiles_pro_pm, label='PRO-PM', marker='s')
axs[2].plot(quantiles_pro_am, label='PRO-AM', marker='^')
axs[2].set_xlabel('Frequency (Percentile-%)')
axs[2].set_ylabel('Protein (%)')
axs[2].set_title('Quantile Plot for Protein')
axs[2].legend()
axs[2].grid(True)

# Adjust layout
plt.tight_layout()
plt.show()


# In[73]:


# Plotting the graphs
plt.figure(figsize=(18, 18))

# Scatter plot for Milk Interval vs Total Milk Yield
plt.subplot(3, 1, 1)
sns.scatterplot(x='MILK_INTERVAL', y='TOTAL_MILK_YIELD', data=dfs, edgecolor=None,alpha=0.03)
plt.title('Milk Interval  vs 24hr Milk Yield')
plt.xlabel('Milk Interval')
plt.ylabel('24hr Milk Yield')

# Scatter plot for Milk Interval vs Total Fat
plt.subplot(3, 1, 2)
sns.scatterplot(x='MILK_INTERVAL', y='Total_fat', data=dfs, edgecolor=None,alpha=0.03)
plt.title('Milk Interval vs 24hr Fat')
plt.xlabel('Milk Interval ')
plt.ylabel('24hr Fat')

# Scatter plot for Milk Interval vs Total Protein
plt.subplot(3, 1, 3)
sns.scatterplot(x='MILK_INTERVAL', y='Total_Pro', data=dfs, edgecolor=None,alpha=0.03)
plt.title('Milk Interval vs 24hr Protein')
plt.xlabel('Milk Interval')
plt.ylabel('24hr Protein')

plt.tight_layout()
plt.show()


# In[17]:


# Group parity into three groups
dfs['Parity_Group'] = pd.cut(dfs['PARITYPM'], bins=[0, 1, 2, dfs['PARITYPM'].max()], labels=['1', '2', '3+'])

# Plot the boxplots
fig, axes = plt.subplots(1, 3, figsize=(18, 6))

# Boxplot for total milk yield
dfs.boxplot(column='TOTAL_MILK_YIELD', by='Parity_Group', grid=False, ax=axes[0])
axes[0].set_xlabel('Parity Group')
axes[0].set_ylabel('24hr Milk Yield (Kg)')
axes[0].set_title('24hr Milk Yield by Parity Group')

# Boxplot for total fat
dfs.boxplot(column='Total_fat', by='Parity_Group', grid=False, ax=axes[1])
axes[1].set_xlabel('Parity Group')
axes[1].set_ylabel('24hr Fat (%)')
axes[1].set_title('24hr Fat by Parity Group')

# Boxplot for total protein
dfs.boxplot(column='Total_Pro', by='Parity_Group', grid=False, ax=axes[2])
axes[2].set_xlabel('Parity Group')
axes[2].set_ylabel('24hr Protein (%)')
axes[2].set_title('24hr Protein by Parity Group')

plt.suptitle('')
plt.tight_layout()
plt.show()


# In[84]:


parity_distribution = dfs['PARITYPM'].value_counts().sort_index()

plt.figure(figsize=(10, 6))
parity_distribution.plot(kind='bar', edgecolor='black')
plt.xlabel('Parity')
plt.ylabel('Frequency')
plt.title('Distribution of Parity')
plt.grid(True)
plt.show()


# In[85]:


#Calculate the lactation period in weeks
dfs['Lactation_Weeks'] = dfs['LACTATION_PERIOD'] / 7

# Plot the histogram
plt.figure(figsize=(12, 6))
plt.hist(dfs['Lactation_Weeks'], bins=20, edgecolor='black')
plt.xlabel('Lactation Period (Weeks)')
plt.ylabel('Total Number of milkings')
plt.title('Histogram of Lactation Period in Weeks vs Total Number of Milkings')
plt.grid(True)
plt.show()


# In[ ]:




