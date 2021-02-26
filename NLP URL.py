#!/usr/bin/env python
# coding: utf-8

# In[17]:


import pandas as pd
import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns
import statsmodels.formula.api as smf
#import urllib2
df = pd.read_csv('https://raw.githubusercontent.com/resbaz/r-novice-gapminder-files/master/data/gapminder-FiveYearData.csv')
df


# In[6]:


df.isnull().sum()


# In[7]:


df.shape


# In[19]:


p = sns.FacetGrid(
    data=df,
    hue='continent',
    col='continent',
    col_wrap=2,
    height=4,
    aspect=16/9
).map(plt.plot, 'year', 'lifeExp')

plt.show()


# In[22]:


models = pd.DataFrame({
    'data': [data for _, data in df.groupby('country')],
})

models.index = [country for country, _ in df.groupby('country')]

models


# In[24]:


def country_model(dfm):
    return smf.ols('lifeExp ~ year', data=dfm).fit()

models['fit'] = [
    country_model(data)
    for _, data in df.groupby('country')
]


# In[26]:


def tidy(fit):
    from statsmodels.iolib.summary import summary_params_frame
    tidied = summary_params_frame(fit).reset_index()
    rename_cols = {
        'index': 'term', 'coef': 'estimate', 'std err': 'std_err',
        't': 'statistic', 'P>|t|': 'p_value',
        'Conf. Int. Low': 'conf_int_low', 'Conf. Int. Upp.': 'conf_int_high'
    }
    
    return tidied.rename(columns = rename_cols)

def glance(fit):
    return pd.DataFrame({
        'aic': fit.aic,
        'bic': fit.bic,
        'ess': fit.ess, # explained sum of squares
        'centered_tss': fit.centered_tss,
        'fvalue': fit.fvalue,
        'f_pvalue': fit.f_pvalue,
        'nobs': fit.nobs,
        'rsquared': fit.rsquared,
        'rsquared_adj': fit.rsquared_adj
    }, index=[0])

# note that augment() takes 2 inputs, whereas tidy() and glance() take 1
def augment(fit, data):
    dfm = data.copy()
    
    if len(dfm) != fit.nobs:
        raise ValueError("`data` does not have same number of observations as in training data.")
    
    dfm['fitted'] = fit.fittedvalues
    dfm['resid'] = fit.resid
    return dfm


# In[27]:


tidy(models.fit[0])


# In[28]:


glance(models.fit[0])


# In[29]:


augment(models.fit[0], models.data[0])


# In[30]:


models['tidied'] = [tidy(fit) for fit in models.fit]
models['glanced'] = [glance(fit) for fit in models.fit]

models['augmented'] = [
    augment(fit, data)
    for fit, data in zip(models.fit, models.data)
]


# In[31]:


models


# In[32]:


def unnest(dfm, value_col):
    lst = dfm[value_col].tolist()
    unnested = pd.concat(lst, keys=dfm.index)
    unnested.index = unnested.index.droplevel(-1)
    return dfm.join(unnested).drop(columns=value_col)


# In[33]:


glance_results = unnest(models, 'glanced')

# equivalently
glance_results = (
    models
    .pipe(unnest, 'glanced')
    
    # little bit of extra cleanup
    .reset_index()
    .rename(columns={'index': 'country'})
)

glance_results


# In[34]:


p = (
    sns.FacetGrid(
        data=glance_results.sort_values('rsquared'),
        height=8,
        aspect=16/9
    )
    .map(plt.scatter, 'rsquared', 'country')
)
plt.show()


# In[35]:


augment_results = unnest(models, 'augmented')

p = (
    sns.FacetGrid(
        data=augment_results,
        col='continent', 
        col_wrap=2,
        hue='continent',
        height=5, aspect=16/9)
    .map(plt.plot, 'year', 'resid')
)
plt.show()


# In[ ]:




