#!/usr/bin/env python
# coding: utf-8

# In[418]:


import praw
import pandas as pd
import datetime as dt
from psaw import PushshiftAPI
import seaborn as sns
import matplotlib.pyplot as plt


# In[419]:


reddit = praw.Reddit(client_id='XkgWRt6rMkwKjQ',                      client_secret='503fhvni4TqnPXYH6iQU-z9AgJw',                      user_agent='Abhi_Portfolio',                      username='012340',                      password='Singerr*889900')


# In[420]:


start_epoch=int(dt.datetime(2019, 1, 26,0,0,0).timestamp())
end_epoch=int(dt.datetime(2019, 1, 26,23,59,59).timestamp())
start_epoch
end_epoch


# In[257]:


subreddit = reddit.subreddit('Askreddit')
api = PushshiftAPI(reddit
                  )


# In[424]:


#We want to find the subreddit with the highest submissions per hour
submissions=api.search_submissions(after=start_epoch, before=end_epoch,subreddit = ['askreddit','politics','worldnews','funny','pics'])

submissions


# In[425]:


topics_dict = { "title":[], 
                "score":[], 
             "sub_id":[],
               "gold":[],
                "comms_num": [], 
                "created": []
                }
count = 0;


# In[426]:


for submission in submissions:
    topics_dict["title"].append(submission.title)
    topics_dict["score"].append(submission.score)
    topics_dict["sub_id"].append(submission.subreddit)
    topics_dict["gold"].append(submission.gilded)
    topics_dict["comms_num"].append(submission.num_comments)
    topics_dict["created"].append(submission.created)
    count = count+1
    
count


# In[427]:


topics_data = pd.DataFrame(topics_dict)
topics_data


# In[428]:


def get_date(created):
    return dt.datetime.fromtimestamp(created)
temp = topics_data["created"].apply(get_date)
topics_data = topics_data.assign(timestamp = temp)


# In[298]:


path = "C:\\Users\\Abhishek\\Desktop\\PlaySimple\\aa.csv"
topics_data.to_csv(path,index= "utf-8")


# In[429]:


day = topics_data['timestamp'].dt.day
hour = topics_data['timestamp'].dt.hour
topics_data = topics_data.assign(day = day)
topics_data = topics_data.assign(hour = hour)
topics_data


# In[431]:


print(topics_data.dtypes)
sub_id_new = topics_data['sub_id'].astype('category')
topics_data = topics_data.assign(sub_id_new = sub_id_new)


# In[433]:


top = pd.DataFrame(topics_data.groupby(['day','hour','sub_id_new'],as_index=False).size().reset_index(name ='Total'))
top = pd.DataFrame(top)
top['Time'] = top['day'].astype(str) + "_" + top['hour'].astype(str)
top.dtypes
top


# In[434]:


plot_data = top.iloc[:,[4,3,2]]

#plot_data.plot(x='Time', y = 'Total',columns = 'sub_id_new')


ax = sns.lineplot(data=plot_data, x='Time', y = 'Total',hue = 'sub_id_new')
plt.xticks(plt.xticks()[0],  rotation=90)
plt.tight_layout()
plt.show()


# In[ ]:


#We see clearly that Ask Reddit has the highest submissions per hour

