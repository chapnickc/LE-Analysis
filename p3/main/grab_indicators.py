
# coding: utf-8

# In[1]:





import datetime                                             
import wbdata
import pandas as pd
                                                            
# get maternal deaths in 1990                               
indicators = {                                                            
        'SP.DYN.LE00.MA.IN' : 'LifeExp_Male',
        'SH.HIV.1524.FE.ZS': 'Percent_HIV-AIDS_Female',     
        'SP.DYN.CDRT.IN': 'Deaths_per_100k_Population',     
        'SP.DYN.IMRT.IN': "Infant_mortality_rate",          
        'SP.DYN.TFRT.IN': "Total_fertility_rate",           
        'SP.POP.65UP.TO.ZS': "Percent_of_pop_over_65", 
}                                                           
                                                            
                                                            
start = datetime.datetime(2014, 1, 1)                       
stop = datetime.datetime(2014, 12, 31)                      
                                                            
                                                            
df1 = wbdata.get_dataframe(indicators = indicators,          
        data_date = (start, stop))                          
                                                            
                                                            
#df1.head()




# In[2]:



maternal_deaths = {'SH.STA.MMRT': "Maternal_Deaths_1990"}    
start = datetime.datetime(1990, 1, 1)                        
stop = datetime.datetime(1990, 12, 31)                       

md_1990 = wbdata.get_dataframe(indicators = maternal_deaths, 
        data_date = (start, stop))          

#md_1990.head()



# In[3]:



# need to use same column name so regression model works in R
inet_usrs = {'IT.NET.USER.P2': "INET_USRS_2014"}           
            
start = datetime.datetime(2013, 1, 1)                     
stop = datetime.datetime(2013, 12, 31)                     
            
inet_usrs_13 = wbdata.get_dataframe(indicators = inet_usrs, 
        data_date = (start, stop))

#inet_usrs_13.head()






# In[5]:


frames = [df1, md_1990, inet_usrs_13]
df = pd.concat(frames, axis = 1) 
#df.head()


# In[6]:

len(df.index)


# In[7]:

df.to_csv('newdata.csv')

