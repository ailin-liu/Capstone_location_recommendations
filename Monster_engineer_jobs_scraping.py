import requests
import dill
from bs4 import BeautifulSoup
from requests_futures.sessions import FuturesSession
from ediblepickle import checkpoint
from urllib.parse import quote
import os
from retrying import retry
import string
import pandas as pd
import time

cache_dir = 'cache'
url = "https://www.monster.com/jobs/search/"

def get_link_info(el):
    url = el.select('a')[0]['href']
    title = el.select('a')[0].text.strip()
    return url , title

def get_links(response):
    soup = BeautifulSoup(response.text, "lxml")
    links = soup.find_all('div', attrs={'class':'summary'})
    joblist = []
    
    for link in links:
        try:
            url,title = get_link_info(link)
            print(url)        
            jd = get_JobDescription(url)
            joblist.append([url,title,jd])
            time.sleep(0.5)
        except:
            print('something goes wrong in get_links loop')
            pass
    
    time.sleep(5)
    
    return joblist

def get_page_args(i,title):
    return {"url": url,
            "params": {"page": i, "q": title}}

@checkpoint(key=lambda args, kwargs: quote(args[0])+ '.p', work_dir=cache_dir)
def get_jobs(title,limit):
    LIMIT = limit
    link_list = []
    session = FuturesSession(max_workers=5)
    try:
        link_list.extend(job
             for future in [session.get(**get_page_args(i,title)) for i in range(LIMIT)]
             for job in get_links(future.result()) )
    except:
        print('something goes wrong in generate link_list')
        pass
    return link_list


if not os.path.exists(cache_dir):
    os.mkdir(cache_dir)

@retry
def get_page(url):
    result = requests.get(url)
    return result

@checkpoint(key=lambda args, kwargs: quote(args[0][33:100]).replace('/','_') + '.p', work_dir=cache_dir)
def get_JobDescription(url):
    page = get_page(url)
    soup = BeautifulSoup(page.text, "lxml")    
    #job_description = soup.find_all('div', attrs={'id':'JobDescription'})
    job_description = soup.find('div', attrs={'id':'JobDescription'})
    if len(job_description) > 0:
        #return job_description[0].text
        return '\n'.join([ string for string in job_description.stripped_strings])
    else:
        return 'MISSING'

if __name__ == "__main__":
#  'Chemical':50, 'Civil':50, 'Aerospace':30, 'Biomedical':50,'Computer-Hardware':50, 'Electrical':50,'Electronics':50, 'Environmental':50,'Industrial':50, 'Health-and-Safety':50, 'Material':50
    title_list={'Chemical':50, 'Civil':50, 'Aerospace':30, 'Biomedical':50,'Computer-Hardware':50, 'Electrical':50,'Electronics':50, 'Environmental':50,'Industrial':50, 'Health-and-Safety':50, 'Material':50}
    
    for title in title_list:
        try:
            etitle = title +'-Engineer'
            print(etitle, '\n\n')
            filename = title + '_jobs.csv'
            jobs = get_jobs(etitle, title_list[title])
            df = pd.DataFrame(jobs, columns = ["url", "Job_title","Job_Description"])
            df.to_csv(filename, index = False)
            time.sleep(20)
        except:
            print(etitle, ' not sucessful, check it again!')
            pass
    
    