import requests,csv,traceback,bs4,json
from threading import Thread
from queue import Queue

""" 
    I have found this endpoint to get all ATMs: "https://maps.rbcroyalbank.com/locator/getAllLocationsXML.php"
    It takes 4 parameters, these are the coordinates for the borders(s,w,n,e).
    I chose these numbers(0,-99999,99999,0) to cover the whole map.
    And it returns an XML file containing atm information. (lat,long, id of the atm and its type).

    To get address information I have used this Url: "https://maps.rbcroyalbank.com/locator/locationDetails.php?l="
    The "l" parameter is the id of the atm.
    
"""

res = requests.get(f'https://maps.rbcroyalbank.com/locator/getAllLocationsXML.php?s={0}&w={-99999}&n={99999}&e={0}&b=1&a=1',headers = {"user-agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/85.0.4183.102 Safari/537.36"})
atms = bs4.BeautifulSoup(res.text,"lxml").select('marker') 


def remove_blank_lines(string):
    """Takes a string including multiple lines, removes extra spaces and empty lines."""
    lines = string.split("\n")
    non_empty_lines = [line.strip() for line in lines if line.strip() != ""]
    string_without_empty_lines = ""
    for line in non_empty_lines:
        string_without_empty_lines += line + "\n"
    return string_without_empty_lines

def run(q): 
    """Takes a queue containing the atm data parsed from the XML file and scrapes the Address."""
    while True:
        counter,data = q.get()
        try:
            res = requests.get(f'https://maps.rbcroyalbank.com/locator/locationDetails.php?l={data["l"]}' ,headers = { "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/85.0.4183.83 Safari/537.36"})
            soup = bs4.BeautifulSoup(res.text,features="lxml")
            dic = {}
            dic["Url"] = f'https://maps.rbcroyalbank.com/locator/locationDetails.php?l={data["l"]}'
            dic["Lat"]  = data["lat"]
            dic["Lng"]  = data["lng"]
            dic["Type"]  = data["t"]
            if dic["Type"] == "branchatm" or dic["Type"] == "branch":
                if soup.select_one('#tab1 > table>  tr:first-of-type > td:first-of-type a'):
                    soup.select_one('#tab1 > table>  tr:first-of-type > td:first-of-type a').decompose()
                dic["Address"] =  soup.select_one('#tab1 > table>  tr:first-of-type > td:first-of-type').text
            else:
                dic["Address"]  = soup.select_one('.sidetabs-container-content td[colspan="2"]').text
            
            dic["Phone"] = [x.next_sibling.strip() for x in soup.select('span') if "Phone:" in x.text][0] if len([x for x in soup.select('span') if "Phone:" in x.text]) else ""
            dic["Number of ATMs"] = [x.next_sibling.replace(':','').strip() for x in soup.select('span') if "Number of ATMs" in x.text][0] if len([x for x in soup.select('span') if "Number of ATMs" in x.text]) else ""
            dic["transit #"] = [x.next_sibling.strip() for x in soup.select('span') if "transit #:" in x.text][0] if len([x for x in soup.select('span') if "transit #:" in x.text]) else ""
            dic["Additional Features"] = [x.next_sibling.strip() for x in soup.select('span') if "Additional Features" in x.text][0] if len([x for x in soup.select('span') if "Additional Features" in x.text]) else ""
            dic["Languages Spoken"] = [x.next_sibling.strip() for x in soup.select('span') if "Languages Spoken" in x.text][0] if len([x for x in soup.select('span') if "Languages Spoken" in x.text]) else ""
            dic["Additional Services"] = [x.next_sibling.strip() for x in soup.select('span') if "Additional Services" in x.text][0] if len([x for x in soup.select('span') if "Additional Services" in x.text]) else ""
            dic["Address"] = dic["Address"].replace(f'Number of ATMs: {dic["Number of ATMs"]}','')
            dic["Address"] = dic["Address"].replace(f'Phone: {dic["Phone"]}','')
            dic["Address"] = dic["Address"].replace(f'transit #: {dic["transit #"]}','')
            dic["Address"] = dic["Address"].replace(f'Additional Features: {dic["Additional Features"]}','')
            dic["Address"] = dic["Address"].replace(f'Languages Spoken: {dic["Languages Spoken"]}','')
            dic["Address"] = dic["Address"].replace(f'Additional Services: {dic["Additional Services"]}','')
            dic["Address"] = remove_blank_lines(dic["Address"] )

            lst.append(dic)
            print(counter," of ",len(atms))
            q.task_done()
        except Exception:
            print(traceback.format_exc())
            errors.append({"url":f'https://maps.rbcroyalbank.com/locator/locationDetails.php?l={data["l"]}',"error":traceback.format_exc()})
            json.dump(errors,open(f"errors.json","w",encoding="utf-8"),indent=4,ensure_ascii=False)
            q.task_done()

lst = []
errors = []
q = Queue(maxsize=0)
for _ in range(15):
    worker = Thread(target=run, args=(q,))
    worker.setDaemon(True)
    worker.start()
counter = 1
for counter,url in enumerate(atms):
    q.put([counter,url])
q.join()

with open("rbc.csv","w",newline="",encoding="utf-8") as f:
    writer = csv.DictWriter(f,lst[0].keys(),quoting=csv.QUOTE_ALL)
    writer.writeheader()
    writer.writerows(lst)