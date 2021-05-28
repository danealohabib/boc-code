import requests,csv,traceback,bs4,json
from threading import Thread
from queue import Queue

lst = []
errors = []

def remove_blank_lines(string):
    lines = string.split("\n")
    non_empty_lines = [line.strip() for line in lines if line.strip() != ""]
    string_without_empty_lines = ""
    for line in non_empty_lines:
        string_without_empty_lines += line + "\n"
    return string_without_empty_lines

def run(q): 
    while True:
        counter,link = q.get()
        try:
            res = requests.get(f'https://maps.rbcroyalbank.com/locator/locationDetails.php?l={link["l"]}' ,headers = { "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/85.0.4183.83 Safari/537.36"})
            soup = bs4.BeautifulSoup(res.text,features="lxml")
            dic = {}
            dic["Url"] = f'https://maps.rbcroyalbank.com/locator/locationDetails.php?l={link["l"]}'
            dic["Lat"]  = link["lat"]
            dic["Lng"]  = link["lng"]
            dic["Type"]  = link["t"]
            if dic["Type"] == "branchatm":
                address = soup.select_one('#tab1 > table>  tr:first-of-type > td:first-of-type')
                if soup.select_one('#tab1 > table>  tr:first-of-type > td:first-of-type a'):
                    soup.select_one('#tab1 > table>  tr:first-of-type > td:first-of-type a').decompose()
                dic["Address"] = address.text
                dic["Address"] = remove_blank_lines(dic["Address"] )
            else:
                address = soup.select_one('.sidetabs-container-content td[colspan="2"]') 
                dic["Address"]  = address.text
                dic["Address"] = remove_blank_lines(dic["Address"] )
            lst.append(dic)
            print(counter," of ",len(locs))
            q.task_done()
        except Exception:
            print(traceback.format_exc())
            errors.append({"url":f'https://maps.rbcroyalbank.com/locator/locationDetails.php?l={link["l"]}',"error":traceback.format_exc()})
            json.dump(errors,open(f"errors.json","w",encoding="utf-8"),indent=4,ensure_ascii=False)
            q.task_done()

res = requests.get(f'https://maps.rbcroyalbank.com/locator/getAllLocationsXML.php?s={0}&w={-99999}&n={99999}&e={0}&b=1&a=1',headers = {"user-agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/85.0.4183.102 Safari/537.36"})
locs = bs4.BeautifulSoup(res.text,"lxml").select('marker') 


q = Queue(maxsize=0)
for _ in range(15):
    worker = Thread(target=run, args=(q,))
    worker.setDaemon(True)
    worker.start()
counter = 1
for counter,url in enumerate(locs):
    q.put([counter,url])
q.join()

with open("rbcroyalbank.csv","w",newline="",encoding="utf-8") as f:
    writer = csv.DictWriter(f,lst[0].keys(),quoting=csv.QUOTE_ALL)
    writer.writeheader()
    writer.writerows(lst)