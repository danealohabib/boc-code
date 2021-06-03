import requests,csv

res = requests.get(f'https://www.tdbank.com/net/get12.ashx?longitude=-99.071562&latitude=52.93870328094501&country=CA&locationtypes=3&json=y&searchradius=99999999&searchunit=km&numresults=999999999',headers = {"user-agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/85.0.4183.102 Safari/537.36"})
data = res.json()["markers"]["marker"]

branch_types = {
    "1":"ATM",
    "2":"BRANCH",
    "3":"BRANCH + ATM",
}

lst = []
for d in data:
    dic = {}
    dic["lat"]  = d["lat"]
    dic["lng"]  = d["lng"]
    dic["address"] = d["address"]
    dic["type"] = branch_types[d["type"]]
    dic["name"]  = d["name"]
    dic["phone"] = d["phoneNo"]
    dic["transit"] = d["transit"]
    lst.append(dic)

with open("td.csv","w",newline="",encoding="utf-8") as f:
    writer = csv.DictWriter(f,lst[0].keys())
    writer.writeheader()
    writer.writerows(lst)