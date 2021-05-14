
import csv
from time import time

import requests
from requests.adapters import HTTPAdapter
from urllib3 import Retry


CATEGORIES = {
    2: 'red',
    4: 'green',
}


def create_session() -> requests.Session:
    session = requests.Session()
    retry = Retry(
        status=3,
        backoff_factor=0.3,
        status_forcelist=(502,),
    )
    adapter = HTTPAdapter(max_retries=retry)
    session.mount('http://', adapter)
    session.mount('https://', adapter)
    return session


def fetch_entries():
    session = create_session()
    response = session.get(
        'https://www.sac-isc.gc.ca/DAM/DAM-ISC-SAC/DAM-WTR/STAGING/texte-text/lTDWA_map_data_1572010201618_eng.txt',
        params={
            'date': round(time()),
        },
        headers={
            'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/14.1 Safari/605.1.15',
            'Referer': 'https://www.sac-isc.gc.ca/eng/1506514143353/1533317130660',
            'X-Requested-With': 'XMLHttpRequest',
        },
    )
    entries = response.json()['data']
    for entry in entries:
        yield entry


def main():
    with open('misc_script\\water_access\\result.csv', 'w') as f:
        fieldnames = [
            'category',
            'region',
            'first nations',
            'water system name',
            'date advisory set',
            'date advisory lifted',
            'project phase',
            'lattitude',
            'longitude',
            'populatio estimated',
            'corrective measure',
            'recommended epho',
        ]
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        for entry in fetch_entries():
            writer.writerow({
                'category':
                    CATEGORIES.get(entry['CategoryID'], entry['CategoryID']),
                'region': entry['Region'],
                'first nations': entry['CommunityName'],
                'water system name': entry['WaterSystemName'],
                'date advisory set': entry['DateSet'],
                'date advisory lifted': entry['DateExpected'],
                'project phase': entry['LongPhase'],
                'lattitude': entry['LattitudeSTR'],
                'longitude': entry['LongitudeSTR'],
                'populatio estimated': entry['PopulatioEstimated'],
                'corrective measure': entry['CorrectiveMeasure'].strip(),
                'recommended epho': entry['RecommendedEPHO'],
            })


if __name__ == '__main__':
    main()
