import googlemaps
import math
import csv

radius = 50000 #in meters, 50000m = 50km
keyword = None #we're looking for retails near the specified location
csv_file_name = "remote_locations.csv"

gmaps = googlemaps.Client(key='AIzaSyD8o33UnagllRFbZFN3rBLn-rKD1tmIoos')


def haversine(lon1, lat1, lon2, lat2):
    """
    Calculate the great circle distance between two points 
    on the earth (specified in decimal degrees).
    Source: https://gis.stackexchange.com/a/56589/15183
    """
    # convert decimal degrees to radians 
    lon1, lat1, lon2, lat2 = map(math.radians, [lon1, lat1, lon2, lat2])
    # haversine formula 
    dlon = lon2 - lon1 
    dlat = lat2 - lat1 
    a = math.sin(dlat/2)**2 + math.cos(lat1) * math.cos(lat2) * math.sin(dlon/2)**2
    c = 2 * math.asin(math.sqrt(a)) 
    km = 6367 * c
    return km*1000



def get_place(res,orlon,orlat):
	#returns a list with name, lat, lng, distance in meters
	dist = gmaps.distance_matrix(origins=(res["geometry"]["location"]["lat"],res["geometry"]["location"]["lng"]),
			destinations=(orlat,orlon),units="metric")["rows"][0]["elements"][0]
	if not "distance" in dist:
		distance = "NA"
	else:
		distance = dist["distance"]["value"]
	result = [
		res["name"],
		res["geometry"]["location"]["lat"],
		res["geometry"]["location"]["lng"],
		haversine(res["geometry"]["location"]["lng"],res["geometry"]["location"]["lat"],orlon,orlat),
		distance,
		res["types"][0],
		]
	
	if "user_ratings_total" in res:
		result.append(res["user_ratings_total"])
	else:
		result.append("NA")
	if "rating" in res:
		result.append(res["rating"])
	else:
		result.append("NA")
	if "vicinity" in res:
		result.append(res["vicinity"])
	else:
		result.append("NA")
	if "business_status" in res:
		result.append(res["business_status"])
	else:
		result.append("NA")

	return result

def handle_csv(filename):
	final = []
	with open(filename) as csv_file:
		csv_reader = csv.reader(csv_file, delimiter=',')
		line_count = 0
		for row in csv_reader:
			line_count += 1
			if line_count>1:
				orlat = float(row[2].split(",")[0])
				orlon = float(row[2].split(",")[1])
				nearby = gmaps.places_nearby(radius=radius,keyword=keyword,location=(orlat,orlon))
				results = nearby["results"]
				print("results for id= "+row[0]+" band_name= "+row[1])
				for result in results:
					rr = get_place(result,orlon,orlat)
					print(rr)
					final.append([row[0],row[1],row[2],row[3]]+rr)
				print("****----- END OF RESULTS FOR THIS ONE -----****")
	with open("result_for_"+filename, mode='w') as employee_file:
	    employee_writer = csv.writer(employee_file, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
	    employee_writer.writerow(['row_id', 'band_name','band_coords','csdname', 'name',"lat",
	    	"lng","geo_distance","travel_distance","type","user_ratings_total","rating","vicinity","business_status"])
	    for res in final:
	    	employee_writer.writerow(res)

	print("read "+str(line_count)+" lines from the csv file")
	print("found "+str(len(final))+" places :D")
#

handle_csv(csv_file_name)