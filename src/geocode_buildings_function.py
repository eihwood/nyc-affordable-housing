# geocode addresses to get position data
from geopy.geocoders import GoogleV3
import geopy.distance
import googlemaps
import pandas as pd


# create geocode function
def geocode_func(data, fileout, address_col_name = "Address", API = 'AIzaSyDZE9SLY3HFxFjb4TXy67ZpEwBAvtskQIY'):
    # generate geolocator object
    geolocator = GoogleV3(api_key = API)
    
    # Apply the geolocator object to the address column
    data["loc"] = data[address_col_name].apply(geolocator.geocode)
    
    # Obtain the points containing latitude and longitude
    data["point"]= data["loc"].apply(lambda loc: tuple(loc.point) if loc else None)
    
    # Split the points into separate lat / lon columns 
    b = data['point'].dropna()
    data[['lat', 'lon', 'altitude']] = pd.DataFrame(b.to_list(), index=b.index)
    
    # write output to csv
    data.to_csv(fileout, sep='\t', encoding='utf-8')
    return 
