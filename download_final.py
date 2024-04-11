
import os
import re
import json
import time
from datetime import date
from time import sleep # helps us avoid getting blocked by Zillow
import requests
from bs4 import BeautifulSoup # allows us to parse data from html
import xlrd
import pandas as pd
import numpy as np
import sys
import random

headers = {
        "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
        "Accept-Encoding": "gzip, deflate, br",
        "Accept-Language": "en-US,en;q=0.9",
        "Cache-Control": "max-age=0",
        "Cookie": "JSESSIONID=08690E76199A137F07F2128DCB3077D4; zguid=24|%24998fbb49-aa98-42a8-9389-bf3c370c1383; zgsession=1|af9a5b30-3e34-46bf-8c8c-93b3fab28f9f; _ga=GA1.2.2078591946.1695264995; _gid=GA1.2.1230604704.1695264995; zjs_anonymous_id=%22998fbb49-aa98-42a8-9389-bf3c370c1383%22; zjs_user_id=null; zg_anonymous_id=%226a142bb6-1c4e-479a-b816-da2d6a2cd924%22; pxcts=7a6a86ea-582a-11ee-9b02-8d0819f1df18; _pxvid=7a6a7278-582a-11ee-9b02-61d3422eec97; _px3=7cadb499aae453804c2dbece02bc6d0cc85f4f4b7e937347d058c36e3c58b032:hOKzsmnHtOuW2jDeM6q/KzdKk73NWlWVNuDtVZMQ2YEMshVmhdXEG9qx4fmyiiuS8U0nsJzjXOtdCVMCjD0LZw==:1000:QnpVNYEjCvaRAPQFzIGb3ZUAlLEb2LXMJQeI1gMAkGXHXF9RIsPD06IBAy6j6uJdpQ+CaNzFcastfHmCBrsjPUCJxKLAJyRwgr7WtbWyvSiAcwgmB4GZfSEZYhNz8K/3MudvcCeAwvPpO1Czmfx89G5BIEqttg+/b3BjrcjEwP5w0IshqywNi+m+A3SweUosqQaE4VZPyPUOChsQyqhck2ojr3gHzgBiboUlcmO0uXw=; __gads=ID=d1abe9fa0c493441:T=1695265011:RT=1695265011:S=ALNI_MZjg7dtrfDq4L7Giae0F1scYZY3RA; __gpi=UID=00000d943b785032:T=1695265011:RT=1695265011:S=ALNI_MYfn40HbxyNw7pMX5GQE1RJ37-69g; _gcl_au=1.1.1147718396.1695265012; DoubleClickSession=true; __pdst=881da88fa38547aeb1c2f778f68f6c0e; _uetsid=84220b50582a11eebd7e2b17a729a363; _uetvid=84224c90582a11ee9c0951ba0157e0ce; tfpsi=768b447f-0fc2-4e10-a8be-91153f60438f; _fbp=fb.1.1695265012668.881705972; _pin_unauth=dWlkPVpEWTVZVGc0TnpndE9ERmpOUzAwWkRobUxUbGpaVEF0WkdKbFl6UmpaRFU1WVRneA; _clck=1701wxj|2|ff7|0|1359; AWSALB=sxquA6G+WxOihl8UKtrz0dZ0SlZJPaUYG/cucm6WpiLV6bPcSCSVBKyL8fjlCf7kOcvfWg+6Kaznxnblj7t3FyWfQrOI6oroqIm6VhMzdUFcs3wEuUOK9VmQ9PzX; AWSALBCORS=sxquA6G+WxOihl8UKtrz0dZ0SlZJPaUYG/cucm6WpiLV6bPcSCSVBKyL8fjlCf7kOcvfWg+6Kaznxnblj7t3FyWfQrOI6oroqIm6VhMzdUFcs3wEuUOK9VmQ9PzX; search=6|1697857014786%7Crect%3D42.485831799393445%252C-70.8107322998047%252C42.14055779383095%252C-71.28451770019532%26rid%3D44269%26disp%3Dmap%26mdm%3Dauto%26p%3D1%26z%3D1%26listPriceActive%3D1%26fs%3D1%26fr%3D0%26mmm%3D0%26rs%3D0%26ah%3D0%26singlestory%3D0%26housing-connector%3D0%26abo%3D0%26garage%3D0%26pool%3D0%26ac%3D0%26waterfront%3D0%26finished%3D0%26unfinished%3D0%26cityview%3D0%26mountainview%3D0%26parkview%3D0%26waterview%3D0%26hoadata%3D1%26zillow-owned%3D0%263dhome%3D0%26featuredMultiFamilyBuilding%3D0%26commuteMode%3Ddriving%26commuteTimeOfDay%3Dnow%09%0944269%09%7B%22isList%22%3Atrue%2C%22isMap%22%3Afalse%7D%09%09%09%09%09; _clsk=1bkr4h|1695265015576|2|0|t.clarity.ms/collect; _gat=1",
        "Sec-Ch-Ua-Mobile": "?0",
        "Sec-Fetch-Dest": "document",
        "Sec-Fetch-Mode": "navigate",
        "Sec-Fetch-Site": "none",
        "Sec-Fetch-User": "?1",
        "Upgrade-Insecure-Requests": "1",
        "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/116.0.0.0 Safari/537.36",
    }

params = {
            "pagination": {},
            "usersSearchTerm": "Boston, MA",
            "mapBounds": {
                "west": -71.28451770019532,
                "east": -70.8107322998047,
                "south": 42.14055779383095,
                "north": 42.485831799393445,
            },
            "regionSelection": [{"regionId": 44269, "regionType": 6}],
            "isMapVisible": False,
            "filterState": {
                "sort": {"value": "globalrelevanceex"},
                "ah": {"value": True},
            },
            "isListVisible": True,
            "mapZoom": 11,
        }

#################################### DOWNLOAD HTMLS ###########################################

def download_html(folder, url, page, location = None):
    """
    Given a url, downloads page as html file
    Parameters:
        folder: name of folder that html page will be downloaded in
        url: string representing a link
        page: number of page currently on
        location: area zillow house is in
    Sample use of code --> download_all_pages("wellesley",
    "https://www.zillow.com/wellesley-ma/sold/", 1, wellesley-ma)
    """
    # if folder doesn't exist, creates it
    os.makedirs(folder, exist_ok=True)
    # get date for naming purposes
    time = str(date.today())
    if location is not None:
        path = os.path.join(folder, f"{location}-{time}-{page}.html")
    else:
        path = os.path.join(folder, f"house-{page}-{time}.html")
    # request data
    htmlPage = requests.get(url, headers=headers, params=params)
    response = htmlPage.status_code
    # write data into html file
    with open(path, 'w', encoding='utf-8') as output:
        output.write(htmlPage.text)
    return response

def download_all_pages(folder, url):
    """
    Downloads each Zillow lisitng page for a given location. Assumes url given belongs
    to the first page.
    Parameters:
        folder: name of folder that html page will be downloaded in
        url: string representing a link. URLs MUST be in a simplified form to
        work, i.e. https://www.zillow.com/boston-ma/sold/
    Sample use of code --> download_all_pages("wellesley",
    "https://www.zillow.com/wellesley-ma/sold/")
    """
    # if folder doesn't exist, creates it
    os.makedirs(folder, exist_ok=True)
    response = requests.get(url, headers=headers, params=params)
    content = BeautifulSoup(response.content, "html.parser")
    # get range of page numbers
    page_items = content.find("div", {"class":"search-pagination"})
    if page_items is not None:
        page = int(page_items.find("span", {"class": "Text-c11n-8-84-3__sc-aiai24-0 hrfydd"}).text.split()[-1])
    else:
        page = 1
    location = re.search(r"(?<=.com/)[^/]+(?=/)", url).group()

    print(f"Downloading houses in the {location} area")
    for i in range(page):
        # wait so we don't get blocked
        time.sleep(5)
        # get current page
        cur_url = url + f"{i+1}_p/"
        status = download_html(folder, cur_url, i+1, location)
        # print status response and page currently on
        print(f"Currently on page: {i+1}, status: {status}")
        if status != 200:
            break
    return status

def get_all_listings(folder, f_name):
    """
    Given a txt file with a Zillow search page link per row, downloads
    all the individual house htmls in every listing
    Parameters:
        folder: name of folder that html page will be downloaded in
        f_name: name of txt file containing links
    Sample use of code --> get_all_listings("listings","sample_links.txt")
    """
    # if folder doesn't exist, creates it
    os.makedirs(folder, exist_ok=True)
    with open(f_name, "r") as file:
        for url in file:
            time.sleep(5)
            url = url.rstrip()
            status = download_all_pages(folder, url)
            if status != 200:
                status_message(status)
                break

def get_indiv(folder, f_name, start = 1):
    """
    Downloads individual house html files
    Parameters:
        folder: name of folder that html page will be downloaded in
        f_name: name of txt file containing links
        start: number of house we want to start reading in data
        from. For example given a txt file with ten links, if we want
        to download htmls of second house and upward, start = 2
    Sample use of code --> get("newton_houses","links.txt")
    """
    count = start
    os.makedirs(folder, exist_ok=True)
    with open(f_name, "r") as file:
        urls = file.readlines()[start-1:]
        for url in urls:
            #time.sleep(2)
            url = url.rstrip()
            zpid = url.split("/")[-2] ###
            status = download_html(folder, url, count, zpid) ###
            status_message(status)
            if status != 200:
                if status == 403:
                    time.sleep(3600*7)
                    status = download_html(folder, url, count, zpid)
                elif status == 404:
                    print(f"Couldn't find: {url}")
                    continue
                else:
                    print("Some other error, moving on")
                    continue
            count += 1

            sleepTime = random.randint(5, 80)
            time.sleep(sleepTime)

def status_message(status):
    """
    Helper function prints out message for user concerning
    status of their request.
    Parameters:
        status: 3 digit integer representing html status code
    Sample use of code --> status_message(200)
    """
    if status == 403:
        print(f"Oh no! HTTP status: {status} ---> Invalid request")
        print("Don't forget to delete last file!")
    elif status == 404:
        print(f"Oh no! HTTP status: {status} ---> Page not found")
        print("Don't forget to delete last file!")
    elif status == 200:
        print("HTML page downloaded successfully!")
    else:
        print(f"Oh no! HTTP status: {status}")
        print("Don't forget to delete last file!")

#################################### DOWNLOAD HTMLS ##################################

############################## GET INDIVIDUAL HOUSE LINKS ############################

def getScriptElement(filename):
    """
    Read file and get us the script element that has the data.
    Its id value is "__NEXT_DATA__"
    Parameters:
        filename: name of file
    """
    with open(filename) as input_file:
        fileContent = input_file.read()
        soup = BeautifulSoup(fileContent, 'html.parser')

    try:
        script = soup.find(id="__NEXT_DATA__")
        return script.string # this gets the content of the <script> tag
    except:
        print("Couldn't find the <script> element with your ID.")
        return ""

def getURLs(data_string, filename):
    """
    Gets a big string that contains the structured data,
    tries to extract the list of URLs that is nested in it.
    Parameters:
        data_string: json string containing data
        filename: name of file we are getting data from
    """
    all_data_dict = json.loads(data_string)
    url_list = []

    try:
        searh_page_data = all_data_dict['props']['pageProps']['searchPageState']
        list_results = searh_page_data['cat1']['searchResults']['listResults']
        url_list = [item['detailUrl'] for item in list_results]
    except:
        print(f"Having trouble getting the data from file '{filename}'")

    return url_list

def combine_urls(path):
    """
    Creates a list of urls of individual houses from search pages
    in a folder
    Params:
        path: pathname of folder containing html files
    Sample use of code --> combine_URLS("listings")
    """
    all_urls = []
    # keep only the HTML files
    files_to_parse = sorted([f for f in os.listdir(path) if f.endswith('.html')])
    for fname in files_to_parse:
        file_path = os.path.join(path, fname)
        data_string = getScriptElement(file_path)
        url_list = getURLs(data_string, fname)
        all_urls.extend(url_list)
    return all_urls

def write_urls_txt(fname, url_list):
    """
    Writes urls from url_list into a txt file
    Params:
        fname: name of txt file to write urls in
        url_list: list of urls
    Sample use of code --> write_urls_txt("links.txt", links)
    """
    with open(fname, 'w') as out_f:
        out_f.write("\n".join(url_list))
############################## GET INDIVIDUAL HOUSE LINKS ############################

############################## GENERATE LINKS FROM CSV ############################

def gen_zillow_links(fname, sheet):
    """
    Generates zillow search page links from a xlsx file containing
    zip codes.
    Params:
        fname: name of xlsx file
        sheet: name of sheet which data is on
    """
    df = pd.read_excel(fname, header = None, sheet_name = sheet)
    zips = df[0].tolist()
    with open("visit_links.txt", "w") as file:
        for zip in zips:
            file.write(f"https://www.zillow.com/ma-0{zip}/sold/"+ "\n")
        

def main():
    """
    Main Function
    """
    gen_zillow_links("zip_ma.xlsx", "all-other-counties")
    get_all_listings("other_listings","visit_links.txt") # creates houses folder and downloads all html
    #links = combine_urls("listings") # generates list of individual house urls
    #write_urls_txt("elinks.txt", links) # writes indiviual house links into txt
    #print(sys.argv)
    #run, folder, files, index = sys.argv
    #get_indiv("middle_houses","middle_links.txt", index) # use this to download individual zillow houses as html
    #3256
    #get_indiv(folder,files, start = int(index))
  

main()

