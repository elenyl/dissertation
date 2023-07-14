#This is the web scaping code used for extracing data for heartworm disease analysis. 
#Business Analytics Dissertation
#July 14, 2023
#Author: Yanliang Li, Khushboo Parikh, Sarthak Sharma

import json
from time import sleep
import mysql.connector
import requests


def get_country(year, month):
    headers = {
        'authority': 'maps.capcvet.org',
        'accept': '*/*',
        'accept-language': 'zh-CN,zh;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6',
        'api-token': '6aFlptxEFObverJNCX+7xO0VNg/2IJSQavGr/oWZvZI=',
        'content-type': 'application/json',
        'origin': 'https://capcvet.org',
        'referer': 'https://capcvet.org/maps/',
        'sec-ch-ua': '"Not.A/Brand";v="8", "Chromium";v="114", "Microsoft Edge";v="114"',
        'sec-ch-ua-mobile': '?0',
        'sec-ch-ua-platform': '"Windows"',
        'sec-fetch-dest': 'empty',
        'sec-fetch-mode': 'cors',
        'sec-fetch-site': 'same-site',
        'user-agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko)'
                      ' Chrome/114.0.0.0 Safari/537.36 Edg/114.0.1823.43',
    }
    data = '{"diseaseId":8,"speciesId":1,"countryId":1,"regionId":null,"year":' + str(year) + \
           ',"month":' + str(month) + '}'
    response = requests.post('https://maps.capcvet.org/v3/map/data', headers=headers, data=data)
    response_data = json.loads(response.text)
    for subregion_data in response_data["regions"]:
        subregion_dict = {
            "name": subregion_data["name"],
            "regionId": subregion_data["regionId"],
        }
        name = subregion_dict.get('name')
        region_id = subregion_dict.get('regionId')
        get_province(name, region_id, year, month)
        sleep(5)


def get_province(name, region_id, year, month):
    headers = {
        'authority': 'maps.capcvet.org',
        'accept': '*/*',
        'accept-language': 'zh-CN,zh;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6',
        'api-token': '6aFlptxEFObverJNCX+7xO0VNg/2IJSQavGr/oWZvZI=',
        'content-type': 'application/json',
        'origin': 'https://capcvet.org',
        'referer': 'https://capcvet.org/maps/',
        'sec-ch-ua': '"Not.A/Brand";v="8", "Chromium";v="114", "Microsoft Edge";v="114"',
        'sec-ch-ua-mobile': '?0',
        'sec-ch-ua-platform': '"Windows"',
        'sec-fetch-dest': 'empty',
        'sec-fetch-mode': 'cors',
        'sec-fetch-site': 'same-site',
        'user-agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) '
                      'Chrome/114.0.0.0 Safari/537.36 Edg/114.0.1823.43',
    }
    data = '{"diseaseId":8,"speciesId":1,"countryId":1,"regionId":'+str(region_id)+',"year":' + str(year)\
           + ',"month":' + str(month) + '}'
    response = requests.post('https://maps.capcvet.org/v3/map/data', headers=headers, data=data)
    response_data = json.loads(response.text)
    try:
        for subregion_data in response_data["subregions"]:
            subregion_dict = {
                "countryId": subregion_data["countryId"],
                "regionId": subregion_data["regionId"],
                "subregionId": subregion_data["subregionId"],
                "name": subregion_data["name"],
                "totalPositive": subregion_data["totalPositive"],
                "totalTested": subregion_data["totalTested"]
            }
            print(subregion_dict)
            sql = "INSERT INTO capcvet (continent_name, county_name, year, month, total_positive, total_tested)" \
                  " VALUES (%s, %s, %s, %s, %s, %s)"
            values = (name, subregion_dict['name'], year, month,
                      subregion_dict['totalPositive'], subregion_dict['totalTested'])
            cursor.execute(sql, values)
            conn.commit()
    except KeyError:
        pass


if __name__ == '__main__':
    conn = mysql.connector.connect(
        host='127.0.0.1',
        user='root',
        password='',
        database='capcvet'
    )
    cursor = conn.cursor()
    for year in range(2013, 2023):
        for month in range(1, 13):
            get_country(2022, month)
    cursor.close()
    conn.close()
