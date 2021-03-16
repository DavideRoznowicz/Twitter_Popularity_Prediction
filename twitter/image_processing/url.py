#usage : create url.csv with R < output redirect in list.txt
import csv
import sys
with open('url.csv') as csv_file:
	csv_reader = csv.reader(csv_file, delimiter=',')
	line_count=0
	for row in csv_reader:
		line_count=line_count+1
		if line_count==2:
			for i in row:
				print(i)				
