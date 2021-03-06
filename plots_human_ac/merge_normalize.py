from os import listdir
from os.path import isfile, join
import json
import pandas as pd

# This file is to be run locally.
# As there are no heavy computations, and mostly plots.

# Why is it called 'normalize' ? it doesn't normalize anything..

# download all <chrom>_<ind>_cn_median.csv files and put them in two folders: x and y
# maybe I should use the one in the plots (monkeys) folder instead? This file is residing in the coverage root dir.



def build_meta_dict():
	content = {}
	with open('sample_info.txt', 'r') as file:
		lines = [line.strip().split('\t') for line in file][1:]
		for line in lines:

			name = line[0]
			spec = line[1]
			sex = line[2]

			content[name] = {'species': spec, 'sex': sex}
	return content

meta_data = build_meta_dict()


frame = pd.DataFrame()
for file in [f for f in listdir('.') if isfile(join('.', f))]:
	if file[0:2] != 'x_' and file[0:2] != 'y_': continue # pass this iteration
	chrom = file[0:1].upper()

	#print(file)

	data = pd.read_csv(file, sep='\t', engine='python').rename(columns={'name': 'gene'}) # read
				
	data['gene'] = data['gene'].str.replace(r'ampliconic_region', '').astype('str') # rename values

	name =  file.split('_')[1][0:]
	data['ind'] = name #attach name
	data['chrom'] = chrom # attach chromosome

	#attach metadata from sample_info.txt
	data['species'] = meta_data[name]['species']
	data['sex'] = meta_data[name]['sex'] #sex

	#print(data)
	
	frame = pd.concat([frame, data])
	#print(frame)


frame = frame[['ind', 'species', 'sex', 'chrom', 'gene', 'pos', 'count']]



print(frame.reset_index())

frame.reset_index(drop = True).to_csv('full_added_zeroes.csv')
