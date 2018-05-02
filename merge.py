from os import listdir
from os.path import isfile, join
import json
import pandas as pd


# Please make sure this is done before the script is run:
'''
# create 'plots' folder
#!/bash
mkdir plots
# mv cn_medians files
#!/bash
cp batch_x6_chimp/cn_medians/*csv plots
cp batch_x6.3_gorilla/cn_medians/*csv plots
cp batch_y6.2_chimp/cn_medians/*csv plots
'''






def build_meta_dict():
	content = {}
	# Here we open the sample_info.txt file containing all the individual across species and sexes.
	# We then build a dictionary, that we can later use to fill a complete dataframe with all results.
	with open('sample_info_all.txt', 'r') as file:
		lines = [line.strip().split('\t') for line in file][1:]
		for line in lines:

			name = line[0]
			spec = line[1]
			sex = line[2]

			content[name] = {'species': spec, 'sex': sex}
	return content


meta_data = build_meta_dict()

wd = 'plots/'

frame = pd.DataFrame()
for file in [f for f in listdir(wd) if isfile(join(wd, f))]: # for each file in the dir
	if file[0:2] != 'x_' and file[0:2] != 'y_': continue # skip files that don't start with either 'x_' or 'y_' pass this iteration
	chrom = file[0:1].upper()

	print(file)

	data = pd.read_csv(wd+file, sep='\t', engine='python').rename(columns={'name': 'gene'}) # read
				
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



frame.reset_index(drop = True).to_csv('plots/full.csv')
