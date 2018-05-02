import sys
import pandas as pd

rel_input = sys.argv[1]
rel_output = sys.argv[2]

# data = pd.read_csv(rel_input, sep='\s', header=None, engine='python')	# read data
# data = data.rename(columns={0: 'name', 1: 'pos', 2: 'count'})	# Rename columns.

# by_gene = data.groupby('name')	# Group by name.
# by_gene = by_gene.median()	# Take the median.

# by_gene.to_csv(rel_output, sep='\t')	# Write to disk.


pd.read_csv(rel_input, sep='\s', header=None, engine='python')\
	.rename(columns={0: 'name', 1: 'pos', 2: 'count'})\
	.groupby('name')\
	.median()\
	.to_csv(rel_output, sep='\t')