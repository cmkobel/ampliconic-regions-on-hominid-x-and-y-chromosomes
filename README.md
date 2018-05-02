### Bachelorproject:
# Ampliconic Regions on Hominid X and Y chromosomes

All resources are collected at one spot here on GitHub.

[Read the bachelorproject](https://github.com/cmkobel/ampliconic-regions-on-hominid-x-and-y-chromosomes/wiki/Bachelorproject:-Ampliconic-Regions-on-Hominid-X-and-Y-chromosomes)
or
[see the code](https://github.com/cmkobel/ampliconic-regions-on-hominid-x-and-y-chromosomes)

* * *

The files `workflow.py` `workflow_templates.py` depends on [gwf-org](http://gwf.readthedocs.io/en/latest/index.html) to run. The files depict the pipeline where genomes are mapped to several artificial chromosomes. The copy number variation is output, merged with `merge.py` and the data is subsequently visualized with `plots.R`

The plots are output to `plots/plots`.

The difference between `plots` and `plots_human_ac` is that in the first; a species specific artificial chromosome is used as reference for mapping, and in the latter an artifical chromosome based on human genes is used.


