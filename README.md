### Bachelorproject:
# Copy Number Variation of Ampliconic Regions on Hominid X and Y chromosomes

Please read the latest `.pdf` file in the `text/` directory, for a complete walkthrough.

***
## Short  description of files

The files `workflow.py` `workflow_templates.py` depends on [gwf-org](http://gwf.readthedocs.io/en/latest/index.html) to run. The files depict the pipeline where genome reads are mapped to several artificial chromosomes. The copy number variation is output, merged with `merge.py` and the data is subsequently visualized with the various `plots*.R` files.

All diagrams reside in `plots/`.

The difference between `plots` and `plots_human_ac` is that in the first; a species specific artificial chromosome is used as reference for mapping, and in the latter an artifical chromosome based on human genes is used.


