# Patch-Graph-Rewriting

Git repository from Wouter Brozius, for a thesis about [Patch Graph Rewriting](https://arxiv.org/abs/2003.06488).

## Input file format

The input file should contain exactly 5 lines, in the following order:
1. Vertex id's
2. Edge id's
3. Edge sources
4. Edge targets
5. Edge labels

The arguments should be separated by semicolons (```;```). Spaces (``` ```) will be ignored. For lines 3-5, every argument should be a vertex id and the ```i```'th argument corresponds to the ```i```'th edge in line 2.

See the ```main.hs``` function for naming your input or output files.

## Output file format

A ```.dot``` file.

