## Trajptr5

This program reads a `prt5-file` (FDS Version 5.5) and writes the pedestrian's trajectories 
to an output file in the following format

`frame index x y z`

alternatively one can also output the following format:

`time index x y z`

Change in the code the `line 167`

### compile
> f95  -o parser read_prt5.f90

### usage
In a terminal give 
> ./parser input_filename.prt5 output_filename.dat

### Requirements
There are no special requirements. Obviosly you will need to have a fortran compiler installed. :-)
With Debian/Ubuntu 
> sudo apt-get install gfortran

should be enough.
Tested with GNU Fortran (**Ubuntu**/Linaro 4.4.4-14 ubuntu5.1)  and **Debian** 7.2
Check the example and the movie in wiki.

