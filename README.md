## Trajptr5

This program reads a `prt5-file` (tested with FDS Version 5.5 and FDS 6.0) and writes the pedestrian's trajectories to an output file in the following format

`frame index x y z`

alternatively one can also output the following format:

`time index x y z`

Change in the code the `lines 171 and 172`

### compile
> f95  -o parser read_prt5.f90
or 
> gfortran  -o parser read_prt5.f90

### usage
In a terminal give 
> ./parser input_filename.prt5 output_filename.txt

### Requirements
There are no special requirements. Obviosly you will need to have a fortran compiler installed. :-)
In Debian/Ubuntu 
> sudo apt-get install gfortran
should be enough.

### Plot scripts
For convenience some plot scripts are provided in the directory `scripts`. 

### Does it work?
This parser has been tested with GNU Fortran (**Ubuntu**/Linaro 4.4.4-14 ubuntu5.1)  and **Debian** 7.2. See also the examples in  the directory `ptr5_files`. 

Check also the example and the movie in [wiki](https://github.com/chraibi/Trajptr5/wiki/Examples) of an evacuation of a vometory of a stadium..

Enjoy!
