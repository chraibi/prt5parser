## Purpose

This program reads a `prt5-file` (tested with FDS Version 5.5 and FDS 6.0) and writes the pedestrian's trajectories to an output file in the following format

`frame index x y z`

alternatively one can also output the following format:

`time index x y z`

Change in the code the `lines 171 and 172`

### compile
> f95  -cpp -DSTRING  -o parser read_prt5.f90
or 
> gfortran   -DSTRING  -o parser read_prt5.f90

where "STRING" is whether *UNIX* or *WIN* depending on your system. Passing this preprocessor directive is important, because in the code the output file is sortedusing system-specific commands (`sort`, `move`, ...)

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
This parser has been tested with GNU Fortran (**Ubuntu**/Linaro 4.4.4-14 ubuntu5.1)  and **Debian** 7.2. See also the examples in  the directory `ptr5_files`. I have no idea if it will work when compiled with other compilers. Since, I do not use FDS, I don't even know if this parser *ALWAYS* works. If you have a file that makes it fail, please send it to me or open an `issue`. 

Check also the example and the movie in [wiki](https://github.com/chraibi/ptr5parser/wiki/Example) showing an evacuation of a stadium vometory.


