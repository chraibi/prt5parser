[![GitHub license](https://img.shields.io/badge/license-GPL-blue.svg)](https://raw.githubusercontent.com/chraibi/ptr5parser/master/LICENSE) [![Twitter](https://img.shields.io/twitter/url/https/github.com/chraibi/ptr5parser.svg?style=social)](https://twitter.com/intent/tweet?text=Wow:&url=%5Bobject%20Object%5D)
[![DOI](https://zenodo.org/badge/13888565.svg)](https://zenodo.org/badge/latestdoi/13888565)


## Purpose

This program reads a `prt5-file` (tested with FDS Version 5.5 and FDS 6.0) and writes the pedestrian's trajectories to an output file in the following format

```
index frame x y z
```

## compile
```
f95  -cpp -DOS  -o parser read_prt5.f90
```
or 

```
gfortran   -DOS  -o parser read_ptr5.f90
```


where `OS` is whether **UNIX** or **WIN** depending on your system. 
Passing this preprocessor directive is important, because in the code the output file is sorted using 
system-specific commands (`sort`, `move`, ...)

With Visual Studio the definition of `OS` is by passing a `/D`. See the documentation [here](https://msdn.microsoft.com/en-us/library/hhzbb5c8.aspx).

## usage
In a terminal give 
```
./parser input_filename.ptr5 output_filename.txt
```

## Requirements
There are no special requirements. Obviosly you will need to have a fortran compiler installed. :-)

In Debian/Ubuntu 
  ```
   sudo apt-get install gfortran
   ```

should be enough.

## Plot scripts
For convenience some plot scripts are provided in the directory `scripts`. 

## Does it work?
This parser has been tested with GNU Fortran (**Ubuntu**/Linaro 4.4.4-14 ubuntu5.1)  and **Debian** 7.2. 
See also the examples in the directory `ptr5_files`. 

I have no idea if it will work when compiled with other compilers. Since, I do not use FDS, I don't even know if this parser *ALWAYS* works. 

If you have a file that makes it fail, please open an `issue`. 

Check also the example and the movie in the [wiki](https://github.com/chraibi/ptr5parser/wiki) showing an evacuation of a stadium vometory.


