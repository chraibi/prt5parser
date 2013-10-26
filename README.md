Trajptr5
========

This program reads a prt5-file (FDS Version 5.5) and writes the pedestrian's trajectories 
to an output file in the fllowing format
! frame index x y z
! alternatively one can also write: time index x y z. See line 167
! compile: f95  -o parser read_prt5.f90
! usage: ./parser <input_filename.prt5> <output_filename.dat>
! tested with GNU Fortran (Ubuntu/Linaro 4.4.4-14ubuntu5.1) 4.4.5
