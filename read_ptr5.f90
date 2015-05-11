! TODO: check getpart5data in smokeview/IOpart.c

! This program reads a prt5-file (FDS Version 5.5) and write the trajectories to an output file in the fllowing format
! frame index x y z
! alternatively one can also write: time index x y z. See line 200
! compile: f95  -o parser read_prt5.f90
! usage: ./parser <filename.prt5> <filename.dat>
! tested with GNU Fortran (Ubuntu/Linaro 4.4.4-14ubuntu5.1) 4.4.5 
!
!------------------------------------------------------------------------------------------------
! MC, 02.11.2012
!     05.06.2014
!------------------------------------------------------------------------------------------------

     
subroutine progress(nowPeds, TotalPeds)
  CHARACTER :: CR = CHAR(13)    ! carriage return character
  integer :: totaldotz, dotz, ii, diffdotz
  real :: fraction, percent
  character(len=7)::bar="????% ["
  totaldotz  = 40
  fraction = real(TotalPeds- nowPeds)/(TotalPeds-1)
  percent =  fraction * 100
  dotz = int(fraction * totaldotz)
  diffdotz = totaldotz - dotz
  ii=0
  write(unit=bar(1:4),fmt="(i4)") int(percent)
  
  write(6, fmt="(a8)", advance="no") bar
  !print *, "dotz=", dotz, "  totaldotz = ", totaldotz
   do ii = 1, dotz
      write(6, fmt="(a)", advance="no") "="
   enddo
   write(6, fmt="(a)", advance="no") ">"
   do ii = 1, diffdotz
      write(6, fmt="(a)", advance="no") " "
   enddo
   write(6, fmt="(A1 ,1a1,$)"), "]" , CR 
 
   flush(unit=6)
return

end subroutine progress

SUBROUTINE ChkMemErr(CodeSect,VarName,IZERO) !from FDS-Code
  
! Memory checking routine
 
CHARACTER(*), INTENT(IN) :: CodeSect, VarName
INTEGER IZERO
CHARACTER(100) MESSAGE
 
IF (IZERO==0) RETURN
 
WRITE(MESSAGE,'(4A)') 'ERROR: Memory allocation failed for ', TRIM(VarName),' in the routine ',TRIM(CodeSect)
!CALL SHUTDOWN(MESSAGE)
END SUBROUTINE ChkMemErr



IMPLICIT  NONE

! prec.f90
! Precision of "Four Byte" and "Eight Byte" reals
INTEGER, PARAMETER :: FB = SELECTED_REAL_KIND(6)
INTEGER, PARAMETER :: EB = SELECTED_REAL_KIND(12)


CHARACTER (len=100) :: in_file
CHARACTER (len=100) :: out_file
CHARACTER (len=100) :: s_out_file = "tmp.dat"
CHARACTER  :: dummy
INTEGER ONE_INTEGER, VERSION_NUMBER, N_EVAC, N_PART, ZERO_INTEGER, EVAC_N_QUANTITIES, file_size, read_size, INT_SIZE
INTEGER ios, I, NN, N, NPLIM, IZERO, EVAC
INTEGER :: frame, status, is_error
INTEGER indianswitch ! I don't know what to do with this. Got it from smokeview/IOpart.c
real ::   counter

REAL(FB) :: T
REAL(FB), ALLOCATABLE, DIMENSION(:) :: XP,YP,ZP
REAL(FB), ALLOCATABLE, DIMENSION(:,:) :: QP, AP ! body angle, semi major axis, semi minor axis
INTEGER, ALLOCATABLE, DIMENSION(:) :: TA

CHARACTER(30), ALLOCATABLE,DIMENSION(:) :: NAME
CHARACTER(30), ALLOCATABLE,DIMENSION(:) :: UNITS
CHARACTER(100) :: sort_command
CHARACTER(100) :: mv_command

CALL get_command_argument(1, in_file)
IF ( LEN_TRIM(in_file) == 0) THEN
   WRITE (0,*) "ERROR: inputfile is not passed"
   WRITE (0,*) "USAGE: ./parser <inputfile> <outputfile>"
   STOP
ENDIF

CALL get_command_argument(2, out_file)
IF ( LEN_TRIM(out_file) == 0) THEN
   WRITE (0,*) "ERROR: outputfile is not passed"
   WRITE (0,*) "USAGE: ./parser <inputfile> <outputfile>"
   STOP
ENDIF
INQUIRE(FILE=in_file, SIZE=file_size)
PRINT *, "INFO: inputfile = <", TRIM(in_file), ">", "  |  size = ", file_size, " bytes"

OPEN(unit = 9, file = in_file, form = "unformatted", status = "old",   iostat = ios)
IF (ios .NE. 0) THEN
   write (0,*) "ERROR: Could not open inputfile ", in_file
   STOP
ENDIF

OPEN(unit = 15, file = out_file, form = "formatted", status = "replace", iostat = ios)
IF (ios .NE. 0) THEN
   write (0,*) "ERROR: Could not open outputfile ", out_file
   STOP
ENDIF

!================== sort file with respect to <id> and than to <frame> (linux-commands) ================== 
sort_command = 'sort -k1,1 -k2,2 -n ' // TRIM(out_file)  //' > ' // TRIM(s_out_file)
mv_command = 'mv ' // TRIM(s_out_file) // ' ' // TRIM(out_file)
read_size = 0
INT_SIZE = sizeof(ONE_INTEGER)

!================== read header ================== 
READ(9) ONE_INTEGER      ! Integer 1 to check Endian-ness
READ(9) VERSION_NUMBER   ! FDS version number
READ(9) N_EVAC           ! Number of PARTicle classes
EVAC = 1                 ! TODO: When is EVAC == 0? 
read_size = read_size + sizeof(ONE_INTEGER) + sizeof(VERSION_NUMBER) + sizeof(N_EVAC) + 6*INT_SIZE
!================== read header ================== 
WRITE(6,'(A, I2)') " #  ONE_INTEGER = ", ONE_INTEGER
WRITE(6,'(A, F4.2)') " #  VERSION_NUMBER = ", VERSION_NUMBER/100.
WRITE(6,'(A, I2)') " #  N_EVAC = ", N_EVAC

IF (ONE_INTEGER /= 1) then 
   indianswitch = 1
ELSE
   indianswitch = 0 
ENDIF

DO N=1,N_EVAC
   READ(9) EVAC_N_QUANTITIES, ZERO_INTEGER  ! ZERO_INTEGER is a place holder
   read_size = read_size + sizeof(EVAC_N_QUANTITIES) + sizeof(ZERO_INTEGER) + 2*INT_SIZE
   WRITE(6,'(A, I2)') " #  EVAC_N_QUANTITIES = ", EVAC_N_QUANTITIES
   WRITE(6,'(A, I2)') " #  ZERO_INTEGER = ", ZERO_INTEGER
   ALLOCATE(NAME(EVAC_N_QUANTITIES))
   ALLOCATE(UNITS(EVAC_N_QUANTITIES))


   DO NN = 1, EVAC_N_QUANTITIES
      READ(9) NAME(NN)   !OUTPUT_QUANTITY(EVAC_QUANTITIES_INDEX(NN))%NAME
      READ(9) UNITS(NN)  !OUTPUT_QUANTITY(EVAC_QUANTITIES_INDEX(NN))%UNITS
      read_size = read_size + sizeof( NAME(NN) ) + sizeof( UNITS(NN) ) + 4*INT_SIZE
      WRITE(6,*) "#  NAME  =  ", NAME(NN)
      WRITE(6,*) "#  UNITS = ", UNITS(NN)
   ENDDO
ENDDO ! N_EVAC

frame = 0 
counter = 1
is_error = 0 ! 1 if something went wrong after allucating the arrays
!================== read data for pedestrians. SUBROUTINE DUMP_EVAC(T,NM) ================== 
DOFILE: DO
   frame = frame + 1
   READ(9), T           ! the time T as 4 byte real
   read_size = read_size + sizeof(T) + 2*INT_SIZE
   !WRITE(6, *) "#  TIME = ", T
   ! print * , "------- frame = ", frame
   DO N = 1, N_EVAC
      ! print * , "======="
      READ(9), NPLIM    ! Number of particles in the PART class
      read_size = read_size + sizeof(NPLIM) + 2*INT_SIZE
      IF (frame .eq. 1) then 
         counter = NPLIM
         WRITE(6,*) "#  MAX PEDESTRIANS = ", NPLIM
      ENDIF
      
      IF (NPLIM < 1) THEN
         WRITE (6, '(A,I4)') " Got NPLIM = ", NPLIM
         STOP 0!DOFILE !STOP
      ENDIF
!============================================================"
      call progress(NPLIM, counter) ! generate the progress bar.

      ALLOCATE(TA(NPLIM),STAT=IZERO)
      CALL ChkMemErr('DUMP','TA',IZERO) 
      ALLOCATE(XP(NPLIM),STAT=IZERO)
      CALL ChkMemErr('DUMP','XP',IZERO) 
      ALLOCATE(YP(NPLIM),STAT=IZERO)
      CALL ChkMemErr('DUMP','YP',IZERO) 
      ALLOCATE(ZP(NPLIM),STAT=IZERO)
      CALL ChkMemErr('DUMP','ZP',IZERO) 
      ALLOCATE(AP(NPLIM, 4),STAT=IZERO)
      CALL ChkMemErr('DUMP_EVAC','AP',IZERO)
      IF (EVAC_N_QUANTITIES > 0) THEN
         ALLOCATE(QP(NPLIM, EVAC_N_QUANTITIES), STAT=IZERO) 
         CALL ChkMemErr('DUMP','QP',IZERO)
      ENDIF
!===================== READ Trajectories ======================
      !if(parti->evac==1) read 7 * NPLIM
      ! else read 3 * NPLIM 
      ! TODO: How to know the value parti-evac?
      IF(EVAC == 1) THEN 
         READ(9, iostat = ios), (XP(I), I=1, NPLIM), (YP(I), I=1, NPLIM), (ZP(I), I=1, NPLIM), &
              (AP(I,1),I=1,NPLIM),(AP(I,2),I=1,NPLIM),(AP(I,3),I=1,NPLIM),(AP(I,4),I=1,NPLIM)
         read_size = read_size + 7*sizeof(XP) + 2*INT_SIZE
      ELSE
         READ(9, iostat = ios), (XP(I), I=1, NPLIM), (YP(I), I=1, NPLIM), (ZP(I), I=1, NPLIM)
         read_size = read_size + 3*sizeof(XP) + 2*INT_SIZE
      ENDIF
      
!==============================================================
      IF (ios .NE. 0) THEN
         write (0,*) " ERROR: Could not read trajectories "
         is_error = 1
         EXIT DOFILE !STOP
      ENDIF
      READ(9, iostat = ios), (TA(I), I=1, NPLIM)
      read_size = read_size + sizeof(TA) + 2*INT_SIZE

      IF (ios .NE. 0) THEN
         write (0,*) " ERROR: Could not read TA "
         is_error = 1
         EXIT DOFILE !STOP
      ENDIF
    
!================ WRITE Trajectories ======================
      DO I=1,NPLIM
         !WRITE (15,*) T, TA(I), XP(I), YP(I)
          WRITE (15, '(I4, x, I4, 3(x, F15.4))') frame, TA(I), XP(I)*100, YP(I)*100, ZP(I)*100   !x and y in[cm] 
         !WRITE (15, '(I4, x, I4, 3(x, F15.4))')  TA(I), frame, XP(I)*100, YP(I)*100, ZP(I)*100   !x and y in[cm] 
      ENDDO
!==========================================================
      ! AP1: phi, AP2: small semi-axis, AP3: large semi-axis AP4: hight
      ! QP color information for smokeview
      IF (EVAC_N_QUANTITIES > 0 ) THEN
         READ(9), ((QP(I,NN), I=1, NPLIM), NN=1,  EVAC_N_QUANTITIES)
         read_size = read_size + sizeof(QP) + 2*INT_SIZE
      END IF

      IF (NPLIM == 1)THEN
         print *, " "
         print *, "INFO: Close files" 
         print *, "INFO: Bytes parsed ", read_size, " /", file_size, "(===> difference ", file_size-read_size,"Bytes)"
         CLOSE(unit = 9)  !input file
         CLOSE(unit = 15) !output file
         PRINT *, sort_command
         status = SYSTEM(sort_command)
         IF (status .NE. 0 ) THEN
            write (0,*) "WARNING: Could not sort"
            EXIT DOFILE
         ENDIF
         PRINT *, mv_command
         status = SYSTEM(mv_command)
         IF (status .NE. 0 ) THEN
            write (0,*) "WARNING: Could not mv"
            EXIT DOFILE
         ENDIF
         EXIT DOFILE
      ENDIF !(NPLIM == 1)
   ENDDO !N = 1, N_EVAC
ENDDO DOFILE

!============================ DEALLOCATE ==============
print *, "INFO: Free memory"
DEALLOCATE(AP)
DEALLOCATE(ZP)
DEALLOCATE(YP)
DEALLOCATE(XP)
DEALLOCATE(TA)
DEALLOCATE(NAME)
DEALLOCATE(UNITS)
IF (EVAC_N_QUANTITIES > 0) THEN
   DEALLOCATE(QP)
ENDIF
IF ( is_error == 0) THEN
   PRINT *, "INFO: Program ends successfully!"
   PRINT *, "INFO: outputfile = <", TRIM(out_file), ">"
ENDIF
END PROGRAM 
