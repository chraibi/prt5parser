
! This program reads a prt5-file (FDS Version 5.5) and write the trajectories to an output file in the fllowing format
! frame index x y z
! alternatively one can also write: time index x y z. See line 167
! compile: f95  -o parser read_prt5.f90
! usage: ./parser <filename.prt5> <filename.dat>
! tested with GNU Fortran (Ubuntu/Linaro 4.4.4-14ubuntu5.1) 4.4.5 
!
!------------------------------------------------------------------------------------------------
! MC, 02.11.2012
!     05.06.2014
!------------------------------------------------------------------------------------------------



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

INTEGER ONE_INTEGER, VERSION_NUMBER, N_EVAC, N_PART, ZERO_INTEGER, EVAC_N_QUANTITIES
INTEGER ios, dummy, I, NN, N, NPLIM, IZERO
INTEGER :: frame, status
REAL(FB) :: T
REAL(FB), ALLOCATABLE, DIMENSION(:) :: XP,YP,ZP, AP1, AP2, AP3, AP4
REAL(FB), ALLOCATABLE, DIMENSION(:,:) :: QP, AP


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

PRINT *, "INFO: inputfile = <", TRIM(in_file), ">"


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
!================== read header ================== 
READ(9) ONE_INTEGER      ! Integer 1 to check Endian-ness
READ(9) VERSION_NUMBER   ! FDS version number
READ(9) N_EVAC           ! Number of PARTicle classes
!================== read header ================== 
WRITE(6,'(A, I2)') " #  ONE_INTEGER = ", ONE_INTEGER
WRITE(6,'(A, F4.2)') " #  VERSION_NUMBER = ", VERSION_NUMBER/100.
WRITE(6,'(A, I2)') " #  N_EVAC = ", N_EVAC

DO N=1,N_EVAC
   READ(9) EVAC_N_QUANTITIES, ZERO_INTEGER  ! ZERO_INTEGER is a place holder
   WRITE(6,'(A, I2)') " #  EVAC_N_QUANTITIES = ", EVAC_N_QUANTITIES
   ALLOCATE(NAME(EVAC_N_QUANTITIES))
   ALLOCATE(UNITS(EVAC_N_QUANTITIES))


   DO NN = 1, EVAC_N_QUANTITIES
      READ(9) NAME(NN)   !OUTPUT_QUANTITY(EVAC_QUANTITIES_INDEX(NN))%NAME
      READ(9) UNITS(NN)  !OUTPUT_QUANTITY(EVAC_QUANTITIES_INDEX(NN))%UNITS
   ENDDO
ENDDO
WRITE(6,*) "#  NAME  =  ", NAME
WRITE(6,*) "#  UNITS = ", UNITS
frame = 0 
!================== read data for pedestrians. SUBROUTINE DUMP_EVAC(T,NM) ================== 
DOFILE: DO
   frame = frame + 1
   READ(9), T           ! the time T as 4 byte real
   !WRITE(6, *) "#  TIME = ", T
   ! print * , "------- frame = ", frame
   DO N = 1, N_EVAC
      ! print * , "======="
      READ(9), NPLIM    ! Number of particles in the PART class
      !============= PRINT something to the screen ============="
      IF (MODULO(NPLIM, 10) == 0) THEN
         WRITE (6, '(A,I4)') " NPLIM = ", NPLIM
      ELSE IF (NPLIM < 10) THEN
        WRITE (6, '(A,I4)') " NPLIM = ", NPLIM
      ENDIF
      WRITE (6, '(A,I4)') " #  NPLIM = ", NPLIM

      IF (NPLIM < 1) THEN
         WRITE (6, '(A,I4)') " EXIT because smaller that 1:  NPLIM = ", NPLIM
         EXIT DOFILE !STOP
      ENDIF
      !============================================================"

      ALLOCATE(TA(NPLIM),STAT=IZERO)
      CALL ChkMemErr('DUMP','TA',IZERO) 
      ALLOCATE(XP(NPLIM),STAT=IZERO)
      CALL ChkMemErr('DUMP','XP',IZERO) 
      ALLOCATE(YP(NPLIM),STAT=IZERO)
      CALL ChkMemErr('DUMP','YP',IZERO) 
      ALLOCATE(ZP(NPLIM),STAT=IZERO)
      CALL ChkMemErr('DUMP','ZP',IZERO) 
      ALLOCATE(AP1(NPLIM),STAT=IZERO)
      CALL ChkMemErr('DUMP','AP1',IZERO) 
      ALLOCATE(AP2(NPLIM),STAT=IZERO)
      CALL ChkMemErr('DUMP','AP2',IZERO) 
      ALLOCATE(AP3(NPLIM),STAT=IZERO)
      CALL ChkMemErr('DUMP','AP3',IZERO) 
      ALLOCATE(AP4(NPLIM),STAT=IZERO)
      CALL ChkMemErr('DUMP','AP4',IZERO)
      ALLOCATE(QP(NPLIM, EVAC_N_QUANTITIES), STAT=IZERO) 
      CALL ChkMemErr('DUMP','QP',IZERO)
      !================================= READ Trajectories ==========================
      READ(9, iostat = ios), (XP(I), I=1, NPLIM), (YP(I), I=1, NPLIM), (ZP(I), I=1, NPLIM)
      !==============================================================================
      IF (ios .NE. 0) THEN
         write (0,*) "ERROR: Could not read trajectories "
         EXIT DOFILE !STOP
      ENDIF
      READ(9, iostat = ios), (TA(I), I=1, NPLIM)  
      IF (ios .NE. 0) THEN
         write (0,*) "ERROR: Could not read TA"
         EXIT DOFILE !STOP
      ENDIF
      ! print *, "TA=", (TA(I), I=1, NPLIM)
    
      !================================= WRITE Trajectories ==========================
      DO I=1,NPLIM
         !WRITE (15,*) T, TA(I), XP(I), YP(I)
         WRITE (15, '(I4, x, I4, 3(x, F15.4))') frame, TA(I), XP(I)*100, YP(I)*100, ZP(I)*100  !x and y in [cm] 
      ENDDO
      !===============================================================================
      ! What does QP stand for?? Smokeview coloring particles?
      IF (EVAC_N_QUANTITIES > 0 ) THEN
         READ(9), ((QP(I,NN), I=1, NPLIM), NN=1,  EVAC_N_QUANTITIES)
      END IF

      IF (NPLIM == 1)THEN
         print *, "INFO: Close files"
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

print *, "INFO: Free memory"
DEALLOCATE(AP1)
DEALLOCATE(AP2)
DEALLOCATE(AP3)
DEALLOCATE(AP4)
DEALLOCATE(ZP)
DEALLOCATE(YP)
DEALLOCATE(XP)
DEALLOCATE(TA)
DEALLOCATE(NAME)
DEALLOCATE(UNITS)

print *, "INFO: Program ends successfully!"
PRINT *, "INFO: outputfile = <", TRIM(out_file), ">"
END PROGRAM 
