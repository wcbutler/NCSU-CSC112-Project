Program Merrily_We_Roll_Along
Implicit None

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! programmer:  Will Butler
! unity id:    wcbutler
! lab section: 201
!
! assignment:  Project for Lecture
! date:        Oct. 21, 2002; last modified Nov. 18, 2002
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!Purpose:
!		To take input from four files and output an analysis based on the
!		data showing the area error of a Stanely Wheel compared to the
!		courseness of the ground.
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!Variables:
!
! Global and local to main:
! REAL:
!	error(4) - stores the error in the Stanley Wheel
!	r        - Stanley Wheel radius
!	pi       - constant for the value of pi
!	factor(4)- stores factor
!
! CHARACTER(11):
!	name(4)  - the name of each data file
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

REAL          :: error(4), factor(4), r
REAL,parameter:: pi=3.141593
CHARACTER(11) :: name(4)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! MILESTONE ONE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
write(*,*) 'MILESTONE ONE -- Input and Data Analysis'
call banner
call input(name)
call stat(name, factor)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! MILESTONE TWO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
write(*,*) 'MILESTONE TWO -- Stanley Wheel Errors'
r=1.0/pi        ! Stanley Wheel Radius

call path(r, name, error)
call report(r, name, factor, error)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! MILESTONE THREE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
write(*,*) 'MILESTONE THREE -- Range of Wheel Circumferences'
call simulation(name)

End Program Merrily_We_Roll_Along


!-----------------------------------------------------------------------------
Subroutine Simulation(name)
	Implicit None
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!Purpose: to calculate errors with very rough, rough, med-smooth, and smooth surfaces
!	  at different circumferences
!Variables:
!
! REAL:
!	r 	- recalculated radius that is j/(r*pi), where j is 1 increasing to 8
!	results(8,4).	- 2D array of the resulting error information
!	error(4)- stores the error in the Stanley Wheel, passed to path to be calced
!	pi	- constant for the value of pi
!
! CHARACTER(11)
!	name(4) - the name of each data file
!
! INTEGER
!	j       - used for the first dimmension of the results array as a counter
!		  ex: results(j,x)
!	i	- used for the second dimmension of the results array as a counter
!		  ex: results(y,i)
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	REAL :: r, results(8,4)
	REAL :: error(4)
	CHARACTER(11), INTENT(in) :: name(4)
	REAL,parameter:: pi=3.141593
	INTEGER:: j, i

!	write(*,*) 'Simulation called'
	write(*,*)
	write(*,*)"      Simulation Error Results for Surveyor's Wheels"
	write(*,*)"                    Yards/Half Mile"
	write(*,*)"         Wheel Circumferences -- 0.5 to 4 Yards"
	write(*,*)
	write(*,*)" ------------------------------------------------------------"
	write(*,*)"  Circum.   Radius    Very Rough  Rough  Med-Smooth  Smooth"
	write(*,*)" ------------------------------------------------------------"

	DO j=1, 8
		r=real(j)/(4*pi)
		call Path(r, name, error)
		DO i=1, 4
			results(j,i)=error(i)
		END DO
		write(*,200) 2*pi*real(j)/(4*pi), real(j)/(4*pi), (results(j,i), i=1,4)
		200 FORMAT (1x, 2(F7.3, 3x), 4(f8.3, 2x))
	END DO
	write(*,*)
	write(*,*)
End
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
Subroutine Report(r, name, factor, error)
	Implicit None
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!Purpose: outputs a table including error from path
!
!Variables:
!
! REAL:
!	circ	  - circumference of the Stanley Wheel
!	r	  - radius of the wheel (calculated in main and is constant)
!	factor(4) - inputted just for the table
!	error(4)  - from path to be in table
!
! INTEGER:
!	i	  - loop variable
!
! CHARACTER(11):
!	name(4)  - the name of each data file
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER::i
	REAL, PARAMETER :: circ=2.0
	CHARACTER(11), INTENT(in) :: name(4)
	REAL, INTENT(in) :: r
	REAL, INTENT(in) :: factor(4), error(4)

	write(*,*)
	write(*,*)"           Error Report(Yards) for the Stanley Wheel(AAA Wheel)"
	write(*,*)
	write(*,*)" ----------------------------------------------------------------------"
	write(*,*)"  File Name   Circumference   Radius    Factor     Error/0.5 miles"
	write(*,*)" ----------------------------------------------------------------------"
	300 FORMAT (2x, A, 2x, F7.3, 7x, F7.3, 3x, F7.3, 4x, F7.3)
	Do i=1, 4
		write(*,300)name(i), circ, r, factor(i), error(i)
	End Do

	call Pause_it
End
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
Subroutine Path(r, name, error)
	Implicit None
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!Purpose: calculates the error to send to report later, also is used by
!	   simulation to calculate
!
!Variables:
!
! REAL:
!	error(4) - stores the error in the Stanley Wheel, goes out of subroutine
!	r        - Stanley Wheel radius
!	xd, y, d - inputs for read from file
!	dist     - distance measured from wheel (contains errors
!	sum      - xd+d, should add up to 880
!	dp	 - the dp value
!	a	 - the a value
!
! CHARACTER(11):
!	name(4)  - the name of each data file
!
! INTEGER
!	eof      - end of file tester
!	i	 - loop variable
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	REAL, INTENT(in) :: r
	CHARACTER(11), INTENT(in) :: name(4)
	REAL, INTENT(out) :: error(4)
	REAL :: xd, y, d, dist, x, a, sum, dp
	INTEGER :: eof, i

!	write(*,*) 'Path Called'
!	write(*,*)
	DO i=1, 4
		xd= 0.0
		y= 0.0
		d= 0.0
		dist= 0.0
		sum= 0.0
		open (unit=30, file=name(i), status="old")
		DO
			read(30,*, iostat=eof)xd, y, d
			if (eof<0) EXIT

			sum=sum+xd+d
			dist=dist+xd
			y=abs(y)

			if (d<=2*r) then
				x=sqrt(r**2-(d/2.0)**2)
				if (y>=(r-x)) then			!stuck
					a=2*asin(d/(2*r))*r
					dist=dist+a
				else					!stuck and roll
					x=r-y
					dp=sqrt(r**2-x**2)
					a=asin(dp/r)*r
					dist=dist+2*a+(d-2*dp)
				end if
			else
				if(y>=r) then				!in and out
					dist=dist+2*(abs(y)-r)+2*(1.0/2.0)*3.141593*r+(d-2*r)
				else					!half in and out
					x=r-y
					dp=sqrt(r**2-x**2)
					a=asin(dp/r)*r
					dist=dist+2*a+(d-2*dp)
				end if
			end if

		END DO
		error(i)=dist-sum
	END DO
	close(30)

End
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
Subroutine Input(name)
	Implicit None
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!Purpose: simply read in the data files
!
!Variables:
!
! CHARACTER(11):
!	name(4)  - the name of each data file, this is inputted and sent out
!		   in this subroutine
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	CHARACTER(11), intent(out):: name(4)

!	write(*,*) 'Input Called'
	!name(1)="smooth1.dat"
	!name(2)="smooth2.dat"
	!name(3)="smooth3.dat"
	!name(4)="smooth4.dat"
	write(*,*)
	write(*,'(a)', advance="no")"   Enter Measured Track File Name -->  "
	read(*,*)name(1)
	write(*,*)
	write(*,'(a)', advance="no")"   Enter Measured Track File Name -->  "
	read(*,*)name(2)
	write(*,*)
	write(*,'(a)', advance="no")"   Enter Measured Track File Name -->  "
	read(*,*)name(3)
	write(*,*)
	write(*,'(a)', advance="no")"   Enter Measured Track File Name -->  "
	read(*,*)name(4)
	write(*,*)

	call pause_it
End
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
Subroutine Banner
	Implicit None
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Purpose: prints out banner
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!	write(*,*) 'Banner Called'
      write(*,100)
	100   FORMAT(//////// &
      1X,T20,'******************************************'/,&
      1X,T20,'*                                        *'/,&
      1X,T20,'*                                        *'/,&
      1X,T20,'*            PROTOTYPE SYSTEM:           *'/,&
      1X,T20,'*                                        *'/,&
      1X,T20,'*         SURVEYOR''S WHEEL ANALYSIS      *'/,&
      1X,T20,'*                                        *'/,&
      1X,T20,'*                                        *'/,&
      1X,T20,'*                                        *'/,&
      1X,T20,'*           KNOWLEDGE DYNAMICS           *'/,&
      1X,T20,'*                                        *'/,&
      1X,T20,'*               WILL BUTLER              *'/,&
      1X,T20,'*                                        *'/,&
      1X,T20,'*                  2002                  *'/,&
      1X,T20,'*                                        *'/,&
      1X,T20,'******************************************'///)

	call pause_it
End
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
Subroutine Stat(name, factor)
	Implicit None
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!Purpose: reads in and calculates factor then creates table
!
!Variables:
!
! REAL:
!	error(4) - stores the error in the Stanley Wheel
!	xd, y, d - readin variables
!	sumxd, sumy, sumd - sums all these
!	track    - length of the track, always 880.00
!	factor(4)- stores factor, intent is out
!
! CHARACTER(11):
!	name(4)  - the name of each data file
!
! INTEGER ::
!	eof	- end of file test
!	i	- loop variable
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      200 FORMAT (2x, I1, 2x, A, 2x,  F7.3, 2x, F7.3, 2x, F4.1, 2x, F7.3, 2x, F4.1, 2x, F7.3, 2x, F4.1, 2x, F6.4)
      REAL, INTENT(out) :: factor(4)
      CHARACTER(11), INTENT(in) :: name(4)
      INTEGER:: eof, i
      REAL:: xd, y, d, sumxd, sumy, sumd
      REAL:: track=880.00

	write(*,*)
      write(*,*)"             Measured Track Analysis(Yards)"
	write(*,*)
	write(*,*)"------------------------------------------------------------------------------"
	write(*,*)"    File Name    Track    Sum_xd    %    Sum_d     %    Sum_y     %    Factor"
	write(*,*)"------------------------------------------------------------------------------"

      DO i=1, 4
		open (unit=20, file=name(i), status="old")
		sumxd=0.0
		sumy= 0.0
		sumd= 0.0
		DO
			read(20,*, iostat=eof)xd, y, d
			if (eof<0) EXIT
			sumxd=sumxd+xd
			sumy =sumy +y
			sumd =sumd +d
		END DO
		factor(i)=1+ ((sumd+abs(sumy)) / (sumxd))
		write(*,200)i, name(i), track, sumxd, ((sumxd/track)*100), sumd, ((sumd/track)*100), sumy, ((sumy/track)*100), factor(i)
		close(20)
      END DO

	call pause_it
End
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
Subroutine PAUSE_IT
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!Purpose: pauses between milestones
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	write(*,110,ADVANCE='NO')
	110   FORMAT(//1X,'TYPE RETURN TO CONTINUE')
      read(*,*)
End
!-----------------------------------------------------------------------------
