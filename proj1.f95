Program Merrily_We_Roll_Along
Implicit None

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! programmer:  Will Butler
! unity id:    wcbutler
! lab section: 201
!
! assignment:  Project for Lecture
! date:        Oct. 21, 2002
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! purpose:
!		To take input from four files and output an analysis based on the
!		data showing the area error of a Stanely Wheel compared to the
!		courseness of the ground.
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! variables:
!
! Global and local to main:
! REAL:
!	error(4) -
!	factor(4)-
!	r        - Stanley Wheel radius
!	pi       - constant for the value of pi
!	sumxd(4) -
!	sumd(4)  -
!	sumy(4)  -
!	factor(4)- stores
!
! CHARACTER(11):
!	name(4)  - the name of each data file
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      Real Error(4),Factor(4),r,pi
      Character(11) name(4)
 !!!!!! MILESTONE ONE

REAL:: sumxd(4), sumd(4), sumy(4)!, factor(4)
100 FORMAT (A, F7.2, A)

      pi=3.141593
      Write(*,*) 'MILESTONE ONE -- Input and Data Analysis'
      Call Banner
      Call Input(name)
      Call Stat(name, factor)
      
 !!!!!!! MILESTONE TWO
      Write(*,*) 'MILESTONE TWO -- Stanley Wheel Errors'
      r=1.0/pi        ! Stanley Wheel Radius

      Call path()
      call report()

 !!!!!!!!! MILESTONE THREE
      Write(*,*) 'MILESTONE THREE -- Range of Wheel Circumferences'
      Call Simulation()

      End

      Subroutine Simulation()
      Implicit None
      Write(*,*) 'Simulation Called'
      Call Path()
      End

     Subroutine report()
     implicit none
     Write(*,*) 'Report Called'
	write(*,*)
	write(*,*)"           Error Report(Yards) for the Stanley Wheel(AAA Wheel)"
	write(*,*)
	write(*,*)" ----------------------------------------------------------------------"
	write(*,*)"  File Name   Circumference   Radius    Factor     Error/0.5 miles"
	write(*,*)" ----------------------------------------------------------------------"



     call Pause_it
     end


     Subroutine path()
     Implicit none
     Write(*,*) 'Path Called'

     End

        Subroutine Input(name)
        Implicit none
	
	character(11), intent(out):: name(4)

        Write(*,*) 'Input Called'

	name(1)="smooth1.dat"
	name(2)="smooth2.dat"
	name(3)="smooth3.dat"
	name(4)="smooth4.dat"
	!write(*,'(a)', advance="no")"Enter Measured Track File Name -->  "
	!read(*,*)name(1)
	!write(*,'(a)', advance="no")"Enter Measured Track File Name -->  "
	!read(*,*)name(2)
	!write(*,'(a)', advance="no")"Enter Measured Track File Name -->  "
	!read(*,*)name(3)
	!write(*,'(a)', advance="no")"Enter Measured Track File Name -->  "
	!read(*,*)name(4)


        call pause_it
        end


        Subroutine Banner
        Implicit None

         Write(*,*) 'Banner Called'
         WRITE(*,100)
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
        1X,T20,'*        TL HONEYCUTT & S KRISHNA        *'/,&
        1X,T20,'*                                        *'/,&
        1X,T20,'*                  2002                  *'/,&
        1X,T20,'*                                        *'/,&
        1X,T20,'******************************************'///)
        call pause_it
       End

      Subroutine Stat(name, factor)
      Implicit none
      
      200 FORMAT (2x, I1, 2x, A, 2x,  F7.3, 2x, F7.3, 2x, F4.1, 2x, F7.3, 2x, F4.1, 2x, F7.3, 2x, F4.1, 2x, F6.4)
      real, intent(out) :: factor(4)
      character(11), intent(in) :: name(4)
      integer:: eof, i
      real:: xd, y, d, sumxd, sumy, sumd
      real:: track=880.00
      Write(*,*) 'Stat Called'
	write(*,*)
	write(*,*)"------------------------------------------------------------------------------"
	write(*,*)"    File Name    Track    Sum_xd    %    Sum_d     %    Sum_y     %    Factor"
	write(*,*)"------------------------------------------------------------------------------"
      Do i=1, 4

 	open (unit=20, file=name(i), status="old")
	sumxd=0.0
	sumy= 0.0
	sumd= 0.0
	Do
		Read(20,*, iostat=eof)xd, y, d
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
      end


 !***PAUSE_IT*****************PAUSE_IT*******PAUSE_IT***
         SUBROUTINE PAUSE_IT
         WRITE(*,110,ADVANCE='NO')
   110   FORMAT(//1X,'TYPE RETURN TO CONTINUE')
         READ(*,*)
         END
