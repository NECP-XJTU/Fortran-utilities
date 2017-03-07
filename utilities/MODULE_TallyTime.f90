!--------------------------------------------------------------------------------------------------
!                  Copyright(c)2013 Xi'an Jiaotong University. All rights reserved.
!--------------------------------------------------------------------------------------------------
!
!  Developed by Nuclear Engineering Computational Physics (NECP) Laboratory.
!
!**************************************************************************************************
!  Version: v1.0                                                               Date: month/day/year
!  Purpose:                                                                               6/12/2013
!     (A tool package to tally user-specified event time.)
!
!  Authors list:
!     Name                       Affiliation                       Email
!     =========================  ================================  ================================
!     Zhang Tengfei              XJTU NECP Laboratory              xfempty@gmail.com
!
!  Subroutines list
!     subroutine TallyTime_ReadEvent(UserTallyTime_Type,INunit,OUTunit)
!     subroutine TallyTime_GetStorage(UserTallyTime_Type,NumINT,NumREAL,NumLOG,NumCha,OUTunit)
!     subroutine TallyTime_StartEvent(UserTallyTime_Type,User_EventNumber,OUTunit)
!     subroutine TallyTime_StopEvent(UserTallyTime_Type,User_EventNumber,OUTunit)
!     subroutine TallyTime_StopReturnTotal(UserTallyTime_Type,User_EventNumber,TallyTime_Total,OUTunit)
!     subroutine TallyTime_StopReturnIncrement(UserTallyTime_Type,User_EventNumber,TallyTime_Increment,OUTunit)
!     subroutine TallyTime_StopAll(UserTallyTime_Type)
!     subroutine TallyTime_Print(UserTallyTime_Type,OUTunit)
!     subroutine TallyTime_Copy(UserTallyTime_Type,UserTallyTime_TypePrime,OUTunit)
!
!     Auxilliary Subroutine list:
!        subroutine TallyTime_Define(UserTallyTime_Type,MaximumEvents,OUTunit)
!        subroutine TallyTime_Void(UserTallyTime_Type)
!        subroutine TT_Allocate((UserTallyTime_Type,MaximumEvents,OUTunit)
!        subroutine TT_StopTimingEvent(UserTallyTime_Type,User_EventNumber,Cumulative,Incremental,OUTunit)
!        subroutine TT_GetProcessTime(CurrentTime)
!        subroutine TT_GetWallClock(WallClock)
!        subroutine TT_ComputeWallClock(WallClockStart,WallClockEnd,ElapsedTime)
!**************************************************************************************************

module TallyTime
   implicit none

type :: SystemDate_Type
   integer :: year    = 0 ! Information of year
   integer :: month   = 0 ! Information of month
   integer :: DAY     = 0 ! Information of day
   integer :: HOUR    = 0 ! Information of hour
   integer :: MINUTES = 0 ! Information of minutes
   integer :: SECONDS = 0 ! Information of seconds
end type SystemDate_Type

type :: TallyTime_Type
   logical  :: Defined = .FALSE.         ! Index of the object array allocation state
   real(8)  :: ProgramTime = 0.          ! Process Time for program
   integer  :: NumEvents = 0             ! Current number of events that are being tallied
   integer  :: MaximumEvents = 0         ! Maximum number of events that are currently managed
   ! Data for user defined tally events (MaximumEvents)
   character(50),allocatable :: EventName(:) ! User-defined name of the event
   real(8),allocatable :: StartingClockTime(:) ! Starting clock time for a tally operation
   real(8),allocatable :: CumulativeTime(:) ! Cumulative time for each user tally operation
   logical,allocatable :: TimingStatus(:) ! Tally status of operation (true=activated, false=not)
   integer,allocatable :: NumTally(:) ! Tally time of operation (how many times have been activated)
   ! Wall clock data structure
   type (SystemDate_Type) :: WallClock    ! Wall clock Time for program
end type TallyTime_Type

contains
!-------------------------------------------------------------------------------
! Read names of events from the specified file
!-------------------------------------------------------------------------------
   subroutine TallyTime_ReadEvent(UserTallyTime_Type,code_para,OUTunit)
      implicit none
      ! Passed in
      integer,intent(in) :: code_para
      integer,intent(in) :: OUTunit
      ! Passed inout
      type(TallyTime_Type) :: UserTallyTime_Type ! The user provided data set
      ! Local
      integer MaximumEvents, IOS, I, Length_NoSpace
      character(50) User_EventName       ! The name of the timing event
      character(50) NULL
      character(len=50),allocatable :: User_EventName_array(:)

      MaximumEvents = 0
      if(code_para == 2) then
        MaximumEvents = 20
      elseif(code_para == 4) then
        MaximumEvents = 12
      endif

      ! Initialize the data structure if it hasn't been defined
      if (.NOT. UserTallyTime_Type%Defined)then
         call TallyTime_Define(UserTallyTime_Type,MaximumEvents,OUTunit)
      endif

      !> subsc
      if(code_para == 2) then
        allocate(User_EventName_array(MaximumEvents))
        User_EventName_array(1) = 'update heat'
        User_EventName_array(2) = 'xscheme'
        User_EventName_array(3) = 'solve energy equation'
        User_EventName_array(4) = 'solve axial momen'
        User_EventName_array(5) = 'solve lateral momentum'
        User_EventName_array(6) = 'solve mass conservation'
        User_EventName_array(7) = 'adjust pressure'
        User_EventName_array(8) = 'solve conduct3d'
        User_EventName_array(9) = 'save f,w &calc parameters'
        User_EventName_array(10) = 'prepare boundary condition'
        User_EventName_array(11) = 'calculate heat to channels'
        User_EventName_array(12) = 'fsave<='
        User_EventName_array(13) = 'calcpara'
        User_EventName_array(14) = 'turbulent&getcon'
        User_EventName_array(15) = 'ph2alp'
        User_EventName_array(16) = 'ph2t'
        User_EventName_array(17) = 'ph2rho'
        User_EventName_array(18) = 'pt2cp'
        User_EventName_array(19) = 'ph2eta'
        User_EventName_array(20) = 'ph2ctx'
      !> necp-x
      elseif(code_para == 4) then
        allocate(User_EventName_array(MaximumEvents))
        !User_EventName_array(1) = 'initialize core geom'
        User_EventName_array(1) = 'Generate core mesh'
        User_EventName_array(2) = 'Generate modular segment'
        User_EventName_array(3) = 'Generate core segment'
        User_EventName_array(4) = 'Resonance'
        User_EventName_array(5) = 'Transport'
        User_EventName_array(6) = '  CMFD'
        User_EventName_array(7) = '  1D Sn'
        User_EventName_array(8) = '    1D Sn sweep'
        User_EventName_array(9) = '  2D MOC'
        User_EventName_array(10) = '    Update scattering source'
        User_EventName_array(11) = '    2D MOC sweep'
        User_EventName_array(12) = 'TH solver'
      endif

      ! Setup the tally event
      UserTallyTime_Type%NumEvents = 0
      do I = 1, MaximumEvents
         User_EventName = User_EventName_array(I)
         if(User_EventName .EQ. NULL)then
            write(*,10)
            write(*,40)
            write(OUTunit,10)
            write(OUTunit,40)
            stop
         endif
         UserTallyTime_Type%NumEvents = UserTallyTime_Type%NumEvents + 1
         UserTallyTime_Type%EventName(UserTallyTime_Type%NumEvents) = User_EventName
      enddo

10    format('[TallyTime]  FATAL ERROR !!!')
20    format('[TallyTime]  Error reading the event name file!')
40    format('[TallyTime]  Event name in the name file could not be null!')
   end subroutine TallyTime_ReadEvent

!-------------------------------------------------------------------------------
!> Provide the numbers of integer, real and logical variables
!> in object UserTallyTime_Type
!-------------------------------------------------------------------------------
   subroutine TallyTime_GetStorage(UserTallyTime_Type,NumINT,NumREAL,NumLOG,NumCha,OUTunit)
      implicit none
      !Passed in
      type(TallyTime_Type) :: UserTallyTime_Type ! The user provided data set
      integer,intent(in) :: OUTunit
      !Passed out
      integer,intent(out) :: NumINT,NumREAL,NumLOG,NumCha

      NumINT = 8 + size(UserTallyTime_Type%NumTally)
      NUMREAL = 1 + size(UserTallyTime_Type%StartingClockTime)+size(UserTallyTime_Type%CumulativeTime)
      NumLOG =  1 + size(UserTallyTime_Type%TimingStatus)
      NumCha = size(UserTallyTime_Type%EventName)
   end subroutine TallyTime_GetStorage

!-------------------------------------------------------------------------------
!> Start the timing operation for a user selected event
!-------------------------------------------------------------------------------
   subroutine TallyTime_StartEvent(UserTallyTime_Type,User_EventNumber,OUTunit)
      implicit none
      ! Passed In
      type (TallyTime_Type),intent(inout) :: UserTallyTime_Type   ! The user provided data set
      integer,intent(in) :: User_EventNumber ! The index number of the timing operation
      integer,intent(in) :: OUTunit
      ! Local variables
      real(8) ClockTime

      if (UserTallyTime_Type%Defined) then
         ! event number in the right interval
         if ((User_EventNumber .GT. 0).AND.(User_EventNumber .LE. UserTallyTime_Type%NumEvents)) then
            ! Obtain the Total process Time
            call TT_GetProcessTime(ClockTime)
            ! Turn on the timer for the tally
            UserTallyTime_Type%StartingClockTime(User_EventNumber) = ClockTime
            ! Tally status: activated
            UserTallyTime_Type%TimingStatus(User_EventNumber)      = .TRUE.
            UserTallyTime_Type%NumTally(User_EventNumber)=UserTallyTime_Type%NumTally(User_EventNumber) + 1
         endif
      else
         write(*,10)
         write(*,20)
         write(OUTunit,10)
         write(OUTunit,20)
      endif

      !Output formats
10    format("[TallyTime]  FATAL ERROR !!!")
20    format("[TallyTime]  The TALLY OBJECT has not been defined!")

   end subroutine TallyTime_StartEvent

!-------------------------------------------------------------------------------
!> Stop the timing operation for a user selected event Without returning any
!> information
!-------------------------------------------------------------------------------
   subroutine TallyTime_StopEvent(UserTallyTime_Type,User_EventNumber,OUTunit)
      implicit none
      ! Passed In
      type (TallyTime_Type),intent(inout) :: UserTallyTime_Type   ! The user provided data set
      integer,intent(in) :: User_EventNumber  ! The index number of the timing operation
      integer,intent(in) :: OUTunit
      ! Local
      real(8) Cumulative,Incremental

      call TT_StopTimingEvent(UserTallyTime_Type,User_EventNumber,Cumulative,Incremental,OUTunit)

   end subroutine TallyTime_StopEvent

!-------------------------------------------------------------------------------
!> Stop the timing operation for a user selected event and return its cumulative
!> time
!-------------------------------------------------------------------------------
   subroutine TallyTime_StopReturnTotal(UserTallyTime_Type,User_EventNumber,TallyTime_Total,OUTunit)
      implicit none
      ! Passed In
      type (TallyTime_Type),intent(inout) :: UserTallyTime_Type   ! The user provided data set
      integer,intent(in) :: User_EventNumber ! The index number of the timing operation
      integer,intent(in) :: OUTunit
      ! Passed out
      real(8),intent(out) :: TallyTime_Total
      ! Local
      real(8) Cumulative,Incremental

      call TT_StopTimingEvent(UserTallyTime_Type,User_EventNumber,Cumulative,Incremental,OUTunit)
      TallyTime_Total = Cumulative

   end subroutine TallyTime_StopReturnTotal

   !-----------------------------------------------------------------------------------------------
   ! Stop the timing operation for a user selected event and return its incremental time between
   ! start and stop
   !-----------------------------------------------------------------------------------------------
   subroutine TallyTime_StopReturnIncrement(UserTallyTime_Type,User_EventNumber,TallyTime_Increment,&
                                              OUTunit)
      implicit none
      ! Passed In
      type (TallyTime_Type),intent(inout) :: UserTallyTime_Type   ! The user provided data set
      integer,intent(in) :: User_EventNumber ! The index number of the timing operation
      integer,intent(in) :: OUTunit
      ! Passed out
      real(8),intent(out) :: TallyTime_Increment
      ! Local
      real(8) Cumulative,Incremental

      call TT_StopTimingEvent(UserTallyTime_Type,User_EventNumber,Cumulative,Incremental,OUTunit)
      TallyTime_Increment = Cumulative
   end subroutine TallyTime_StopReturnIncrement

!-------------------------------------------------------------------------------
! Stops all timing operations
!-------------------------------------------------------------------------------
   subroutine TallyTime_StopAll(UserTallyTime_Type)
      implicit none
      ! Passed In
      type (TallyTime_Type),intent(inout) :: UserTallyTime_Type   ! The user provided data set
      ! Local
      integer I
      real(8) ClockTime

      if (UserTallyTime_Type%Defined) then
         call TT_GetProcessTime(ClockTime)
         do I = 1,UserTallyTime_Type%NumEvents
            ! Obtain the total process time
            ! Timer is on for this tally so add on tally Time
            if(UserTallyTime_Type%TimingStatus(I))then
               UserTallyTime_Type%CumulativeTime(I) =                                                 &
               UserTallyTime_Type%CumulativeTime(I) + ClockTime - UserTallyTime_Type%StartingClockTime(I)
            endif
            UserTallyTime_Type%TimingStatus(I)     = .FALSE.
         enddo
      endif

   end subroutine TallyTime_StopAll

!-------------------------------------------------------------------------------
!> Prints the timing , average, maximum, minimum tables of the timing
!> information on the system
!-------------------------------------------------------------------------------
   subroutine TallyTime_Print(UserTallyTime_Type,OUTunit,TotalProgramTime_in)
      implicit none
      ! Passed in variables
      type (TallyTime_Type),intent(in) :: UserTallyTime_Type  ! The user provided data set
      integer(kind=4),intent(in) :: OUTunit
      real(kind=8),optional,intent(in) :: TotalProgramTime_in ! pass in total time

      ! Local
      real(8) InverseTotalTime,TotalProgramTime,TotalWallClockTime
      type (SystemDate_Type) :: WallClock                     ! Wall clock Time
      real(8) ClockTime
      integer Event

      if (UserTallyTime_Type%Defined) then
         write(OUTunit,*)
         write(OUTunit,'(a)')'  >>>>>>>> Detailed timing information <<<<<<<<'
         write(OUTunit,100)
         ! Get the current process time and wall clock time
         call TT_GetProcessTime(ClockTime)
         !call TT_GetWallClock(WallClock)
         ! Compute the total program and wall clock time
         if(present(TotalProgramTime_in)) then
           TotalProgramTime = TotalProgramTime_in
         else
           TotalProgramTime = ClockTime - UserTallyTime_Type%ProgramTime
         endif
         !
         !call TT_ComputeWallClock(UserTallyTime_Type%WallClock,WallClock,TotalWallClockTime)
         ! Print the OS id based timing information
         write(OUTunit,'("  [TallyTime]...Timing history information for total job",56("."))')
         write(OUTunit,'("  [TallyTime]...Total cumulative program time(seconds)",42("."),F16.2)')         &
               TotalProgramTime
         ! Print the wall clock based information
         !write(OUTunit,200) UserTallyTime_Type%WallClock%month,UserTallyTime_Type%WallClock%DAY,   &
         !                   UserTallyTime_Type%WallClock%year,UserTallyTime_Type%WallClock%HOUR,   &
         !                   UserTallyTime_Type%WallClock%MINUTES,UserTallyTime_Type%WallClock%SECONDS
         !write(OUTunit,200) WallClock%month,WallClock%DAY,WallClock%year,WallClock%HOUR,           &
         !                   WallClock%MINUTES,WallClock%SECONDS
         !write(OUTunit,'("[TallyTime]...Total cumulative wall clock time",48("."),F16.2)')         &
         !                   TotalWallClockTime

         InverseTotalTime = 1.0D0
         if (TotalProgramTime .NE. 0.0D0) then
            InverseTotalTime = 100.0D0 / TotalProgramTime
         else
            InverseTotalTime = 0
         endif

         write(OUTunit,100)
         write(OUTunit,'("  [TallyTime]...",28("."),"Detailed timing information per event ..",28("."))')
         write(OUTunit,100)
         write(OUTunit,300)
         write(OUTunit,400)
         do Event = 1,UserTallyTime_Type%NumEvents
            if(UserTallyTime_Type%NumTally(Event).NE.0)then
               write(OUTunit,305) Event,UserTallyTime_Type%EventName(Event),   &
                                 UserTallyTime_Type%NumTally(Event),           &
                                 UserTallyTime_Type%CumulativeTime(Event)/     &
                                 UserTallyTime_Type%NumTally(Event),           &
                                 UserTallyTime_Type%CumulativeTime(Event),     &
                                 UserTallyTime_Type%CumulativeTime(Event)*InverseTotalTime
            else
               write(OUTunit,305) Event,UserTallyTime_Type%EventName(Event),   &
                                 UserTallyTime_Type%NumTally(Event),           &
                                 0.0,                                          &
                                 UserTallyTime_Type%CumulativeTime(Event),     &
                                 UserTallyTime_Type%CumulativeTime(Event)*InverseTotalTime
            endif
         enddo
         write(OUTunit,100)

      endif

100   format('  [TallyTime]',99('.'))
200   format('  [TallyTime]   Wall clock time',45('.'),'Date (',I2.2,'/',I2.2,'/',I4.2,              &
           ')Time (',I2.2,':',I2.2,':',I2.2,')')
300   format('  [TallyTime]..| ID |Event Name',30(' '),                                              &
             '|  History   |            Time(seconds)           |')
400   format('  [TallyTime]..|',4(' '),'|',40(' '),'|',12(' '),'|  Avg/Call  |','   Total   |',      &
             '  Percent  |')
305   format('  [TallyTime]..|',I4,'|',A40,'|',I12,'|',F12.2,'|',F11.2,'|',F11.2,'|')
   end subroutine TallyTime_Print

!-------------------------------------------------------------------------------
!> Copy variable and array values from object UserTallyTime_Type to object
!> UserTallyTime_TypePrime
!-------------------------------------------------------------------------------
   subroutine TallyTime_Copy(UserTallyTime_Type,UserTallyTime_TypePrime,OUTunit)
      implicit none
      ! Passed in
      type(TallyTime_Type),intent(inout) :: UserTallyTime_Type,UserTallyTime_TypePrime
      integer,intent(in)                 :: OUTunit
      ! Locals
      integer :: I
      integer :: NumName1

      ! Copy values if the object is defined
      if(UserTallyTime_TypePrime%Defined) then

         NumName1 = UserTallyTime_TypePrime%MaximumEvents
         call TallyTime_Define(UserTallyTime_TypePrime,NumName1,OUTunit)

         UserTallyTime_TypePrime%ProgramTime          = UserTallyTime_Type%ProgramTime
         UserTallyTime_TypePrime%NumEvents            = UserTallyTime_Type%NumEvents
         UserTallyTime_TypePrime%WallClock%year       = UserTallyTime_Type%WallClock%year
         UserTallyTime_TypePrime%WallClock%month      = UserTallyTime_Type%WallClock%month
         UserTallyTime_TypePrime%WallClock%DAY        = UserTallyTime_Type%WallClock%DAY
         UserTallyTime_TypePrime%WallClock%HOUR       = UserTallyTime_Type%WallClock%HOUR
         UserTallyTime_TypePrime%WallClock%MINUTES    = UserTallyTime_Type%WallClock%MINUTES
         UserTallyTime_TypePrime%WallClock%SECONDS    = UserTallyTime_Type%WallClock%SECONDS

         do I = 1,NumName1
            UserTallyTime_TypePrime%EventName(I)          = UserTallyTime_Type%EventName(I)
            UserTallyTime_TypePrime%StartingClockTime(I)  = UserTallyTime_Type%StartingClockTime(I)
            UserTallyTime_TypePrime%TimingStatus(I)       = UserTallyTime_Type%TimingStatus(I)
            UserTallyTime_TypePrime%CumulativeTime(I)     = UserTallyTime_Type%CumulativeTime(I)
            UserTallyTime_TypePrime%NumTally(I)    = UserTallyTime_Type%NumTally(I)
         enddo
      else
         write(*,10)
         write(*,20)
         write(OUTunit,10)
         write(OUTunit,20)
      end if
      ! Output formats
10    format("[TallyTime]  FATAL ERROR !!!")
20    format("[TallyTime] Sorry, UNDEFINED object UserTallyTime_TypePrime")
   end subroutine TallyTime_Copy
!-------------------------------------------------------------------------------
!  Auxilliary Subroutines Below:

!-------------------------------------------------------------------------------
! Define object UserTallyTime_Type and correlated initializations
!-------------------------------------------------------------------------------
   subroutine TallyTime_Define(UserTallyTime_Type,MaximumEvents,OUTunit)
      implicit none
      ! Passed in
      type (TallyTime_Type),intent(inout) :: UserTallyTime_Type  ! The user provided data set
      integer,intent(in) :: MaximumEvents, OUTunit                ! Maximum event number and output
                                                                  ! unit

      if (.NOT. UserTallyTime_Type%Defined) then  ! Do not allow reinitialization
         ! Allocate the pieces of the data structure
         call TT_Allocate(UserTallyTime_Type,MaximumEvents,OUTunit)
         ! Set the process clock starting time
         call TT_GetProcessTime(UserTallyTime_Type%ProgramTime) ! Sets the "Start" of the program Time
                                                            ! as the current process time
         ! Obtain the current wall clock
         call TT_GetWallClock(UserTallyTime_Type%WallClock)     ! Fill the data type with the current
                                                            ! wall clock time
      else
         write(*,10)
         write(*,20)
         write(OUTunit,10)
         write(OUTunit,20)
      endif

10    format('[TallyTime]  FATAL ERROR !!!')
20    format('[TallyTime]  Sorry, the object has ALREADY been defined!')
   end subroutine TallyTime_Define

!-------------------------------------------------------------------------------
! Void object UserTallyTime_Type
!-------------------------------------------------------------------------------
   subroutine TallyTime_Void(UserTallyTime_Type,OUTunit)
      implicit none
      type (TallyTime_Type) :: UserTallyTime_Type ! The user provided data set
      integer,intent(in) :: OUTunit
      ! Local
      integer IOS

      ! If the object has been defined, release the memory
      if (UserTallyTime_Type%Defined) then
         deallocate( UserTallyTime_Type%EventName,         &
                     UserTallyTime_Type%StartingClockTime, &
                     UserTallyTime_Type%CumulativeTime,    &
                     UserTallyTime_Type%TimingStatus,      &
                     UserTallyTime_Type%NumTally,   STAT=IOS)
         if (IOS .NE. 0) then
            write(*,10)
            write(OUTunit,10)
            stop
         endif
      endif
      ! redefine values
      UserTallyTime_Type%Defined        = .FALSE.
      UserTallyTime_Type%ProgramTime    = 0
      UserTallyTime_Type%MaximumEvents  = 0
      UserTallyTime_Type%NumEvents      = 0
10    format("[TallyTime]  Abnormal failure to deallocate memory in void")
   end subroutine TallyTime_Void

!-------------------------------------------------------------------------------
! Allocate arrays in the object
!-------------------------------------------------------------------------------
   subroutine TT_Allocate(UserTallyTime_Type,MaximumEvents,OUTunit)
      implicit none
      ! Passed in
      type (TallyTime_Type),intent(inout) :: UserTallyTime_Type ! The user provided data set
      integer,intent(in) :: MaximumEvents  ! Dimension
      integer,intent(in) :: OUTunit
      ! Local
      integer IOS
      type (TallyTime_Type) :: Temp_TallyTime ! Auxillary data set
      integer NumEvents,MaxEvents
      integer I

      if (UserTallyTime_Type%Defined) then
         call TallyTime_Void(UserTallyTime_Type,OUTunit)
      endif

      if(MaximumEvents.LE.0)then
         write(*,10)
         write(*,20) MaximumEvents
         write(OUTunit,10)
         write(OUTunit,20) MaximumEvents
      endif

      ! Allocate the object
      allocate(UserTallyTime_Type%EventName(MaximumEvents),           &
               UserTallyTime_Type%StartingClockTime(MaximumEvents),   &
               UserTallyTime_Type%CumulativeTime(MaximumEvents),      &
               UserTallyTime_Type%TimingStatus(MaximumEvents),        &
               UserTallyTime_Type%NumTally(MaximumEvents),            &
               STAT = IOS)

      ! Initialize the object
      UserTallyTime_Type%Defined       = .TRUE.
      UserTallyTime_Type%MaximumEvents = MaximumEvents
      do I =1,MaximumEvents
         UserTallyTime_Type%EventName(I)         = 'UNDEFINED'
         UserTallyTime_Type%StartingClockTime(I) = 0.0D0
         UserTallyTime_Type%TimingStatus(I)      = .FALSE.
         UserTallyTime_Type%CumulativeTime(I)    = 0.0D0
         UserTallyTime_Type%NumTally(I)   = 0
      enddo

      !Output formats
10    format("[TallyTime]  FATAL ERROR !!!")
20    format("[TallyTime]  WRONG number of MaximumEvents:",I5)

   end subroutine  TT_Allocate

!-------------------------------------------------------------------------------
!> Stop timing event and calculate cumulative and incremental time interval
!-------------------------------------------------------------------------------
   subroutine TT_StopTimingEvent(UserTallyTime_Type,User_EventNumber,Cumulative,Incremental,OUTunit)
      implicit none
      ! Passed In
      type (TallyTime_Type),intent(inout) :: UserTallyTime_Type    ! The user provided data set
      integer,intent(in) :: User_EventNumber     ! The index number of the timing operation
      integer,intent(in) :: OUTunit
      real(8),intent(out) :: Cumulative,Incremental

      Cumulative  = 0.0D0
      Incremental = 0.0D0
      if (UserTallyTime_Type%Defined) then
         if ((User_EventNumber .GT. 0) .AND. (User_EventNumber .LE. UserTallyTime_Type%NumEvents)) then
            if (UserTallyTime_Type%TimingStatus(User_EventNumber)) then  ! Timer is on for this event,
                                                                     ! so add on the measured time
               ! Obtain the Total process Time
               call TT_GetProcessTime(Incremental)
               ! time inteval since last activation
               Incremental = Incremental - UserTallyTime_Type%StartingClockTime(User_EventNumber)
               ! add to the total cumulative time for this event
               UserTallyTime_Type%CumulativeTime(User_EventNumber) =                                  &
               UserTallyTime_Type%CumulativeTime(User_EventNumber) + Incremental
            endif
            ! close the tally
            UserTallyTime_Type%TimingStatus(User_EventNumber) = .FALSE.
            Cumulative = UserTallyTime_Type%CumulativeTime(User_EventNumber)
         else
            write(*,10)
            write(*,20) User_EventNumber
            write(OUTunit,10)
            write(OUTunit,20) User_EventNumber
         endif
      endif

      !Output formats
10    format("[TallyTime]  FATAL ERROR !!!")
20    format("[TallyTime]  WRONG event number:",I5)

   end subroutine TT_StopTimingEvent

!-------------------------------------------------------------------------------
! Get the elapsed cpu Time in seconds for "this" process
!-------------------------------------------------------------------------------
   subroutine TT_GetProcessTime(CurrentTime)
      implicit none
      ! Passed Out
      real(8),intent(out) :: CurrentTime
      ! Local
      real(8) CumulativeTime

      CumulativeTime = 0.0D0
      call CPU_Time(CumulativeTime)   ! In theory this returns the Total cpu Time
      CurrentTime = CumulativeTime
   end subroutine  TT_GetProcessTime

!-------------------------------------------------------------------------------
! Gets the system date result and converts it into the data type SystemDate_Type
!-------------------------------------------------------------------------------
   subroutine TT_GetWallClock(WallClock)
      implicit none
      ! Passed in
      type (SystemDate_Type),intent(inout) :: WallClock
      ! Local
      integer IOS
      integer ReturnedValues(8)

      call DATE_AND_Time(VALUES=ReturnedValues)
      WallClock%year    = ReturnedValues(1)
      WallClock%month   = ReturnedValues(2)
      WallClock%DAY     = ReturnedValues(3)
      WallClock%HOUR    = ReturnedValues(5)
      WallClock%MINUTES = ReturnedValues(6)
      WallClock%SECONDS = ReturnedValues(7)
   end subroutine TT_GetWallClock

!-------------------------------------------------------------------------------------
! Computes the elapsed wall clock Time as a real number stored in variable "WallClock"
!-------------------------------------------------------------------------------------
   subroutine TT_ComputeWallClock(WallClockStart,WallClockEnd,ElapsedTime)
      implicit none
      ! Passed in
      type (SystemDate_Type),intent(in):: WallClockStart,WallClockEnd
      ! Passed out
      real(8),intent(out) :: ElapsedTime  ! The elapsed wall clock Time in seconds

      !Local
      ! first twelve elements for leap year
      ! the rest twelve elements for normal year
      integer :: DAYSPERmonth(24) = (/ 31,29,31,30,31,30,31,31,30,31,30,31,                       &
                                       31,28,31,30,31,30,31,31,30,31,30,31 /)
      integer :: TotalDAYS(2)       = (/366,365/)! Total number of days in leap year & normal year
      integer I,Start_yearTYPE,END_yearTYPE,StartDAY,ENDDAY
      real(8) DAYDIFF

      ElapsedTime = 0.0D0
      ! The month and day are converted to day positions in a given year

      ! Start of the Time measurement
      Start_yearTYPE = (WallClockStart%year - 2000)/ 4 ! only 4,8,12 will produce the correct result
      if (Start_yearTYPE*4 .EQ. WallClockStart%year - 2000) then
         !leap year
         Start_yearTYPE = 1
      else
         !normal year
         Start_yearTYPE = 2
      endif
      StartDAY = 0
      do I = 1,WallClockStart%month-1
         ! All of the days precending the current wall clock Time
         StartDAY = StartDAY + DAYSPERmonth(I+(Start_yearTYPE-1)*12)
      enddo
      StartDAY = StartDAY + WallClockStart%DAY  ! The current day of the year (1-365 or 1-366)

      ! End of the Time Tally
      END_yearTYPE = (WallClockEnd%year - 2000)/ 4  ! only 4,8,12 will produce the correct result
      if (END_yearTYPE*4 .EQ. WallClockEnd%year - 2000) then
         END_yearTYPE = 1
      else
         END_yearTYPE = 2
      endif

      ENDDAY = 0
      do I = 1,WallClockEnd%month-1
         ! All of the days precending the current wall clock Time
         ENDDAY = ENDDAY + DAYSPERmonth(I+(END_yearTYPE-1)*12)
      enddo
      ENDDAY = ENDDAY + WallClockEnd%DAY  ! The current day of the year (1-365 or 1-366)

      ! Compute the Total number of days seperating the two points
      if (WallClockEnd%year .EQ. WallClockStart%year) then          ! Same year
         DAYDIFF = (ENDDAY - StartDAY)
      elseif (WallClockEnd%year .GT. WallClockStart%year+1) then    ! Multiple years
         DAYDIFF = ENDDAY + TotalDAYS(Start_yearTYPE)-StartDAY +                                  &
                   (WallClockEnd%year-WallClockStart%year-1)*365.25D0
      else                                                          ! Overlapped a year
         DAYDIFF = ENDDAY + TotalDAYS(Start_yearTYPE)-StartDAY
      endif

      ElapsedTime = 86400.0D0*DAYDIFF                                         &
                  +  3600.0D0*(WallClockEnd%HOUR    - WallClockStart%HOUR)    &
                  +    60.0D0*(WallClockEnd%MINUTES - WallClockStart%MINUTES) &
                  +           (WallClockEnd%SECONDS - WallClockStart%SECONDS)
   end subroutine TT_ComputeWallClock

end module TallyTime