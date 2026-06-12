!=======================================================================

! Diagnostic information output during run
!
! author: Tony Craig

module icedrv_system

  use icedrv_kinds
  use icedrv_constants, only: ice_stderr
  ! use icedrv_main, only: aice ! does not work because icedrv_system is not defined as a submodule - frank.kauker@awi.de
  use icepack_intfc, only: icepack_warnings_flush, icepack_warnings_aborted

  ! FESOM2 modules
  use mod_parsup, only: par_ex
  use mod_partit
  
  implicit none
  
  public :: icedrv_system_abort, icedrv_system_init, icedrv_system_flush
  
  private
  type(t_partit), save, pointer :: p_partit ! a pointer to the mesh partitioning (has been accessed via "use g_parsup" in the original code)

  !=======================================================================
  
contains
  
  !=======================================================================
  ! prints error information prior to aborting
  
  subroutine icedrv_system_abort(icell, istep, string, file, line)
    
    integer (kind=int_kind), intent(in), optional :: &
         icell       , & ! indices of grid cell where model aborts
         istep       , & ! time step number
         line            ! line number
    
    character (len=*), intent(in), optional :: string, file
    
    ! local variables
    
    character(len=*), parameter :: subname='(icedrv_system_abort)'
    
    write(ice_stderr,*) ' '
    
    call icepack_warnings_flush(ice_stderr)
    
    write(ice_stderr,*) ' '
    write(ice_stderr,*) subname,' ABORTED: '
    if (present(file))   write (ice_stderr,*) subname,' called from ', trim(file)
    if (present(line))   write (ice_stderr,*) subname,' line number',  line
    if (present(istep))  write (ice_stderr,*) subname,' istep =',      istep
    ! if (present(icell))  write (ice_stderr,*) subname,' i, aice =',    icell, aice(icell) - see above
    if (present(string)) write (ice_stderr,*) subname,' string =',     trim(string)
    
    ! Stop FESOM2
    
    call par_ex(p_partit%MPI_COMM_FESOM, p_partit%mype, 1)
    
    stop

  end subroutine icedrv_system_abort
    
  !=======================================================================
  ! flushes iunit IO buffer
  
  subroutine icedrv_system_flush(iunit)
    
    integer (kind=int_kind), intent(in) :: &
         iunit        ! unit number to flush
    
    ! local variables
    
    character(len=*), parameter :: subname='(icedrv_system_flush)'
    
#ifndef NO_F2003
    flush(iunit)
#endif   
    
  end subroutine icedrv_system_flush
  
  !=======================================================================
  subroutine icedrv_system_init(partit)
    implicit none
    type(t_partit), intent(inout), target :: partit
    
    p_partit => partit
  end subroutine icedrv_system_init
  
  !=======================================================================
  
end module icedrv_system

!=======================================================================
