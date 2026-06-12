!=======================================================================
!
! This submodule initializes the IO subroutines
!
! Author: L. Zampieri ( lorenzo.zampieri@awi.de )
!
!=======================================================================

submodule (icedrv_main) icedrv_io

  use icepack_intfc, only: &
       icepack_query_parameters, icepack_query_tracer_flags, icepack_query_tracer_sizes, &
       icepack_query_tracer_indices, icepack_warnings_flush, icepack_warnings_aborted                  
  use icedrv_system, only: icedrv_system_abort
 
contains

  !
  !
  !_________________________________________________________________________
  ! define mean IO output of icepack 
  module subroutine ini_mean_icepack_io(mesh)
    
    use mod_mesh
    use io_meandata, only: def_stream
    
    implicit none

    type(t_mesh), target, intent(in) :: mesh

    integer (kind=int_kind)   :: &
         i, j, k, n,             & ! do loop counter
         nt_Tsfc, nt_sice, nt_qice, nt_qsno, nt_apnd, nt_hpnd, nt_ipnd, nt_alvl,    &
         nt_vlvl, nt_iage, nt_FY, nt_aero, ktherm, nt_fbri, nt_smice, nt_smliq,     &
         nt_rhos, nt_rsnw, iost

    integer, save :: &
         nm_io_unit  = 102, nm_icepack_unit = 103, io_listsize=0
    character(500)    :: longname, trname, units
        
    logical (kind=log_kind) :: &
         skl_bgc, z_tracers, tr_iage, tr_FY, tr_lvl, tr_aero, tr_pond_topo, tr_pond_lvl, tr_pond_sealvl, &
         tr_snow, tr_brine, tr_bgc_N, tr_bgc_C, tr_bgc_Nit, tr_bgc_Sil, tr_bgc_DMS, tr_bgc_chl, &
         tr_bgc_Am, tr_bgc_PON, tr_bgc_DON, tr_zaero, tr_bgc_Fe, tr_bgc_hum

    type io_entry
       character(len=10)        :: id        ='unknown   '
       integer                  :: freq      =0
       character                :: unit      =''
       integer                  :: precision =0
    end type io_entry
        
    type(io_entry), save, allocatable, target   :: io_list_icepack(:)

    namelist /nml_general       / io_listsize
    namelist /nml_list_icepack  / io_list_icepack        

#include "associate_mesh.h"

    ! Get the tracers information from icepack
    call icepack_query_parameters( &
         skl_bgc_out=skl_bgc, z_tracers_out=z_tracers, ktherm_out=ktherm)
    call icepack_query_tracer_flags( &
         tr_iage_out=tr_iage, tr_FY_out=tr_FY, tr_lvl_out=tr_lvl,  tr_aero_out=tr_aero,        &
         tr_pond_topo_out=tr_pond_topo, tr_pond_lvl_out=tr_pond_lvl, tr_pond_sealvl_out=tr_pond_sealvl, &
         tr_snow_out=tr_snow,                                                                  &
         tr_brine_out=tr_brine, tr_bgc_N_out=tr_bgc_N, tr_bgc_C_out=tr_bgc_C,                  &
         tr_bgc_Nit_out=tr_bgc_Nit, tr_bgc_Sil_out=tr_bgc_Sil, tr_bgc_DMS_out=tr_bgc_DMS,      &
         tr_bgc_chl_out=tr_bgc_chl, tr_bgc_Am_out=tr_bgc_Am, tr_bgc_PON_out=tr_bgc_PON,        &
         tr_bgc_DON_out=tr_bgc_DON, tr_zaero_out=tr_zaero, tr_bgc_Fe_out=tr_bgc_Fe,            &
         tr_bgc_hum_out=tr_bgc_hum)
    call icepack_query_tracer_indices( &
         nt_apnd_out=nt_apnd, nt_hpnd_out=nt_hpnd, nt_ipnd_out=nt_ipnd, nt_alvl_out=nt_alvl,   &
         nt_vlvl_out=nt_vlvl, nt_Tsfc_out=nt_Tsfc, nt_iage_out=nt_iage, nt_FY_out=nt_FY,       &
         nt_qice_out=nt_qice, nt_sice_out=nt_sice, nt_fbri_out=nt_fbri, nt_aero_out=nt_aero,   &
         nt_qsno_out=nt_qsno, nt_smice_out=nt_smice, nt_smliq_out=nt_smliq,                    &
         nt_rhos_out=nt_rhos, nt_rsnw_out=nt_rsnw)

    !_______________________________________________________________________
    ! OPEN and read namelist.io --> need to extract variable io_listsize
    open( unit=nm_io_unit, file='namelist.io', form='formatted', access='sequential', status='old', iostat=iost )
    if (iost == 0) then
       if (mype==0) write(*,*) '     file   : ', 'namelist.io',' open ok'
    else
       if (mype==0) write(*,*) 'ERROR: --> bad opening file   : ','namelist.io',' ;    iostat=',iost
       call par_ex(p_partit%MPI_COMM_FESOM, p_partit%mype)
       stop
    end if
        
    ! read list_size from namelist.io for allocation
    read(nm_io_unit, nml=nml_general, iostat=iost )
    close(nm_io_unit)
    allocate(io_list_icepack(io_listsize))
        
    !_______________________________________________________________________
    ! OPEN and read namelist.icepack --> need to extract io_list_icepack
    open( unit=nm_icepack_unit, file='namelist.icepack', form='formatted', access='sequential', status='old', iostat=iost )
    if (iost == 0) then
       if (mype==0) write(*,*) '     file   : ', 'namelist.icepack',' open ok' 
    else
       if (mype==0) write(*,*) 'ERROR: --> bad opening file   : ','namelist.icepack',' ;    iostat=',iost
       call par_ex(p_partit%MPI_COMM_FESOM, p_partit%mype)
       stop
    end if
        
    ! read io_list_icepack from namelist to fill up what has been previously 
    ! allocated --> allocate(io_list_icepack(io_listsize))
    read(nm_icepack_unit, nml=nml_list_icepack, iostat=iost )
    close(nm_icepack_unit)
    
    !_______________________________________________________________________
    ! reduce running index to the number that is actually filt up 
    do i=1, io_listsize
       if (trim(io_list_icepack(i)%id)=='unknown   ') then
          if (mype==0) write(*,*) 'io_listsize will be changed from ', io_listsize, ' to ', i-1, '!'
          io_listsize=i-1
          exit
       end if
    end do
        
    !_______________________________________________________________________
    ! define output streams
    do i=1, io_listsize
       select case (trim(io_list_icepack(i)%id))
       ! Ice category independent variables
       case ('aice0     ')
          call def_stream(nod2D, nx_nh, 'aice0', 'open water fraction', 'none', aice0(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('aice      ')
          call def_stream(nod2D, nx_nh, 'aice', 'sea ice concentration', 'none', aice(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh) 
       case ('vice      ')
          call def_stream(nod2D, nx_nh, 'vice', 'volume per unit area of ice', 'm', vice(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('vsno      ')
          call def_stream(nod2D, nx_nh, 'vsno', 'volume per unit area of snow', 'm', vsno(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('uvel      ')
          ! Ice u-velocity component
          call def_stream(nod2D, nx_nh, 'uvel', 'x-component of ice velocity', 'm/s', uvel(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('vvel      ')
          ! Ice v-velocity component
          call def_stream(nod2D, nx_nh, 'vvel', 'y-component of ice velocity', 'm/s', vvel(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('Tsfc      ')
          ! Ice or snow surface temperature
          call def_stream(nod2D, nx_nh, 'Tsfc', 'sea ice surf. temperature', 'degC', trcr(:,nt_Tsfc), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('Tf        ')
          ! Sea surface freezing temperature
          call def_stream(nod2D, nx_nh, 'Tf', 'sea freez. temp.', 'degC', Tf(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('qice      ')
          do k = 1,nilyr  ! Separate variable for each sea ice layer
             write(trname,'(A5,i1)') 'qice_', k
             write(longname,'(A22,i1)') 'sea ice enthalpy lyr: ', k 
             units='J/m3'
             call def_stream(nod2D, nx_nh, trim(trname), trim(longname), trim(units), trcr(:,nt_qice+k-1), &
                  io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
          end do
       case ('sice      ')
          do k = 1,nilyr  ! Separate variable for each sea ice layer
             write(trname,'(A5,i1)') 'sice_', k
             write(longname,'(A22,i1)') 'sea ice salinity lyr: ', k
             units='psu'
             call def_stream(nod2D, nx_nh, trim(trname), trim(longname), trim(units), trcr(:,nt_sice+k-1), &
                  io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
          end do
       case ('qsno      ')
          do k = 1,nslyr  ! Separate variable for each snow layer
             write(trname,'(A5,i1)') 'qsno_', k
             write(longname,'(A19,i1)') 'snow enthalpy lyr: ', k
             units='J/m3'
             call def_stream(nod2D, nx_nh, trim(trname), trim(longname), trim(units), trcr(:,nt_qsno+k-1), &
                  io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
          end do
       case ('strength  ') 
          call def_stream(nod2D, nx_nh, 'strength', 'sea ice strength', 'N', strength(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('rdg_conv  ')
          call def_stream(nod2D, nx_nh, 'rdg_conv', 'Convergence term for ridging', '1/s', rdg_conv(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('rdg_shear ')
          call def_stream(nod2D, nx_nh, 'rdg_shear', 'Shear term for ridging', '1/s' , rdg_shear(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('fbot ')
          call def_stream(nod2D, nx_nh, 'fbot', 'net ice bottom heat flux', 'W/m^2' , fbot(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('fsurf ')
          call def_stream(nod2D, nx_nh, 'fsurf', 'net ice/snow surface heat flux', 'W/m^2' , fsurf(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('fcondtop ')
          call def_stream(nod2D, nx_nh, 'fcondtop', 'ice/snow surface conductive flux', 'W/m^2' , fcondtop(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('fcondbot ')
          call def_stream(nod2D, nx_nh, 'fcondbot', 'ice bottom conductive flux', 'W/m^2' , fcondbot(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('fsens ')
          call def_stream(nod2D, nx_nh, 'fsens', 'sensitive heat flux', 'W/m^2' , fsens(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('flat ')
          call def_stream(nod2D, nx_nh, 'flat', 'latent heat flux', 'W/m^2' , flat(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('fhocn ')
          call def_stream(nod2D, nx_nh, 'fhocn', 'net heat flux into ocean', 'W/m^2' , fhocn(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('Tbot ')
          call def_stream(nod2D, nx_nh, 'Tbot', 'ice bottom temperature', 'degC' , Tbot(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('dsnow ')
          call def_stream(nod2D, nx_nh, 'dsnow', 'change in snow_depth', 'm' , dsnow(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('congel ')
          call def_stream(nod2D, nx_nh, 'congel', 'basal ice growth rate', 'm/time_step' , congel(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('snoice ')
          call def_stream(nod2D, nx_nh, 'snoice', 'snow ice formation rate', 'm/time_step' , snoice(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('frazil ')
          call def_stream(nod2D, nx_nh, 'frazil', 'frazil ice growth rate', 'm/time_step' , frazil(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('meltt ')
          call def_stream(nod2D, nx_nh, 'meltt', 'surface ice melt rate', 'm/time_step' , meltt(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('meltb ')
          call def_stream(nod2D, nx_nh, 'meltb', 'bottom ice melt rate', 'm/time_step' , meltb(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('melts ')
          call def_stream(nod2D, nx_nh, 'melts', 'surface snow melt rate', 'm/time_step' , melts(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('meltl ')
          call def_stream(nod2D, nx_nh, 'meltl', 'lateral ice melt rate', 'm/time_step' , meltl(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('rside ')
          call def_stream(nod2D, nx_nh, 'rside', 'ice fraction lateral melt', 'none' , rside(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('daidtt ')
          call def_stream(nod2D, nx_nh, 'daidtt', 'thermodynamic ice area tendency', '1/s' , daidtt(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('dvidtt ')
          call def_stream(nod2D, nx_nh, 'dvidtt', 'thermodynamic ice volume tentendy', 'm/s' , dvidtt(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('dagedtt ')
          call def_stream(nod2D, nx_nh, 'dagedtt', 'thermodynamic ice age tendency', 's/s' , dagedtt(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('mlt_onset ')
          call def_stream(nod2D, nx_nh, 'mlt_onset', 'day of year that surface melt begins', 'dayofyear' , mlt_onset(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('frz_onset ')
          call def_stream(nod2D, nx_nh, 'frz_onset', 'day of year that freezing begins (congel or frazil)', 'dayofyear' , frz_onset(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('albocn ')
          call def_stream(nod2D, nx_nh, 'albocn', 'open ocean albedo', 'none' , albocn2D(:), &
	       io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('albice ')
          call def_stream(nod2D, nx_nh, 'albice', 'bare ice albedo', 'none' , albice(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('albsno ')
          call def_stream(nod2D, nx_nh, 'albsno', 'snow albedo', 'none' , albsno(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('snowfrac ')
          call def_stream(nod2D, nx_nh, 'snowfrac', 'snow fraction used in radiation', 'none' , snowfrac(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('apeff_ai ')
          call def_stream(nod2D, nx_nh, 'apeff_ai', 'effective pond area used for radiation calculation', 'none' , apeff_ai(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('alvdr ')
          call def_stream(nod2D, nx_nh, 'alvdr', 'visible, direct (fraction)', 'none' , alvdr(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('alvdf ')
          call def_stream(nod2D, nx_nh, 'alvdf', 'visible, diffuse (fraction)', 'none' , alvdf(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('alidr ')
          call def_stream(nod2D, nx_nh, 'alidr', 'near-IR, direct (fraction)', 'none' , alidr(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       case ('alidf ')
          call def_stream(nod2D, nx_nh, 'alidf', 'near-IR, diffuse (fraction)', 'none' , alidf(:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
       ! Ice category dependent (3D) variables    
       case ('aicen     ')
          call def_stream((/ncat, nod2D/), (/ncat, nx_nh/), 'aicen', 'sea ice concentration', 'none', aicen(:,:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh, .true.) 
       case ('vicen     ')
          call def_stream((/ncat, nod2D/), (/ncat, nx_nh/), 'vicen', 'volume per unit area of ice' , 'm', vicen(:,:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh, .true.)
       case ('vsnon     ')
          call def_stream((/ncat, nod2D/), (/ncat, nx_nh/), 'vsnon', 'volume per unit area of snow', 'm', vsnon(:,:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh, .true.)
       case ('Tsfcn     ')
          call def_stream((/ncat, nod2D/), (/ncat, nx_nh/), 'Tsfcn', 'sea ice surf. temperature', 'degC', trcrn(:,nt_Tsfc,:), &
               io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh, .true.)
       !
       ! there will be no output, if the specific tracers are not defined (i.e. 'tr_xxx = .true.' in 'namelist.icepack')
       !
       ! Ice category independent tracer variables
       case ('iage      ')
          if (tr_iage) then
             call def_stream(nod2D, nx_nh, 'iage', 'sea ice age', 's', trcr(:,nt_iage), &
                  io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
          end if
       case ('FY        ')
          if (tr_FY) then 
             call def_stream(nod2D, nx_nh, 'FY', 'first year ice', 'none', trcr(:,nt_FY), &
                  io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
          end if
       case ('lvl       ')
          if (tr_lvl) then
             call def_stream(nod2D, nx_nh, 'alvl', 'level ice area', 'none', trcr(:,nt_alvl), &
                  io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
             call def_stream(nod2D, nx_nh, 'vlvl', 'level ice volume', 'm', trcr(:,nt_vlvl), &
                  io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
          end if
       case ('pond_topo ')
          if (tr_pond_topo) then
             call def_stream(nod2D, nx_nh, 'apnd', 'melt pond area fraction', 'none', trcr(:,nt_apnd), &
                  io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
             call def_stream(nod2D, nx_nh, 'hpnd', 'melt pond depth', 'm', trcr(:,nt_hpnd), &
                  io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
             call def_stream(nod2D, nx_nh, 'ipnd', 'melt pond refrozen lid thickness', 'm', trcr(:,nt_ipnd), &
                  io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
          end if
       case ('pond_lvl  ')
          if (tr_pond_lvl) then
             call def_stream(nod2D, nx_nh, 'apnd', 'melt pond area fraction', 'none', trcr(:,nt_apnd), &
                  io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
             call def_stream(nod2D, nx_nh, 'hpnd', 'melt pond depth', 'm', trcr(:,nt_hpnd), &
                  io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
             call def_stream(nod2D,  nx_nh, 'ipnd', 'melt pond refrozen lid thickness', 'm', trcr(:,nt_ipnd), &
                  io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
          end if
       case ('pond_slvl ')
          if (tr_pond_sealvl) then
             call def_stream(nod2D, nx_nh, 'apnd', 'melt pond area fraction', 'none', trcr(:,nt_apnd), &
                  io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
             call def_stream(nod2D, nx_nh, 'hpnd', 'melt pond depth', 'm', trcr(:,nt_hpnd), &
                  io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
             call def_stream(nod2D,  nx_nh, 'ipnd', 'melt pond refrozen lid thickness', 'm', trcr(:,nt_ipnd), &
                  io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
          end if

! add pond diagnostic !!! frank.kauker@awi.de
!
!      real (kind=dbl_kind), &
!          dimension (nx,ncat), public :: &
!          ! Like melttn these are defined as volume per unit category area
!          dpnd_flushn,   & ! category pond flushing rate due to ice permeability
!          dpnd_exponn,   & ! category exponential pond drainage rate
!          dpnd_freebdn,  & ! category pond drainage rate due to freeboard constraint
!          dpnd_initialn, & ! category runoff rate due to rfrac (m/step)
!          dpnd_dlidn       ! category pond loss/gain due to ice lid (m/step)
! 
!       real (kind=dbl_kind), dimension (nx), public :: &
!          dpnd_flush,    & ! pond flushing rate due to ice permeability (m/step)
!          dpnd_expon,    & ! exponential pond drainage rate (m/step)
!          dpnd_freebd,   & ! pond drainage rate due to freeboard constraint (m/step)
!          dpnd_initial,  & ! runoff rate due to rfrac (m/step)
!          dpnd_dlid,     & ! pond loss/gain (+/-) to ice lid freezing/melting (m/step)
!          dpnd_melt,     & ! pond 'drainage' due to ice melting (m / step)
!          dpnd_ridge       ! pond drainage due to ridging (m / step)


       case ('brine     ')
          if (tr_brine) then
             call def_stream(nod2D, nx_nh, 'fbri', 'volume fraction of ice with dynamic salt', 'none', trcr(:,nt_fbri), &
                  io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
          end if
       case ('smliq     ')
          if (tr_snow) then ! snow tracer quantities
             do k = 1,nslyr  ! Separate variable for each snow layer
                write(trname,'(A6,i1)') 'smliq_', k
                write(longname,'(A30,i1)') 'volume of liquid in snow lyr: ', k
                units='m3/m2'
                call def_stream(nod2D, nx_nh, trim(trname), trim(longname), trim(units), smliqtot(:,k), &
                     io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
             end do
          end if
       case ('smice     ')
          if (tr_snow) then ! snow tracer quantities
             do k = 1,nslyr  ! Separate variable for each snow layer
                write(trname,'(A6,i1)') 'smice_', k
                write(longname,'(A27,i1)') 'volume of ice in snow lyr: ', k
                units='m3/m2'
                call def_stream(nod2D, nx_nh, trim(trname), trim(longname), trim(units), smicetot(:,k), &
                     io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
             end do
          end if
      case ('rsnw      ')
         if (tr_snow) then ! snow tracer quantities
            do k = 1,nslyr  ! Separate variable for each snow layer
               write(trname,'(A5,i1)') 'rsnw_', k
               write(longname,'(A34,i1)') 'average snow grain radius in lyr: ', k
               units='mum'
               call def_stream(nod2D, nx_nh, trim(trname), trim(longname), trim(units), rsnwavg(:,k), &
                    io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
            end do
         end if
       case ('rhos      ')
          if (tr_snow) then ! snow tracer quantities
             do k = 1,nslyr  ! Separate variable for each snow layer
                write(trname,'(A5,i1)') 'rhos_', k
                write(longname,'(A39,i1)') 'average compacted snow density in lyr: ', k
                units='kg/m3'
                call def_stream(nod2D, nx_nh, trim(trname), trim(longname), trim(units), rhosavg(:,k), &
                     io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh)
             end do
          end if
       ! Category dependent (3D) tracer variables
       case ('iagen     ')
          if (tr_iage) then
             call def_stream((/ncat, nod2D/), (/ncat, nx_nh/), 'iagen', 'sea ice age', 's', trcrn(:,nt_iage,:), &
                  io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh, .true.)
          end if
       case ('FYn       ')
          if (tr_FY) then 
             call def_stream((/ncat, nod2D/), (/ncat, nx_nh/), 'FYn', 'first year ice', 'none', trcrn(:,nt_FY,:), &
                  io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh, .true.)
          end if
       case ('lvln      ')
          if (tr_lvl) then
             call def_stream((/ncat, nod2D/), (/ncat, nx_nh/), 'alvln', 'level ice area', 'none', trcrn(:,nt_alvl,:), &
                  io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh, .true.)
             call def_stream((/ncat, nod2D/), (/ncat, nx_nh/), 'vlvln', 'level ice volume', 'm', trcrn(:,nt_vlvl,:), &
                  io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh, .true.)
          end if
       case ('pond_topon')
          if (tr_pond_topo) then
             call def_stream((/ncat, nod2D/), (/ncat, nx_nh/), 'apndn', 'melt pond area fraction', 'none', trcrn(:,nt_apnd,:), &
                  io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh, .true.)
             call def_stream((/ncat, nod2D/), (/ncat, nx_nh/), 'hpndn', 'melt pond depth', 'm', trcrn(:,nt_hpnd,:), &
                  io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh, .true.)
             call def_stream((/ncat, nod2D/), (/ncat, nx_nh/), 'ipndn', 'melt pond refrozen lid thickness', 'm', trcrn(:,nt_ipnd,:), &
                  io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh, .true.)
          end if
       case ('pond_lvln ')
          if (tr_pond_lvl) then
             call def_stream((/ncat, nod2D/), (/ncat, nx_nh/), 'apndn', 'melt pond area fraction', 'none', trcrn(:,nt_apnd,:), &
                  io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh, .true.)
             call def_stream((/ncat, nod2D/), (/ncat, nx_nh/), 'hpndn', 'melt pond depth', 'm', trcrn(:,nt_hpnd,:), &
                  io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh, .true.)
             call def_stream((/ncat, nod2D/), (/ncat, nx_nh/), 'ipndn', 'melt pond refrozen lid thickness', 'm', trcrn(:,nt_ipnd,:), &
                  io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh, .true.)
          end if
       case ('pond_slvln')
          if (tr_pond_sealvl) then
             call def_stream((/ncat, nod2D/), (/ncat, nx_nh/), 'apndn', 'melt pond area fraction', 'none', trcrn(:,nt_apnd,:), &
                  io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh, .true.)
             call def_stream((/ncat, nod2D/), (/ncat, nx_nh/), 'hpndn', 'melt pond depth', 'm', trcrn(:,nt_hpnd,:), &
                  io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh, .true.)
             call def_stream((/ncat, nod2D/), (/ncat, nx_nh/), 'ipndn', 'melt pond refrozen lid thickness', 'm', trcrn(:,nt_ipnd,:), &
                  io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh, .true.)
          end if
       case ('brinen    ')
          if (tr_brine) then
             call def_stream((/ncat, nod2D/),  (/ncat, nx_nh/), 'fbrin', 'volume frac. of ice with dyn. salt', 'none', trcrn(:,nt_fbri,:), &
                  io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh, .true.)
          end if
       case ('qicen     ')
          do k = 1,nilyr  ! Separate variable for each sea ice layer
             write(trname,'(A6,i1)') 'qicen_', k
             write(longname,'(A22,i1)') 'sea ice enthalpy lyr: ', k 
             units='J/m3'
             call def_stream((/ncat, nod2D/),  (/ncat, nx_nh/), trim(trname), trim(longname), trim(units), trcrn(:,nt_qice+k-1,:), &
                  io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh, .true.)
          end do
       case ('sicen     ')
          do k = 1,nilyr  ! Separate variable for each sea ice layer
             write(trname,'(A6,i1)') 'sicen_', k
             write(longname,'(A22,i1)') 'sea ice salinity lyr: ', k
             units='psu'
             call def_stream((/ncat, nod2D/),  (/ncat, nx_nh/), trim(trname), trim(longname), trim(units), trcrn(:,nt_sice+k-1,:), &
                  io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh, .true.)
          end do
       case ('qsnon     ')
          do k = 1,nslyr  ! Separate variable for each snow layer
             write(trname,'(A6,i1)') 'qsnon_', k
             write(longname,'(A19,i1)') 'snow enthalpy lyr: ', k
             units='J/m3'
             call def_stream((/ncat, nod2D/),  (/ncat, nx_nh/), trim(trname), trim(longname), trim(units), trcrn(:,nt_qsno+k-1,:), &
                  io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh, .true.)
          end do
       case ('smliqn    ')
          if (tr_snow) then ! snow tracer quantities
             do k = 1,nslyr  ! Separate variable for each snow layer
                write(trname,'(A7,i1)') 'smliqn_', k
                write(longname,'(A27,i1)') 'tracer liquid in snow lyr: ', k
                units='none'
                call def_stream((/ncat, nod2D/), (/ncat, nx_nh/), trim(trname), trim(longname), trim(units), trcrn(:,nt_smliq+k-1,:), &
                     io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh, .true.)
             end do
          end if
       case ('smicen    ')
          if (tr_snow) then ! snow tracer quantities
             do k = 1,nslyr  ! Separate variable for each snow layer
                write(trname,'(A7,i1)') 'smicen_', k
                write(longname,'(A24,i1)') 'tracer ice in snow lyr: ', k
                units='none'
                call def_stream((/ncat, nod2D/), (/ncat, nx_nh/), trim(trname), trim(longname), trim(units), trcrn(:,nt_smice+k-1,:), &
                     io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh, .true.)
             end do
          end if
       case ('rsnwn     ')
          if (tr_snow) then ! snow tracer quantities
             do k = 1,nslyr  ! Separate variable for each snow layer
                write(trname,'(A6,i1)') 'rsnwn_', k
                write(longname,'(A23,i1)') 'snow grain radius lyr: ', k
                units='mum'
                call def_stream((/ncat, nod2D/),  (/ncat, nx_nh/), trim(trname), trim(longname), trim(units), trcrn(:,nt_rsnw+k-1,:), &
                     io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh, .true.)
             end do
          end if
       case ('rhosn     ')
          if (tr_snow) then ! snow tracer quantities
             do k = 1,nslyr  ! Separate variable for each snow layer
                write(trname,'(A6,i1)') 'rhosn_', k
                write(longname,'(A28,i1)') 'compacted snow density lyr: ', k
                units='kg/m3'
                call def_stream((/ncat, nod2D/),  (/ncat, nx_nh/), trim(trname), trim(longname), trim(units), trcrn(:,nt_rhos+k-1,:), &
                     io_list_icepack(i)%freq, io_list_icepack(i)%unit, io_list_icepack(i)%precision, p_partit, mesh, .true.)
             end do
          end if
       case default
          if (mype==0) write(*,*) 'stream ', io_list_icepack(i)%id, ' is not defined !'
       end select
    end do ! --> do i=1, io_listsize

  end subroutine ini_mean_icepack_io

  !
  !
  !___________________________________________________________________________
  ! define mean IO output of icepack 
  module subroutine ini_icepack_io(year, partit, mesh)
    
    use mod_mesh
    use mod_partit
    use mod_parsup
    use g_config,     only: runid, ResultPath
    use io_restart,   only: icepack_files, icepack_path
    
    implicit none
    
    type(t_mesh)  , intent(in)   , target :: mesh
    type(t_partit), intent(inout), target :: partit
    integer       , intent(in)            :: year
    logical       , save                  :: has_been_called = .false.
    
    integer (kind=int_kind)   :: &
         i, k,                   & ! counter
         nt_Tsfc, nt_sice, nt_qice, nt_qsno, nt_apnd, nt_hpnd, nt_ipnd, nt_alvl,    &
         nt_vlvl, nt_iage, nt_FY, nt_aero, ktherm, nt_fbri, nt_smice, nt_smliq,     &
         nt_rhos, nt_rsnw

    character(500)            :: longname, trname, units
    character(4)              :: cyear
    
    logical (kind=log_kind)   :: &
         skl_bgc, z_tracers, tr_iage, tr_FY, tr_lvl, tr_aero, tr_pond_topo, tr_pond_lvl, tr_pond_sealvl, &
         tr_snow, tr_brine, tr_bgc_N, tr_bgc_C, tr_bgc_Nit, tr_bgc_Sil, tr_bgc_DMS, tr_bgc_chl,          &
         tr_bgc_Am, tr_bgc_PON, tr_bgc_DON, tr_zaero, tr_bgc_Fe, tr_bgc_hum
    
#include "associate_mesh.h"^
    
    ! Get the tracers information from icepack
    call icepack_query_parameters( &
         skl_bgc_out=skl_bgc, z_tracers_out=z_tracers, ktherm_out=ktherm)
    call icepack_query_tracer_flags( &
         tr_iage_out=tr_iage, tr_FY_out=tr_FY, tr_lvl_out=tr_lvl,  tr_aero_out=tr_aero,        &
         tr_pond_topo_out=tr_pond_topo, tr_pond_lvl_out=tr_pond_lvl, tr_pond_sealvl_out=tr_pond_sealvl, &
         tr_snow_out=tr_snow,                                                                  &
         tr_brine_out=tr_brine, tr_bgc_N_out=tr_bgc_N, tr_bgc_C_out=tr_bgc_C,                  &
         tr_bgc_Nit_out=tr_bgc_Nit, tr_bgc_Sil_out=tr_bgc_Sil, tr_bgc_DMS_out=tr_bgc_DMS,      &
         tr_bgc_chl_out=tr_bgc_chl, tr_bgc_Am_out=tr_bgc_Am, tr_bgc_PON_out=tr_bgc_PON,        &
         tr_bgc_DON_out=tr_bgc_DON, tr_zaero_out=tr_zaero, tr_bgc_Fe_out=tr_bgc_Fe,            &
         tr_bgc_hum_out=tr_bgc_hum)
    call icepack_query_tracer_indices( &
         nt_apnd_out=nt_apnd, nt_hpnd_out=nt_hpnd, nt_ipnd_out=nt_ipnd, nt_alvl_out=nt_alvl,   &
         nt_vlvl_out=nt_vlvl, nt_Tsfc_out=nt_Tsfc, nt_iage_out=nt_iage, nt_FY_out=nt_FY,       &
         nt_qice_out=nt_qice, nt_sice_out=nt_sice, nt_fbri_out=nt_fbri, nt_aero_out=nt_aero,   &
         nt_qsno_out=nt_qsno, nt_smice_out=nt_smice, nt_smliq_out=nt_smliq,   	      	       &
         nt_rhos_out=nt_rhos, nt_rsnw_out=nt_rsnw)

    call icepack_warnings_flush(nu_diag)
        ! The following error message needs to be fixed
        !if (icepack_warnings_aborted()) call abort_ice(error_message=subname,       &
        !    file=__FILE__, line=__LINE__)
      
    write(cyear,'(i4)') year
    ! Create an icepack restart file
    ! Only serial output implemented so far
    icepack_path=trim(ResultPath)//trim(runid)//'.'//cyear//'.icepack.restart.nc'
        
    if(has_been_called) return
    has_been_called = .true.
        
    ! Define the netCDF variables for surface
    ! and vertically constant fields
      
    !-----------------------------------------------------------------
    ! 2D and 3D (ncat) restart fields
    !-----------------------------------------------------------------
    call icepack_files%def_node_var('aice'     , 'sea ice concentration'                        , 'none', aice(:)           , mesh, partit)
    call icepack_files%def_node_var('vice'     , 'volum per unit area of ice'                   , 'm'   , vice(:)           , mesh, partit)
    call icepack_files%def_node_var('vsno'     , 'volum per unit area of snow'                  , 'm'   , vsno(:)           , mesh, partit)
    
    call icepack_files%def_node_var('aicen'     , 'sea ice concentration per class'             , 'none', aicen(:,:)        , mesh, partit, ncat)
    call icepack_files%def_node_var('vicen'     , 'volum per unit area of ice per class'        , 'm'   , vicen(:,:)        , mesh, partit, ncat)
    call icepack_files%def_node_var('vsnon'     , 'volum per unit area of snow per class'       , 'm'   , vsnon(:,:)        , mesh, partit, ncat)
    call icepack_files%def_node_var('Tsfc'      , 'sea ice surf. temperature'                   , 'degC', trcrn(:,nt_Tsfc,:), mesh, partit, ncat)
    call icepack_files%def_node_var('uvel'      , 'zonal component of ice velocity'             , 'm/s' , uvel(:)           , mesh, partit)
    call icepack_files%def_node_var('vvel'      , 'meridional component of ice velocity'        , 'm/s' , vvel(:)           , mesh, partit)
    
    if (tr_iage) then
       call icepack_files%def_node_var('iage'  , 'sea ice age'                                 , 's'   , trcrn(:,nt_iage,:), mesh, partit, ncat)
    end if
    
    if (tr_FY) then
       call icepack_files%def_node_var('FY'    , 'first year ice'                              , 'none', trcrn(:,nt_FY,:)  , mesh, partit, ncat)
    end if
    
    if (tr_lvl) then
       call icepack_files%def_node_var('alvl'  , 'ridged sea ice area'                         , 'none', trcrn(:,nt_alvl,:), mesh, partit, ncat)
       call icepack_files%def_node_var('vlvl'  , 'ridged sea ice volume'                       , 'm'   , trcrn(:,nt_vlvl,:), mesh, partit, ncat)
    end if
    if (tr_pond_topo) then
       call icepack_files%def_node_var('apnd'  , 'melt pond area fraction'                     , 'none', trcrn(:,nt_apnd,:), mesh, partit, ncat)
       call icepack_files%def_node_var('hpnd'  , 'melt pond depth'                             , 'm'   , trcrn(:,nt_hpnd,:), mesh, partit, ncat)
       call icepack_files%def_node_var('ipnd'  , 'melt pond refrozen lid thickness'            , 'm'   , trcrn(:,nt_ipnd,:), mesh, partit, ncat)
    end if
    if (tr_pond_lvl) then
       call icepack_files%def_node_var('apnd'  , 'melt pond area fraction'                     , 'none', trcrn(:,nt_apnd,:), mesh, partit, ncat)
       call icepack_files%def_node_var('hpnd'  , 'melt pond depth'                             , 'm'   , trcrn(:,nt_hpnd,:), mesh, partit, ncat)
       call icepack_files%def_node_var('ipnd'  , 'melt pond refrozen lid thickness'            , 'm'   , trcrn(:,nt_ipnd,:), mesh, partit, ncat)
       call icepack_files%def_node_var('ffracn', 'fraction of fsurfn over pond used to melt ipond', 'none', ffracn, mesh, partit, ncat)
       call icepack_files%def_node_var('dhsn'  , 'difference of snow depth on sea ice and pond ice', 'm', dhsn  , mesh, partit, ncat)
    end if
    if (tr_pond_sealvl) then
       call icepack_files%def_node_var('apnd'  , 'melt pond area fraction'                     , 'none', trcrn(:,nt_apnd,:), mesh, partit, ncat)
       call icepack_files%def_node_var('hpnd'  , 'melt pond depth'                             , 'm'   , trcrn(:,nt_hpnd,:), mesh, partit, ncat)
       call icepack_files%def_node_var('ipnd'  , 'melt pond refrozen lid thickness'            , 'm'   , trcrn(:,nt_ipnd,:), mesh, partit, ncat)
       call icepack_files%def_node_var('fsnow' , 'snowfall rate'                               , 'kg/m^2/s', fsnow, mesh, partit) 
       call icepack_files%def_node_var('ffracn', 'fraction of fsurfn over pond used to melt ipond', 'none', ffracn, mesh, partit, ncat)
       call icepack_files%def_node_var('dhsn'  , 'difference of snow depth on sea ice and pond ice', 'm', dhsn, mesh, partit, ncat)
    end if
    
    if (tr_brine) then
       call icepack_files%def_node_var('fbri'  ,     'volume fraction of ice with dynamic salt', 'none', trcrn(:,nt_fbri,:), mesh, partit, ncat)
       call icepack_files%def_node_var('first_ice', 'distinguishes ice that disappears'        , 'logical', first_ice_real(:,:), mesh, partit, ncat)
    end if
      
    !-----------------------------------------------------------------
    ! 4D restart fields, are stored as 3D layers
    !-----------------------------------------------------------------
      
    ! Ice
    do k = 1,nilyr
       write(trname,'(A6,i1)') 'sicen_', k
       write(longname,'(A21,i1)') 'sea ice salinity lyr:', k
       units='psu'
       call icepack_files%def_node_var(trim(trname), trim(longname), trim(units), trcrn(:,nt_sice+k-1,:), mesh, partit, ncat)
       write(trname,'(A6,i1)') 'qicen_', k
       write(longname,'(A21,i1)') 'sea ice enthalpy lyr:', k
       units='J/m3'
       call icepack_files%def_node_var(trim(trname), trim(longname), trim(units), trcrn(:,nt_qice+k-1,:), mesh, partit, ncat)
    end do
      
    ! Snow
    do k = 1,nslyr
       write(trname,'(A6,i1)') 'qsnon_', k
       write(longname,'(A18,i1)') 'snow enthalpy lyr:', k
       units='J/m3'
       call icepack_files%def_node_var(trim(trname), trim(longname), trim(units), trcrn(:,nt_qsno+k-1,:), mesh, partit, ncat)
       if (tr_snow) then
          write(trname,'(A7,i1)') 'smicen_', k
          write(longname,'(A21,i1)') 'ice mass in snow lyr:', k
          units='none'
          call icepack_files%def_node_var(trim(trname), trim(longname), trim(units), trcrn(:,nt_smice+k-1,:), mesh, partit, ncat)
          write(trname,'(A7,i1)') 'smliqn_', k
          write(longname,'(A24,i1)') 'liquid mass in snow lyr:', k
          units='none'
          call icepack_files%def_node_var(trim(trname), trim(longname), trim(units), trcrn(:,nt_smliq+k-1,:), mesh, partit, ncat)
          write(trname,'(A6,i1)') 'rhosn_', k
          write(longname,'(A27,i1)') 'effective snow density lyr:', k
          units='kg/m3/m2'
          call icepack_files%def_node_var(trim(trname), trim(longname), trim(units), trcrn(:,nt_rhos+k-1,:), mesh, partit, ncat)
          write(trname,'(A6,i1)') 'rsnwn_', k
          write(longname,'(A15,i1)') 'snow radius lyr:', k
          units='mum'
          call icepack_files%def_node_var(trim(trname), trim(longname), trim(units), trcrn(:,nt_rsnw+k-1,:), mesh, partit, ncat)
       end if
    end do
      
    !
    ! All the other 4D tracers (linked to aerosols and biogeochemistry) are at the
    ! moment not supported for restart. This might change if someone is interested
    ! in using the biogeochemistry modules. At this stage, I do not know the model
    ! enough to use these options. Lorenzo Zampieri - 16/10/2019.
    !
      
  end subroutine ini_icepack_io

end submodule icedrv_io
