!
! This work (Common Community Physics Package Framework), identified by
! NOAA, NCAR, CU/CIRES, is free of known copyright restrictions and is
! placed in the public domain.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
! THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
! IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
! CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


!>
!! @brief Auto-generated cap for mpas calls to CCPP API
!!
!
module mpas_ccpp_cap


   use ccpp_kinds
   use mpas_atmphys_vars,      only: rqvblten_p
   use mpas_atmphys_vars,      only: qi_p
   use mpas_atmphys_vars,      only: pi_p
   use mpas_atmphys_vars,      only: rublten_p
   use mpas_atmphys_vars,      only: ctopo_p
   use mpas_atmphys_vars,      only: hfx_p
   use mpas_atmphys_vars,      only: dz_p
   use mpas_atmphys_constants, only: ep_1
   use mpas_atmphys_vars,      only: wspd_p
   use mpas_atmphys_vars,      only: pres_hyd_p
   use mpas_atmphys_vars,      only: delta_p
   use mpas_atmphys_vars,      only: rqcblten_p
   use mpas_atmphys_vars,      only: wstar_p
   use mpas_atmphys_constants, only: rcp
   use mpas_atmphys_vars,      only: rqiblten_p
   use mpas_atmphys_vars,      only: its
   use mpas_atmphys_vars,      only: rthblten_p
   use mpas_atmphys_vars,      only: ysu_pblmix
   use mpas_constants,   only: gravity
   use mpas_atmphys_constants, only: karman
   use mpas_atmphys_vars,      only: qfx_p
   use mpas_atmphys_vars,      only: qc_p
   use mpas_atmphys_constants, only: ep_2
   use mpas_constants,   only: cp
   use mpas_atmphys_constants, only: xlv
   use mpas_atmphys_vars,      only: t_p
   use mpas_atmphys_vars,      only: f_qi
   use mpas_atmphys_vars,      only: kpbl_p
   use mpas_atmphys_vars,      only: u_p
   use mpas_atmphys_vars,      only: v_p
   use mpas_atmphys_vars,      only: kzh_p
   use mpas_atmphys_vars,      only: hpbl_p
   use mpas_atmphys_constants, only: R_v
   use mpas_atmphys_vars,      only: ite
   use mpas_atmphys_vars,      only: rvblten_p
   use mpas_atmphys_vars,      only: kzm_p
   use mpas_atmphys_vars,      only: kms
   use mpas_atmphys_vars,      only: kte
   use mpas_atmphys_vars,      only: ust_p
   use mpas_atmphys_constants, only: R_d
   use mpas_atmphys_vars,      only: voce_p
   use mpas_atmphys_vars,      only: kts
   use mpas_atmphys_vars,      only: pres2_hyd_p
   use mpas_atmphys_vars,      only: f_qc
   use mpas_atmphys_vars,      only: qv_p
   use mpas_atmphys_vars,      only: psfc_p
   use mpas_atmphys_vars,      only: dt_pbl
   use mpas_atmphys_vars,      only: br_p
   use mpas_atmphys_vars,      only: znt_p
   use mpas_atmphys_vars,      only: u10_p
   use mpas_atmphys_vars,      only: ctopo2_p
   use mpas_atmphys_vars,      only: v10_p
   use mpas_atmphys_vars,      only: psih_p
   use mpas_atmphys_vars,      only: kme
   use mpas_atmphys_vars,      only: uoce_p
   use mpas_atmphys_vars,      only: rthraten_p
   use mpas_atmphys_vars,      only: psim_p
   use mpas_atmphys_vars,      only: xland_p
   use mpas_atmphys_constants, only: rdg
   
   implicit none
   private
   
   
   public :: mpas_ccpp_physics_timestep_initial
   public :: mpas_ccpp_physics_timestep_final
   public :: mpas_ccpp_physics_initialize
   public :: mpas_ccpp_physics_finalize
   public :: mpas_ccpp_physics_run
   public :: ccpp_physics_suite_list
   public :: ccpp_physics_suite_part_list

contains

   
   subroutine mpas_ccpp_physics_timestep_initial(suite_name, j, errmsg, errflg)
   
      use ccpp_mpas_suite_cap, only: mpas_suite_timestep_initial
   
      character(len=*),   intent(in)    :: suite_name
      integer,            intent(inout) :: j
      character(len=*),   intent(inout) :: errmsg
      integer,            intent(inout) :: errflg

      errflg = 0
      errmsg = ""
      if (trim(suite_name) == 'mpas_suite') then
         call mpas_suite_timestep_initial(errflg, errmsg)
      else
         write(errmsg, '(3a)')"No suite named ", trim(suite_name), "found"
         errflg = 1
      end if
   
   end subroutine mpas_ccpp_physics_timestep_initial
   
   
   subroutine mpas_ccpp_physics_timestep_final(suite_name, j, errmsg, errflg)
   
      use ccpp_mpas_suite_cap, only: mpas_suite_timestep_final
   
      character(len=*),   intent(in)    :: suite_name
      integer,            intent(inout) :: j
      character(len=*),   intent(inout) :: errmsg
      integer,            intent(inout) :: errflg

      errflg = 0
      errmsg = ""
      if (trim(suite_name) == 'mpas_suite') then
         call mpas_suite_timestep_final(errflg, errmsg)
      else
         write(errmsg, '(3a)')"No suite named ", trim(suite_name), "found"
         errflg = 1
      end if
   
   end subroutine mpas_ccpp_physics_timestep_final
   
   
   subroutine mpas_ccpp_physics_initialize(suite_name, j, errmsg, errflg)
   
      use ccpp_mpas_suite_cap, only: mpas_suite_initialize
   
      character(len=*),   intent(in)    :: suite_name
      integer,            intent(inout) :: j
      character(len=*),   intent(inout) :: errmsg
      integer,            intent(inout) :: errflg

      errflg = 0
      errmsg = ""
      if (trim(suite_name) == 'mpas_suite') then
         call mpas_suite_initialize(errflg, errmsg)
      else
         write(errmsg, '(3a)')"No suite named ", trim(suite_name), "found"
         errflg = 1
      end if
   
   end subroutine mpas_ccpp_physics_initialize
   
   
   subroutine mpas_ccpp_physics_finalize(suite_name, j, errmsg, errflg)
   
      use ccpp_mpas_suite_cap, only: mpas_suite_finalize
   
      character(len=*),   intent(in)    :: suite_name
      integer,            intent(inout) :: j
      character(len=*),   intent(inout) :: errmsg
      integer,            intent(inout) :: errflg

      errflg = 0
      errmsg = ""
      if (trim(suite_name) == 'mpas_suite') then
         call mpas_suite_finalize(errflg, errmsg)
      else
         write(errmsg, '(3a)')"No suite named ", trim(suite_name), "found"
         errflg = 1
      end if
   
   end subroutine mpas_ccpp_physics_finalize
   
   
   subroutine mpas_ccpp_physics_run(suite_name, suite_part, j, errmsg, errflg)
   
      use ccpp_mpas_suite_cap, only: mpas_suite_physics
   
      character(len=*),   intent(in)    :: suite_name
      character(len=*),   intent(in)    :: suite_part
      integer,            intent(inout) :: j
      character(len=*),   intent(inout) :: errmsg
      integer,            intent(inout) :: errflg

      errflg = 0
      errmsg = ""
      if (trim(suite_name) == 'mpas_suite') then
         if (trim(suite_part) == 'physics') then
            call mpas_suite_physics(errflg, errmsg, j, u_p, v_p, t_p, qv_p, qc_p, qi_p,           &
                 pres_hyd_p, pres2_hyd_p, pi_p, f_qc, f_qi, rublten_p, rvblten_p, rthblten_p,     &
                 rqvblten_p, rqcblten_p, rqiblten_p, cp, gravity, rcp, R_d, rdg, ep_1, ep_2,      &
                 karman, xlv, R_v, dz_p, psfc_p, znt_p, ust_p, hpbl_p, psim_p, psih_p, xland_p,   &
                 hfx_p, qfx_p, wspd_p, br_p, dt_pbl, kpbl_p, kzh_p, kzm_p, wstar_p, delta_p,      &
                 u10_p, v10_p, uoce_p, voce_p, rthraten_p, ysu_pblmix, ctopo_p, ctopo2_p, its,    &
                 ite, kts, kte, kms, kme)
         else
            write(errmsg, '(3a)')"No suite part named ", trim(suite_part),                        &
                 " found in suite mpas_suite"
            errflg = 1
         end if
      else
         write(errmsg, '(3a)')"No suite named ", trim(suite_name), "found"
         errflg = 1
      end if
   
   end subroutine mpas_ccpp_physics_run
   
   subroutine ccpp_physics_suite_list(suites)
      character(len=*), allocatable, intent(out) :: suites(:)
      
      integer                                    :: sindex
      
      allocate(suites(1))
      do sindex = 1, 1
         suites(sindex) = 'mpas_suite'
      end do
   end subroutine ccpp_physics_suite_list
   
   subroutine ccpp_physics_suite_part_list(suite_name, part_list, errmsg, errflg)
      character(len=*),              intent(in)  :: suite_name
      character(len=*), allocatable, intent(out) :: part_list(:)
      character(len=*),   intent(out)   :: errmsg
      integer,            intent(out)   :: errflg
      
      integer                                   :: pindex
      
      if(trim(suite_name) == 'mpas_suite') then
         allocate(part_list(1))
         
         do pindex = 1, 1
            part_list(pindex) = 'physics'
         end do
      else
         write(errmsg, '(3a)')'No suite named ', trim(suite_name), ' found'
         errflg = 1
      end if
   end subroutine ccpp_physics_suite_part_list

end module mpas_ccpp_cap

