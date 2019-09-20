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
!! @brief Auto-generated cap module for the CCPP suite
!!
!
module ccpp_mpas_suite_cap

   use ccpp_kinds
   implicit none
   private
   
   ! Suite interfaces
   
   character(len=16) :: ccpp_suite_state = 'uninitialized'
   
   public :: mpas_suite_initialize
   public :: mpas_suite_timestep_initial
   public :: mpas_suite_physics
   public :: mpas_suite_timestep_final
   public :: mpas_suite_finalize
   
   ! Private suite variables

contains
   
   subroutine mpas_suite_initialize(errflg, errmsg)
   
      use bl_ysu, only: bl_ysu_init

      ! Dummy arguments
      integer,            intent(out)   :: errflg
      character(len=512), intent(out)   :: errmsg

      if (trim(ccpp_suite_state) /= 'uninitialized') then
         errflg = 1
         write(errmsg, '(3a)') "Invalid initial CCPP state, '", trim(ccpp_suite_state),           &
              "' in mpas_suite_initialize"
         return
      end if
      if (errflg == 0) then
         call bl_ysu_init(errmsg, errflg)
      end if
      ccpp_suite_state = 'initialized'
   
   end subroutine mpas_suite_initialize
   
   
   subroutine mpas_suite_timestep_initial(errflg, errmsg)
   

      ! Dummy arguments
      integer,            intent(out)   :: errflg
      character(len=512), intent(out)   :: errmsg

      if (trim(ccpp_suite_state) /= 'initialized') then
         errflg = 1
         write(errmsg, '(3a)') "Invalid initial CCPP state, '", trim(ccpp_suite_state),           &
              "' in mpas_suite_timestep_initial"
         return
      end if
      ccpp_suite_state = 'in_time_step'
   
   end subroutine mpas_suite_timestep_initial
   
   
   subroutine mpas_suite_physics(errflg, errmsg, j, ux, vx, tx, qvx, qcx, qix, p2d, p2di, pi2d,   &
        f_qc, f_qi, utnp, vtnp, ttnp, qvtnp, qctnp, qitnp, cp, g, rovcp, rd, rovg, ep1, ep2,      &
        karman, xlv, rv, dz8w2d, psfcpa, znt, ust, hpbl, psim, psih, xland, hfx, qfx, wspd, br,   &
        dt, kpbl1d, exch_hx, exch_mx, wstar, delta, u10, v10, uox, vox, rthraten,                 &
        ysu_topdown_pblmix, ctopo, ctopo2, its, ite, kts, kte, kms, kme)
   
      use bl_ysu, only: bl_ysu_run

      ! Dummy arguments
      integer,            intent(out)   :: errflg
      character(len=512), intent(out)   :: errmsg
      integer,            intent(in)    :: j
      real(kind_phys),    intent(in)    :: ux(:,:)
      real(kind_phys),    intent(in)    :: vx(:,:)
      real(kind_phys),    intent(in)    :: tx(:,:)
      real(kind_phys),    intent(in)    :: qvx(:,:)
      real(kind_phys),    intent(in)    :: qcx(:,:)
      real(kind_phys),    intent(in)    :: qix(:,:)
      real(kind_phys),    intent(in)    :: p2d(:,:)
      real(kind_phys),    intent(in)    :: p2di(:,:)
      real(kind_phys),    intent(in)    :: pi2d(:,:)
      logical,            intent(in)    :: f_qc
      logical,            intent(in)    :: f_qi
      real(kind_phys),    intent(out)   :: utnp(:,:)
      real(kind_phys),    intent(out)   :: vtnp(:,:)
      real(kind_phys),    intent(out)   :: ttnp(:,:)
      real(kind_phys),    intent(out)   :: qvtnp(:,:)
      real(kind_phys),    intent(out)   :: qctnp(:,:)
      real(kind_phys),    intent(out)   :: qitnp(:,:)
      real(kind_phys),    intent(in)    :: cp
      real(kind_phys),    intent(in)    :: g
      real(kind_phys),    intent(in)    :: rovcp
      real(kind_phys),    intent(in)    :: rd
      real(kind_phys),    intent(in)    :: rovg
      real(kind_phys),    intent(in)    :: ep1
      real(kind_phys),    intent(in)    :: ep2
      real(kind_phys),    intent(in)    :: karman
      real(kind_phys),    intent(in)    :: xlv
      real(kind_phys),    intent(in)    :: rv
      real(kind_phys),    intent(in)    :: dz8w2d(:,:)
      real(kind_phys),    intent(in)    :: psfcpa(:)
      real(kind_phys),    intent(in)    :: znt(:)
      real(kind_phys),    intent(in)    :: ust(:)
      real(kind_phys),    intent(out)   :: hpbl(:)
      real(kind_phys),    intent(in)    :: psim(:)
      real(kind_phys),    intent(in)    :: psih(:)
      real(kind_phys),    intent(in)    :: xland(:)
      real(kind_phys),    intent(in)    :: hfx(:)
      real(kind_phys),    intent(in)    :: qfx(:)
      real(kind_phys),    intent(in)    :: wspd(:)
      real(kind_phys),    intent(in)    :: br(:)
      real(kind_phys),    intent(in)    :: dt
      integer,            intent(out)   :: kpbl1d(:)
      real(kind_phys),    intent(out)   :: exch_hx(:,:)
      real(kind_phys),    intent(out)   :: exch_mx(:,:)
      real(kind_phys),    intent(out)   :: wstar(:)
      real(kind_phys),    intent(out)   :: delta(:)
      real(kind_phys),    intent(inout) :: u10(:)
      real(kind_phys),    intent(inout) :: v10(:)
      real(kind_phys),    intent(in)    :: uox(:)
      real(kind_phys),    intent(in)    :: vox(:)
      real(kind_phys),    intent(in)    :: rthraten(:,:)
      integer,            intent(in)    :: ysu_topdown_pblmix
      real(kind_phys),    intent(in)    :: ctopo(:)
      real(kind_phys),    intent(in)    :: ctopo2(:)
      integer,            intent(in)    :: its
      integer,            intent(in)    :: ite
      integer,            intent(in)    :: kts
      integer,            intent(in)    :: kte
      integer,            intent(in)    :: kms
      integer,            intent(in)    :: kme

      if (trim(ccpp_suite_state) /= 'in_time_step') then
         errflg = 1
         write(errmsg, '(3a)') "Invalid initial CCPP state, '", trim(ccpp_suite_state),           &
              "' in mpas_suite_physics"
         return
      end if
      if (errflg == 0) then
         call bl_ysu_run(j, ux, vx, tx, qvx, qcx, qix, p2d, p2di, pi2d, f_qc, f_qi, utnp, vtnp,   &
              ttnp, qvtnp, qctnp, qitnp, cp, g, rovcp, rd, rovg, ep1, ep2, karman, xlv, rv,       &
              dz8w2d, psfcpa, znt, ust, hpbl, psim, psih, xland, hfx, qfx, wspd, br, dt, kpbl1d,  &
              exch_hx, exch_mx, wstar, delta, u10, v10, uox, vox, rthraten, ysu_topdown_pblmix,   &
              ctopo, ctopo2, its, ite, kts, kte, kms, kme, errmsg, errflg)
      end if
      ccpp_suite_state = 'in_time_step'
   
   end subroutine mpas_suite_physics
   
   
   subroutine mpas_suite_timestep_final(errflg, errmsg)
   

      ! Dummy arguments
      integer,            intent(out)   :: errflg
      character(len=512), intent(out)   :: errmsg

      if (trim(ccpp_suite_state) /= 'in_time_step') then
         errflg = 1
         write(errmsg, '(3a)') "Invalid initial CCPP state, '", trim(ccpp_suite_state),           &
              "' in mpas_suite_timestep_final"
         return
      end if
      ccpp_suite_state = 'initialized'
   
   end subroutine mpas_suite_timestep_final
   
   
   subroutine mpas_suite_finalize(errflg, errmsg)
   
      use bl_ysu, only: bl_ysu_finalize

      ! Dummy arguments
      integer,            intent(out)   :: errflg
      character(len=512), intent(out)   :: errmsg

      if (trim(ccpp_suite_state) /= 'initialized') then
         errflg = 1
         write(errmsg, '(3a)') "Invalid initial CCPP state, '", trim(ccpp_suite_state),           &
              "' in mpas_suite_finalize"
         return
      end if
      if (errflg == 0) then
         call bl_ysu_finalize(errmsg, errflg)
      end if
      ccpp_suite_state = 'uninitialized'
   
   end subroutine mpas_suite_finalize
   

end module ccpp_mpas_suite_cap

