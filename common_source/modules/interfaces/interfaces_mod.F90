!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
!hd|====================================================================
!hd|  interfaces_mod                modules/interfaces/interfaces_mod.f
!hd|-- called by -----------
!hd|        ddsplit                       starter/source/restart/ddsplit/ddsplit.f
!hd|        hm_read_interfaces            starter/source/interfaces/reader/hm_read_interfaces.f
!hd|        hm_read_inter_struct          starter/source/interfaces/reader/hm_read_inter_struct.f
!hd|        hm_read_thgrou                starter/source/output/th/hm_read_thgrou.f
!hd|        initia                        starter/source/elements/initia/initia.f
!hd|        lecins                        starter/source/interfaces/interf1/lecins.f
!hd|        lecint                        starter/source/interfaces/interf1/lecint.f
!hd|        lectur                        starter/source/starter/lectur.f
!hd|        i24mainf                      engine/source/interfaces/int24/i24main.f
!hd|        i25mainf                      engine/source/interfaces/int25/i25mainf.f
!hd|        imp_inttd0                    engine/source/implicit/imp_int_k.f
!hd|        imp_solv                      engine/source/implicit/imp_solv.f
!hd|        imp_tripi                     engine/source/implicit/imp_int_k.f
!hd|        intfop2                       engine/source/interfaces/interf/intfop2.f
!hd|        inttri                        engine/source/interfaces/intsort/inttri.f
!hd|        radioss2                      engine/source/engine/radioss2.f
!hd|        rdresa                        engine/source/output/restart/rdresa.f
!hd|        rdresb                        engine/source/output/restart/rdresb.f
!hd|        resol                         engine/source/engine/resol.f
!hd|        resol_head                    engine/source/engine/resol_head.f
!hd|        restalloc                     engine/source/output/restart/arralloc.f
!hd|        spmd_i7fcom_poff              engine/source/mpi/forces/spmd_i7fcom_poff.f
!hd|        spmd_i7fcom_pon               engine/source/mpi/forces/spmd_i7fcom_pon.f
!hd|        spmd_i7xvcom2                 engine/source/mpi/interfaces/spmd_i7xvcom2.f
!hd|        wrrestp                       engine/source/output/restart/wrrestp.f
!hd|-- calls ---------------
!hd|        parameters_mod                modules/interfaces/parameters_mod.f
!hd|        spmd_arrays_mod               modules/interfaces/spmd_arrays_mod.f
!hd|====================================================================
module interfaces_mod
!-----------------------------------------------
!   m o d u l e s
!-----------------------------------------------
   use parameters_mod
   use spmd_arrays_mod
   use intbufdef_mod
   use intbuf_fric_mod
   use parith_on_mod
   implicit none
!-----------------------------------------------
!   m y _ r e a l
!-----------------------------------------------
#include      "my_real.inc"


! IPARI indexes
      integer, parameter :: INDEX_JINBUF     =                    1
      integer, parameter :: INDEX_JBUFIN     =                    2
      integer, parameter :: INDEX_NRTS       =                    3
      integer, parameter :: INDEX_NRTM       =                    4
      integer, parameter :: INDEX_NSN        =                    5
      integer, parameter :: INDEX_NMN        =                    6
      integer, parameter :: INDEX_NTY        =                    7
      integer, parameter :: INDEX_ITYPE      =                    7
      integer, parameter :: INDEX_NST        =                    8
      integer, parameter :: INDEX_NMT        =                    9
      integer, parameter :: INDEX_JINSCR     =                   10
      integer, parameter :: INDEX_IBC        =                   11
      integer, parameter :: INDEX_IBUC       =                   12
      integer, parameter :: INDEX_IDEF       =                   13
      integer, parameter :: INDEX_IVSIZ      =                   14
      integer, parameter :: INDEX_NOINT      =                   15
      integer, parameter :: INDEX_IDEL       =                   17
      integer, parameter :: INDEX_NCONT      =                   18
      integer, parameter :: INDEX_ISINT      =                   19
      integer, parameter :: INDEX_ILEV       =                   20
      integer, parameter :: INDEX_IGAP       =                   21
      integer, parameter :: INDEX_INACTI     =                   22
      integer, parameter :: INDEX_MULTIMP    =                   23
      integer, parameter :: INDEX_NSNR       =                   24
      integer, parameter :: INDEX_IRS        =                   25
      integer, parameter :: INDEX_HIERA      =                   26
      integer, parameter :: INDEX_IADFIN     =                   27
      integer, parameter :: INDEX_INTSEC     =                   28
      integer, parameter :: INDEX_ICONT      =                   29
      integer, parameter :: INDEX_MFROT      =                   30
      integer, parameter :: INDEX_IFQ        =                   31
      integer, parameter :: INDEX_IBAG       =                   32
      integer, parameter :: INDEX_ILAGM      =                   33
      integer, parameter :: INDEX_IGSTI      =                   34
      integer, parameter :: INDEX_USRPID     =                   35
      integer, parameter :: INDEX_NISUB      =                   36
      integer, parameter :: INDEX_NISUBS     =                   37
      integer, parameter :: INDEX_NISUBM     =                   38
      integer, parameter :: INDEX_ICURV      =                   39
      integer, parameter :: INDEX_NA1        =                   40
      integer, parameter :: INDEX_NA2        =                   41
      integer, parameter :: INDEX_ISYM       =                   42
      integer, parameter :: INDEX_IUBUF      =                   43                            
      integer, parameter :: INDEX_IADM       =                   44                  
      integer, parameter :: INDEX_ISU1       =                   45
      integer, parameter :: INDEX_ISU2       =                   46
      integer, parameter :: INDEX_INTTH      =                   47                      
      integer, parameter :: INDEX_IFORM      =                   48                         
      integer, parameter :: INDEX_NRADM      =                   49                         
      integer, parameter :: INDEX_IFNOR      =                   50                      
      integer, parameter :: INDEX_NLINS      =                   51
      integer, parameter :: INDEX_NLINS1     =                   51
      integer, parameter :: INDEX_MLINM      =                   52
      integer, parameter :: INDEX_NLINS2     =                   52
      integer, parameter :: INDEX_NLINSA     =                   53
      integer, parameter :: INDEX_MLINMA     =                   54
      integer, parameter :: INDEX_NSNE       =                   55
      integer, parameter :: INDEX_NMNE       =                   56
      integer, parameter :: INDEX_NSNER      =                   57
      integer, parameter :: INDEX_IEDGE      =                   58
      integer, parameter :: INDEX_LINE1      =                   59
      integer, parameter :: INDEX_LINE2      =                   60
      integer, parameter :: INDEX_IDELKEEP   =                   61
      integer, parameter :: INDEX_NREMNODE   =                   62
      integer, parameter :: INDEX_FLAGREMNODE=                   63
      integer, parameter :: INDEX_IDSENS     =                   64
      integer, parameter :: INDEX_INTKG      =                   65
      integer, parameter :: INDEX_INTPLY     =                   66
      integer, parameter :: INDEX_NADMSR     =                   67
      integer, parameter :: INDEX_NEDGE      =                   68
      integer, parameter :: INDEX_INTFRIC    =                   72
      integer, parameter :: INDEX_NREMNOR    =                   81
      integer, parameter :: INDEX_IREM25I2   =                   83
      integer, parameter :: INDEX_ITIED      =                   85

!   -----------------------------------------------
!   d e r i v e d   t y p e   d e f i n i t i o n s
!   -----------------------------------------------

!   -------------------------

!----------------------------------------------
!   global interfaces structure
!---------------------------------------------
   type interfaces_
      type(intbuf_struct_),dimension(:),allocatable :: intbuf_tab
      type(intbuf_fric_struct_),dimension(:),allocatable :: intbuf_fric_tab
      type (parameters_) parameters
      type (spmd_arrays_) spmd_arrays
      integer :: ninter, npari
      type(interface_pon_) :: pon
   end type interfaces_
!
end module interfaces_mod
