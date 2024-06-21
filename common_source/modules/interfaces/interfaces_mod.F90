!copyright>        openradioss
!copyright>        copyright (c) 1986-2024 altair engineering inc.
!copyright>
!copyright>        this program is free software: you can redistribute it and/or modify
!copyright>        it under the terms of the gnu affero general public license as published by
!copyright>        the free software foundation, either version 3 of the license, or
!copyright>        (at your option) any later version.
!copyright>
!copyright>        this program is distributed in the hope that it will be useful,
!copyright>        but without any warranty; without even the implied warranty of
!copyright>        merchantability or fitness for a particular purpose.  see the
!copyright>        gnu affero general public license for more details.
!copyright>
!copyright>        you should have received a copy of the gnu affero general public license
!copyright>        along with this program.  if not, see <https://www.gnu.org/licenses/>.
!copyright>
!copyright>
!copyright>        commercial alternative: altair radioss software
!copyright>
!copyright>        as an alternative to this open-source version, altair also offers altair radioss
!copyright>        software under a commercial license.  contact altair to discuss further if the
!copyright>        commercial version may interest you: https://www.altair.com/radioss/.
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
   implicit none
!-----------------------------------------------
!   m y _ r e a l
!-----------------------------------------------
#include      "my_real.inc"

!   -----------------------------------------------
!   d e r i v e d   t y p e   d e f i n i t i o n s
!   -----------------------------------------------

!   -------------------------

!----------------------------------------------
!   global interfaces stucture
!---------------------------------------------
   type interfaces_
      type(intbuf_struct_),dimension(:),allocatable :: intbuf_tab
      type(intbuf_fric_struct_),dimension(:),allocatable :: intbuf_fric_tab
      type (parameters_) parameters
      type (spmd_arrays_) spmd_arrays
   end type interfaces_
!
end module interfaces_mod
