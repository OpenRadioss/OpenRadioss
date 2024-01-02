Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
Chd|====================================================================
Chd|  TIME_HISTORY_MOD              modules/output/time_history_mod.F
Chd|-- called by -----------
Chd|        OUTPUT_MOD                    common_source/modules/output/output_mod.F
Chd|        WRITE_TH_BUFFER               starter/source/restart/ddsplit/write_th_buffer.F
Chd|        INIT_TH0                      engine/source/output/th/init_th0.F
Chd|        READ_TH_RESTART               engine/source/output/th/read_th_restart.F
Chd|        WRITE_TH_RESTART              engine/source/output/th/write_th_restart.F
Chd|-- calls ---------------
Chd|        TH_SURF_MOD                   modules/interfaces/th_surf_mod.F
Chd|====================================================================
      MODULE TIME_HISTORY_MOD
       USE TH_SURF_MOD
#include "my_real.inc" 

         TYPE TH_
            INTEGER  NITHGR
            INTEGER, DIMENSION(:), ALLOCATABLE :: ITHBUF
            INTEGER  SITHBUF
            INTEGER, DIMENSION(:), ALLOCATABLE :: ITHBUFA
            INTEGER  SITHBUFA
            INTEGER, DIMENSION(:), ALLOCATABLE :: ITHBUFB
            INTEGER  SITHBUFB
            INTEGER, DIMENSION(:), ALLOCATABLE :: ITHBUFC
            INTEGER  SITHBUFC
            INTEGER, DIMENSION(:), ALLOCATABLE :: ITHBUFD
            INTEGER  SITHBUFD
            INTEGER, DIMENSION(:), ALLOCATABLE :: ITHBUFE
            INTEGER  SITHBUFE
            INTEGER, DIMENSION(:), ALLOCATABLE :: ITHBUFF
            INTEGER  SITHBUFF
            INTEGER, DIMENSION(:), ALLOCATABLE :: ITHBUFG
            INTEGER  SITHBUFG
            INTEGER, DIMENSION(:), ALLOCATABLE :: ITHBUFH
            INTEGER  SITHBUFH
            INTEGER, DIMENSION(:), ALLOCATABLE :: ITHBUFI
            INTEGER  SITHBUFI

            INTEGER, DIMENSION(:), ALLOCATABLE :: ITHGRP
            INTEGER  SITHGRP
            INTEGER, DIMENSION(:), ALLOCATABLE :: ITHGRPA
            INTEGER  SITHGRPA
            INTEGER, DIMENSION(:), ALLOCATABLE :: ITHGRPB
            INTEGER  SITHGRPB
            INTEGER, DIMENSION(:), ALLOCATABLE :: ITHGRPC
            INTEGER  SITHGRPC
            INTEGER, DIMENSION(:), ALLOCATABLE :: ITHGRPD
            INTEGER  SITHGRPD
            INTEGER, DIMENSION(:), ALLOCATABLE :: ITHGRPE
            INTEGER  SITHGRPE
            INTEGER, DIMENSION(:), ALLOCATABLE :: ITHGRPF
            INTEGER  SITHGRPF
            INTEGER, DIMENSION(:), ALLOCATABLE :: ITHGRPG
            INTEGER  SITHGRPG
            INTEGER, DIMENSION(:), ALLOCATABLE :: ITHGRPH
            INTEGER  SITHGRPH
            INTEGER, DIMENSION(:), ALLOCATABLE :: ITHGRPI
            INTEGER  SITHGRPI
            REAL(KIND=8) :: THIS    ! time of next TH file
            REAL(KIND=8), DIMENSION(9) :: THIS1 ! time of next TH files (aTH,bTH,...)
            REAL(KIND=8) :: DTHIS   ! time step of TH file
            REAL(KIND=8), DIMENSION(9) :: DTHIS1 ! time step of TH files (aTH,bTH,...)
C
            TYPE (TH_SURF_) TH_SURF
C
         END TYPE TH_

         CONTAINS

Chd|====================================================================
Chd|  TH_ALLOC                      modules/output/time_history_mod.F
Chd|-- called by -----------
Chd|        READ_TH_RESTART               engine/source/output/th/read_th_restart.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   starter/share/message_module/message_mod.F
Chd|        MESSAGE_MOD                   engine/share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE TH_ALLOC(TH,
     .                       SITHGRP,  SITHBUF ,
     .                       SITHGRPA, SITHBUFA,
     .                       SITHGRPB, SITHBUFB,
     .                       SITHGRPC, SITHBUFC,
     .                       SITHGRPD, SITHBUFD,
     .                       SITHGRPE, SITHBUFE,
     .                       SITHGRPF, SITHBUFF,
     .                       SITHGRPG, SITHBUFG,
     .                       SITHGRPH, SITHBUFH,
     .                       SITHGRPI, SITHBUFI )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
           TYPE(TH_),INTENT(INOUT)  :: TH
           INTEGER, INTENT(IN) :: SITHGRP,  SITHBUF 
           INTEGER, INTENT(IN) :: SITHGRPA, SITHBUFA
           INTEGER, INTENT(IN) :: SITHGRPB, SITHBUFB
           INTEGER, INTENT(IN) :: SITHGRPC, SITHBUFC
           INTEGER, INTENT(IN) :: SITHGRPD, SITHBUFD
           INTEGER, INTENT(IN) :: SITHGRPE, SITHBUFE
           INTEGER, INTENT(IN) :: SITHGRPF, SITHBUFF
           INTEGER, INTENT(IN) :: SITHGRPG, SITHBUFG
           INTEGER, INTENT(IN) :: SITHGRPH, SITHBUFH
           INTEGER, INTENT(IN) :: SITHGRPI, SITHBUFI 
C-----------------------------------------------
           ALLOCATE(TH%ITHGRP (TH%SITHGRP))
           ALLOCATE(TH%ITHBUF (TH%SITHBUF))

           ALLOCATE(TH%ITHGRPA(TH%SITHGRPA))
           ALLOCATE(TH%ITHBUFA(TH%SITHBUFA))

           ALLOCATE(TH%ITHGRPB(TH%SITHGRPB))
           ALLOCATE(TH%ITHBUFB(TH%SITHBUFB))

           ALLOCATE(TH%ITHGRPC(TH%SITHGRPC))
           ALLOCATE(TH%ITHBUFC(TH%SITHBUFC))

           ALLOCATE(TH%ITHGRPD(TH%SITHGRPD))
           ALLOCATE(TH%ITHBUFD(TH%SITHBUFD))

           ALLOCATE(TH%ITHGRPE(TH%SITHGRPE))
           ALLOCATE(TH%ITHBUFE(TH%SITHBUFE))

           ALLOCATE(TH%ITHGRPF(TH%SITHGRPF))
           ALLOCATE(TH%ITHBUFF(TH%SITHBUFF))

           ALLOCATE(TH%ITHGRPG(TH%SITHGRPG))
           ALLOCATE(TH%ITHBUFG(TH%SITHBUFG))

           ALLOCATE(TH%ITHGRPH(TH%SITHGRPH))
           ALLOCATE(TH%ITHBUFH(TH%SITHBUFH))

           ALLOCATE(TH%ITHGRPI(TH%SITHGRPI))
           ALLOCATE(TH%ITHBUFI(TH%SITHBUFI))

         END SUBROUTINE TH_ALLOC


Chd|====================================================================
Chd|  TH_CLEAN                      modules/output/time_history_mod.F
Chd|-- called by -----------
Chd|        STARTER0                      starter/source/starter/starter0.F
Chd|        RADIOSS2                      engine/source/engine/radioss2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TH_CLEAN(TH )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
         TYPE(TH_),INTENT(INOUT)  :: TH
C-----------------------------------------------
         TH%SITHGRP = 0
         TH%SITHBUF = 0

         TH%SITHGRPA = 0
         TH%SITHBUFA = 0

         TH%SITHGRPB = 0
         TH%SITHBUFB = 0

         TH%SITHGRPC = 0
         TH%SITHBUFC = 0

         TH%SITHGRPD = 0
         TH%SITHBUFD = 0

         TH%SITHGRPE = 0
         TH%SITHBUFE = 0

         TH%SITHGRPF = 0
         TH%SITHBUFF = 0

         TH%SITHGRPG = 0
         TH%SITHBUFG = 0

         TH%SITHGRPH = 0
         TH%SITHBUFH = 0

         TH%SITHGRPI = 0
         TH%SITHBUFI = 0


         DEALLOCATE(TH%ITHGRP)
         DEALLOCATE(TH%ITHBUF)

         DEALLOCATE(TH%ITHGRPA)
         DEALLOCATE(TH%ITHBUFA)

         DEALLOCATE(TH%ITHGRPB)
         DEALLOCATE(TH%ITHBUFB)

         DEALLOCATE(TH%ITHGRPC)
         DEALLOCATE(TH%ITHBUFC)

         DEALLOCATE(TH%ITHGRPD)
         DEALLOCATE(TH%ITHBUFD)

         DEALLOCATE(TH%ITHGRPE)
         DEALLOCATE(TH%ITHBUFE)

         DEALLOCATE(TH%ITHGRPF)
         DEALLOCATE(TH%ITHBUFF)

         DEALLOCATE(TH%ITHGRPG)
         DEALLOCATE(TH%ITHBUFG)

         DEALLOCATE(TH%ITHGRPH)
         DEALLOCATE(TH%ITHBUFH)

         DEALLOCATE(TH%ITHGRPI)
         DEALLOCATE(TH%ITHBUFI)

       END SUBROUTINE TH_CLEAN

      END MODULE TIME_HISTORY_MOD
