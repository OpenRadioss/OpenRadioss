Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
C
Chd|====================================================================
Chd|  W_TH                          source/restart/ddsplit/w_th.F 
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE WRITE_TH_BUFFER(TH       ,NODLOCAL ,CEL      ,P,
     2                           NTHGRP0  ,NTHGRP   ,NTHGRP01 ,NTHGRP1  , 
     3                           SCEL     ,NUMSPH   ,NUMNOD   ,NCLUSTER ,
     4                           CELSPH   ,NUMLOCCLUSTER ,NBR_TH_MONVOL)
        ! -----------------------------------------------
   !   ROUTINE DESCRIPTION    :
   !   ========================
   !   Split and write Time History buffer
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TIME_HISTORY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "my_allocate.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(TH_),INTENT(INOUT)                :: TH               ! TH Type 
      INTEGER,INTENT(IN)                     :: P                ! Current SPMD domain
      INTEGER,INTENT(IN)                     :: NUMNOD           ! Total number of Model Nodes
      INTEGER,INTENT(IN)                     :: NUMSPH           ! Total number of SPH partikles in model
      INTEGER,INTENT(IN)                     :: SCEL             ! Size of CEL
      INTEGER,INTENT(IN)                     :: NCLUSTER         ! Total number of SPH partikles in model
      INTEGER,INTENT(IN)                     :: NBR_TH_MONVOL    ! number of /TH/MONV
      INTEGER,DIMENSION(NUMNOD), INTENT(IN)  :: NODLOCAL         ! Conversion Starter to Engine node table
      INTEGER,INTENT(IN)                     :: NTHGRP0,NTHGRP   ! Number of variables for main Time History
      INTEGER,DIMENSION(9),INTENT(IN)        :: NTHGRP01,NTHGRP1 ! Number of variables for secondaty TH: ath-ith
      INTEGER,DIMENSION(SCEL),INTENT(IN)     :: CEL              ! Assign Finite Elmeent to SPMD domain
      INTEGER,DIMENSION(NUMSPH),INTENT(IN)   :: CELSPH           ! Assign SPH partikle to SPMD domain
      INTEGER,DIMENSION(NCLUSTER),INTENT(IN) :: NUMLOCCLUSTER    ! Assign SPH partikle to SPMD domain
C-----------------------------------------------

      CALL WRITE_I_C(TH%NITHGR,1)
      CALL WRITE_I_C(TH%SITHGRP, 1) 
      CALL WRITE_I_C(TH%SITHBUF, 1)

      CALL WRITE_I_C(TH%SITHGRPA,1)
      CALL WRITE_I_C(TH%SITHBUFA,1)

      CALL WRITE_I_C(TH%SITHGRPB,1)
      CALL WRITE_I_C(TH%SITHBUFB,1)

      CALL WRITE_I_C(TH%SITHGRPC,1)
      CALL WRITE_I_C(TH%SITHBUFC,1)

      CALL WRITE_I_C(TH%SITHGRPD,1)
      CALL WRITE_I_C(TH%SITHBUFD,1)

      CALL WRITE_I_C(TH%SITHGRPE,1)
      CALL WRITE_I_C(TH%SITHBUFE,1)

      CALL WRITE_I_C(TH%SITHGRPF,1)
      CALL WRITE_I_C(TH%SITHBUFF,1)

      CALL WRITE_I_C(TH%SITHGRPG,1)
      CALL WRITE_I_C(TH%SITHBUFG,1)

      CALL WRITE_I_C(TH%SITHGRPH,1)
      CALL WRITE_I_C(TH%SITHBUFH,1)

      CALL WRITE_I_C(TH%SITHGRPI,1)
      CALL WRITE_I_C(TH%SITHBUFI,1)


      CALL W_TH(TH%ITHGRP  ,TH%ITHBUF ,TH%SITHBUF,NODLOCAL,CEL   ,
     2          NTHGRP0 ,NTHGRP ,P     ,
     3          CELSPH  ,NUMLOCCLUSTER ,NBR_TH_MONVOL)
 

      IF (NTHGRP01(1) > 0) THEN
        CALL W_TH(TH%ITHGRPA  ,TH%ITHBUFA ,TH%SITHBUFA ,NODLOCAL,CEL   ,
     2            NTHGRP01(1) ,NTHGRP1(1) ,P           ,
     3            CELSPH, NUMLOCCLUSTER   ,NBR_TH_MONVOL)
      END IF

      IF (NTHGRP01(2) > 0) THEN
        CALL W_TH(TH%ITHGRPB  ,TH%ITHBUFB ,TH%SITHBUFB,NODLOCAL,CEL,
     2            NTHGRP01(2) ,NTHGRP1(2) ,P          ,
     3            CELSPH ,NUMLOCCLUSTER   ,NBR_TH_MONVOL)
      END IF

      IF (NTHGRP01(3) > 0) THEN
        CALL W_TH(TH%ITHGRPC  ,TH%ITHBUFC ,TH%SITHBUFC,NODLOCAL,CEL,
     2            NTHGRP01(3) ,NTHGRP1(3) ,P          ,
     3            CELSPH ,NUMLOCCLUSTER   ,NBR_TH_MONVOL)
      END IF

      IF (NTHGRP01(4) > 0) THEN
        CALL W_TH(TH%ITHGRPD  ,TH%ITHBUFD ,TH%SITHBUFD,NODLOCAL,CEL,
     2            NTHGRP01(4) ,NTHGRP1(4) ,P          ,
     3            CELSPH ,NUMLOCCLUSTER   ,NBR_TH_MONVOL)
      END IF

      IF (NTHGRP01(5) > 0) THEN
        CALL W_TH(TH%ITHGRPE ,TH%ITHBUFE ,TH%SITHBUFE,NODLOCAL,CEL   ,
     2            NTHGRP01(5),NTHGRP1(5) ,P ,
     3            CELSPH, NUMLOCCLUSTER  ,NBR_TH_MONVOL)
      END IF

      IF (NTHGRP01(6) > 0) THEN
        CALL W_TH(TH%ITHGRPF ,TH%ITHBUFF ,TH%SITHBUFF,NODLOCAL,CEL   ,
     2            NTHGRP01(6),NTHGRP1(6) ,P ,
     3            CELSPH, NUMLOCCLUSTER ,NBR_TH_MONVOL)
      END IF

      IF (NTHGRP01(7) > 0) THEN
        CALL W_TH(TH%ITHGRPG ,TH%ITHBUFG ,TH%SITHBUFG,NODLOCAL,CEL   ,
     2            NTHGRP01(7),NTHGRP1(7) ,P ,
     3            CELSPH, NUMLOCCLUSTER  ,NBR_TH_MONVOL)
      END IF

      IF (NTHGRP01(8) > 0) THEN
        CALL W_TH(TH%ITHGRPH ,TH%ITHBUFH ,TH%SITHBUFH,NODLOCAL,CEL   ,
     2            NTHGRP01(8),NTHGRP1(8) ,P ,
     3            CELSPH, NUMLOCCLUSTER  ,NBR_TH_MONVOL)
      END IF

      IF (NTHGRP01(9) > 0) THEN
        CALL W_TH(TH%ITHGRPI ,TH%ITHBUFI ,TH%SITHBUFI,NODLOCAL,CEL   ,
     2            NTHGRP01(9),NTHGRP1(9) ,P ,
     3            CELSPH, NUMLOCCLUSTER  ,NBR_TH_MONVOL)
      END IF

      END SUBROUTINE WRITE_TH_BUFFER

