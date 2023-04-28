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
Chd|====================================================================
Chd|  H3D_SKIN_DIM                  source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|-- called by -----------
Chd|        LECH3D                        source/output/h3d/h3d_build_fortran/lech3d.F
Chd|-- calls ---------------
Chd|        H3D_SKIN_PRE_DIM              source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|        H3D_SKIN_SOL_DIM              source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|        H3D_SKIN_TSH_DIM              source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|        SPMD_ALLGLOB_ISUM9            source/mpi/generic/spmd_allglob_isum9.F
Chd|====================================================================
      SUBROUTINE H3D_SKIN_DIM(IPARG ,NUMSKIN, NUMSKING,TAG_SKINS6,IBCL,
     .   	                  ILOADP,LLOADP ,NUMSKINP0,NUMSKINP,IFLAG )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(OUT) :: NUMSKINP0
      INTEGER NUMSKIN, NUMSKING ,IPARG(*),TAG_SKINS6(*),
     .        IBCL(*),ILOADP(*),LLOADP(*),NUMSKINP,IFLAG	  
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C----------------------------------------------- 
      INTEGER I, LEN ,N0  ,NUMSKIN0

C-----------------------------------------------
       NUMSKIN = 0
C--- thick-shell first, solid after
       IF (IFLAG==1.OR.IFLAG==3) THEN
        CALL  H3D_SKIN_TSH_DIM(IPARG    ,TAG_SKINS6, NUMSKIN)     
        CALL  H3D_SKIN_SOL_DIM(IPARG    ,TAG_SKINS6, NUMSKIN)
       END IF        
C--- followed by pressure: pblast, pfluid,force
       N0 = NUMSKIN
       NUMSKIN0 = NUMSKIN
C------!!!!create NODAL_IPART(numnod) before       
       IF (IFLAG>1) CALL H3D_SKIN_PRE_DIM (IBCL   ,ILOADP ,LLOADP, NUMSKIN0,NUMSKIN)
	   NUMSKINP =NUMSKIN- N0
	   NUMSKINP0=NUMSKIN0- N0
C
       LEN = 1       
       IF (NSPMD>1) THEN
         NUMSKING = NUMSKIN
         CALL SPMD_ALLGLOB_ISUM9(NUMSKING,LEN)
       ELSE
        NUMSKING = NUMSKIN
       END IF
C-----------
      RETURN
      END
