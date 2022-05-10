Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  PNOISE                        source/general_controls/computation/pnoise.F
Chd|-- called by -----------
Chd|        NOISE                         source/general_controls/computation/noise.F
Chd|-- calls ---------------
Chd|        SPMD_GLOB_DSUM9               source/mpi/interfaces/spmd_th.F
Chd|        SPMD_GLOB_ISUM9               source/mpi/interfaces/spmd_th.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE PNOISE(ELNOI,ELG,NOIADD,ELBUF_TAB,WA,IPARG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scrnoi_c.inc"
#include      "com01_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ELNOI(*),ELG(*),NOIADD(*),IPARG(NPARG,*)
      my_real
     .   WA(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,NG,IWA(NNOISE),NEL,IEL
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
C
      DO I=1,NNOISE
       WA(I)=ZERO
      ENDDO
      DO I=1,NNOISE
       DO K=NOIADD(I),NOIADD(I+1)-1
        NG = ELG(K)
        GBUF => ELBUF_TAB(NG)%GBUF
        NEL = IPARG(2,NG)
        IEL = ELNOI(K)
        WA(I)=WA(I)+ GBUF%SIG(IEL) + GBUF%SIG(IEL+NEL) + GBUF%SIG(IEL+2*NEL)
       ENDDO       
      ENDDO
      IF(IMACH.NE.3.OR.NSPMD.EQ.1) THEN
        DO I=1,NNOISE
          K=NOIADD(I+1)-NOIADD(I)
          IF(K.GT.0)WA(I)=WA(I)/TROIS/FLOAT(K)
        ENDDO
      ELSE
        CALL SPMD_GLOB_DSUM9(WA,NNOISE)
        DO I=1,NNOISE
          IWA(I) = NOIADD(I+1)-NOIADD(I)
        ENDDO
        CALL SPMD_GLOB_ISUM9(IWA,NNOISE)
        IF(ISPMD.EQ.0) THEN
          DO I=1,NNOISE
            IF(IWA(I).GT.0)WA(I)=WA(I)/TROIS/FLOAT(IWA(I))
          ENDDO
        ENDIF
      ENDIF
C-----------
      RETURN
      END
