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
Chd|  SET_FAILWAVE_SH4N             source/materials/fail/failwave/upd_failwave_sh4n.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|-- calls ---------------
Chd|        FAILWAVE_MOD                  ../common_source/modules/failwave_mod.F
Chd|====================================================================
      SUBROUTINE SET_FAILWAVE_SH4N(FAILWAVE    ,FWAVE_EL  ,DADV     ,
     .           NEL      ,IXC      ,ITAB      ,NGL       ,OFFLY    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FAILWAVE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "scr17_c.inc"
#include "com04_c.inc"
#include "units_c.inc"
#include "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      INTEGER IXC(NIXC,*)
      INTEGER, DIMENSION(NEL)   , INTENT(IN)    :: NGL,OFFLY
      INTEGER, DIMENSION(NUMNOD), INTENT(IN)    :: ITAB 
      my_real ,DIMENSION(NEL)   , INTENT(IN)    :: DADV  
      INTEGER, DIMENSION(NEL)   , INTENT(OUT)   :: FWAVE_EL
      TYPE (FAILWAVE_STR_)  :: FAILWAVE 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,K,N1,N2,N3,N4,FOUND,LEVEL,NINDX,NFAIL,FNOD1,FNOD2,
     .   KNEXT,KPREV,NCURR
      INTEGER ,DIMENSION(NEL) :: INDX
      INTEGER ,DIMENSION(4)   :: NDL,NDR,NOD_ID,NOD_NN
c---
      DATA NDR/2,3,4,1/
      DATA NDL/4,1,2,3/
c-----------------------------------------------
c set failure flag to elements using nodal frontwave information from neighbors
C=======================================================================
c
c---------------
      SELECT CASE (FAILWAVE%WAVE_MOD)
c---------------
        CASE (1)   ! isotropic propagation
c---------------
          DO I=1,NEL
            IF (OFFLY(I) == 1 .and. DADV(I) == ONE) THEN
              N1 = FAILWAVE%IDXI(IXC(2,I))
              N2 = FAILWAVE%IDXI(IXC(3,I))
              N3 = FAILWAVE%IDXI(IXC(4,I))
              N4 = FAILWAVE%IDXI(IXC(5,I))
              NFAIL = FAILWAVE%FWAVE_NOD(1,N1,1) 
     .              + FAILWAVE%FWAVE_NOD(1,N2,1)
     .              + FAILWAVE%FWAVE_NOD(1,N3,1)
     .              + FAILWAVE%FWAVE_NOD(1,N4,1)
              IF (NFAIL > 0) FWAVE_EL(I) = 1
            ENDIF
          ENDDO
c---------------
        CASE (2)   ! directional propagation through edges only
c---------------
          NINDX = 0
          DO I=1,NEL
            IF (OFFLY(I) == 1 .and. DADV(I) == ONE) THEN
              NINDX = NINDX + 1    ! count of non damaged elements
              INDX(NINDX) = I
            ENDIF
          ENDDO
c
          DO II=1,NINDX         
            I = INDX(II)        
            N1 = IXC(2,I)
            N2 = IXC(3,I)
            N3 = IXC(4,I)
            N4 = IXC(5,I)
            NOD_NN(1) = FAILWAVE%IDXI(N1)  
            NOD_NN(2) = FAILWAVE%IDXI(N2)  
            NOD_NN(3) = FAILWAVE%IDXI(N3)  
            NOD_NN(4) = FAILWAVE%IDXI(N4)  
            NOD_ID(1) = ITAB(N1)   
            NOD_ID(2) = ITAB(N2)   
            NOD_ID(3) = ITAB(N3)   
            NOD_ID(4) = ITAB(N4)
            FOUND = 0   
c
            DO K=1,4
              NCURR = NOD_NN(K)
              IF (FAILWAVE%MAXLEV(NCURR) > 0) THEN
                KNEXT = NDR(K)  
                KPREV = NDL(K)
c
                DO LEVEL = 1,FAILWAVE%MAXLEV(NCURR)
                  FNOD1 = FAILWAVE%FWAVE_NOD(1,NCURR,LEVEL)
                  FNOD2 = FAILWAVE%FWAVE_NOD(2,NCURR,LEVEL)
c
                  IF (FNOD1 == NOD_ID(KNEXT) .and. FNOD2 == 0) THEN
                    FOUND = 1
                    FWAVE_EL(I) = 1
                    EXIT
                  ENDIF
                ENDDO  !  LEVEL
                IF (FOUND == 1) EXIT
              ENDIF
            ENDDO    !  K=1,4 
c
c            IF (FOUND == 1) THEN
c#include "lockon.inc"
c              write(iout,'(A,I10)')'set failwave flag to element=',ngl(I)
c#include "lockoff.inc"
c            ENDIF
c
          ENDDO     !  II=1,NINDX   
c
c---------------
        CASE (3)   ! directional propagation through edges and diagonals
c---------------
c
          NINDX = 0
          DO I=1,NEL
            IF (OFFLY(I) == 1 .and. DADV(I) == ONE) THEN
              NINDX = NINDX + 1
              INDX(NINDX) = I
            ENDIF
          ENDDO
c
          DO II=1,NINDX
            I = INDX(II)
            N1 = IXC(2,I)
            N2 = IXC(3,I)
            N3 = IXC(4,I)
            N4 = IXC(5,I)
            NOD_NN(1) = FAILWAVE%IDXI(N1)  
            NOD_NN(2) = FAILWAVE%IDXI(N2)  
            NOD_NN(3) = FAILWAVE%IDXI(N3)  
            NOD_NN(4) = FAILWAVE%IDXI(N4)  
            NOD_ID(1) = ITAB(N1)   
            NOD_ID(2) = ITAB(N2)   
            NOD_ID(3) = ITAB(N3)   
            NOD_ID(4) = ITAB(N4)
            FOUND = 0   
c
            DO K=1,4
              NCURR = NOD_NN(K)       
              IF (FAILWAVE%MAXLEV(NCURR) > 0) THEN
                KNEXT = NDR(K)  
                KPREV = NDL(K)
c
                DO LEVEL = 1,FAILWAVE%MAXLEV(NCURR)
                  FNOD1 = FAILWAVE%FWAVE_NOD(1,NCURR,LEVEL)
                  FNOD2 = FAILWAVE%FWAVE_NOD(2,NCURR,LEVEL)
c
                  IF (FNOD2 == 0 .and. 
     .               (FNOD1 == NOD_ID(KNEXT) .or. FNOD1 == NOD_ID(KPREV))) THEN
                    FOUND = 1   ! failwave coming by edge
                    EXIT
                  ELSE IF (FNOD1 > 0 .and. FNOD2 > 0 .and.
     .              FNOD1 /= NOD_ID(KPREV) .and. FNOD1 /= NOD_ID(KNEXT) .and.
     .              FNOD2 /= NOD_ID(KPREV) .and. FNOD2 /= NOD_ID(KNEXT)) THEN
                    FOUND = 2   ! failwave coming by diagonal
                    EXIT
                  ENDIF
                ENDDO  !  LEVEL
                IF (FOUND > 0) THEN
                  FWAVE_EL(I) = 1
                  EXIT
                ENDIF
c
              ENDIF
            ENDDO    !  K=1,4 
c
c            IF (FOUND == 1) THEN
c#include "lockon.inc"
c              write(iout,'(A,I10)') 'set edge failwave to element=',ngl(I)
c#include "lockoff.inc"
c            ELSE IF (FOUND == 2) THEN
c#include "lockon.inc"
c              write(iout,'(A,I10)') 'set diag failwave to element=',ngl(I)
c#include "lockoff.inc"
c            ENDIF
c
          ENDDO       !  II=1,NINDX  
c---------------
      END SELECT
c---------------
      RETURN
      END
