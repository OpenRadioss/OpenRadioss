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
Chd|  THERMEXPPI                    source/elements/beam/thermexpp.F
Chd|-- called by -----------
Chd|        PFORC3                        source/elements/beam/pforc3.F 
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE THERMEXPPI(ELBUF_STR,
     1                  NEL    ,NPT    ,MAT    , PID    ,
     2                  PM     ,GEO    , AL     ,ETH    ,
     2                  OFF    ,FOR    ,EINT       )
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr06_c.inc"
#include      "param_c.inc"
#include      "com08_c.inc"
#include      "com20_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPT,NEL
      INTEGER MAT(MVSIZ),PID(MVSIZ)
C     REAL
      my_real
     .   PM(NPROPM,*), GEO(NPROPG,*),FOR(NEL,3), EINT(NEL,2),
     .   OFF(*), AL(*),ETH(NEL)
C
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ICC(MVSIZ),IRTY(MVSIZ),INDX(MVSIZ)
      INTEGER I, J, J1, J2, NPIF, MX, IPT, IPY,IPZ,IPA,NINDX,
     .        IR,IS,ILAYER,II(3)
C     REAL
      my_real
     .  E, SIGNXX,DFXX,APT
C
      TYPE(L_BUFEL_),POINTER :: LBUF  
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
C=======================================================================
!   
   
C-------------------------------------
C     DEBUT DE BOUCLE SUR POINTS INTEGRATION
C--------------------------------------
C             
      IPA  = 400  
      DO I=1,3
        II(I) = NEL*(I-1)
      ENDDO 
      DO IPT= 1,NPT
        ILAYER=1
        IR = 1
        IS = 1
        LBUF => ELBUF_STR%BUFLY(ILAYER)%LBUF(IR,IS,IPT)
        BUFLY => ELBUF_STR%BUFLY(ILAYER)
c---    Total strain
C 
        IF (BUFLY%L_STRA > 0) THEN
          DO I= 1,NEL
            LBUF%STRA(II(1)+I) = LBUF%STRA(II(1)+I) + ETH(I)
          ENDDO
        ENDIF        
C
C---    Contraintes elastiques
C
        DO I =1 ,NEL             
          MX = MAT(I)                            
          E    = PM(20,MX)                         
          SIGNXX =  -E*ETH(I)  
C       FORCES ET MOMENTS
          APT = GEO(IPA+IPT,PID(I)) 
          DFXX = APT*SIGNXX
          FOR(I,1) = FOR(I,1) + DFXX		 
C                                                    
          LBUF%SIG(II(1)+I) = LBUF%SIG(II(1)+I) + SIGNXX
        ENDDO                                                       
C-------------------------------------
C       FIN DE BOUCLE SUR POINT INTEGRATION
C-------------------------------------
       ENDDO   
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  THERMEXPPG                    source/elements/beam/thermexpp.F
Chd|-- called by -----------
Chd|        PFORC3                        source/elements/beam/pforc3.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE THERMEXPPG(
     .              NEL    ,MAT     ,PID   , PM, GEO ,
     .              OFF    ,ETH     ,FOR     ,EINT  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr06_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,MAT(MVSIZ),PID(MVSIZ)
C     REAL
      my_real
     .   PM(NPROPM,*), FOR(NEL,3),  EINT(NEL,2), GEO(NPROPG,*),
     .   OFF(*),AL(MVSIZ),ETH(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C     REAL
      my_real
     .   YM,A1
C-----------------------------------------------
      DO I=1,NEL
        YM  =PM(20,MAT(I))
        A1  =GEO(1,PID(I))
        FOR(I,1) = (FOR(I,1)  - ETH(I)*A1*YM )*OFF(I)   
!!        EINT(I,1) = EINT(I,1) - FOR(I,1)*ETH(I)*AL(I)*HALF*OFF(I)
      ENDDO             
C-----------------------------------------------
      RETURN
      END
