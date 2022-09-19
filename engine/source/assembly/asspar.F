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
Chd|  ASSPAR                        source/assembly/asspar.F      
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|====================================================================
      SUBROUTINE ASSPAR(NTHREAD ,NUMNOD,NODFT ,NODLT,IRODDL,
     .                  NPART,PARTFT ,PARTLT,A    ,AR    ,
     .                  PARTSAV,STIFN ,STIFR,VISCN       ,
     .                  FTHE ,ITHERM_FE ,STCND,GREFT,GRELT ,
     .                  GRESAV,NGPE     ,NTHPART,IALELAG,AF,
     .                  DMSPH ,CONDN,
     .                  APINCH,STIFPINCH)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE PINCHTYPE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "scr18_c.inc"
#include      "sphcom.inc"
C
      INTEGER NTHREAD,NUMNOD,NODFT,NODLT,IRODDL,
     .              NPART,PARTFT,PARTLT,ITHERM_FE,GREFT,GRELT,NGPE,
     .              NTHPART,IALELAG
      INTEGER K,KN,IKN,IKN1,IKN2,I,KM,KM1,KM2,NUM7,NUM8,KM3,KM4,KM5
      my_real
     .  A(3,*),AR(3,*),PARTSAV(*),STIFN(*),STIFR(*),VISCN(*),
     .  FTHE(*), STCND(*),GRESAV(*),AF(3,*), DMSPH(*),CONDN(*)
      my_real
     .       APINCH(3,*),STIFPINCH(*)
C
      NUM7 = NPSAV*NPART
      NUM8 = NPSAV*NGPE
C
      GOTO(100,200,300)NTHREAD-1
      GOTO 900
C------------
C     2 TASKS
 100  CONTINUE
#include "vectorize.inc"
          DO 120 I=NODFT,NODLT
            IKN = I+ NUMNOD
            STIFN(I) = STIFN(I) + STIFN(IKN)
            STIFN(IKN) = ZERO
            A(1,I) = A(1,I) + A(1,IKN)
            A(2,I) = A(2,I) + A(2,IKN)
            A(3,I) = A(3,I) + A(3,IKN)
            A(1,IKN) = ZERO
            A(2,IKN) = ZERO
            A(3,IKN) = ZERO
 120      CONTINUE

          IF(NPINCH > 0) THEN
#include "vectorize.inc"
            DO I=NODFT,NODLT
              IKN = I+ NUMNOD
              STIFPINCH(I) = STIFPINCH(I) + STIFPINCH(IKN)
              STIFPINCH(IKN) = ZERO
              APINCH(1,I) = APINCH(1,I) + APINCH(1,IKN)
              APINCH(2,I) = APINCH(2,I) + APINCH(2,IKN)
              APINCH(3,I) = APINCH(3,I) + APINCH(3,IKN)
              APINCH(1,IKN) = ZERO
              APINCH(2,IKN) = ZERO
              APINCH(3,IKN) = ZERO
            ENDDO
          ENDIF

          IF (IRODDL/=0) THEN
#include "vectorize.inc"
           DO 140 I=NODFT,NODLT
            IKN = I + NUMNOD
            STIFR(I) = STIFR(I) + STIFR(IKN)
            STIFR(IKN) = ZERO
            AR(1,I) = AR(1,I) + AR(1,IKN)
            AR(2,I) = AR(2,I) + AR(2,IKN)
            AR(3,I) = AR(3,I) + AR(3,IKN)
            AR(1,IKN) = ZERO
            AR(2,IKN) = ZERO
            AR(3,IKN) = ZERO
 140       CONTINUE
          ENDIF
C
          IF(ITHERM_FE > 0 ) THEN
#include "vectorize.inc"
           DO  I=NODFT,NODLT
            IKN = I+ NUMNOD
            FTHE(I) = FTHE(I) + FTHE(IKN)
            FTHE(IKN) = ZERO
           ENDDO
          ENDIF
C
          IF(NODADT_THERM > 0 ) THEN
#include "vectorize.inc"
           DO  I=NODFT,NODLT
            IKN = I+ NUMNOD
            CONDN(I) = CONDN(I) + CONDN(IKN)
            CONDN(IKN) = ZERO
           ENDDO
          ENDIF
C        
          IF(ISTATCND /=0) THEN
           DO  I=NODFT,NODLT
            IKN = I+ NUMNOD
            STCND(I) = STCND(I) + STCND(IKN)
            STCND(IKN) = ZERO
           ENDDO
          ENDIF
C
          IF(IALELAG> 0) THEN
#include "vectorize.inc"
             DO  I=NODFT,NODLT
               IKN = I+ NUMNOD
               AF(1,I) = AF(1,I) + AF(1,IKN)
               AF(2,I) = AF(2,I) + AF(2,IKN)
               AF(3,I) = AF(3,I) + AF(3,IKN)
               AF(1,IKN) = ZERO
               AF(2,IKN) = ZERO
               AF(3,IKN) = ZERO
             ENDDO
          ENDIF

C         viscosite
          IF(KDTINT/=0)THEN
#include "vectorize.inc"
            DO I=NODFT,NODLT
             IKN  = I    + NUMNOD
             VISCN(I) = VISCN(I) + VISCN(IKN)
             VISCN(IKN) = ZERO
            ENDDO
          ENDIF
C        
          IF(SOL2SPH_FLAG /=0) THEN
#include "vectorize.inc"
           DO  I=NODFT,NODLT
            IKN = I+ NUMNOD
            DMSPH(I) = DMSPH(I) + DMSPH(IKN)
            DMSPH(IKN) = ZERO
           ENDDO
          ENDIF
C
#include "vectorize.inc"
          DO 160 I=PARTFT,PARTLT
           PARTSAV(I) = PARTSAV(I) + PARTSAV(I+NUM7)
           PARTSAV(I+NUM7) = ZERO
 160      CONTINUE
      GOTO 1000
C------------
C     3 TASKS
 200  CONTINUE
#include "vectorize.inc"
          DO 220 I=NODFT,NODLT
            IKN  = I   + NUMNOD
            IKN1 = IKN + NUMNOD
            STIFN(I) = STIFN(I) + STIFN(IKN) + STIFN(IKN1)
            STIFN(IKN) = ZERO
            STIFN(IKN1) = ZERO
            A(1,I) = A(1,I) + A(1,IKN) + A(1,IKN1)
            A(2,I) = A(2,I) + A(2,IKN) + A(2,IKN1)
            A(3,I) = A(3,I) + A(3,IKN) + A(3,IKN1)
            A(1,IKN) = ZERO
            A(2,IKN) = ZERO
            A(3,IKN) = ZERO
            A(1,IKN1) = ZERO
            A(2,IKN1) = ZERO
            A(3,IKN1) = ZERO
 220      CONTINUE

         IF(NPINCH > 0) THEN
#include "vectorize.inc"
           DO I=NODFT,NODLT
             IKN  = I   + NUMNOD
             IKN1 = IKN + NUMNOD
             STIFPINCH(I) = STIFPINCH(I) + STIFPINCH(IKN) + STIFPINCH(IKN1)
             STIFPINCH(IKN) = ZERO
             STIFPINCH(IKN1) = ZERO
             APINCH(1,I) = APINCH(1,I) + APINCH(1,IKN) + APINCH(1,IKN1)
             APINCH(2,I) = APINCH(2,I) + APINCH(2,IKN) + APINCH(2,IKN1)
             APINCH(3,I) = APINCH(3,I) + APINCH(3,IKN) + APINCH(3,IKN1)
             APINCH(1,IKN) = ZERO
             APINCH(2,IKN) = ZERO
             APINCH(3,IKN) = ZERO
             APINCH(1,IKN1) = ZERO
             APINCH(2,IKN1) = ZERO
             APINCH(3,IKN1) = ZERO
           ENDDO
         ENDIF


          IF (IRODDL/=0) THEN
#include "vectorize.inc"
           DO 240 I=NODFT,NODLT
            IKN  = I   + NUMNOD
            IKN1 = IKN + NUMNOD
            STIFR(I) = STIFR(I) + STIFR(IKN) + STIFR(IKN1)
            STIFR(IKN) = ZERO
            STIFR(IKN1) = ZERO
            AR(1,I) = AR(1,I) + AR(1,IKN) + AR(1,IKN1)
            AR(2,I) = AR(2,I) + AR(2,IKN) + AR(2,IKN1)
            AR(3,I) = AR(3,I) + AR(3,IKN) + AR(3,IKN1)
            AR(1,IKN) = ZERO
            AR(2,IKN) = ZERO
            AR(3,IKN) = ZERO
            AR(1,IKN1) = ZERO
            AR(2,IKN1) = ZERO
            AR(3,IKN1) = ZERO
 240       CONTINUE
          ENDIF
C
          IF(ITHERM_FE > 0 ) THEN
#include "vectorize.inc"
           DO  I=NODFT,NODLT
            IKN  = I   + NUMNOD
            IKN1 = IKN + NUMNOD
            FTHE(I) = FTHE(I) + FTHE(IKN) + FTHE(IKN1)
            FTHE(IKN) = ZERO
            FTHE(IKN1) = ZERO
           ENDDO
          ENDIF
C
          IF(NODADT_THERM > 0 ) THEN
#include "vectorize.inc"
           DO  I=NODFT,NODLT
            IKN  = I   + NUMNOD
            IKN1 = IKN + NUMNOD
            CONDN(I) = CONDN(I) + CONDN(IKN) + CONDN(IKN1)
            CONDN(IKN) = ZERO
            CONDN(IKN1) = ZERO
           ENDDO
          ENDIF
C        
          IF(ISTATCND /=0) THEN
           DO  I=NODFT,NODLT
            IKN = I+ NUMNOD
            IKN1 = IKN + NUMNOD
            STCND(I) = STCND(I) + STCND(IKN) + STCND(IKN1)
            STCND(IKN) = ZERO
            STCND(IKN1) = ZERO
           ENDDO
          ENDIF
C
C         viscosite
          IF(KDTINT/=0)THEN
#include "vectorize.inc"
            DO I=NODFT,NODLT
             IKN  = I    + NUMNOD
             IKN1 = IKN  + NUMNOD
             VISCN(I) = VISCN(I) + VISCN(IKN) + VISCN(IKN1)
             VISCN(IKN) = ZERO
             VISCN(IKN1) = ZERO
            ENDDO
          ENDIF
         IF(IALELAG > 0) THEN
#include "vectorize.inc"
             DO  I=NODFT,NODLT
               IKN  = I   + NUMNOD
               IKN1 = IKN + NUMNOD
C             
               AF(1,I) = AF(1,I) + AF(1,IKN) + AF(1,IKN1)
               AF(2,I) = AF(2,I) + AF(2,IKN) + AF(2,IKN1)
               AF(3,I) = AF(3,I) + AF(3,IKN) + AF(3,IKN1)
               AF(1,IKN) = ZERO
               AF(2,IKN) = ZERO
               AF(3,IKN) = ZERO
               AF(1,IKN1) = ZERO
               AF(2,IKN1) = ZERO
               AF(3,IKN1) = ZERO
              ENDDO
          ENDIF
C        
          IF(SOL2SPH_FLAG /=0) THEN
#include "vectorize.inc"
           DO  I=NODFT,NODLT
            IKN = I+ NUMNOD
            IKN1 = IKN + NUMNOD
            DMSPH(I) = DMSPH(I) + DMSPH(IKN) + DMSPH(IKN1)
            DMSPH(IKN)  = ZERO
            DMSPH(IKN1) = ZERO
           ENDDO
          ENDIF
C
          KM  = NUM7
          KM1 = NUM7 + NUM7
#include "vectorize.inc"
          DO 260 I=PARTFT,PARTLT
           PARTSAV(I) = PARTSAV(I) + PARTSAV(I+KM) + PARTSAV(I+KM1)
           PARTSAV(I+KM) = ZERO
           PARTSAV(I+KM1) = ZERO
 260      CONTINUE
          KM3 = NUM8
          KM4 = NUM8 + NUM8
          IF (NTHPART > 0) THEN
#include "vectorize.inc"
            DO 270 I=GREFT,GRELT
             GRESAV(I) = GRESAV(I) + GRESAV(I+KM3) + GRESAV(I+KM4)
             GRESAV(I+KM3) = ZERO
             GRESAV(I+KM4) = ZERO
 270        CONTINUE
          ENDIF
      GOTO 1000
C------------
C     4 TASKS
 300  CONTINUE
#include "vectorize.inc"
          DO 320 I=NODFT,NODLT
            IKN  = I    + NUMNOD
            IKN1 = IKN  + NUMNOD
            IKN2 = IKN1 + NUMNOD
            STIFN(I) = STIFN(I) + STIFN(IKN) +
     .                 STIFN(IKN1) + STIFN(IKN2)
            STIFN(IKN) = ZERO
            STIFN(IKN1) = ZERO
            STIFN(IKN2) = ZERO
            A(1,I) = A(1,I) + A(1,IKN) + A(1,IKN1) + A(1,IKN2)
            A(2,I) = A(2,I) + A(2,IKN) + A(2,IKN1) + A(2,IKN2)
            A(3,I) = A(3,I) + A(3,IKN) + A(3,IKN1) + A(3,IKN2)
            A(1,IKN)  = ZERO
            A(2,IKN)  = ZERO
            A(3,IKN)  = ZERO
            A(1,IKN1) = ZERO
            A(2,IKN1) = ZERO
            A(3,IKN1) = ZERO
            A(1,IKN2) = ZERO
            A(2,IKN2) = ZERO
            A(3,IKN2) = ZERO
 320      CONTINUE

         IF(NPINCH > 0) THEN
#include "vectorize.inc"
           DO I=NODFT,NODLT
             IKN  = I    + NUMNOD
             IKN1 = IKN  + NUMNOD
             IKN2 = IKN1 + NUMNOD
             STIFPINCH(I) = STIFPINCH(I) + STIFPINCH(IKN) +  STIFPINCH(IKN1) + STIFPINCH(IKN2)
             STIFPINCH(IKN) = ZERO
             STIFPINCH(IKN1) = ZERO
             STIFPINCH(IKN2) = ZERO
             APINCH(1,I) = APINCH(1,I) + APINCH(1,IKN) + APINCH(1,IKN1) + APINCH(1,IKN2)
             APINCH(2,I) = APINCH(2,I) + APINCH(2,IKN) + APINCH(2,IKN1) + APINCH(2,IKN2)
             APINCH(3,I) = APINCH(3,I) + APINCH(3,IKN) + APINCH(3,IKN1) + APINCH(3,IKN2)
             APINCH(1,IKN)  = ZERO
             APINCH(2,IKN)  = ZERO
             APINCH(3,IKN)  = ZERO
             APINCH(1,IKN1) = ZERO
             APINCH(2,IKN1) = ZERO
             APINCH(3,IKN1) = ZERO
             APINCH(1,IKN2) = ZERO
             APINCH(2,IKN2) = ZERO
             APINCH(3,IKN2) = ZERO
           ENDDO
         ENDIF

          IF (IRODDL/=0) THEN
#include "vectorize.inc"
           DO 340 I=NODFT,NODLT
            IKN  = I    + NUMNOD
            IKN1 = IKN  + NUMNOD
            IKN2 = IKN1 + NUMNOD
            STIFR(I) = STIFR(I) + STIFR(IKN) +
     .                 STIFR(IKN1) + STIFR(IKN2)
            STIFR(IKN) = ZERO
            STIFR(IKN1) = ZERO
            STIFR(IKN2) = ZERO
            AR(1,I) = AR(1,I) + AR(1,IKN) + AR(1,IKN1) + AR(1,IKN2)
            AR(2,I) = AR(2,I) + AR(2,IKN) + AR(2,IKN1) + AR(2,IKN2)
            AR(3,I) = AR(3,I) + AR(3,IKN) + AR(3,IKN1) + AR(3,IKN2)
            AR(1,IKN)  = ZERO
            AR(2,IKN)  = ZERO
            AR(3,IKN)  = ZERO
            AR(1,IKN1) = ZERO
            AR(2,IKN1) = ZERO
            AR(3,IKN1) = ZERO
            AR(1,IKN2) = ZERO
            AR(2,IKN2) = ZERO
            AR(3,IKN2) = ZERO
 340       CONTINUE
          ENDIF
C
          IF(ITHERM_FE > 0 ) THEN
#include "vectorize.inc"
           DO  I=NODFT,NODLT
            IKN  = I   + NUMNOD
            IKN1 = IKN + NUMNOD
            IKN2 = IKN1 + NUMNOD
            FTHE(I)    = FTHE(I) + FTHE(IKN) + FTHE(IKN1) + FTHE(IKN2)
            FTHE(IKN)  = ZERO
            FTHE(IKN1) = ZERO
            FTHE(IKN2) = ZERO
           ENDDO
          ENDIF
C
          IF(NODADT_THERM > 0 ) THEN
#include "vectorize.inc"
           DO  I=NODFT,NODLT
            IKN  = I   + NUMNOD
            IKN1 = IKN + NUMNOD
            IKN2 = IKN1 + NUMNOD
            CONDN(I)    = CONDN(I) + CONDN(IKN) + CONDN(IKN1) + CONDN(IKN2)
            CONDN(IKN)  = ZERO
            CONDN(IKN1) = ZERO
            CONDN(IKN2) = ZERO
           ENDDO
          ENDIF
C        
          IF(ISTATCND /=0) THEN
           DO  I=NODFT,NODLT
            IKN = I+ NUMNOD
            IKN1 = IKN + NUMNOD
            IKN2 = IKN1 + NUMNOD
            STCND(I) = STCND(I) + STCND(IKN) + STCND(IKN1) + STCND(IKN2)
            STCND(IKN) = ZERO
            STCND(IKN1) = ZERO
            STCND(IKN2) = ZERO
           ENDDO
          ENDIF
C
          IF(IALELAG > 0) THEN
#include "vectorize.inc"
             DO  I=NODFT,NODLT
               IKN  = I   + NUMNOD
               IKN1 = IKN + NUMNOD
               IKN2 = IKN1 + NUMNOD
C             
               A(1,I) = A(1,I) + A(1,IKN) + A(1,IKN1) + A(1,IKN2)
               A(2,I) = A(2,I) + A(2,IKN) + A(2,IKN1) + A(2,IKN2)
               A(3,I) = A(3,I) + A(3,IKN) + A(3,IKN1) + A(3,IKN2)
               A(1,IKN)  = ZERO
               A(2,IKN)  = ZERO
               A(3,IKN)  = ZERO
               A(1,IKN1) = ZERO
               A(2,IKN1) = ZERO
               A(3,IKN1) = ZERO
               A(1,IKN2) = ZERO
               A(2,IKN2) = ZERO
               A(3,IKN2) = ZERO
              ENDDO
          ENDIF
C
C         viscosite
          IF(KDTINT/=0)THEN
#include "vectorize.inc"
            DO I=NODFT,NODLT
             IKN  = I    + NUMNOD
             IKN1 = IKN  + NUMNOD
             IKN2 = IKN1 + NUMNOD
             VISCN(I) = VISCN(I) + VISCN(IKN) +
     .                  VISCN(IKN1) + VISCN(IKN2)
             VISCN(IKN) = ZERO
             VISCN(IKN1) = ZERO
             VISCN(IKN2) = ZERO
            ENDDO
          ENDIF
C        
          IF(SOL2SPH_FLAG /=0) THEN
#include "vectorize.inc"
           DO  I=NODFT,NODLT
            IKN = I+ NUMNOD
            IKN1 = IKN + NUMNOD
            IKN2 = IKN1 + NUMNOD
            DMSPH(I) = DMSPH(I) + DMSPH(IKN) + DMSPH(IKN1) + DMSPH(IKN2)
            DMSPH(IKN)  = ZERO
            DMSPH(IKN1) = ZERO
            DMSPH(IKN2) = ZERO
           ENDDO
          ENDIF
C
          KM  = NUM7
          KM1 = KM  + NUM7
          KM2 = KM1 + NUM7
#include "vectorize.inc"
          DO 360 I=PARTFT,PARTLT
           PARTSAV(I) = PARTSAV(I) + PARTSAV(I+KM) +
     .                PARTSAV(I+KM1) + PARTSAV(I+KM2)
           PARTSAV(I+KM)  = ZERO
           PARTSAV(I+KM1) = ZERO
           PARTSAV(I+KM2) = ZERO
 360      CONTINUE
          KM3 = NUM8
          KM4 = KM3 + NUM8
          KM5 = KM4 + NUM8
          IF (NTHPART > 0) THEN
#include "vectorize.inc"
            DO 370 I=GREFT,GRELT
             GRESAV(I) = GRESAV(I) + GRESAV(I+KM3) +
     .                GRESAV(I+KM4) + GRESAV(I+KM5)
             GRESAV(I+KM3)  = ZERO
             GRESAV(I+KM4) = ZERO
             GRESAV(I+KM5) = ZERO
 370        CONTINUE
          ENDIF
      GOTO 1000
C------------
C     NTHREADS > 4
 900  CONTINUE
      KN = 0
      KM = 0
      KM3 = 0
      DO 950 K=1,NTHREAD-1
          KN = KN + NUMNOD
#include "vectorize.inc"
          DO 930 I=NODFT,NODLT
            IKN = I+KN
            STIFN(I) = STIFN(I) + STIFN(IKN)
            STIFN(IKN) = ZERO
            A(1,I) = A(1,I) + A(1,IKN)
            A(2,I) = A(2,I) + A(2,IKN)
            A(3,I) = A(3,I) + A(3,IKN)
            A(1,IKN) = ZERO
            A(2,IKN) = ZERO
            A(3,IKN) = ZERO
 930      CONTINUE

         IF (NPINCH > 0) THEN
#include "vectorize.inc"
           DO I=NODFT,NODLT
             IKN = I+KN
             STIFPINCH(I) = STIFPINCH(I) + STIFPINCH(IKN)
             STIFPINCH(IKN) = ZERO
             APINCH(1,I) = APINCH(1,I) + APINCH(1,IKN)
             APINCH(2,I) = APINCH(2,I) + APINCH(2,IKN)
             APINCH(3,I) = APINCH(3,I) + APINCH(3,IKN)
             APINCH(1,IKN) = ZERO
             APINCH(2,IKN) = ZERO
             APINCH(3,IKN) = ZERO
           ENDDO
         ENDIF

          IF (IRODDL/=0) THEN
#include "vectorize.inc"
           DO 935 I=NODFT,NODLT
            IKN = I+KN
            STIFR(I) = STIFR(I) + STIFR(IKN)
            STIFR(IKN) = ZERO
            AR(1,I) = AR(1,I) + AR(1,IKN)
            AR(2,I) = AR(2,I) + AR(2,IKN)
            AR(3,I) = AR(3,I) + AR(3,IKN)
            AR(1,IKN) = ZERO
            AR(2,IKN) = ZERO
            AR(3,IKN) = ZERO
 935       CONTINUE
          ENDIF
C
          IF(ITHERM_FE > 0 ) THEN
#include "vectorize.inc"
           DO  I=NODFT,NODLT
            IKN  = I + KN
            FTHE(I)    = FTHE(I) + FTHE(IKN)
            FTHE(IKN)  = ZERO
           ENDDO
          ENDIF
C
          IF(NODADT_THERM > 0 ) THEN
#include "vectorize.inc"
           DO  I=NODFT,NODLT
            IKN  = I + KN
            CONDN(I)   = CONDN(I) + CONDN(IKN)
            CONDN(IKN) = ZERO
           ENDDO
          ENDIF
C        
          IF(ISTATCND /=0) THEN
           DO  I=NODFT,NODLT
            IKN = I + KN
            STCND(I) = STCND(I) + STCND(IKN)
            STCND(IKN) = ZERO
           ENDDO
          ENDIF
C
          IF(IALELAG > 0) THEN
#include "vectorize.inc"
             DO  I=NODFT,NODLT
              IKN = I+KN
              AF(1,I) = AF(1,I) + AF(1,IKN)
              AF(2,I) = AF(2,I) + AF(2,IKN)
              AF(3,I) = AF(3,I) + AF(3,IKN)
              AF(1,IKN) = ZERO
              AF(2,IKN) = ZERO
              AF(3,IKN) = ZERO
             ENDDO
          ENDIF
C         viscosite
          IF(KDTINT/=0)THEN
#include "vectorize.inc"
              DO I=NODFT,NODLT
                IKN = I+KN
                VISCN(I)   = VISCN(I) + VISCN(IKN)
                VISCN(IKN) = ZERO
              ENDDO
          ENDIF
C         
          IF(SOL2SPH_FLAG/=0)THEN
#include "vectorize.inc"
            DO I=NODFT,NODLT
              IKN = I+KN
              DMSPH(I)   = DMSPH(I) + DMSPH(IKN)
              DMSPH(IKN) = ZERO
            ENDDO
          ENDIF
C
          KM = KM + NUM7
#include "vectorize.inc"
          DO 940 I=PARTFT,PARTLT
           PARTSAV(I) = PARTSAV(I) + PARTSAV(I+KM)
           PARTSAV(I+KM) = ZERO
 940      CONTINUE
          KM3 = KM3 + NUM8
          IF (NTHPART > 0) THEN
#include "vectorize.inc"
            DO 960 I=GREFT,GRELT
             GRESAV(I) = GRESAV(I) + GRESAV(I+KM3)
             GRESAV(I+KM3) = ZERO
 960        CONTINUE
          ENDIF
 950  CONTINUE
C
 1000 CONTINUE
      RETURN
      END
