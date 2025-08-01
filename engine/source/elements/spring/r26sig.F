Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    r26sig                ../engine/source/elements/spring/r26sig.F
!||--- called by ------------------------------------------------------
!||    r26def3               ../engine/source/elements/spring/r26def3.F
!||--- calls      -----------------------------------------------------
!||    finter                ../engine/source/tools/curve/finter.F
!||--- uses       -----------------------------------------------------
!||    python_funct_mod      ../common_source/modules/python_mod.F90
!||====================================================================
      SUBROUTINE R26SIG(PYTHON, 
     1   FX,      XK,      DX,      DXOLD,
     2   E,       OFF,     XL0,     TF,
     3   NPF,     ANIM,    IANI,    FR_WAVE,
     4   DMN,     DMX,     IGEO,    GEO,
     5   PID,     DV0,     ALPHA,   NEL,
     6   NFT,     IAD)
      use python_funct_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      type(python_), intent(inout) :: python  
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: NFT
      INTEGER, INTENT(INOUT) :: IAD
      INTEGER IANI
      INTEGER NPF(*),IGEO(NPROPGI,*),PID(*)
C     REAL
      my_real
     .   GEO(NPROPG,*),FX(*),XK(*),DX(*),DXOLD(*),TF(*),XL0(*),E(*),
     .   OFF(*), DMN(*), DMX(*), ANIM(*),FR_WAVE(*),DV0(*),
     .   ALPHA(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, J1, J2
      INTEGER FUNC1(MVSIZ), FUNC2(MVSIZ),NFUNC(MVSIZ),NFUND(MVSIZ) 
C     REAL
      my_real
     .    DDX(MVSIZ),FOLD(MVSIZ),DVX(MVSIZ),DV(MVSIZ),DL(MVSIZ),
     .    RATE1(MVSIZ),RATE2(MVSIZ),YFAC1(MVSIZ), YFAC2(MVSIZ),
     .    FMIN(MVSIZ),FMAX(MVSIZ),XFAC(MVSIZ)
      my_real
     .    DF1,DF2,Y1,Y2,VEL,FAC,DAMP,DAMM,DT11
      INTEGER :: NFUNCT, PYID
C-----------------------------------------------
C   E x t e r n a l
C-----------------------------------------------
      my_real
     .    finter
C=======================================================================
      NFUNCT = PYTHON%funct_offset + PYTHON%nb_functs - PYTHON%nb_sensors! total number of functions = non-python + python
      DT11 = DT1
      IF (DT11 == ZERO) DT11 = EP30
      DO I=1,NEL
        E(I)    =E(I)/XL0(I)
        DX(I)   =DX(I)/XL0(I)
        DXOLD(I)=DXOLD(I)/XL0(I)
      ENDDO
C
      DO I=1,NEL
        FOLD(I)=FX(I)
        DDX(I)= (DX(I)-DXOLD(I))
        DVX(I)= DDX(I)/ DT11
        DV(I) = ABS(DVX(I))
        DL(I) = ABS(DX(I))
      ENDDO
      IF (TT == ZERO) THEN
        DO I=1,NEL
          DV0(I) = DV(I)
        ENDDO
      ENDIF
c     filtrage des vitesses
      DO I=1,NEL
        DV(I)  = (ONE-ALPHA(I))*DV0(I) + ALPHA(I)*DV(I)
        DV0(I) =  DV(I)
      ENDDO
C
      IF(IANI/=0)THEN
        DO I=1,NEL
          J =I+NFT
          DAMP=DX(I)/MAX(DMX(I),EM15)
          DAMM=DX(I)/MIN(DMN(I),-EM15)
          ANIM(J)=MAX(ANIM(J),DAMP,DAMM)
          ANIM(J)=MIN(ANIM(J),ONE)
        ENDDO
      ENDIF
C-------------------------------------
      DO I=1,NEL
        IF (OFF(I) == ONE) THEN 
          IF (DX(I) >= ZERO) THEN
            FX(I) = XK(I)*DX(I)
          ELSE
            FX(I) = FOLD(I) + XK(I)*DDX(I)
          ENDIF
          IAD = 100
C calcul du y_max
          NFUNC(I) = IGEO(20,PID(I))  
          XFAC(I) = GEO(5,PID(I))
          J1 = 1                                                   
          DO J=2,NFUNC(I)-1                                        
            VEL = GEO(IAD+100+J,PID(I))                            
            IF (DV(I) >= VEL) J1 = J                               
          ENDDO                                                    
          FUNC1(I) = IGEO(IAD+J1,PID(I))                           
          RATE1(I) = GEO(IAD+100+J1,PID(I))                        
          YFAC1(I) = GEO(IAD+200+J1,PID(I))                        
          PYID = python_funct_id(NFUNCT,FUNC1(I),NPF)
          IF(PYID > 0) FUNC1(I) = -PYID

          IF (NFUNC(I) > 1)THEN                                    
            J2 = J1+1                                              
            FUNC2(I) = IGEO(IAD+J2,PID(I))                         
            RATE2(I) = GEO(IAD+100+J2,PID(I))                      
            YFAC2(I) = GEO(IAD+200+J2,PID(I))                      
            PYID = python_funct_id(NFUNCT,FUNC2(I),NPF)
            IF(PYID > 0) FUNC2(I) = -PYID
C
            IF(FUNC1(I) >= 0) THEN
              Y1 = YFAC1(I)*FINTER(FUNC1(I),DL(I)/XFAC(I),NPF,TF,DF1)        
            ELSE ! python
              CALL PYTHON_CALL_FUNCT1D(PYTHON, -FUNC1(I),DL(I),Y1)
              Y1 = Y1*YFAC1(I)
            ENDIF
            IF(FUNC2(I) >= 0) THEN
              Y2 = YFAC2(I)*FINTER(FUNC2(I),DL(I)/XFAC(I),NPF,TF,DF2)         
            ELSE ! python
              CALL PYTHON_CALL_FUNCT1D(PYTHON, -FUNC2(I),DL(I),Y2)
              Y2 = Y2*YFAC2(I)
            ENDIF
C                                                                
            FAC     = (DV(I) - RATE1(I))/(RATE2(I) - RATE1(I))     
            FMAX(I) =  -MAX(Y1 + FAC*(Y2 - Y1), EM20)               
          ELSE                                                     
            IF(FUNC1(I) >= 0) THEN
              FMAX(I) =  -YFAC1(I)*FINTER(FUNC1(I),DL(I)/XFAC(I),NPF,TF,DF1)  
            ELSE ! python
              CALL PYTHON_CALL_FUNCT1D(PYTHON, -FUNC1(I),DL(I),FMAX(I))
              FMAX(I) = -FMAX(I)*YFAC1(I)  
             ENDIF
          ENDIF                                                    
C calcul du y_min  
          J1 = NFUNC(I) + 1                                        
          NFUND(I) = IGEO(21,PID(I))                               
          DO J=2,NFUND(I)-1                                        
            VEL = GEO(IAD+100+J,PID(I))                            
            IF (DV(I) >= VEL) J1 = NFUNC(I) + J                    
          ENDDO                                                    
          FUNC1(I) = IGEO(IAD+J1,PID(I))                           
          PYID = python_funct_id(NFUNCT,FUNC1(I),NPF)
          IF(PYID > 0) FUNC1(I) = -PYID

          RATE1(I) = GEO(IAD+100+J1,PID(I))                        
          YFAC1(I) = GEO(IAD+200+J1,PID(I))                        
C                                                                
          IF (NFUND(I) > 1) THEN                                   
            J2 = J1+1                                              
            FUNC2(I) = IGEO(IAD+J2,PID(I))                         
            PYID = python_funct_id(NFUNCT,FUNC2(I),NPF)
            IF(PYID > 0) FUNC2(I) = -PYID
            RATE2(I) = GEO(IAD+100+J2,PID(I))                      
            YFAC2(I) = GEO(IAD+200+J2,PID(I))                      
C
            IF(FUNC1(I) >= 0) THEN
              Y1 = YFAC1(I)*FINTER(FUNC1(I),DL(I)/XFAC(I),NPF,TF,DF1)        
            ELSE ! python
              CALL PYTHON_CALL_FUNCT1D(PYTHON, -FUNC1(I),DL(I),Y1)
              Y1 = Y1*YFAC1(I)
            ENDIF
            IF(FUNC1(I) >= 0) THEN
              Y2 = YFAC2(I)*FINTER(FUNC2(I),DL(I)/XFAC(I),NPF,TF,DF2)         
            ELSE ! python
              CALL PYTHON_CALL_FUNCT1D(PYTHON, -FUNC2(I),DL(I),Y2)
              Y2 = Y2*YFAC2(I)
            ENDIF
            FAC = (DV(I) - RATE1(I))/(RATE2(I) - RATE1(I))         
c          FMIN(I) = -MAX(Y2 + FAC*(Y1-Y2), EM20)                
            FMIN(I) =  -MAX(Y1 + FAC*(Y2 - Y1), EM20)               
          ELSE                                                     
            IF(FUNC1(I) >= 0) THEN
              FMIN(I) =  -YFAC1(I)*FINTER(FUNC1(I),DL(I)/XFAC(I),NPF,TF,DF1)  
            ELSE ! python
              CALL PYTHON_CALL_FUNCT1D(PYTHON, -FUNC1(I),DL(I),FMIN(I))
              FMIN(I) = -FMIN(I)*YFAC1(I)
            ENDIF
          ENDIF                                                    
C--------------
          IF (DX(I) < ZERO) THEN             
            IF (FX(I) > FMIN(I)) THEN    
              FX(I) = FMIN(I)            
            ELSEIF (FX(I) < FMAX(I) )THEN
              FX(I) = FMAX(I)            
            ENDIF                                                
          ENDIF                            
        ELSE
          FX(I) = ZERO
        ENDIF
      ENDDO
C--------------
      DO I=1,NEL
        IF (ABS(DDX(I)) > ZERO)  XK(I) = ABS(FX(I)-FOLD(I))/ABS(DDX(I))
        E(I) = E(I) + (DX(I)-DXOLD(I)) * (FX(I)+FOLD(I)) * HALF
        E(I) = E(I)*XL0(I)
        DX(I)= DX(I)*XL0(I)
        DXOLD( I)=DXOLD(I)*XL0(I)
      ENDDO
C---
      RETURN
      END
