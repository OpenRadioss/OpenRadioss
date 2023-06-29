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
Chd|  CORTH3                        source/elements/shell/coque/corth3.F
Chd|-- called by -----------
Chd|        C3EPSINI                      source/elements/sh3n/coque3n/c3epsini.F
Chd|        C3INIT3                       source/elements/sh3n/coque3n/c3init3.F
Chd|        CBAINIT3                      source/elements/shell/coqueba/cbainit3.F
Chd|        CDKEPSINI                     source/elements/sh3n/coquedk/cdkepsini.F
Chd|        CDKINIT3                      source/elements/sh3n/coquedk/cdkinit3.F
Chd|        CEPSINI                       source/elements/shell/coque/cepsini.F
Chd|        CINIT3                        source/elements/shell/coque/cinit3.F
Chd|        CNEPSINI                      source/elements/shell/coqueba/cnepsini.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE CORTH3(ELBUF_STR,DIR_A   ,DIR_B   ,JFT    ,JLT    ,
     .                  NLAY     ,IREP    ,NEL     ,
     .                  X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .                  Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .                  E1X, E2X, E3X, E1Y, E2Y, E3Y ,E1Z, E2Z, E3Z ,
     .                  IDRAPE, IGTYP )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,NLAY,IREP,NEL,IGTYP,IDRAPE
      my_real
     .   DIR_A(*),DIR_B(*)
      my_real, DIMENSION(MVSIZ), INTENT(IN)  :: 
     .   X1,X2,X3,X4,Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,
     .   E1X, E2X, E3X, E1Y, E2Y, E3Y ,E1Z, E2Z, E3Z
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,N,ILAW,IDIR,IT,NPTT
C     REAL
      my_real
     .  X31(MVSIZ), Y31(MVSIZ), Z31(MVSIZ),
     .  X32(MVSIZ), Y32(MVSIZ), Z32(MVSIZ),
     .  X21(MVSIZ), Y21(MVSIZ), Z21(MVSIZ),
     .  E11(MVSIZ),E12(MVSIZ),E13(MVSIZ),
     .  E21(MVSIZ),E22(MVSIZ),E23(MVSIZ)
      my_real
     .     V1,V2,V3,VR,VS,AA,BB,SUMA,S1,S2
      my_real, 
     .   DIMENSION(:) , POINTER :: DIR1, DIR2
      TYPE(L_BUFEl_DIR_), POINTER :: LBUF_DIR
C=======================================================================
      IF (ITY == 3)THEN                    
C---    coque 4N                           
        DO I=JFT,JLT                       
          E11(I)= X2(I)+X3(I)-X1(I)-X4(I)  
          E12(I)= Y2(I)+Y3(I)-Y1(I)-Y4(I)  
          E13(I)= Z2(I)+Z3(I)-Z1(I)-Z4(I)  
          E21(I)= X3(I)+X4(I)-X1(I)-X2(I)  
          E22(I)= Y3(I)+Y4(I)-Y1(I)-Y2(I)  
          E23(I)= Z3(I)+Z4(I)-Z1(I)-Z2(I)  
        ENDDO                              
      ELSEIF (ITY == 7) THEN               
C---    coque 3N                           
        DO I=JFT,JLT                       
          E11(I)= X2(I)-X1(I)              
          E12(I)= Y2(I)-Y1(I)              
          E13(I)= Z2(I)-Z1(I)              
          E21(I)= X3(I)-X1(I)              
          E22(I)= Y3(I)-Y1(I)              
          E23(I)= Z3(I)-Z1(I)              
        ENDDO                              
      ENDIF                                
C
      IF((IGTYP == 51 .OR. IGTYP == 52) .AND. IDRAPE > 0) THEN
          IF (ELBUF_STR%BUFLY(1)%LY_DIRA == 0) THEN
            IDIR = 0
            DO N=1,NLAY  
             NPTT =  ELBUF_STR%BUFLY(N)%NPTT
             DO IT=1,NPTT                          
              J = IDIR + (IT-1)*NEL*2
              DO I=JFT,JLT                          
                DIR_A(J+I) = ONE          
                DIR_A(J+I+NEL) = ZERO                    
              ENDDO                                 
            ENDDO   
            IDIR = IDIR + 2*NEL*NPTT    
           ENDDO                             
          ELSEIF (IREP == 0) THEN
            IDIR = 0
            DO N=1,NLAY
              NPTT =  ELBUF_STR%BUFLY(N)%NPTT             
              DO IT=1,NPTT                           
                DIR1 => ELBUF_STR%BUFLY(N)%LBUF_DIR(IT)%DIRA
                J = IDIR + (IT-1)*NEL*2
                DO I=JFT,JLT                          
                   DIR_A(J+I) = DIR1(I)
                   DIR_A(J+I+NEL) = DIR1(I+NEL)
                ENDDO                                 
              ENDDO 
              IDIR = IDIR + 2*NEL*NPTT
             ENDDO           
          ELSEIF (IREP == 1) THEN
            IDIR = 0
            DO N=1,NLAY                            
             NPTT =  ELBUF_STR%BUFLY(N)%NPTT
             DO IT=1,NPTT                          
              J = IDIR + (IT-1)*NEL*2
              DO I=JFT,JLT                        
                AA = DIR1(J+I)                    
                BB = DIR1(J+I+NEL)                    
                V1 = AA*E11(I) + BB*E21(I)        
                V2 = AA*E12(I) + BB*E22(I)        
                V3 = AA*E13(I) + BB*E23(I)        
                VR = V1*E1X(I) + V2*E1Y(I) + V3*E1Z(I)  
                VS = V1*E2X(I) + V2*E2Y(I) + V3*E2Z(I)  
                SUMA=SQRT(VR*VR + VS*VS)          
                DIR_A(J+I) = VR/SUMA                   
                DIR_A(J+I+NEL) = VS/SUMA               
              ENDDO 
             ENDDO 
             IDIR = IDIR + 2*NPTT*NEL                              
            ENDDO                                 
          ELSEIF (IREP == 2) THEN
            IDIR = 0
            DO N=1,NLAY
              NPTT =  ELBUF_STR%BUFLY(N)%NPTT             
              DO IT=1,NPTT                           
                LBUF_DIR => ELBUF_STR%BUFLY(N)%LBUF_DIR(IT)    
                DIR1 => LBUF_DIR%DIRA
                DIR2 => LBUF_DIR%DIRB
                J = IDIR + (IT-1)*NEL*2
                DO I=JFT,JLT
C---              Axe I
                  AA = DIR1(I)
                  BB = DIR1(I+NEL)
                  V1 = AA*E11(I) + BB*E21(I)
                  V2 = AA*E12(I) + BB*E22(I)
                  V3 = AA*E13(I) + BB*E23(I)
                  VR = V1*E1X(I) + V2*E1Y(I) + V3*E1Z(I)
                  VS = V1*E2X(I) + V2*E2Y(I) + V3*E2Z(I)
                  SUMA = ONE / MAX( SQRT(VR*VR + VS*VS), EM20)
                  DIR_A(J+I) = VR*SUMA
                  DIR_A(J+I+NEL) = VS*SUMA
C---              Axe II
                  AA = DIR2(I)
                  BB = DIR2(I+NEL)
                  V1 = AA*E11(I) + BB*E21(I)
                  V2 = AA*E12(I) + BB*E22(I)
                  V3 = AA*E13(I) + BB*E23(I)
                  VR = V1*E1X(I) + V2*E1Y(I) + V3*E1Z(I)
                  VS = V1*E2X(I) + V2*E2Y(I) + V3*E2Z(I)
                  SUMA = ONE / MAX( SQRT(VR*VR + VS*VS), EM20)
                  DIR_B(J+I) = VR*SUMA
                  DIR_B(J+I+NEL) = VS*SUMA
                ENDDO
             ENDDO 
             IDIR = IDIR + 2*NPTT*NEL 
            ENDDO
          ELSEIF (IREP == 3) THEN
C   mi    xing law58 with other user laws with IREP = 0 within PID51
            IDIR =  0
            DO N=1,NLAY
              ILAW = ELBUF_STR%BUFLY(N)%ILAW 
              NPTT =  ELBUF_STR%BUFLY(N)%NPTT             
              IF (ILAW == 58) THEN
                 DO IT=1,NPTT
                  J = IDIR + (IT-1)*NEL*2
                  LBUF_DIR =>ELBUF_STR%BUFLY(N)%LBUF_DIR(IT)    
                  DIR1 => LBUF_DIR%DIRA
                  DIR2 => LBUF_DIR%DIRB
                  DO I=JFT,JLT
C---              Axe I
                    AA = DIR1(I)
                    BB = DIR1(I+NEL)
                    V1 = AA*E11(I) + BB*E21(I)
                    V2 = AA*E12(I) + BB*E22(I)
                    V3 = AA*E13(I) + BB*E23(I)
                    VR = V1*E1X(I) + V2*E1Y(I) + V3*E1Z(I)
                    VS = V1*E2X(I) + V2*E2Y(I) + V3*E2Z(I)
                    SUMA = ONE / MAX( SQRT(VR*VR + VS*VS), EM20)
                    DIR_A(J+I) = VR*SUMA
                    DIR_A(J+I+NEL) = VS*SUMA
C---              Axe II
                    AA = DIR2(I)
                    BB = DIR2(I+NEL)
                    V1 = AA*E11(I) + BB*E21(I)
                    V2 = AA*E12(I) + BB*E22(I)
                    V3 = AA*E13(I) + BB*E23(I)
                    VR = V1*E1X(I) + V2*E1Y(I) + V3*E1Z(I)
                    VS = V1*E2X(I) + V2*E2Y(I) + V3*E2Z(I)
                    SUMA = ONE / MAX( SQRT(VR*VR + VS*VS), EM20)
                    DIR_B(J+I) = VR*SUMA
                    DIR_B(J+I+NEL) = VS*SUMA
                  ENDDO
                 ENDDO
                 IDIR = IDIR + 2*NEL*NPTT 
                ELSE  ! IREP = 0 within PID51
                 DO IT = 1, NPTT
                  J = IDIR + (IT-1)*NEL*2
                  LBUF_DIR =>ELBUF_STR%BUFLY(N)%LBUF_DIR(IT)    
                  DIR1 => LBUF_DIR%DIRA
                  DO I=JFT,JLT                          
                    DIR_A(J+I) = DIR1(I)                   
                    DIR_A(J+I+NEL) = DIR1(I+NEL)                   
                  ENDDO
                 ENDDO
                 IDIR = IDIR + 2*NEL*NPTT 
                ENDIF ! IF (ILAW == 58) THEN  
            ENDDO ! DO N=1,NLAY
          ELSEIF (IREP == 4) THEN
C   mi    xing law58 with other user laws with IREP = 1 within PID51
            IDIR = 0
            DO N=1,NLAY
              ILAW = ELBUF_STR%BUFLY(N)%ILAW
              NPTT =  ELBUF_STR%BUFLY(N)%NPTT  
              IF (ILAW == 58) THEN
                 DO IT=1,NPTT
                  J = IDIR + (IT-1)*NEL*2
                  LBUF_DIR =>ELBUF_STR%BUFLY(N)%LBUF_DIR(IT)    
                  DIR1 => LBUF_DIR%DIRA
                  DIR2 => LBUF_DIR%DIRB
                  DO I=JFT,JLT
C---              Axe I
                    AA = DIR1(I)
                    BB = DIR1(I+NEL)
                    V1 = AA*E11(I) + BB*E21(I)
                    V2 = AA*E12(I) + BB*E22(I)
                    V3 = AA*E13(I) + BB*E23(I)
                    VR = V1*E1X(I) + V2*E1Y(I) + V3*E1Z(I)
                    VS = V1*E2X(I) + V2*E2Y(I) + V3*E2Z(I)
                    SUMA = ONE / MAX( SQRT(VR*VR + VS*VS), EM20)
                    DIR_A(J+I) = VR*SUMA
                    DIR_A(J+I+NEL) = VS*SUMA
C---              Axe II
                    AA = DIR2(I)
                    BB = DIR2(I+NEL)
                    V1 = AA*E11(I) + BB*E21(I)
                    V2 = AA*E12(I) + BB*E22(I)
                    V3 = AA*E13(I) + BB*E23(I)
                    VR = V1*E1X(I) + V2*E1Y(I) + V3*E1Z(I)
                    VS = V1*E2X(I) + V2*E2Y(I) + V3*E2Z(I)
                    SUMA = ONE / MAX( SQRT(VR*VR + VS*VS), EM20)
                    DIR_B(J+I) = VR*SUMA
                    DIR_B(J+I+NEL) = VS*SUMA
                  ENDDO
                 ENDDO
                 IDIR = IDIR + 2*NEL*NPTT 
              ELSE  ! IREP = 1 within PID51
               DO IT=1,NPTT
                   J = IDIR + (IT-1)*NEL*2
                   LBUF_DIR =>ELBUF_STR%BUFLY(N)%LBUF_DIR(IT)    
                   DIR1 => LBUF_DIR%DIRA
                   DO I=JFT,JLT                        
                     AA = DIR1(I)                    
                     BB = DIR1(I+NEL)                    
                     V1 = AA*E11(I) + BB*E21(I)        
                     V2 = AA*E12(I) + BB*E22(I)        
                     V3 = AA*E13(I) + BB*E23(I)        
                     VR = V1*E1X(I) + V2*E1Y(I) + V3*E1Z(I)  
                     VS = V1*E2X(I) + V2*E2Y(I) + V3*E2Z(I)  
                     SUMA=SQRT(VR*VR + VS*VS)          
                     DIR_A(J+I) = VR/SUMA           
                     DIR_A(J+I+NEL) = VS/SUMA               
                   ENDDO
                 ENDDO
                 IDIR = IDIR + 2*NEL*NPTT  
              ENDIF ! IF (ILAW == 58) THEN
            ENDDO ! DO N=1,NLAY
          ENDIF
      ELSE
          IF (ELBUF_STR%BUFLY(1)%LY_DIRA == 0) THEN
            DO N=1,NLAY                              
              J = (N-1)*NEL*2
              DO I=JFT,JLT                          
                DIR_A(J+I) = ONE          
                DIR_A(J+I+NEL) = ZERO                    
              ENDDO                                 
            ENDDO                                   
          ELSEIF (IREP == 0) THEN
            DO N=1,NLAY                              
              DIR1 => ELBUF_STR%BUFLY(N)%DIRA   
              J = (N-1)*NEL*2
              DO I=JFT,JLT                          
                DIR_A(J+I) = DIR1(I)
                DIR_A(J+I+NEL) = DIR1(I+NEL)
              ENDDO                                 
            ENDDO                                   
          ELSEIF (IREP == 1) THEN
            DO N=1,NLAY                            
              DIR1 => ELBUF_STR%BUFLY(N)%DIRA
              J = (N-1)*NEL*2
              DO I=JFT,JLT                        
                AA = DIR1(I)                    
                BB = DIR1(I+NEL)                    
                V1 = AA*E11(I) + BB*E21(I)        
                V2 = AA*E12(I) + BB*E22(I)        
                V3 = AA*E13(I) + BB*E23(I)        
                VR = V1*E1X(I) + V2*E1Y(I) + V3*E1Z(I)  
                VS = V1*E2X(I) + V2*E2Y(I) + V3*E2Z(I)  
                SUMA=SQRT(VR*VR + VS*VS)          
                DIR_A(J+I) = VR/SUMA                   
                DIR_A(J+I+NEL) = VS/SUMA               
              ENDDO                               
            ENDDO                                 
          ELSEIF (IREP == 2) THEN
            DO N=1,NLAY
              DIR1 => ELBUF_STR%BUFLY(N)%DIRA
              DIR2 => ELBUF_STR%BUFLY(N)%DIRB
              J = (N-1)*NEL*2
              DO I=JFT,JLT
C---            Axe I
                AA = DIR1(I)
                BB = DIR1(I+NEL)
                V1 = AA*E11(I) + BB*E21(I)
                V2 = AA*E12(I) + BB*E22(I)
                V3 = AA*E13(I) + BB*E23(I)
                VR = V1*E1X(I) + V2*E1Y(I) + V3*E1Z(I)
                VS = V1*E2X(I) + V2*E2Y(I) + V3*E2Z(I)
                SUMA = ONE / MAX( SQRT(VR*VR + VS*VS), EM20)
                DIR_A(J+I) = VR*SUMA
                DIR_A(J+I+NEL) = VS*SUMA
C---            Axe II
                AA = DIR2(I)
                BB = DIR2(I+NEL)
                V1 = AA*E11(I) + BB*E21(I)
                V2 = AA*E12(I) + BB*E22(I)
                V3 = AA*E13(I) + BB*E23(I)
                VR = V1*E1X(I) + V2*E1Y(I) + V3*E1Z(I)
                VS = V1*E2X(I) + V2*E2Y(I) + V3*E2Z(I)
                SUMA = ONE / MAX( SQRT(VR*VR + VS*VS), EM20)
                DIR_B(J+I) = VR*SUMA
                DIR_B(J+I+NEL) = VS*SUMA
              ENDDO
            ENDDO
          ELSEIF (IREP == 3) THEN
C   mi    xing law58 with other user laws with IREP = 0 within PID51
            DO N=1,NLAY
              ILAW = ELBUF_STR%BUFLY(N)%ILAW
              J = (N-1)*NEL*2
              IF (ILAW == 58) THEN
                DIR1 => ELBUF_STR%BUFLY(N)%DIRA
                DIR2 => ELBUF_STR%BUFLY(N)%DIRB
                DO I=JFT,JLT
C---            Axe I
                  AA = DIR1(I)
                  BB = DIR1(I+NEL)
                  V1 = AA*E11(I) + BB*E21(I)
                  V2 = AA*E12(I) + BB*E22(I)
                  V3 = AA*E13(I) + BB*E23(I)
                  VR = V1*E1X(I) + V2*E1Y(I) + V3*E1Z(I)
                  VS = V1*E2X(I) + V2*E2Y(I) + V3*E2Z(I)
                  SUMA = ONE / MAX( SQRT(VR*VR + VS*VS), EM20)
                  DIR_A(J+I) = VR*SUMA
                  DIR_A(J+I+NEL) = VS*SUMA
C---            Axe II
                  AA = DIR2(I)
                  BB = DIR2(I+NEL)
                  V1 = AA*E11(I) + BB*E21(I)
                  V2 = AA*E12(I) + BB*E22(I)
                  V3 = AA*E13(I) + BB*E23(I)
                  VR = V1*E1X(I) + V2*E1Y(I) + V3*E1Z(I)
                  VS = V1*E2X(I) + V2*E2Y(I) + V3*E2Z(I)
                  SUMA = ONE / MAX( SQRT(VR*VR + VS*VS), EM20)
                  DIR_B(J+I) = VR*SUMA
                  DIR_B(J+I+NEL) = VS*SUMA
                ENDDO
              ELSE  ! IREP = 0 within PID51
                DIR1 => ELBUF_STR%BUFLY(N)%DIRA
                DO I=JFT,JLT                          
                  DIR_A(J+I) = DIR1(I)                   
                  DIR_A(J+I+NEL) = DIR1(I+NEL)                     
                ENDDO
              ENDIF ! IF (ILAW == 58) THEN
            ENDDO ! DO N=1,NLAY
          ELSEIF (IREP == 4) THEN
C   mi    xing law58 with other user laws with IREP = 1 within PID51
            DO N=1,NLAY
              ILAW = ELBUF_STR%BUFLY(N)%ILAW
              J = (N-1)*NEL*2
              IF (ILAW == 58) THEN
                DIR1 => ELBUF_STR%BUFLY(N)%DIRA
                DIR2 => ELBUF_STR%BUFLY(N)%DIRB
                DO I=JFT,JLT
C---            Axe I
                  AA = DIR1(I)
                  BB = DIR1(I+NEL)
                  V1 = AA*E11(I) + BB*E21(I)
                  V2 = AA*E12(I) + BB*E22(I)
                  V3 = AA*E13(I) + BB*E23(I)
                  VR = V1*E1X(I) + V2*E1Y(I) + V3*E1Z(I)
                  VS = V1*E2X(I) + V2*E2Y(I) + V3*E2Z(I)
                  SUMA = ONE / MAX( SQRT(VR*VR + VS*VS), EM20)
                  DIR_A(J+I) = VR*SUMA
                  DIR_A(J+I+NEL) = VS*SUMA
C---            Axe II
                  AA = DIR2(I)
                  BB = DIR2(I+NEL)
                  V1 = AA*E11(I) + BB*E21(I)
                  V2 = AA*E12(I) + BB*E22(I)
                  V3 = AA*E13(I) + BB*E23(I)
                  VR = V1*E1X(I) + V2*E1Y(I) + V3*E1Z(I)
                  VS = V1*E2X(I) + V2*E2Y(I) + V3*E2Z(I)
                  SUMA = ONE / MAX( SQRT(VR*VR + VS*VS), EM20)
                  DIR_B(J+I) = VR*SUMA
                  DIR_B(J+I+NEL) = VS*SUMA
                ENDDO
              ELSE  ! IREP = 1 within PID51
                DIR1 => ELBUF_STR%BUFLY(N)%DIRA
                DO I=JFT,JLT                        
                  AA = DIR1(I)                    
                  BB = DIR1(I+NEL)                    
                  V1 = AA*E11(I) + BB*E21(I)        
                  V2 = AA*E12(I) + BB*E22(I)        
                  V3 = AA*E13(I) + BB*E23(I)        
                  VR = V1*E1X(I) + V2*E1Y(I) + V3*E1Z(I)  
                  VS = V1*E2X(I) + V2*E2Y(I) + V3*E2Z(I)  
                  SUMA=SQRT(VR*VR + VS*VS)          
                  DIR_A(J+I) = VR/SUMA           
                  DIR_A(J+I+NEL) = VS/SUMA               
                ENDDO
              ENDIF ! IF (ILAW == 58) THEN
            ENDDO ! DO N=1,NLAY
          ENDIF
      ENDIF
C-----------
      RETURN
      END
