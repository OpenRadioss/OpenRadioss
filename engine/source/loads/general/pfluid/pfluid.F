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
Chd|  PFLUID                        source/loads/general/pfluid/pfluid.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        GET_U_SENS_VALUE              source/user_interface/usensor.F
Chd|        SET_U_SENS_VALUE              source/user_interface/usensor.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE PFLUID(ILOADP  ,FAC     ,NPC    ,TF    ,A       ,
     2                  V       ,X       ,XFRAME ,AR    ,VR      ,
     3                  SENSOR  ,LSENSOR ,WEIGHT ,TFEXC ,IADC    ,
     4                  FSKY    ,FSKYV   ,LLOADP ,FEXT  ,H3D_DATA
     5                  )
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE H3D_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   E x t e r n a l  F u n c t i o n s
C-----------------------------------------------
      INTEGER  GET_U_NUMSENS,GET_U_SENS_FPAR,GET_U_SENS_IPAR,GET_U_SENS_VALUE,SET_U_SENS_VALUE
      EXTERNAL GET_U_NUMSENS,GET_U_SENS_FPAR,GET_U_SENS_IPAR,GET_U_SENS_VALUE,SET_U_SENS_VALUE
C-----------------------------------------------,
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPC(SNPC),LLOADP(SLLOADP)
      INTEGER ILOADP(SIZLOADP,NLOADP),LSENSOR(NSENSI,NSENSOR)
      INTEGER WEIGHT(SWEIGHT), IADC(DSNCOL)
      my_real FAC(LFACLOAD,NLOADP), TF(STF), A(3,NUMNOD), V(3,NUMNOD), AR(3,NUMNOD), VR(3,NUMNOD),
     .        X(3,NUMNOD), XFRAME(NXFRAME,NUMFRAM+1), SENSOR(NSENSR,NSENSOR),TFEXC,
     .        FSKY(8,SFSKY/8), FSKYV(SFSKY/8,8),FEXT(3,NUMNOD)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NL, N1, ISK, N2, N3, N4, FUN_HSP, K1, K2, K3, ISENS,K,LL,
     .        IERR,ICODE,IAD,N_OLD,IFRA1,N1FRAM,DIR_HSP,I,
     .        FUN_CX,FUN_VEL,DIR_VEL,IFRA2, IANIM,IJK,UP_BOUND
      my_real NX, NY, NZ, AXI, AA, A0, VV, FX, FY, FZ, AX, DYDX, TS,
     .        SIXTH,TFEXTT,X_OLD, F1, F2,XSENS,FCX,FCY, VSEGX, VSEGY, VSEGZ,
     .        FCX1,FCY1,FCX2,FCY2,VX,VY,VZ,VEL,VSEG,FINTER,
     .        COORMEAN,YMEAN,ZMEAN,PVEL,NORM,NSIGN,AREA
      EXTERNAL FINTER
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      ! init.
      SIXTH  = ONE_OVER_6  
      TFEXC  = ZERO
      TFEXTT = ZERO
      N_OLD  = 0
      X_OLD  = ZERO
      IANIM  = ANIM_V(5)+OUTP_V(5) + H3D_DATA%N_VECT_FINT + ANIM_V(6)+OUTP_V(6) + H3D_DATA%N_VECT_FEXT

      !-----------------------------------!
      !            PARITH/OFF             !
      !-----------------------------------!
      IF(IPARIT == 0) THEN
        DO NL=1,NLOADP_F                                                                      
          FUN_HSP=ILOADP(7,NL)                                                                     
          DIR_HSP=ILOADP(8,NL)                                                                     
          IFRA1=ILOADP(9,NL)                                                                       
          FCY = FAC(1,NL)                                                                          
          FCX = FAC(2,NL)                                                                          
          FUN_CX=ILOADP(10,NL)                                                                     
          FCY1 = FAC(3,NL)                                                                         
          FCX1 = FAC(4,NL)                                                                         
          FUN_VEL=ILOADP(11,NL)                                                                    
          FCY2 = FAC(5,NL)                                                                         
          FCX2 = FAC(6,NL)                                                                                                      
          DIR_VEL=MAX(ILOADP(12,NL),1)    !To avoid a check bound issue when the velocity options are not set in the input, the DIR_VEL variable is bounded to a minimal value of 1                                                         
          IFRA2=ILOADP(13,NL) 
          IFRA2=MAX(IFRA2,1)                                                                     
          ISENS=0                                                                                  
          XSENS = ONE                                                                              
          DO K=1,NSENSOR                                                                           
            IF(ILOADP(6,NL) == LSENSOR(1,K))ISENS=K                                                
          ENDDO                                                                                    
          IF(ISENS == 0)THEN                                                                       
             TS=TT                                                                                 
          ELSEIF (LSENSOR(4,ISENS)==753096514 .AND. TT /= ZERO) THEN                               
            IERR = SET_U_SENS_VALUE(ISENS,1,ZERO)                                                  
            IERR = GET_U_SENS_VALUE(ISENS,2,XSENS)                                                 
          ELSE                                                                                     
             TS = TT-SENSOR(2,ISENS)                                                               
             IF(TS < ZERO) CYCLE                                                                   
          ENDIF                                                                                    
          DO I = 1,ILOADP(1,NL)/4                                                                  
            N1=LLOADP(ILOADP(4,NL)+4*(I-1))                                                        
            N2=LLOADP(ILOADP(4,NL)+4*(I-1)+1)                                                      
            N3=LLOADP(ILOADP(4,NL)+4*(I-1)+2)                                                      
            N4=LLOADP(ILOADP(4,NL)+4*(I-1)+3)                                                      
            AA=ZERO                                                                              
            VEL=ZERO                                                                             
            PVEL=ZERO                                                                              
            !---QUADRAGLE--------------------------------------------
            IF(N4 /= 0 .AND. N1 /= N2 .AND. N1 /= N3 .AND. N1 /= N4 .AND. N2 /= N3 .AND. N2 /= N4 .AND. N3 /= N4 )THEN  !this check should be done at starter ! (can't we keep only case N4=0 for triangles ?)                      
              K1=3*DIR_HSP-2                                                                       
              K2=3*DIR_HSP-1                                                                       
              K3=3*DIR_HSP                                                                         
              ! hydrostatic pressure                                                               
              IF(FUN_HSP /= 0)THEN                                                                
                 COORMEAN =           (XFRAME(K1,IFRA1)*(X(1,N1)+X(1,N2)+X(1,N3)+X(1,N4))/FOUR)
                 COORMEAN = COORMEAN +(XFRAME(K2,IFRA1)*(X(2,N1)+X(2,N2)+X(2,N3)+X(2,N4))/FOUR)
                 COORMEAN = COORMEAN +(XFRAME(K3,IFRA1)*(X(3,N1)+X(3,N2)+X(3,N3)+X(3,N4))/FOUR)              
                 AA = FCY*FINTER(FUN_HSP,(COORMEAN-XFRAME(9+DIR_HSP,IFRA1))*FCX,NPC,TF,DYDX) 
              ENDIF                                                                                
              NX=(X(2,N3)-X(2,N1))*(X(3,N4)-X(3,N2)) - (X(3,N3)-X(3,N1))*(X(2,N4)-X(2,N2))        
              NY=(X(3,N3)-X(3,N1))*(X(1,N4)-X(1,N2)) - (X(1,N3)-X(1,N1))*(X(3,N4)-X(3,N2))        
              NZ=(X(1,N3)-X(1,N1))*(X(2,N4)-X(2,N2)) - (X(2,N3)-X(2,N1))*(X(1,N4)-X(1,N2))        
              NORM = SQRT(NX*NX+NY*NY+NZ*NZ)                                                       
              AREA = HALF * NORM                                                                   
              AA = AA * AREA  
                                                                                                                                                                    
              K1=3*DIR_VEL-2                                                                       
              K2=3*DIR_VEL-1                                                                       
              K3=3*DIR_VEL 
              VSEG =        (XFRAME(K1,IFRA2)*(V(1,N1) + V(1,N2) + V(1,N3) + V(1,N4)) /FOUR)              
              VSEG = VSEG + (XFRAME(K2,IFRA2)*(V(2,N1) + V(2,N2) + V(2,N3) + V(2,N4)) /FOUR)                             
              VSEG = VSEG + (XFRAME(K3,IFRA2)*(V(3,N1) + V(3,N2) + V(3,N3) + V(3,N4)) /FOUR)                               

              IF(FUN_VEL /= 0)THEN                                                                
                 VEL =  FCY2*FINTER(FUN_VEL,TT*FCX2,NPC,TF,DYDX) - VSEG                             
              ELSE                                                                                 
                 VEL =  -VSEG                                                                     
              ENDIF                                                                                                
              IF(FUN_CX /= 0)  THEN                                                                   
                PVEL = ( (-(NX/NORM)*VEL*XFRAME(K1,IFRA2)-(NY/NORM)*VEL*XFRAME(K2,IFRA2)-(NZ/NORM)*VEL*XFRAME(K3,IFRA2))**2 )                               
                PVEL=PVEL*FCY1*FINTER(FUN_CX,TT*FCX1,NPC,TF,DYDX)/TWO  
              ENDIF                                      
              NSIGN = VEL*(NX*XFRAME(K1,IFRA2) + NY*XFRAME(K2,IFRA2) +  NZ*XFRAME(K3,IFRA2))   !  <Vseg.d,n>  where d is flow direction X,Y,Z    ! IFRA2=1 is general frame, IFRA2 in [2:NFRAME+1] are for user frames
              NSIGN = SIGN(ONE,NSIGN)                                              
              
              !hydrostatic pressure                  
              FX=-AA*(NX/NORM)                                         
              FY=-AA*(NY/NORM)                                         
              FZ=-AA*(NZ/NORM)
              !drag force
              FX=FX+PVEL*HALF*NX*NSIGN
              FY=FY+PVEL*HALF*NY*NSIGN
              FZ=FZ+PVEL*HALF*NZ*NSIGN
              !expanding on the 4-node-face
              FX=FX*FOURTH
              FY=FY*FOURTH
              FZ=FZ*FOURTH                                  

              A(1,N1)=A(1,N1)+FX                                                                   
              A(2,N1)=A(2,N1)+FY                                                                   
              A(3,N1)=A(3,N1)+FZ                                                                   
              IF(IANIM  > 0) THEN                                                                 
                FEXT(1,N1) = FEXT(1,N1)+FX                                                         
                FEXT(2,N1) = FEXT(2,N1)+FY                                                         
                FEXT(3,N1) = FEXT(3,N1)+FZ                                                         
              ENDIF                                                                                

              A(1,N2)=A(1,N2)+FX                                                                   
              A(2,N2)=A(2,N2)+FY                                                                   
              A(3,N2)=A(3,N2)+FZ                                                                   
              IF(IANIM  > 0) THEN                                                                 
                FEXT(1,N2) = FEXT(1,N2)+FX                                                         
                FEXT(2,N2) = FEXT(2,N2)+FY                                                         
                FEXT(3,N2) = FEXT(3,N2)+FZ                                                         
              ENDIF                                                                                

              A(1,N3)=A(1,N3)+FX                                                                   
              A(2,N3)=A(2,N3)+FY                                                                   
              A(3,N3)=A(3,N3)+FZ                                                                   
              IF(IANIM  > 0) THEN                                                                 
                FEXT(1,N3) = FEXT(1,N3)+FX                                                         
                FEXT(2,N3) = FEXT(2,N3)+FY                                                         
                FEXT(3,N3) = FEXT(3,N3)+FZ                                                         
              ENDIF                                                                                

              A(1,N4)=A(1,N4)+FX                                                                   
              A(2,N4)=A(2,N4)+FY                                                                   
              A(3,N4)=A(3,N4)+FZ                                                                   
              IF(IANIM  > 0) THEN                                                                 
                FEXT(1,N4) = FEXT(1,N4)+FX                                                         
                FEXT(2,N4) = FEXT(2,N4)+FY                                                         
                FEXT(3,N4) = FEXT(3,N4)+FZ                                                         
              ENDIF                                                                                

              TFEXTT=TFEXTT+DT1*(FX*(V(1,N1)+V(1,N2)+V(1,N3)+V(1,N4))
     +                          +FY*(V(2,N1)+V(2,N2)+V(2,N3)+V(2,N4))
     +                          +FZ*(V(3,N1)+V(3,N2)+V(3,N3)+V(3,N4)))

            !---TRIANGLE--------------------------------------------                                                                                                     
            ELSE     
              IF(N1 == N2)THEN                                                                      
                N2 = N3                                                                             
                N3 = N4                                                                             
                N4 = 0                                                                              
              ELSEIF(N1 == N3)THEN                                                                  
                N3 = N4                                                                             
                N4 = 0                                                                              
              ELSEIF(N1 == N4)THEN                                                                  
                N4 = 0                                                                              
              ELSEIF(N2 == N3)THEN                                                                  
                N3 = N4                                                                             
                N4 = 0                                                                              
              ELSEIF(N2 == N4)THEN                                                                  
                N2 = N3                                                                             
                N3 = N4                                                                             
                N4 = 0                                                                              
              ELSEIF(N3 == N4)THEN                                                                  
                N4 = 0                                                                              
              ENDIF                                                                                 
                                                                       
              IF(FUN_HSP /= 0)THEN                                                                 
                 K1=3*DIR_HSP-2                                                                     
                 K2=3*DIR_HSP-1                                                                     
                 K3=3*DIR_HSP                                                                       
                 ! hydrostatic pressure                                                             
                 COORMEAN =          (XFRAME(K1,IFRA1)*(X(1,N1)+X(1,N2)+X(1,N3))/THREE)                  
                 COORMEAN = COORMEAN+(XFRAME(K2,IFRA1)*(X(2,N1)+X(2,N2)+X(2,N3))/THREE)
                 COORMEAN = COORMEAN+(XFRAME(K3,IFRA1)*(X(3,N1)+X(3,N2)+X(3,N3))/THREE)                      
                 AA = FCY*FINTER(FUN_HSP,(COORMEAN-XFRAME(9+DIR_HSP,IFRA1))*FCX,NPC,TF,DYDX) 
               ENDIF                                                                                
               NX   = (X(2,N3)-X(2,N1))*(X(3,N3)-X(3,N2)) - (X(3,N3)-X(3,N1))*(X(2,N3)-X(2,N2))     
               NY   = (X(3,N3)-X(3,N1))*(X(1,N3)-X(1,N2)) - (X(1,N3)-X(1,N1))*(X(3,N3)-X(3,N2))     
               NZ   = (X(1,N3)-X(1,N1))*(X(2,N3)-X(2,N2)) - (X(2,N3)-X(2,N1))*(X(1,N3)-X(1,N2))     
               NORM = SQRT(NX*NX+NY*NY+NZ*NZ)                                                       
               AREA = HALF * NORM                                                                   
               AA = AA * AREA                                                                       
                                                                              
               K1=3*DIR_VEL-2                                                                       
               K2=3*DIR_VEL-1                                                                       
               K3=3*DIR_VEL 
               VSEG=     (XFRAME(K1,IFRA2)* (V(1,N1) + V(1,N2) + V(1,N3)) /THREE)
               VSEG=VSEG+(XFRAME(K2,IFRA2)* (V(2,N1) + V(2,N2) + V(2,N3)) /THREE)
               VSEG=VSEG+(XFRAME(K3,IFRA2)* (V(3,N1) + V(3,N2) + V(3,N3)) /THREE)
                                                                                                    
               IF(FUN_VEL /= 0)THEN                                                                
                  VEL = FCY2*FINTER(FUN_VEL,TT*FCX2,NPC,TF,DYDX) - VSEG                             
               ELSE                                                                                 
                  VEL = -VSEG                                                                     
               ENDIF 
                                                                                              
               IF(FUN_CX /= 0)THEN                                                                    
                 PVEL = (  (-(NX/NORM)*VEL*XFRAME(K1,IFRA2)-(NY/NORM)*VEL*XFRAME(K2,IFRA2)-(NZ/NORM)*VEL*XFRAME(K3,IFRA2))**2  )                              
                 PVEL=PVEL*FCY1*FINTER(FUN_CX,TT*FCX1,NPC,TF,DYDX)/TWO  
               ENDIF 
               NSIGN = VEL*(NX*XFRAME(K1,IFRA2) + NY*XFRAME(K2,IFRA2) +  NZ*XFRAME(K3,IFRA2))   !  <Vseg.d,n>  where d is flow direction X,Y,Z    ! IFRA2=1 is general frame, IFRA2 in [2:NFRAME+1] are for user frames
               NSIGN = SIGN(ONE,NSIGN)                                           
                               
               !hydrostatic pressure                 
               FX=-AA*(NX/NORM)                                        
               FY=-AA*(NY/NORM)                                        
               FZ=-AA*(NZ/NORM)
               !drag force
               FX=FX+PVEL*HALF*NX*NSIGN
               FY=FY+PVEL*HALF*NY*NSIGN
               FZ=FZ+PVEL*HALF*NZ*NSIGN
               !expanding on the 4-node-face
               FX=FX*THIRD
               FY=FY*THIRD
               FZ=FZ*THIRD 

               A(1,N1)=A(1,N1)+FX                                                                   
               A(2,N1)=A(2,N1)+FY                                                                   
               A(3,N1)=A(3,N1)+FZ                                                                   
               IF(IANIM  > 0) THEN                                                                 
                 FEXT(1,N1) = FEXT(1,N1)+FX                                                         
                 FEXT(2,N1) = FEXT(2,N1)+FY                                                         
                 FEXT(3,N1) = FEXT(3,N1)+FZ                                                         
               ENDIF                                                                                

               A(1,N2)=A(1,N2)+FX                                                                   
               A(2,N2)=A(2,N2)+FY                                                                   
               A(3,N2)=A(3,N2)+FZ                                                                   
               IF(IANIM  > 0) THEN                                                                 
                 FEXT(1,N2) = FEXT(1,N2)+FX                                                         
                 FEXT(2,N2) = FEXT(2,N2)+FY                                                         
                 FEXT(3,N2) = FEXT(3,N2)+FZ                                                         
               ENDIF                                                                                

               A(1,N3)=A(1,N3)+FX                                                                   
               A(2,N3)=A(2,N3)+FY                                                                   
               A(3,N3)=A(3,N3)+FZ                                                                   
               IF(IANIM  > 0) THEN                                                                 
                 FEXT(1,N3) = FEXT(1,N3)+FX                                                         
                 FEXT(2,N3) = FEXT(2,N3)+FY                                                         
                 FEXT(3,N3) = FEXT(3,N3)+FZ                                                         
               ENDIF
               
               TFEXTT=TFEXTT+DT1*(FX*(V(1,N1)+V(1,N2)+V(1,N3))
     +                           +FY*(V(2,N1)+V(2,N2)+V(2,N3))
     +                           +FZ*(V(3,N1)+V(3,N2)+V(3,N3)))   
                                                       
            ENDIF                                                                                  
          ENDDO !next I                                                                                 
        ENDDO !nextNL

#include "atomic.inc"
              TFEXT = TFEXT + TFEXTT
#include "atomend.inc"




      ELSE      
      !-----------------------------------!
      !            PARITH/ON              !
      !-----------------------------------!
       IF(IVECTOR == 0) THEN

         DO NL=1,NLOADP_F
           FUN_HSP=ILOADP(7,NL)
           DIR_HSP=ILOADP(8,NL)
           IFRA1=ILOADP(9,NL)
           FCY = FAC(1,NL)
           FCX = FAC(2,NL)
           FUN_CX=ILOADP(10,NL)
           FCY1 = FAC(3,NL)
           FCX1 = FAC(4,NL)
           FUN_VEL=ILOADP(11,NL)
           FCY2 = FAC(5,NL)
           FCX2 = FAC(6,NL)
           DIR_VEL=MAX(ILOADP(12,NL),1) ! To avoid a check bound issue when the velocity options are not set in the input, the DIR_VEL variable is bounded to a minimal value of 1
           IFRA2=ILOADP(13,NL)
           ISENS=0
           XSENS = ONE
           ! -------------
           ! flush fsky array to 0.
           DO I = 1,ILOADP(1,NL)/4
             N1=LLOADP(ILOADP(4,NL)+4*(I-1))
             N2=LLOADP(ILOADP(4,NL)+4*(I-1)+1)
             N3=LLOADP(ILOADP(4,NL)+4*(I-1)+2)
             N4=LLOADP(ILOADP(4,NL)+4*(I-1)+3)
             !---QUADRAGLE--------------------------------------------
             IF(N4 /= 0 .AND. N1 /= N2 .AND. N1 /= N3 .AND. N1 /= N4 .AND.N2 /= N3 .AND. N2 /= N4 .AND. N3 /= N4 )THEN
              UP_BOUND=4
             ELSE
              UP_BOUND=3
             ENDIF
             DO IJK=1,UP_BOUND
                  IAD = IADC(ILOADP(4,NL)+4*(I-1)+(IJK-1))
                  FSKY(1:3,IAD) = ZERO
             ENDDO
           ENDDO
           ! -------------
           DO K=1,NSENSOR
             IF(ILOADP(6,NL) == LSENSOR(1,K))ISENS=K
           ENDDO
           IF(ISENS == 0)THEN
              TS=TT
           ELSEIF (LSENSOR(4,ISENS)==753096514 .AND. TT /= ZERO) THEN
             IERR = SET_U_SENS_VALUE(ISENS,1,ZERO)         
             IERR = GET_U_SENS_VALUE(ISENS,2,XSENS)  
           ELSE                        
              TS = TT-SENSOR(2,ISENS)  
              IF(TS < ZERO) CYCLE     
           ENDIF
           DO I = 1,ILOADP(1,NL)/4
             N1=LLOADP(ILOADP(4,NL)+4*(I-1))
             N2=LLOADP(ILOADP(4,NL)+4*(I-1)+1)
             N3=LLOADP(ILOADP(4,NL)+4*(I-1)+2)
             N4=LLOADP(ILOADP(4,NL)+4*(I-1)+3)

             AA = ZERO
             VEL=ZERO
             PVEL=ZERO
             
             !---QUADRAGLE--------------------------------------------
             IF(N4 /= 0 .AND. N1 /= N2 .AND. N1 /= N3 .AND. N1 /= N4 .AND.N2 /= N3 .AND. N2 /= N4 .AND. N3 /= N4 )THEN
               K1=3*DIR_HSP-2
               K2=3*DIR_HSP-1
               K3=3*DIR_HSP
               IF(FUN_HSP /= 0) THEN
                 ! hydrostatic pressure
                 COORMEAN =        (XFRAME(K1,IFRA1)*(X(1,N1)+X(1,N2)+X(1,N3)+X(1,N4))/FOUR)
                 COORMEAN=COORMEAN+(XFRAME(K2,IFRA1)*(X(2,N1)+X(2,N2)+X(2,N3)+X(2,N4))/FOUR)
                 COORMEAN=COORMEAN+(XFRAME(K3,IFRA1)*(X(3,N1)+X(3,N2)+X(3,N3)+X(3,N4))/FOUR)
                 AA = FCY*FINTER(FUN_HSP,(COORMEAN-XFRAME(9+DIR_HSP,IFRA1))*FCX,NPC,TF,DYDX)
               ENDIF
               NX   = (X(2,N3)-X(2,N1))*(X(3,N4)-X(3,N2))-(X(3,N3)-X(3,N1))*(X(2,N4)-X(2,N2))
               NY   = (X(3,N3)-X(3,N1))*(X(1,N4)-X(1,N2))-(X(1,N3)-X(1,N1))*(X(3,N4)-X(3,N2))
               NZ   = (X(1,N3)-X(1,N1))*(X(2,N4)-X(2,N2))-(X(2,N3)-X(2,N1))*(X(1,N4)-X(1,N2))
               NORM = SQRT(NX*NX+NY*NY+NZ*NZ)
               AREA = HALF * NORM
               AA = AA * AREA

               K1=3*DIR_VEL-2                                                                       
               K2=3*DIR_VEL-1                                                                       
               K3=3*DIR_VEL 
               VSEG=     (XFRAME(K1,IFRA2)*(V(1,N1) + V(1,N2) + V(1,N3) + V(1,N4)) /FOUR)
               VSEG=VSEG+(XFRAME(K2,IFRA2)*(V(2,N1) + V(2,N2) + V(2,N3) + V(2,N4)) /FOUR)
               VSEG=VSEG+(XFRAME(K3,IFRA2)*(V(3,N1) + V(3,N2) + V(3,N3) + V(3,N4)) /FOUR)
               IF(FUN_VEL /= 0)THEN
                  VEL =  FCY2*FINTER(FUN_VEL,TT*FCX2,NPC,TF,DYDX) - VSEG
               ELSE
                  VEL =  -VSEG
               ENDIF

               IF(FUN_CX /= 0)THEN   
                 PVEL = (  (-(NX/NORM)*VEL*XFRAME(K1,IFRA2) -(NY/NORM)*VEL*XFRAME(K2,IFRA2)-(NZ/NORM)*VEL*XFRAME(K3,IFRA2))**2  )
                 PVEL = PVEL * FCY1*FINTER(FUN_CX,TT*FCX1,NPC,TF,DYDX)/TWO
               ENDIF                                                                        
               NSIGN = VEL*(NX*XFRAME(K1,IFRA2) + NY*XFRAME(K2,IFRA2) +  NZ*XFRAME(K3,IFRA2))   !  <Vseg.d,n>  where d is flow direction X,Y,Z    ! IFRA2=1 is general frame, IFRA2 in [2:NFRAME+1] are for user frames
               NSIGN = SIGN(ONE,NSIGN)    
                                           
               !hydrostatic pressure                 
               FX=-AA*(NX/NORM)                                        
               FY=-AA*(NY/NORM)                                        
               FZ=-AA*(NZ/NORM)
               !drag force
               FX=FX+PVEL*HALF*NX*NSIGN
               FY=FY+PVEL*HALF*NY*NSIGN
               FZ=FZ+PVEL*HALF*NZ*NSIGN
               !expanding on the 4-node-face
               FX=FX*FOURTH
               FY=FY*FOURTH
               FZ=FZ*FOURTH                 

               IAD = IADC(ILOADP(4,NL)+4*(I-1))
               FSKY(1,IAD) = FX
               FSKY(2,IAD) = FY
               FSKY(3,IAD) = FZ
               IF(IANIM  > 0) THEN
                 FEXT(1,N1) = FEXT(1,N1)+FX
                 FEXT(2,N1) = FEXT(2,N1)+FY
                 FEXT(3,N1) = FEXT(3,N1)+FZ
               ENDIF

               IAD = IADC(ILOADP(4,NL)+4*(I-1)+1)
               FSKY(1,IAD) = FX
               FSKY(2,IAD) = FY
               FSKY(3,IAD) = FZ
               IF(IANIM  > 0) THEN
                 FEXT(1,N2) = FEXT(1,N2)+FX
                 FEXT(2,N2) = FEXT(2,N2)+FY
                 FEXT(3,N2) = FEXT(3,N2)+FZ
               ENDIF

               IAD = IADC(ILOADP(4,NL)+4*(I-1)+2)
               FSKY(1,IAD) = FX
               FSKY(2,IAD) = FY
               FSKY(3,IAD) = FZ
               IF(IANIM  > 0) THEN
                 FEXT(1,N3) = FEXT(1,N3)+FX
                 FEXT(2,N3) = FEXT(2,N3)+FY
                 FEXT(3,N3) = FEXT(3,N3)+FZ
               ENDIF

               IAD = IADC(ILOADP(4,NL)+4*(I-1)+3)
               FSKY(1,IAD) = FX
               FSKY(2,IAD) = FY
               FSKY(3,IAD) = FZ
               IF(IANIM  > 0) THEN
                 FEXT(1,N4) = FEXT(1,N4)+FX
                 FEXT(2,N4) = FEXT(2,N4)+FY
                 FEXT(3,N4) = FEXT(3,N4)+FZ
               ENDIF

               TFEXTT=TFEXTT+DT1*(FX*(V(1,N1)+V(1,N2)+V(1,N3)+V(1,N4))
     +                           +FY*(V(2,N1)+V(2,N2)+V(2,N3)+V(2,N4))
     +                           +FZ*(V(3,N1)+V(3,N2)+V(3,N3)+V(3,N4)))

             ELSE

              !---TRIANGLE--------------------------------------------                                                                                                     
              IF(N1 == N2)THEN
                N2 = N3
                N3 = N4
                N4 = 0
              ELSEIF(N1 == N3)THEN
                N3 = N4
                N4 = 0
              ELSEIF(N1 == N4)THEN
                N4 = 0
              ELSEIF(N2 == N3)THEN
                N3 = N4
                N4 = 0
              ELSEIF(N2 == N4)THEN
                N2 = N3
                N3 = N4
                N4 = 0
              ELSEIF(N3 == N4)THEN
                N4 = 0
              ENDIF
 
               IF(FUN_HSP /= 0)THEN
                 K1=3*DIR_HSP-2
                 K2=3*DIR_HSP-1
                 K3=3*DIR_HSP
                 ! hydrostatic pressure
                 COORMEAN =            (XFRAME(K1,IFRA1)*(X(1,N1)+X(1,N2)+X(1,N3))/THREE)
                 COORMEAN = COORMEAN + (XFRAME(K2,IFRA1)*(X(2,N1)+X(2,N2)+X(2,N3))/THREE)
                 COORMEAN = COORMEAN + (XFRAME(K3,IFRA1)*(X(3,N1)+X(3,N2)+X(3,N3))/THREE)
                 AA = FCY*FINTER(FUN_HSP,(COORMEAN-XFRAME(9+DIR_HSP,IFRA1))*FCX,NPC,TF,DYDX)
               ENDIF

               NX   = (X(2,N3)-X(2,N1))*(X(3,N3)-X(3,N2))-(X(3,N3)-X(3,N1))*(X(2,N3)-X(2,N2))
               NY   = (X(3,N3)-X(3,N1))*(X(1,N3)-X(1,N2))-(X(1,N3)-X(1,N1))*(X(3,N3)-X(3,N2))
               NZ   = (X(1,N3)-X(1,N1))*(X(2,N3)-X(2,N2))-(X(2,N3)-X(2,N1))*(X(1,N3)-X(1,N2))
               NORM = SQRT(NX*NX+NY*NY+NZ*NZ)
               AREA = HALF * NORM
               AA = AA * AREA

               K1=3*DIR_VEL-2                                                                       
               K2=3*DIR_VEL-1                                                                       
               K3=3*DIR_VEL  
               VSEG=     (XFRAME(K1,IFRA2)*(V(1,N1) + V(1,N2) + V(1,N3)) /THREE)
               VSEG=VSEG+(XFRAME(K2,IFRA2)*(V(2,N1) + V(2,N2) + V(2,N3)) /THREE)
               VSEG=VSEG+(XFRAME(K3,IFRA2)*(V(3,N1) + V(3,N2) + V(3,N3)) /THREE)
               IF(FUN_VEL /= 0)THEN
                  VEL =  FCY2*FINTER(FUN_VEL,TT*FCX2,NPC,TF,DYDX)- VSEG
               ELSE
                  VEL =  -VSEG
               ENDIF

               IF(FUN_CX /= 0) THEN  
                 PVEL = (  (-(NX/NORM)*VEL*XFRAME(K1,IFRA2)-(NY/NORM)*VEL*XFRAME(K2,IFRA2)-(NZ/NORM)*VEL*XFRAME(K3,IFRA2))**2  )
                 PVEL = PVEL * FCY1*FINTER(FUN_CX,TT*FCX1,NPC,TF,DYDX)/TWO
               ENDIF                                                                       
               NSIGN = VEL*(NX*XFRAME(K1,IFRA2) + NY*XFRAME(K2,IFRA2) +  NZ*XFRAME(K3,IFRA2))   !  <Vseg.d,n>  where d is flow direction X,Y,Z    ! IFRA2=1 is general frame, IFRA2 in [2:NFRAME+1] are for user frames
               NSIGN = SIGN(ONE,NSIGN)    
               
               !hydrostatic pressure                  
               FX=-AA*(NX/NORM)                                         
               FY=-AA*(NY/NORM)                                         
               FZ=-AA*(NZ/NORM)
               !drag force
               FX=FX+PVEL*HALF*NX*NSIGN
               FY=FY+PVEL*HALF*NY*NSIGN
               FZ=FZ+PVEL*HALF*NZ*NSIGN
               !expanding on the 4-node-face
               FX=FX*THIRD
               FY=FY*THIRD
               FZ=FZ*THIRD  

               IAD = IADC(ILOADP(4,NL)+4*(I-1))
               FSKY(1,IAD) = FX
               FSKY(2,IAD) = FY
               FSKY(3,IAD) = FZ
               IF(IANIM  > 0) THEN
                 FEXT(1,N1) = FEXT(1,N1)+FX
                 FEXT(2,N1) = FEXT(2,N1)+FY
                 FEXT(3,N1) = FEXT(3,N1)+FZ
               ENDIF

               IAD = IADC(ILOADP(4,NL)+4*(I-1)+1)
               FSKY(1,IAD) = FX
               FSKY(2,IAD) = FY
               FSKY(3,IAD) = FZ
               IF(IANIM  > 0) THEN
                 FEXT(1,N2) = FEXT(1,N2)+FX
                 FEXT(2,N2) = FEXT(2,N2)+FY
                 FEXT(3,N2) = FEXT(3,N2)+FZ
               ENDIF

               IAD = IADC(ILOADP(4,NL)+4*(I-1)+2)
               FSKY(1,IAD) = FX
               FSKY(2,IAD) = FY
               FSKY(3,IAD) = FZ
               IF(IANIM  > 0) THEN
                 FEXT(1,N3) = FEXT(1,N3)+FX
                 FEXT(2,N3) = FEXT(2,N3)+FY
                 FEXT(3,N3) = FEXT(3,N3)+FZ
               ENDIF

              TFEXTT=TFEXTT+DT1*(FX*(V(1,N1)+V(1,N2)+V(1,N3))
     +                          +FY*(V(2,N1)+V(2,N2)+V(2,N3))
     +                          +FZ*(V(3,N1)+V(3,N2)+V(3,N3)))
             ENDIF
            ENDDO!next I
          ENDDO!next NL

#include "atomic.inc"
                TFEXT = TFEXT + TFEXTT
#include "atomend.inc"

         ELSE
C---------------------
C vectorial code
C---------------------

         DO NL=1,NLOADP_F
           FUN_HSP=ILOADP(7,NL)
           DIR_HSP=ILOADP(8,NL)
           IFRA1=ILOADP(9,NL)
           FCY = FAC(1,NL)
           FCX = FAC(2,NL)
           FUN_CX=ILOADP(10,NL)
           FCY1 = FAC(3,NL)
           FCX1 = FAC(4,NL)
           FUN_VEL=ILOADP(11,NL)
           FCY2 = FAC(5,NL)
           FCX2 = FAC(6,NL)
           DIR_VEL=MAX(ILOADP(12,NL),1)! To avoid a check bound issue when the velocity options are not set in the input, the DIR_VEL variable is bounded to a minimal value of 1 
           IFRA2=ILOADP(13,NL)
           ISENS=0
           XSENS = ONE

           DO K=1,NSENSOR
             IF(ILOADP(6,NL) == LSENSOR(1,K))ISENS=K
           ENDDO
           IF(ISENS == 0)THEN
              TS=TT
           ELSEIF (LSENSOR(4,ISENS)==753096514 .AND. TT /= ZERO) THEN
             IERR = SET_U_SENS_VALUE(ISENS,1,ZERO)         
             IERR = GET_U_SENS_VALUE(ISENS,2,XSENS)  
           ELSE                        
              TS = TT-SENSOR(2,ISENS)  
              IF(TS < ZERO) CYCLE     
           ENDIF
           DO I = 1,ILOADP(1,NL)/4
             N1=LLOADP(ILOADP(4,NL)+4*(I-1))
             N2=LLOADP(ILOADP(4,NL)+4*(I-1)+1)
             N3=LLOADP(ILOADP(4,NL)+4*(I-1)+2)
             N4=LLOADP(ILOADP(4,NL)+4*(I-1)+3)

             AA = ZERO
             VEL = ZERO
             PVEL=ZERO
 
             IF(N4 /= 0 .AND. N1 /= N2 .AND. N1 /= N3 .AND. N1 /= N4 .AND. N2 /= N3 .AND. N2 /= N4 .AND. N3 /= N4 )THEN

               K1=3*DIR_HSP-2
               K2=3*DIR_HSP-1
               K3=3*DIR_HSP
               IF(FUN_HSP /= 0) THEN
                 ! hydrostatic pressure
                 COORMEAN =          (XFRAME(K1,IFRA1)*(X(1,N1)+X(1,N2)+X(1,N3)+X(1,N4))/FOUR)
                 COORMEAN = COORMEAN+(XFRAME(K2,IFRA1)*(X(2,N1)+X(2,N2)+X(2,N3)+X(2,N4))/FOUR)
                 COORMEAN = COORMEAN+(XFRAME(K3,IFRA1)*(X(3,N1)+X(3,N2)+X(3,N3)+X(3,N4))/FOUR)
                 AA = FCY*FINTER(FUN_HSP,(COORMEAN-XFRAME(9+DIR_HSP,IFRA1))*FCX,NPC,TF,DYDX)
               ENDIF
               NX   = (X(2,N3)-X(2,N1))*(X(3,N4)-X(3,N2)) - (X(3,N3)-X(3,N1))*(X(2,N4)-X(2,N2))
               NY   = (X(3,N3)-X(3,N1))*(X(1,N4)-X(1,N2)) - (X(1,N3)-X(1,N1))*(X(3,N4)-X(3,N2))
               NZ   = (X(1,N3)-X(1,N1))*(X(2,N4)-X(2,N2)) - (X(2,N3)-X(2,N1))*(X(1,N4)-X(1,N2))
               NORM = SQRT(NX*NX+NY*NY+NZ*NZ)
               AREA = HALF * NORM
               AA = AA * AREA

               K1=3*DIR_VEL-2                                                                       
               K2=3*DIR_VEL-1                                                                       
               K3=3*DIR_VEL 
               VSEG=     (XFRAME(K1,IFRA2)*(V(1,N1) + V(1,N2) + V(1,N3) + V(1,N4)) /FOUR)
               VSEG=VSEG+(XFRAME(K2,IFRA2)*(V(2,N1) + V(2,N2) + V(2,N3) + V(2,N4)) /FOUR)
               VSEG=VSEG+(XFRAME(K3,IFRA2)*(V(3,N1) + V(3,N2) + V(3,N3) + V(3,N4)) /FOUR)
                  
               IF(FUN_VEL /= 0)THEN
                  VEL =  FCY2*FINTER(FUN_VEL,TT*FCX2,NPC,TF,DYDX) - VSEG
               ELSE
                  VEL =  -VSEG
               ENDIF

               IF(FUN_CX /= 0) THEN  
                 PVEL = (-NX/NORM*VEL*XFRAME(K1,IFRA2)- (NY/NORM)*VEL*XFRAME(K2,IFRA2)-(NZ/NORM)*VEL*XFRAME(K3,IFRA2))**2
                 PVEL = PVEL * FCY1*FINTER(FUN_CX,TT*FCX1,NPC,TF,DYDX)/TWO
               ENDIF                                                                        
               NSIGN = VEL*(NX*XFRAME(K1,IFRA2) + NY*XFRAME(K2,IFRA2) +  NZ*XFRAME(K3,IFRA2))   !  <Vseg.d,n>  where d is flow direction X,Y,Z    ! IFRA2=1 is general frame, IFRA2 in [2:NFRAME+1] are for user frames
               NSIGN = SIGN(ONE,NSIGN)    
               
               !hydrostatic pressure                 
               FX=-AA*(NX/NORM)                                        
               FY=-AA*(NY/NORM)                                        
               FZ=-AA*(NZ/NORM)
               !drag force
               FX=FX+PVEL*HALF*NX*NSIGN
               FY=FY+PVEL*HALF*NY*NSIGN
               FZ=FZ+PVEL*HALF*NZ*NSIGN
               !expanding on the 4-node-face
               FX=FX*FOURTH
               FY=FY*FOURTH
               FZ=FZ*FOURTH  

               IAD = IADC(ILOADP(4,NL)+4*(I-1))
               FSKYV(IAD,1) = FX
               FSKYV(IAD,2) = FY
               FSKYV(IAD,3) = FZ
               IF(IANIM  > 0) THEN
                 FEXT(1,N1) = FEXT(1,N1)+FX
                 FEXT(2,N1) = FEXT(2,N1)+FY
                 FEXT(3,N1) = FEXT(3,N1)+FZ
               ENDIF

               IAD = IADC(ILOADP(4,NL)+4*(I-1)+1)
               FSKYV(IAD,1) = FX
               FSKYV(IAD,2) = FY
               FSKYV(IAD,3) = FZ
               IF(IANIM  > 0) THEN
                 FEXT(1,N2) = FEXT(1,N2)+FX
                 FEXT(2,N2) = FEXT(2,N2)+FY
                 FEXT(3,N2) = FEXT(3,N2)+FZ
               ENDIF

               IAD = IADC(ILOADP(4,NL)+4*(I-1)+2)
               FSKYV(IAD,1) = FX
               FSKYV(IAD,2) = FY
               FSKYV(IAD,3) = FZ
               IF(IANIM  > 0) THEN
                 FEXT(1,N3) = FEXT(1,N3)+FX
                 FEXT(2,N3) = FEXT(2,N3)+FY
                 FEXT(3,N3) = FEXT(3,N3)+FZ
               ENDIF

               IAD = IADC(ILOADP(4,NL)+4*(I-1)+3)
               FSKYV(IAD,1) = FX
               FSKYV(IAD,2) = FY
               FSKYV(IAD,3) = FZ
               IF(IANIM  > 0) THEN
                 FEXT(1,N4) = FEXT(1,N4)+FX
                 FEXT(2,N4) = FEXT(2,N4)+FY
                 FEXT(3,N4) = FEXT(3,N4)+FZ
               ENDIF
               
              TFEXTT=TFEXTT+DT1*(FX*(V(1,N1)+V(1,N2)+V(1,N3)+V(1,N4))
     +                          +FY*(V(2,N1)+V(2,N2)+V(2,N3)+V(2,N4))
     +                          +FZ*(V(3,N1)+V(3,N2)+V(3,N3)+V(3,N4)))
               
             ELSE

              !---TRIANGLE--------------------------------------------                                                                                                                  
              IF(N1 == N2)THEN
                N2 = N3
                N3 = N4
                N4 = 0
              ELSEIF(N1 == N3)THEN
                N3 = N4
                N4 = 0
              ELSEIF(N1 == N4)THEN
                N4 = 0
              ELSEIF(N2 == N3)THEN
                N3 = N4
                N4 = 0
              ELSEIF(N2 == N4)THEN
                N2 = N3
                N3 = N4
                N4 = 0
              ELSEIF(N3 == N4)THEN
                N4 = 0
              ENDIF
 
               K1=3*DIR_HSP-2
               K2=3*DIR_HSP-1
               K3=3*DIR_HSP
               IF(FUN_HSP /= 0)THEN
                 COORMEAN =          (XFRAME(K1,IFRA1)*(X(1,N1)+X(1,N2)+X(1,N3))/THREE)
                 COORMEAN = COORMEAN+(XFRAME(K2,IFRA1)*(X(2,N1)+X(2,N2)+X(2,N3))/THREE)
                 COORMEAN = COORMEAN+(XFRAME(K3,IFRA1)*(X(3,N1)+X(3,N2)+X(3,N3))/THREE)
                 AA = FCY*FINTER(FUN_HSP,(COORMEAN-XFRAME(9+DIR_HSP,IFRA1))*FCX,NPC,TF,DYDX)
               ENDIF
               NX   = (X(2,N3)-X(2,N1))*(X(3,N3)-X(3,N2)) - (X(3,N3)-X(3,N1))*(X(2,N3)-X(2,N2))
               NY   = (X(3,N3)-X(3,N1))*(X(1,N3)-X(1,N2)) - (X(1,N3)-X(1,N1))*(X(3,N3)-X(3,N2))
               NZ   = (X(1,N3)-X(1,N1))*(X(2,N3)-X(2,N2)) - (X(2,N3)-X(2,N1))*(X(1,N3)-X(1,N2))
               NORM = SQRT(NX*NX+NY*NY+NZ*NZ)
               AREA = HALF * NORM
               AA = AA * AREA

               K1=3*DIR_VEL-2                                                                       
               K2=3*DIR_VEL-1                                                                       
               K3=3*DIR_VEL 
               VSEG=     (XFRAME(K1,IFRA2)* (V(1,N1) + V(1,N2) + V(1,N3)) /THREE)
               VSEG=VSEG+(XFRAME(K2,IFRA2)* (V(2,N1) + V(2,N2) + V(2,N3)) /THREE)
               VSEG=VSEG+(XFRAME(K3,IFRA2)* (V(3,N1) + V(3,N2) + V(3,N3)) /THREE)
               
               IF(FUN_VEL /= 0)THEN
                  VEL = FCY2*FINTER(FUN_VEL,TT*FCX2,NPC,TF,DYDX) - VSEG
               ELSE
                  VEL = -VSEG
               ENDIF
 
               IF(FUN_CX /= 0)THEN   
                 PVEL = (  (-(NX/NORM)*VEL*XFRAME(K1,IFRA2)-(NY/NORM)*VEL*XFRAME(K2,IFRA2)-(NZ/NORM)*VEL*XFRAME(K3,IFRA2))**2  )
                 PVEL = PVEL * FCY1* FINTER(FUN_CX,TT*FCX1,NPC,TF,DYDX)/TWO
               ENDIF                                                                       
               NSIGN = VEL*(NX*XFRAME(K1,IFRA2) + NY*XFRAME(K2,IFRA2) +  NZ*XFRAME(K3,IFRA2))   !  <Vseg.d,n>  where d is flow direction X,Y,Z    ! IFRA2=1 is general frame, IFRA2 in [2:NFRAME+1] are for user frames
               NSIGN = SIGN(ONE,NSIGN)    
               
               !hydrostatic pressure                 
               FX=-AA*(NX/NORM)                                        
               FY=-AA*(NY/NORM)                                        
               FZ=-AA*(NZ/NORM)
               !drag force
               FX=FX+PVEL*HALF*NX*NSIGN
               FY=FY+PVEL*HALF*NY*NSIGN
               FZ=FZ+PVEL*HALF*NZ*NSIGN
               !expanding on the 4-node-face
               FX=FX*THIRD
               FY=FY*THIRD
               FZ=FZ*THIRD  

               IAD = IADC(ILOADP(4,NL)+4*(I-1))
               FSKYV(IAD,1) = FX
               FSKYV(IAD,2) = FY
               FSKYV(IAD,3) = FZ
               IF(IANIM  > 0) THEN
                 FEXT(1,N1) = FEXT(1,N1)+FX
                 FEXT(2,N1) = FEXT(2,N1)+FY
                 FEXT(3,N1) = FEXT(3,N1)+FZ
               ENDIF

               IAD = IADC(ILOADP(4,NL)+4*(I-1)+1)
               FSKYV(IAD,1) = FX
               FSKYV(IAD,2) = FY
               FSKYV(IAD,3) = FZ
               IF(IANIM  > 0) THEN
                 FEXT(1,N2) = FEXT(1,N2)+FX
                 FEXT(2,N2) = FEXT(2,N2)+FY
                 FEXT(3,N2) = FEXT(3,N2)+FZ
               ENDIF

               IAD = IADC(ILOADP(4,NL)+4*(I-1)+2)
               FSKYV(IAD,1) = FX
               FSKYV(IAD,2) = FY
               FSKYV(IAD,3) = FZ
               IF(IANIM  > 0) THEN
                 FEXT(1,N3) = FEXT(1,N3)+FX
                 FEXT(2,N3) = FEXT(2,N3)+FY
                 FEXT(3,N3) = FEXT(3,N3)+FZ
               ENDIF
               TFEXTT=TFEXTT+DT1*(FX*(V(1,N1)+V(1,N2)+V(1,N3))
     +                           +FY*(V(2,N1)+V(2,N2)+V(2,N3))
     +                           +FZ*(V(3,N1)+V(3,N2)+V(3,N3)))
             ENDIF
           ENDDO!next I
         ENDDO!next NL
C
#include "atomic.inc"
                TFEXT = TFEXT + TFEXTT
#include "atomend.inc"
         ENDIF ! IF(IVECTOR == 0)


       ENDIF
C
      RETURN
      END
