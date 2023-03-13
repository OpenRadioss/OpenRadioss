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
Chd|  ALEFLOW                       source/ale/porous/aleflow.F   
Chd|-- called by -----------
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|-- calls ---------------
Chd|        ALECONVE                      source/ale/porous/aleconv.F   
Chd|        ALEFLUX                       source/ale/porous/aleflux.F   
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE ALEFLOW(
     .         PM    ,IPM      ,MAT         ,X      ,IPARG   ,
     .         MBUF  ,NEL      ,NPF         ,TF     ,RHON    ,
     .         D     ,IXS      ,ALE_CONNECT ,V      ,A       ,
     .         W     ,DFE      ,FRHO0       ,ALPHA  ,
     .         NC1   ,NC2      ,NC3         ,NC4    ,NC5     ,NC6  ,
     .         NC7   ,NC8      ,X1          ,X2     ,X3      ,X4   ,
     .         X5    ,X6       ,X7          ,X8     ,Y1      ,Y2   ,
     .         Y3    ,Y4       ,Y5          ,Y6     ,Y7      ,Y8   ,
     .         Z1    ,Z2       ,Z3          ,Z4     ,Z5      ,Z6   ,
     .         Z7    ,Z8       ,PHI1        ,PHI2   ,FLUX    ,FLU1 ,
     .         VX1   ,VX2      ,VX3         ,VX4    ,VX5     ,VX6  ,
     .         VX7   ,VX8      ,VY1         ,VY2    ,VY3     ,VY4  ,
     .         VY5   ,VY6      ,VY7         ,VY8    ,VZ1     ,VZ2  ,
     .         VZ3   ,VZ4      ,VZ5         ,VZ6    ,VZ7     ,VZ8  ,
     .         VDX1  ,VDX2     ,VDX3        ,VDX4   ,VDX5    ,VDX6 ,
     .         VDX7  ,VDX8     ,VDY1        ,VDY2   ,VDY3    ,VDY4 ,
     .         VDY5  ,VDY6     ,VDY7        ,VDY8   ,VDZ1    ,VDZ2 ,
     .         VDZ3  ,VDZ4     ,VDZ5        ,VDZ6   ,VDZ7    ,VDZ8 ,
     .         VDX   ,VDY      ,VDZ         ,VD2    ,NGL     ,
     .         LVX1  ,LVX2     ,LVX3        ,LVX4   ,LVX5    ,LVX6 ,
     .         LVX7  ,LVX8     ,LVY1        ,LVY2   ,LVY3    ,LVY4 ,
     .         LVY5  ,LVY6     ,LVY7        ,LVY8   ,LVZ1    ,LVZ2 ,
     .         LVZ3  ,LVZ4     ,LVZ5        ,LVZ6   ,LVZ7    ,LVZ8 ,
     .         POR   ,ICONTACT ,IFOAM)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine is related to porous material law 77
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "vect01_c.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      INTEGER 
     .   IPARG(NPARG,NGROUP),IPM(NPROPMI,NUMMAT),
     .   NC1(*),NC2(*),NC3(*),NC4(*),NC5(*),NC6(*),
     ,   NC7(*),NC8(*),IXS(NIXS,*),MAT(*),NGL(*),
     .   ICONTACT(*),IFOAM(*)
      my_real
     .  V(3,NUMNOD),W(3,NUMNOD)   ,FLUX(MVSIZ,6),
     .  PHI1(*),PHI2(*),D(3,*),A(3,NUMNOD),
     .  FLU1(*)  ,X1(*)  ,X2(*)   ,X3(*)    ,X4(*)  ,
     .  X5(*)    ,X6(*)  ,X7(*)   ,X8(*)    ,Y1(*)  ,Y2(*)  ,
     .  Y3(*)    ,Y4(*)  ,Y5(*)   ,Y6(*)    ,Y7(*)  ,Y8(*)  ,
     .  Z1(*)    ,Z2(*)  ,Z3(*)   ,Z4(*)    ,Z5(*)  ,Z6(*)  ,
     .  Z7(*)    ,Z8(*) ,  
     .  VX1(*) ,VX2(*) ,VX3(*) ,VX4(*) ,VX5(*) ,VX6(*)  ,
     .  VX7(*) ,VX8(*) ,VY1(*) ,VY2(*) ,VY3(*) ,VY4(*) ,
     .  VY5(*) ,VY6(*) ,VY7(*) ,VY8(*) ,VZ1(*) ,VZ2(*) ,
     .  VZ3(*) ,VZ4(*) ,VZ5(*) ,VZ6(*) ,VZ7(*) ,VZ8(*) ,
     .  VDX1(*) ,VDX2(*) ,VDX3(*) ,VDX4(*) ,VDX5(*) ,VDX6(*) ,
     .  VDX7(*) ,VDX8(*) ,VDY1(*) ,VDY2(*) ,VDY3(*) ,VDY4(*) ,
     .  VDY5(*) ,VDY6(*) ,VDY7(*) ,VDY8(*) ,VDZ1(*) ,VDZ2(*) ,
     .  VDZ3(*) ,VDZ4(*) ,VDZ5(*) ,VDZ6(*) ,VDZ7(*) ,VDZ8(*),
     .  DFE(MVSIZ,3), FRHO0(*),PM(NPROPM,NUMMAT), ALPHA(*),VDX(*),
     .  VDY(*), VDZ(*),VD2(*),X(3,*),RHON(*),  
     .  LVX1(*) ,LVX2(*) ,LVX3(*) ,LVX4(*) ,LVX5(*) ,LVX6(*)  ,
     .  LVX7(*) ,LVX8(*) ,LVY1(*) ,LVY2(*) ,LVY3(*) ,LVY4(*) ,
     .  LVY5(*) ,LVY6(*) ,LVY7(*) ,LVY8(*) ,LVZ1(*) ,LVZ2(*) ,
     .  LVZ3(*) ,LVZ4(*) ,LVZ5(*) ,LVZ6(*) ,LVZ7(*) ,LVZ8(*),
     .  POR(*) 
C-----------------------------------------------,   
      TYPE(BUF_MAT_) :: MBUF  
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT   
C-----------------------------------------------
C   VARIABLES FOR INTERPOLATION FUNCTIONS
C-----------------------------------------------
      INTEGER NPF(*)
      my_real TF(*),FINTER
      EXTERNAL FINTER
C   EXTERNAL FINTER
C        Y = FINTER(IFUNC(J),X,NPF,TF,DYDX)
C        Y       : y = f(x)
C        X       : x
C        DYDX    : f'(x) = dy/dx
C        IFUNC(J): FUNCTION INDEX
C              J : FIRST(J=1), SECOND(J=2) .. FUNCTION USED FOR THIS LAW
C        NPF,TF  : FUNCTION PARAMETER
C ---------------------------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,IV,IE,MX,IAD2
      my_real FAC, VFE(3),AFE(3),
     .        DTINV,RV,
     .        REXCH(MVSIZ,6), ALPHA0,
     .        RHOEXT(MVSIZ),EIEXT(MVSIZ),
     .        KK(MVSIZ)
      my_real AA,BB,TAUX
      INTEGER NVAR,I
      INTEGER ICLOS,II,ICONGAS
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------   
       DO I = LFT, LLT
        RHOEXT(I)  = ZERO
        EIEXT(I) = ZERO
          
        VX1(I)=V(1,NC1(I))
        VY1(I)=V(2,NC1(I))
        VZ1(I)=V(3,NC1(I))

        VX2(I)=V(1,NC2(I)) 
        VY2(I)=V(2,NC2(I))
        VZ2(I)=V(3,NC2(I))  
                   
        VX3(I)=V(1,NC3(I))
        VY3(I)=V(2,NC3(I))
        VZ3(I)=V(3,NC3(I))
        
        VX4(I)=V(1,NC4(I))
        VY4(I)=V(2,NC4(I))
        VZ4(I)=V(3,NC4(I))
             
        VX5(I)=V(1,NC5(I))
        VY5(I)=V(2,NC5(I))
        VZ5(I)=V(3,NC5(I)) 
               
        VX6(I)=V(1,NC6(I))
        VY6(I)=V(2,NC6(I))
        VZ6(I)=V(3,NC6(I)) 

        VX7(I)=V(1,NC7(I))
        VY7(I)=V(2,NC7(I))
        VZ7(I)=V(3,NC7(I))
              
        VX8(I)=V(1,NC8(I))
        VY8(I)=V(2,NC8(I))
        VZ8(I)=V(3,NC8(I))

        VDX1(I)=V(1,NC1(I)) - W(1,NC1(I))
        VDY1(I)=V(2,NC1(I)) - W(2,NC1(I))
        VDZ1(I)=V(3,NC1(I)) - W(3,NC1(I))

        VDX2(I)=V(1,NC2(I)) - W(1,NC2(I))
        VDY2(I)=V(2,NC2(I)) - W(2,NC2(I))
        VDZ2(I)=V(3,NC2(I)) - W(3,NC2(I))

        VDX3(I)=V(1,NC3(I)) - W(1,NC3(I))
        VDY3(I)=V(2,NC3(I)) - W(2,NC3(I))
        VDZ3(I)=V(3,NC3(I)) - W(3,NC3(I))

        VDX4(I)=V(1,NC4(I)) - W(1,NC4(I))
        VDY4(I)=V(2,NC4(I)) - W(2,NC4(I))
        VDZ4(I)=V(3,NC4(I)) - W(3,NC4(I))

        VDX5(I)=V(1,NC5(I)) - W(1,NC5(I))
        VDY5(I)=V(2,NC5(I)) - W(2,NC5(I))
        VDZ5(I)=V(3,NC5(I)) - W(3,NC5(I))

        VDX6(I)=V(1,NC6(I)) - W(1,NC6(I))
        VDY6(I)=V(2,NC6(I)) - W(2,NC6(I))
        VDZ6(I)=V(3,NC6(I)) - W(3,NC6(I))

        VDX7(I)=V(1,NC7(I)) - W(1,NC7(I))
        VDY7(I)=V(2,NC7(I)) - W(2,NC7(I))
        VDZ7(I)=V(3,NC7(I)) - W(3,NC7(I))

        VDX8(I)=V(1,NC8(I)) - W(1,NC8(I))
        VDY8(I)=V(2,NC8(I)) - W(2,NC8(I))
        VDZ8(I)=V(3,NC8(I)) - W(3,NC8(I))
  
C coordinates
        X1(I)=X(1,NC1(I))  
        Y1(I)=X(2,NC1(I))  
        Z1(I)=X(3,NC1(I))  
        X2(I)=X(1,NC2(I))  
        Y2(I)=X(2,NC2(I))  
        Z2(I)=X(3,NC2(I))  
        X3(I)=X(1,NC3(I))  
        Y3(I)=X(2,NC3(I))  
        Z3(I)=X(3,NC3(I))  
        X4(I)=X(1,NC4(I))  
        Y4(I)=X(2,NC4(I))  
        Z4(I)=X(3,NC4(I))  
        X5(I)=X(1,NC5(I))  
        Y5(I)=X(2,NC5(I))  
        Z5(I)=X(3,NC5(I))  
        X6(I)=X(1,NC6(I))  
        Y6(I)=X(2,NC6(I))  
        Z6(I)=X(3,NC6(I))  
        X7(I)=X(1,NC7(I))  
        Y7(I)=X(2,NC7(I))  
        Z7(I)=X(3,NC7(I))  
        X8(I)=X(1,NC8(I))  
        Y8(I)=X(2,NC8(I))  
        Z8(I)=X(3,NC8(I))  
       ENDDO

C different Alpha values
       DO  I=LFT,LLT 
           IE =NFT+I
           IAD2 = ALE_CONNECT%ee_connect%iad_connect(IE)
           ALPHA(I) = POR(IE)
          DO J=1,6
             IV=ALE_CONNECT%ee_connect%connected(IAD2 + J - 1)
             REXCH(I,J)= ALPHA(I)
            IF(IV /= 0) THEN
               RV = POR(IV)
               REXCH(I,J) = MIN(ALPHA(I),RV)
             ENDIF
          ENDDO
        ENDDO
C External gas
       MX = MAT(LFT)
       ICONGAS = NINT(PM(201, MX))
       IF(ICONGAS > 0) THEN
         DO I= LFT,LLT 
           RHOEXT(I) = PM(199, MX)
           EIEXT(I)  = PM(200, MX)
         ENDDO
       ENDIF 
C
C with interface
C 
       ICLOS = NINT(PM(198, MAT(1)))
       IF(ICLOS == 2 .AND. INTBAG > 0  ) THEN
          DO  I=LFT,LLT 
             IAD2 = ALE_CONNECT%ee_connect%iad_connect(I+NFT)
            II=ALE_CONNECT%ee_connect%connected(IAD2 + 1 - 1)
            IF(II == 0) THEN
              ALPHA0 = REXCH(I,1)
              IF(ICONTACT(NC1(I)) > 0)REXCH(I,1)=REXCH(I,1)-FOURTH*ALPHA0
              IF(ICONTACT(NC2(I)) > 0)REXCH(I,1)=REXCH(I,1)-FOURTH*ALPHA0
              IF(ICONTACT(NC3(I)) > 0)REXCH(I,1)=REXCH(I,1)-FOURTH*ALPHA0
              IF(ICONTACT(NC4(I)) > 0)REXCH(I,1)=REXCH(I,1)-FOURTH*ALPHA0
            ENDIF
            II=ALE_CONNECT%ee_connect%connected(IAD2 + 2 - 1)
            IF(II == 0) THEN
              ALPHA0 = REXCH(I,2)
              IF(ICONTACT(NC3(I)) > 0)REXCH(I,2)=REXCH(I,2)-FOURTH*ALPHA0
              IF(ICONTACT(NC4(I)) > 0)REXCH(I,2)=REXCH(I,2)-FOURTH*ALPHA0
              IF(ICONTACT(NC7(I)) > 0)REXCH(I,2)=REXCH(I,2)-FOURTH*ALPHA0
              IF(ICONTACT(NC8(I)) > 0)REXCH(I,2)=REXCH(I,2)-FOURTH*ALPHA0
            ENDIF
            II=ALE_CONNECT%ee_connect%connected(IAD2 + 3 - 1)
            IF(II == 0) THEN
              ALPHA0 = REXCH(I,3)
              IF(ICONTACT(NC5(I)) > 0)REXCH(I,3)=REXCH(I,3)-FOURTH*ALPHA0
              IF(ICONTACT(NC6(I)) > 0)REXCH(I,3)=REXCH(I,3)-FOURTH*ALPHA0
              IF(ICONTACT(NC7(I)) > 0)REXCH(I,3)=REXCH(I,3)-FOURTH*ALPHA0
              IF(ICONTACT(NC8(I)) > 0)REXCH(I,3)=REXCH(I,3)-FOURTH*ALPHA0
            ENDIF  
           
            II=ALE_CONNECT%ee_connect%connected(IAD2 + 4 - 1)
            IF(II == 0) THEN
              ALPHA0 = REXCH(I,4)
              IF(ICONTACT(NC1(I)) > 0)REXCH(I,4)=REXCH(I,4)-FOURTH*ALPHA0
              IF(ICONTACT(NC2(I)) > 0)REXCH(I,4)=REXCH(I,4)-FOURTH*ALPHA0
              IF(ICONTACT(NC5(I)) > 0)REXCH(I,4)=REXCH(I,4)-FOURTH*ALPHA0
              IF(ICONTACT(NC6(I)) > 0)REXCH(I,4)=REXCH(I,4)-FOURTH*ALPHA0
            ENDIF 
            II=ALE_CONNECT%ee_connect%connected(IAD2 + 5 - 1)
            IF(II == 0) THEN
              ALPHA0 = REXCH(I,5)
              IF(ICONTACT(NC2(I)) > 0)REXCH(I,5)=REXCH(I,5)-FOURTH*ALPHA0
              IF(ICONTACT(NC3(I)) > 0)REXCH(I,5)=REXCH(I,5)-FOURTH*ALPHA0
              IF(ICONTACT(NC6(I)) > 0)REXCH(I,5)=REXCH(I,5)-FOURTH*ALPHA0
              IF(ICONTACT(NC7(I)) > 0)REXCH(I,5)=REXCH(I,5)-FOURTH*ALPHA0
            ENDIF 
            II=ALE_CONNECT%ee_connect%connected(IAD2 + 6 - 1)
            IF(II == 0) THEN
              ALPHA0 = REXCH(I,6)
              IF(ICONTACT(NC1(I)) > 0)REXCH(I,6)=REXCH(I,6)-FOURTH*ALPHA0
              IF(ICONTACT(NC4(I)) > 0)REXCH(I,6)=REXCH(I,6)-FOURTH*ALPHA0
              IF(ICONTACT(NC5(I)) > 0)REXCH(I,6)=REXCH(I,6)-FOURTH*ALPHA0
              IF(ICONTACT(NC8(I)) > 0)REXCH(I,6)=REXCH(I,6)-FOURTH*ALPHA0
            ENDIF
          ENDDO
       ENDIF
C        
C 
C Closed foam
C

      IF(ICLOS == 1 ) THEN      
#include "lockon.inc"
       DO  I=LFT,LLT 
          IAD2 = ALE_CONNECT%ee_connect%iad_connect(I + NFT)
           II=ALE_CONNECT%ee_connect%connected(IAD2 + 1 - 1)
           IF(II == 0)THEN
             
             VDX1(I) = ZERO   
             VDX2(I) = ZERO 
             VDX3(I) = ZERO   
             VDX4(I) = ZERO
            
             VDY1(I) = ZERO   
             VDY2(I) = ZERO 
             VDY3(I) = ZERO   
             VDY4(I) = ZERO
             
             VDZ1(I) = ZERO   
             VDZ2(I) = ZERO 
             VDZ3(I) = ZERO   
             VDZ4(I) = ZERO 
               
             VX1(I) = LVX1(I)   
             VX2(I) = LVX2(I)
             VX3(I) = LVX3(I)
             VX4(I) = LVX4(I)
                         
             VY1(I) = LVY1(I)
             VY2(I) = LVY2(I)
             VY3(I) = LVY3(I) 
             VY4(I) = LVY4(I) 
            
             VZ1(I) =  LVZ1(I)  
             VZ2(I) =  LVZ2(I) 
             VZ3(I) =  LVZ3(I)  
             VZ4(I) =  LVZ4(I)           
             
             IFOAM(NC1(I)) = 1
             IFOAM(NC2(I)) = 1
             IFOAM(NC3(I)) = 1 
             IFOAM(NC4(I)) = 1             
                          
           ENDIF
           II=ALE_CONNECT%ee_connect%connected(IAD2 + 2 - 1)
           IF(II == 0) THEN
             
             VDX3(I) = ZERO
             VDX4(I) = ZERO  
             VDX7(I) = ZERO  
             VDX8(I) = ZERO  
             
             VDY3(I) =  ZERO
             VDY4(I) =  ZERO 
             VDY7(I) =  ZERO  
             VDY8(I) =  ZERO 
             
             VDZ3(I) = ZERO   
             VDZ4(I) = ZERO 
             VDZ7(I) = ZERO   
             VDZ8(I) = ZERO        
             
             VX3(I) =  LVX3(I)  
             VX4(I) =  LVX4(I)
             VX7(I) =  LVX7(I)  
             VX8(I) =  LVX8(I)
            
             VY3(I) =  LVY3(I)  
             VY4(I) =  LVY4(I) 
             VY7(I) =  LVY7(I)  
             VY8(I) =  LVY8(I) 
             
             VZ3(I) =  LVZ3(I)  
             VZ4(I) =  LVZ4(I) 
             VZ7(I) =  LVZ7(I)  
             VZ8(I) =  LVZ8(I)  
            
             IFOAM(NC3(I)) =  1
             IFOAM(NC4(I)) =  1
             IFOAM(NC7(I)) =  1  
             IFOAM(NC8(I)) =  1
                 
           ENDIF           
           II=ALE_CONNECT%ee_connect%connected(IAD2 + 3 - 1)
           IF(II == 0)THEN
             
             VDX5(I) = ZERO   
             VDX6(I) = ZERO 
             VDX7(I) = ZERO   
             VDX8(I) = ZERO
             
             VDY5(I) = ZERO   
             VDY6(I) = ZERO 
             VDY7(I) = ZERO   
             VDY8(I) = ZERO
             
             VDZ5(I) = ZERO   
             VDZ6(I) = ZERO 
             VDZ7(I) = ZERO   
             VDZ8(I) = ZERO       
            
             VX5(I) = LVX5(I)
             VX6(I) = LVX6(I)
             VX7(I) = LVX7(I)  
             VX8(I) = LVX8(I)
            
             VY5(I) = LVY5(I)   
             VY6(I) = LVY6(I)
             VY7(I) = LVY7(I)  
             VY8(I) = LVY8(I)
            
             VZ5(I) =  LVZ5(I)  
             VZ6(I) =  LVZ6(I) 
             VZ7(I) =  LVZ7(I)  
             VZ8(I) =  LVZ8(I)
           
             IFOAM(NC5(I)) =1
             IFOAM(NC6(I)) =1
             IFOAM(NC7(I)) =1 
             IFOAM(NC8(I)) =1
     
           ENDIF
           II=ALE_CONNECT%ee_connect%connected(IAD2 + 4 - 1)
           IF(II == 0)THEN
            
             VDX1(I) = ZERO   
             VDX2(I) = ZERO 
             VDX5(I) = ZERO   
             VDX6(I) = ZERO
            
             VDY1(I) = ZERO   
             VDY2(I) = ZERO 
             VDY5(I) = ZERO   
             VDY6(I) = ZERO
            
             VDZ1(I) = ZERO   
             VDZ2(I) = ZERO 
             VDZ5(I) = ZERO   
             VDZ6(I) = ZERO          
            
             VX1(I) =  LVX1(I)   
             VX2(I) =  LVX2(I)
             VX5(I) =  LVX5(I) 
             VX6(I) =  LVX6(I)
          
             VY1(I) = LVY1(I)
             VY2(I) = LVY2(I)
             VY5(I) = LVY5(I)
             VY6(I) = LVY6(I)
            
             VZ1(I) =  LVZ1(I)
             VZ2(I) =  LVZ2(I)
             VZ5(I) =  LVZ5(I)
             VZ6(I) =  LVZ6(I) 
           
             IFOAM(NC1(I)) =  1  
             IFOAM(NC2(I)) =  1
             IFOAM(NC5(I)) =  1 
             IFOAM(NC6(I)) =  1
                
           ENDIF
           II=ALE_CONNECT%ee_connect%connected(IAD2 + 5 - 1)
           IF(II == 0)THEN
           
             VDX2(I) = ZERO   
             VDX3(I) = ZERO 
             VDX6(I) = ZERO   
             VDX7(I) = ZERO
           
             VDY2(I) = ZERO   
             VDY3(I) = ZERO 
             VDY6(I) = ZERO   
             VDY7(I) = ZERO
           
             VDZ2(I) = ZERO   
             VDZ3(I) = ZERO 
             VDZ6(I) = ZERO   
             VDZ7(I) = ZERO 
            
             VX2(I) = LVX2(I)  
             VX3(I) = LVX3(I)
             VX6(I) = LVX6(I) 
             VX7(I) = LVX7(I)
             
             VY2(I) =  LVY2(I)  
             VY3(I) =  LVY3(I)
             VY6(I) =  LVY6(I)   
             VY7(I) =  LVY7(I)
             
             VZ2(I) =  LVZ2(I)  
             VZ3(I) =  LVZ3(I)
             VZ6(I) =  LVZ6(I)   
             VZ7(I) =  LVZ7(I)
                          
             IFOAM(NC2(I)) = 1 
             IFOAM(NC3(I)) = 1
             IFOAM(NC6(I)) = 1
             IFOAM(NC7(I)) = 1
                       
           ENDIF
           II=ALE_CONNECT%ee_connect%connected(IAD2 + 6 - 1)
           IF(II == 0)THEN
             
             VDX1(I) = ZERO   
             VDX4(I) = ZERO 
             VDX5(I) = ZERO   
             VDX8(I) = ZERO
            
             VDY1(I) = ZERO   
             VDY4(I) = ZERO 
             VDY5(I) = ZERO   
             VDY8(I) = ZERO
             
             VDZ1(I) = ZERO   
             VDZ4(I) = ZERO 
             VDZ5(I) = ZERO   
             VDZ8(I) = ZERO  
             
             VX1(I) = LVX1(I)   
             VX4(I) = LVX4(I)
             VX5(I) = LVX5(I)  
             VX8(I) = LVX8(I)
             
             VY1(I) =  LVY1(I)  
             VY4(I) =  LVY4(I)
             VY5(I) =  LVY5(I) 
             VY8(I) =  LVY8(I)
             
             VZ1(I) =  LVZ1(I) 
             VZ4(I) =  LVZ4(I)
             VZ5(I) =  LVZ5(I)   
             VZ8(I) =  LVZ8(I) 
            
             IFOAM(NC1(I)) = 1  
             IFOAM(NC4(I)) = 1
             IFOAM(NC5(I)) = 1 
             IFOAM(NC8(I)) = 1       
           ENDIF
        ENDDO
#include "lockoff.inc"       
      ELSEIF(ICLOS == 2 .AND. INTBAG > 0  ) THEN
#include "lockon.inc"
          DO  I=LFT,LLT 
             IAD2 = ALE_CONNECT%ee_connect%iad_connect(I + NFT)
           II=ALE_CONNECT%ee_connect%connected(IAD2 + 1 - 1)
           IF(II == 0 .AND. REXCH(I,1) < EM15 )THEN
             
             VDX1(I) = ZERO   
             VDX2(I) = ZERO 
             VDX3(I) = ZERO   
             VDX4(I) = ZERO
             
             VDY1(I) = ZERO   
             VDY2(I) = ZERO 
             VDY3(I) = ZERO   
             VDY4(I) = ZERO
             
             VDZ1(I) = ZERO   
             VDZ2(I) = ZERO 
             VDZ3(I) = ZERO   
             VDZ4(I) = ZERO 
              
             VX1(I) = LVX1(I)   
             VX2(I) = LVX2(I)
             VX3(I) = LVX3(I)
             VX4(I) = LVX4(I)
             
            
             VY1(I) = LVY1(I)
             VY2(I) = LVY2(I)
             VY3(I) = LVY3(I) 
             VY4(I) = LVY4(I) 
             
             VZ1(I) =  LVZ1(I)  
             VZ2(I) =  LVZ2(I) 
             VZ3(I) =  LVZ3(I)  
             VZ4(I) =  LVZ4(I)           

             IFOAM(NC1(I)) = 1   
             IFOAM(NC2(I)) = 1
             IFOAM(NC3(I)) = 1
             IFOAM(NC4(I)) = 1                        
           ENDIF
           II=ALE_CONNECT%ee_connect%connected(IAD2 + 2 - 1)
           IF(II == 0 .AND. REXCH(I,2) < EM15) THEN
            
             VDX3(I) = ZERO
             VDX4(I) = ZERO  
             VDX7(I) = ZERO  
             VDX8(I) = ZERO  
             
             VDY3(I) =  ZERO
             VDY4(I) =  ZERO 
             VDY7(I) =  ZERO  
             VDY8(I) =  ZERO 
             
             VDZ3(I) = ZERO   
             VDZ4(I) = ZERO 
             VDZ7(I) = ZERO   
             VDZ8(I) = ZERO        
             
             VX3(I) =  LVX3(I)  
             VX4(I) =  LVX4(I)
             VX7(I) =  LVX7(I)  
             VX8(I) =  LVX8(I)
            
             VY3(I) =  LVY3(I)  
             VY4(I) =  LVY4(I) 
             VY7(I) =  LVY7(I)  
             VY8(I) =  LVY8(I) 
            
             VZ3(I) =  LVZ3(I)  
             VZ4(I) =  LVZ4(I) 
             VZ7(I) =  LVZ7(I)  
             VZ8(I) =  LVZ8(I)  
           
             IFOAM(NC3(I)) = 1
             IFOAM(NC4(I)) = 1
             IFOAM(NC7(I)) = 1
             IFOAM(NC8(I)) = 1
           ENDIF           
           II=ALE_CONNECT%ee_connect%connected(IAD2 + 3 - 1)
           IF(II == 0 .AND. REXCH(I,3) < EM15)THEN
             
             VDX5(I) = ZERO   
             VDX6(I) = ZERO 
             VDX7(I) = ZERO   
             VDX8(I) = ZERO
             
             VDY5(I) = ZERO   
             VDY6(I) = ZERO 
             VDY7(I) = ZERO   
             VDY8(I) = ZERO
             
             VDZ5(I) = ZERO   
             VDZ6(I) = ZERO 
             VDZ7(I) = ZERO   
             VDZ8(I) = ZERO       
             
             VX5(I) = LVX5(I)
             VX6(I) = LVX6(I)
             VX7(I) = LVX7(I)  
             VX8(I) = LVX8(I)
             
             VY5(I) = LVY5(I)   
             VY6(I) = LVY6(I)
             VY7(I) = LVY7(I)  
             VY8(I) = LVY8(I)
            
             VZ5(I) =  LVZ5(I)  
             VZ6(I) =  LVZ6(I) 
             VZ7(I) =  LVZ7(I)  
             VZ8(I) =  LVZ8(I)
            
             IFOAM(NC5(I)) = 1
             IFOAM(NC6(I)) = 1
             IFOAM(NC7(I)) = 1
             IFOAM(NC8(I)) = 1
             
           ENDIF
           II=ALE_CONNECT%ee_connect%connected(IAD2 + 4 - 1)
           IF(II == 0 .AND. REXCH(I,4) < EM15)THEN
            
             VDX1(I) = ZERO   
             VDX2(I) = ZERO 
             VDX5(I) = ZERO   
             VDX6(I) = ZERO
             
             VDY1(I) = ZERO   
             VDY2(I) = ZERO 
             VDY5(I) = ZERO   
             VDY6(I) = ZERO
             
             VDZ1(I) = ZERO   
             VDZ2(I) = ZERO 
             VDZ5(I) = ZERO   
             VDZ6(I) = ZERO          
             
             VX1(I) =  LVX1(I)   
             VX2(I) =  LVX2(I)
             VX5(I) =  LVX5(I) 
             VX6(I) =  LVX6(I)
            
             VY1(I) = LVY1(I)
             VY2(I) = LVY2(I)
             VY5(I) = LVY5(I)
             VY6(I) = LVY6(I)
             
             VZ1(I) =  LVZ1(I)
             VZ2(I) =  LVZ2(I)
             VZ5(I) =  LVZ5(I)
             VZ6(I) =  LVZ6(I) 
           
             IFOAM(NC1(I)) =  1   
             IFOAM(NC2(I)) =  1
             IFOAM(NC5(I)) =  1 
             IFOAM(NC6(I)) =  1
           ENDIF
           II=ALE_CONNECT%ee_connect%connected(IAD2 + 5 - 1)
           IF(II == 0 .AND. REXCH(I,5) < EM15)THEN
             
             VDX2(I) = ZERO   
             VDX3(I) = ZERO 
             VDX6(I) = ZERO   
             VDX7(I) = ZERO
            
             VDY2(I) = ZERO   
             VDY3(I) = ZERO 
             VDY6(I) = ZERO   
             VDY7(I) = ZERO
             
             VDZ2(I) = ZERO   
             VDZ3(I) = ZERO 
             VDZ6(I) = ZERO   
             VDZ7(I) = ZERO 
             
             VX2(I) = LVX2(I)  
             VX3(I) = LVX3(I)
             VX6(I) = LVX6(I) 
             VX7(I) = LVX7(I)
             
             VY2(I) =  LVY2(I)  
             VY3(I) =  LVY3(I)
             VY6(I) =  LVY6(I)   
             VY7(I) =  LVY7(I)
            
             VZ2(I) =  LVZ2(I)  
             VZ3(I) =  LVZ3(I)
             VZ6(I) =  LVZ6(I)   
             VZ7(I) =  LVZ7(I)
                           
             IFOAM(NC2(I)) = 1  
             IFOAM(NC3(I)) = 1
             IFOAM(NC6(I)) = 1  
             IFOAM(NC7(I)) = 1
           ENDIF
           II=ALE_CONNECT%ee_connect%connected(IAD2 + 6 - 1)
           IF(II == 0 .AND. REXCH(I,6) < EM15)THEN
             
             VDX1(I) = ZERO   
             VDX4(I) = ZERO 
             VDX5(I) = ZERO   
             VDX8(I) = ZERO
            
             VDY1(I) = ZERO   
             VDY4(I) = ZERO 
             VDY5(I) = ZERO   
             VDY8(I) = ZERO
             
             VDZ1(I) = ZERO   
             VDZ4(I) = ZERO 
             VDZ5(I) = ZERO   
             VDZ8(I) = ZERO  
             
             VX1(I) = LVX1(I)   
             VX4(I) = LVX4(I)
             VX5(I) = LVX5(I)  
             VX8(I) = LVX8(I)
            
             VY1(I) =  LVY1(I)  
             VY4(I) =  LVY4(I)
             VY5(I) =  LVY5(I) 
             VY8(I) =  LVY8(I)
             
             VZ1(I) =  LVZ1(I) 
             VZ4(I) =  LVZ4(I)
             VZ5(I) =  LVZ5(I)   
             VZ8(I) =  LVZ8(I) 
             
             IFOAM(NC1(I)) = 1   
             IFOAM(NC4(I)) = 1
             IFOAM(NC5(I)) = 1   
             IFOAM(NC8(I)) = 1
           ENDIF
          ENDDO 
#include "lockoff.inc"
        ENDIF       
C----------------------------
C       CALCULATION OF CONVECTIVE FLUXES
C       MASSES SET TO ZERO
C-----------------------------
C            
        CALL ALEFLUX(PM ,IXS  ,NC1 ,NC2 ,NC3 ,NC4 ,NC5 ,
     .               NC6 ,NC7,NC8 ,X1  ,X2  ,X3  ,X4  ,
     .               X5  ,X6 ,X7  ,X8  ,Y1  ,Y2  ,Y3  ,
     .               Y4  ,Y5 ,Y6  ,Y7  ,Y8  ,Z1  ,Z2  ,
     .               Z3  ,Z4 ,Z5  ,Z6  ,Z7 ,Z8   ,V   ,
     .               VDX1,VDX2,VDX3,VDX4,VDX5,VDX6,VDX7,VDX8,
     .               VDY1,VDY2,VDY3,VDY4,VDY5,VDY6,VDY7,VDY8,
     .               VDZ1,VDZ2,VDZ3,VDZ4,VDZ5,VDZ6,VDZ7,VDZ8,
     .               W  ,FLUX,FLU1,ALE_CONNECT,REXCH , NGL)
     
        IF(ICONGAS == 0) THEN
#include "lockon.inc"
          DO  I=LFT,LLT 
             IAD2 = ALE_CONNECT%ee_connect%iad_connect(I + NFT)
           II=ALE_CONNECT%ee_connect%connected(IAD2 + 1 - 1)
           IF(II == 0 .AND. FLUX(I,1) < ZERO )THEN
            
             VDX1(I) = ZERO   
             VDX2(I) = ZERO 
             VDX3(I) = ZERO   
             VDX4(I) = ZERO
             
             VDY1(I) = ZERO   
             VDY2(I) = ZERO 
             VDY3(I) = ZERO   
             VDY4(I) = ZERO
             
             VDZ1(I) = ZERO   
             VDZ2(I) = ZERO 
             VDZ3(I) = ZERO   
             VDZ4(I) = ZERO 
               
             VX1(I) = LVX1(I)   
             VX2(I) = LVX2(I)
             VX3(I) = LVX3(I)
             VX4(I) = LVX4(I)
             
             
             VY1(I) = LVY1(I)
             VY2(I) = LVY2(I)
             VY3(I) = LVY3(I) 
             VY4(I) = LVY4(I) 
             
             VZ1(I) =  LVZ1(I)  
             VZ2(I) =  LVZ2(I) 
             VZ3(I) =  LVZ3(I)  
             VZ4(I) =  LVZ4(I)           

             IFOAM(NC1(I)) = 1   
             IFOAM(NC2(I)) = 1
             IFOAM(NC3(I)) = 1
             IFOAM(NC4(I)) = 1                        
           ENDIF
           II=ALE_CONNECT%ee_connect%connected(IAD2 + 2 - 1)
           IF(II == 0 .AND. FLUX(I,2) < ZERO) THEN
             
             VDX3(I) = ZERO
             VDX4(I) = ZERO  
             VDX7(I) = ZERO  
             VDX8(I) = ZERO  
             
             VDY3(I) =  ZERO
             VDY4(I) =  ZERO 
             VDY7(I) =  ZERO  
             VDY8(I) =  ZERO 
             
             VDZ3(I) = ZERO   
             VDZ4(I) = ZERO 
             VDZ7(I) = ZERO   
             VDZ8(I) = ZERO        
             
             VX3(I) =  LVX3(I)  
             VX4(I) =  LVX4(I)
             VX7(I) =  LVX7(I)  
             VX8(I) =  LVX8(I)
            
             VY3(I) =  LVY3(I)  
             VY4(I) =  LVY4(I) 
             VY7(I) =  LVY7(I)  
             VY8(I) =  LVY8(I) 
             
             VZ3(I) =  LVZ3(I)  
             VZ4(I) =  LVZ4(I) 
             VZ7(I) =  LVZ7(I)  
             VZ8(I) =  LVZ8(I)  
            
             IFOAM(NC3(I)) = 1
             IFOAM(NC4(I)) = 1
             IFOAM(NC7(I)) = 1
             IFOAM(NC8(I)) = 1
           ENDIF           
           II=ALE_CONNECT%ee_connect%connected(IAD2 + 3 - 1)
           IF(II == 0 .AND. FLUX(I,3) < ZERO)THEN
             
             VDX5(I) = ZERO   
             VDX6(I) = ZERO 
             VDX7(I) = ZERO   
             VDX8(I) = ZERO
             
             VDY5(I) = ZERO   
             VDY6(I) = ZERO 
             VDY7(I) = ZERO   
             VDY8(I) = ZERO
             
             VDZ5(I) = ZERO   
             VDZ6(I) = ZERO 
             VDZ7(I) = ZERO   
             VDZ8(I) = ZERO       
             
             VX5(I) = LVX5(I)
             VX6(I) = LVX6(I)
             VX7(I) = LVX7(I)  
             VX8(I) = LVX8(I)
             
             VY5(I) = LVY5(I)   
             VY6(I) = LVY6(I)
             VY7(I) = LVY7(I)  
             VY8(I) = LVY8(I)
            
             VZ5(I) =  LVZ5(I)  
             VZ6(I) =  LVZ6(I) 
             VZ7(I) =  LVZ7(I)  
             VZ8(I) =  LVZ8(I)
           
             IFOAM(NC5(I)) = 1
             IFOAM(NC6(I)) = 1
             IFOAM(NC7(I)) = 1
             IFOAM(NC8(I)) = 1
            
           ENDIF
           II=ALE_CONNECT%ee_connect%connected(IAD2 + 4 - 1)
           IF(II == 0 .AND. FLUX(I,4) < ZERO)THEN
            
             VDX1(I) = ZERO   
             VDX2(I) = ZERO 
             VDX5(I) = ZERO   
             VDX6(I) = ZERO
            
             VDY1(I) = ZERO   
             VDY2(I) = ZERO 
             VDY5(I) = ZERO   
             VDY6(I) = ZERO
            
             VDZ1(I) = ZERO   
             VDZ2(I) = ZERO 
             VDZ5(I) = ZERO   
             VDZ6(I) = ZERO          
             
             VX1(I) =  LVX1(I)   
             VX2(I) =  LVX2(I)
             VX5(I) =  LVX5(I) 
             VX6(I) =  LVX6(I)
            
             VY1(I) = LVY1(I)
             VY2(I) = LVY2(I)
             VY5(I) = LVY5(I)
             VY6(I) = LVY6(I)
            
             VZ1(I) =  LVZ1(I)
             VZ2(I) =  LVZ2(I)
             VZ5(I) =  LVZ5(I)
             VZ6(I) =  LVZ6(I) 
            
             IFOAM(NC1(I)) =  1   
             IFOAM(NC2(I)) =  1
             IFOAM(NC5(I)) =  1 
             IFOAM(NC6(I)) =  1
           ENDIF
           II=ALE_CONNECT%ee_connect%connected(IAD2 + 5 - 1)
           IF(II == 0 .AND. FLUX(I,5) < ZERO)THEN
            
             VDX2(I) = ZERO   
             VDX3(I) = ZERO 
             VDX6(I) = ZERO   
             VDX7(I) = ZERO
             
             VDY2(I) = ZERO   
             VDY3(I) = ZERO 
             VDY6(I) = ZERO   
             VDY7(I) = ZERO
             
             VDZ2(I) = ZERO   
             VDZ3(I) = ZERO 
             VDZ6(I) = ZERO   
             VDZ7(I) = ZERO 
             
             VX2(I) = LVX2(I)  
             VX3(I) = LVX3(I)
             VX6(I) = LVX6(I) 
             VX7(I) = LVX7(I)
            
             VY2(I) =  LVY2(I)  
             VY3(I) =  LVY3(I)
             VY6(I) =  LVY6(I)   
             VY7(I) =  LVY7(I)
             
             VZ2(I) =  LVZ2(I)  
             VZ3(I) =  LVZ3(I)
             VZ6(I) =  LVZ6(I)   
             VZ7(I) =  LVZ7(I)
                           
             IFOAM(NC2(I)) = 1  
             IFOAM(NC3(I)) = 1
             IFOAM(NC6(I)) = 1  
             IFOAM(NC7(I)) = 1
           ENDIF
           II=ALE_CONNECT%ee_connect%connected(IAD2 + 6 - 1)
           IF(II == 0 .AND. FLUX(I,6) < ZERO)THEN
            
             VDX1(I) = ZERO   
             VDX4(I) = ZERO 
             VDX5(I) = ZERO   
             VDX8(I) = ZERO
            
             VDY1(I) = ZERO   
             VDY4(I) = ZERO 
             VDY5(I) = ZERO   
             VDY8(I) = ZERO
             
             VDZ1(I) = ZERO   
             VDZ4(I) = ZERO 
             VDZ5(I) = ZERO   
             VDZ8(I) = ZERO  
             
             VX1(I) = LVX1(I)   
             VX4(I) = LVX4(I)
             VX5(I) = LVX5(I)  
             VX8(I) = LVX8(I)
            
             VY1(I) =  LVY1(I)  
             VY4(I) =  LVY4(I)
             VY5(I) =  LVY5(I) 
             VY8(I) =  LVY8(I)
           
             VZ1(I) =  LVZ1(I) 
             VZ4(I) =  LVZ4(I)
             VZ5(I) =  LVZ5(I)   
             VZ8(I) =  LVZ8(I) 
             
             IFOAM(NC1(I)) = 1   
             IFOAM(NC4(I)) = 1
             IFOAM(NC5(I)) = 1   
             IFOAM(NC8(I)) = 1
           ENDIF
          ENDDO 
#include "lockoff.inc"   
        ENDIF

C-----------------------------
C       CONVECTION OF ENERGY AND MASS ...
C-----------------------------
            NVAR = 1
            CALL ALECONVE(MBUF%VAR((NVAR-1)*NEL+1),FLUX  ,FLU1  ,
     .                    PHI1   ,ALE_CONNECT  ,RHOEXT ,NGL   ,NVAR  )
            NVAR = 2
            CALL ALECONVE(MBUF%VAR((NVAR-1)*NEL+1),FLUX  ,FLU1  ,
     .                    PHI2   ,ALE_CONNECT  ,EIEXT  ,NGL   ,NVAR  )     
C-----------
C Darcy force computed from flow velocity 
C------------

      AA   = PM(194,MAT(1))
      BB   = PM(195,MAT(1))
      TAUX = PM(196,MAT(1))

           
      DTINV = DT1/MAX(DT1*DT1, EM20)  
    
      MX = MAT(LFT) 
      DO I=LFT,LLT
        FRHO0(I) =PM(192,MX) 
         KK(I) = MBUF%VAR(21*NEL + I)
         FAC = ONE_OVER_8*(ONE - ALPHA(I))/MAX(EM20,KK(I))       

        VDX(I)=ONE_OVER_8*(VDX1(I)+VDX2(I)+VDX3(I)+VDX4(I)
     .               +VDX5(I)+VDX6(I)+VDX7(I)+VDX8(I))
        VDY(I)=ONE_OVER_8*(VDY1(I)+VDY2(I)+VDY3(I)+VDY4(I)
     .               +VDY5(I)+VDY6(I)+VDY7(I)+VDY8(I))
        VDZ(I)=ONE_OVER_8*(VDZ1(I)+VDZ2(I)+VDZ3(I)+VDZ4(I)
     .               +VDZ5(I)+VDZ6(I)+VDZ7(I)+VDZ8(I))
        VD2(I)=(VDX(I)**2+VDY(I)**2+VDZ(I)**2)    
C  darcy forces       
        VFE(1) = ONE_OVER_8*(VDX1(I) + VDX2(I) + VDX3(I) + VdX4(I) +
     .                   VDX5(I) + VDX6(I) + VDX7(I) + VDX8(I))
        VFE(2) = ONE_OVER_8*(VDY1(I) + VDY2(I) + VDY3(I) + VDY4(I) +
     .                   VDY5(I) + VDY6(I) + VDY7(I) + VDY8(I)) 
        VFE(3) = ONE_OVER_8*(VDZ1(I) + VDZ2(I) + VDZ3(I) + VDZ4(I) +
     .                   VDZ5(I) + VDZ6(I) + VDZ7(I) + VDZ8(I))
C acceleration 
C But is always A=Zero is not activated. Is the Gaz acceleration .
C if is required we should use the relative acceleration as velocity
C   A= (A_gaz - A_foam).     
        AFE(1) = ONE_OVER_8*(A(1,NC1(I)) + A(1,NC2(I)) + A(1,NC3(I)) + 
     .                   A(1,NC4(I)) + A(1,NC5(I)) + A(1,NC6(I)) +
     .                   A(1,NC7(I)) + A(1,NC8(I)))
C     
        AFE(2) = ONE_OVER_8*(A(2,NC1(I)) + A(2,NC2(I)) + A(2,NC3(I)) + 
     .                   A(2,NC4(I)) + A(2,NC5(I)) + A(2,NC6(I)) +
     .                   A(2,NC7(I)) + A(2,NC8(I)))
C     
        AFE(3) = ONE_OVER_8*(A(3,NC1(I)) + A(3,NC2(I)) + A(3,NC3(I)) + 
     .                   A(3,NC4(I)) + A(3,NC5(I)) + A(3,NC6(I)) +
     .                   A(3,NC7(I)) + A(3,NC8(I)))     
C        

C stablit condition 
cc         F1 =  FOURTH*VFE(1)*MASS*DTINV
cc         F2 =  FOURTH*VFE(2)*MASS*DTINV   
cc         F3 =  FOURTH*VFE(3)*MASS*DTINV 
C                 
         DFE(I,1)=FAC*(AA*VFE(1) + BB*ABS(VFE(1))*VFE(1) + TAUX*AFE(1))
         DFE(I,2)=FAC*(AA*VFE(2) + BB*ABS(VFE(2))*VFE(2) + TAUX*AFE(2))
         DFE(I,3)=FAC*(AA*VFE(3) + BB*ABS(VFE(3))*VFE(3) + TAUX*AFE(3))   
      ENDDO
c-----------
      RETURN
      END
