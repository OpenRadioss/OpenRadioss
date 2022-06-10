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
Chd|  CUPDT_PLY                     source/properties/composite_options/stack/cupdt_ply.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- calls ---------------
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|====================================================================
      SUBROUTINE CUPDT_PLY(JFT  ,JLT   ,NVC    ,OFFG   ,OFF  ,IADC ,
     2                     IEL , INOD  ,IXC    ,MS     ,IN   ,MS_PLY ,
     3                     ZI_PLY,ISTACK,POSLY,
     4                     FLY11 ,FLY12 ,FLY13  ,FLY14 ,FLY21 ,FLY22,
     5                     FLY23 ,FLY24 ,FLY31  ,FLY32 ,FLY33,FLY34 ,
     6                     FAC   ,STI   ,MSZ2) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
       USE PLYXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include     "vect01_c.inc"
#include     "parit_c.inc"
#include     "com04_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT, NVC
      INTEGER IADC(4,*),IXC(NIXC,MVSIZ),ISTACK(MVSIZ,NPT),IEL(*),
     .        INOD(*)
C     REAL
      my_real
     .    OFFG(*), OFF(*),  IN(*), MS(*),MS_PLY(NPLYXFE,*),
     .     POSLY(MVSIZ,NPT),ZI_PLY(NPLYXFE,*),
     .   FLY11(MVSIZ, NPT), FLY21(MVSIZ, NPT), FLY31(MVSIZ, NPT),
     .   FLY12(MVSIZ, NPT), FLY22(MVSIZ, NPT), FLY32(MVSIZ, NPT),
     .   FLY13(MVSIZ, NPT), FLY23(MVSIZ, NPT), FLY33(MVSIZ, NPT),
     .   FLY14(MVSIZ, NPT), FLY24(MVSIZ, NPT), FLY34(MVSIZ, NPT),
     .   PTH(MVSIZ,NPT),FAC(MVSIZ,2),STI(MVSIZ,*),MSZ2(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NVC1, NVC2, NVC3, NVC4, I, J, MX,IOFF,IP,I1,I2,I3,I4,N,
     .        IN1,IN2,IN3,IN4
      my_real
     .   OFF_L,  
     .   FPLY11,FPLY21,FPLY31,FPLY12,FPLY22,FPLY32,FPLY13,FPLY23,
     .   FPLY33,FPLY14,FPLY24,FPLY34,FPLY,MPLY,
     .   MPLY11,MPLY21,MPLY31,MPLY12,MPLY22,MPLY32,MPLY13,MPLY23,
     .   MPLY33,MPLY14,MPLY24,MPLY34
C-----------------------------------------------
C
C cumul de l'energie des elements deletes AU moment du delete
C
      OFF_L = ZERO
      DO I=JFT,JLT
        IF(OFF(I).LT.ONE)OFFG(I) = OFF(I)
        OFF_L = MIN(OFF_L,OFFG(I))
      ENDDO
      IF(OFF_L.LT.ZERO)THEN
        DO I=JFT,JLT
         IF(OFFG(I).LT.ZERO)THEN
          DO J=1,NPT
           FLY11(I,J)=ZERO
           FLY21(I,J)=ZERO
           FLY31(I,J)=ZERO
           FLY12(I,J)=ZERO
           FLY22(I,J)=ZERO
           FLY32(I,J)=ZERO
           FLY13(I,J)=ZERO
           FLY23(I,J)=ZERO
           FLY33(I,J)=ZERO
           FLY14(I,J)=ZERO
           FLY24(I,J)=ZERO
           FLY34(I,J)=ZERO
          ENDDO
         ENDIF
        ENDDO
      ENDIF
C
      IF(IPARIT == 0) THEN
        NVC1= NVC/8
        NVC2=(NVC-NVC1*8)/4
        NVC3=(NVC-NVC1*8-NVC2*4)/2
        NVC4=(NVC-NVC1*8-NVC2*4-NVC3*2)
C to do
C    
        DO I=JFT,JLT
        
         I1 = INOD(IXC(2,I))      
         I2 = INOD(IXC(3,I))      
         I3 = INOD(IXC(4,I))      
         I4 = INOD(IXC(5,I))      
C                                 
         IN1 = IXC(2,I)           
         IN2 = IXC(3,I)           
         IN3 = IXC(4,I)           
         IN4 = IXC(5,I)
         N = IEL(I + NFT)
         DO J=1,NPT
          IP = ISTACK(I,J)  
C         PRINT *,J,IP
          PLY(IP)%A(1,I1)= PLY(IP)%A(1,I1) - (FLY11(I,J) )
          PLY(IP)%A(2,I1)= PLY(IP)%A(2,I1) - (FLY21(I,J) )
          PLY(IP)%A(3,I1)= PLY(IP)%A(3,I1) - (FLY31(I,J) )
          PLY(IP)%A(4,I1)= PLY(IP)%A(4,I1) + STI(I,J)*FAC(I,1)
c
          PLY(IP)%A(1,I2)= PLY(IP)%A(1,I2) - (FLY12(I,J) )
          PLY(IP)%A(2,I2)= PLY(IP)%A(2,I2) - (FLY22(I,J) )
          PLY(IP)%A(3,I2)= PLY(IP)%A(3,I2) - (FLY32(I,J) )
          PLY(IP)%A(4,I2)= PLY(IP)%A(4,I2) + STI(I,J)*FAC(I,2)

c
          PLY(IP)%A(1,I3)= PLY(IP)%A(1,I3) - (FLY13(I,J) )
          PLY(IP)%A(2,I3)= PLY(IP)%A(2,I3) - (FLY23(I,J) )
          PLY(IP)%A(3,I3)= PLY(IP)%A(3,I3) - (FLY33(I,J) )
          PLY(IP)%A(4,I3)= PLY(IP)%A(4,I3) + STI(I,J)*FAC(I,1)
c
          PLY(IP)%A(1,I4)= PLY(IP)%A(1,I4) - (FLY14(I,J) )
          PLY(IP)%A(2,I4)= PLY(IP)%A(2,I4) - (FLY24(I,J) )
          PLY(IP)%A(3,I4)= PLY(IP)%A(3,I4) - (FLY34(I,J) )
          PLY(IP)%A(4,I4)= PLY(IP)%A(4,I4) + STI(I,J)*FAC(I,2)
C   
         ENDDO
       ENDDO 

      ELSE  
cc#include "cupdt3_pf.inc"
        DO I=JFT,JLT
        
         I1 = INOD(IXC(2,I))      
         I2 = INOD(IXC(3,I))      
         I3 = INOD(IXC(4,I))      
         I4 = INOD(IXC(5,I))      
C                                 
         IN1 = IXC(2,I)           
         IN2 = IXC(3,I)           
         IN3 = IXC(4,I)           
         IN4 = IXC(5,I)
C    
         N = IEL(I + NFT)
         DO J=1,NPT
             IP = ISTACK(I,J)  
             PLYSKY(IP)%FSKY(1,IADC(1,N))= -(FLY11(I,J) )   
             PLYSKY(IP)%FSKY(2,IADC(1,N))= -(FLY21(I,J) )    
             PLYSKY(IP)%FSKY(3,IADC(1,N))= -(FLY31(I,J) )   
C stifness by ply
             PLYSKY(IP)%FSKY(4,IADC(1,N))= STI(I,J)*FAC(I,1) 
C               
             PLYSKY(IP)%FSKY(1,IADC(2,N))= -(FLY12(I,J) ) 
             PLYSKY(IP)%FSKY(2,IADC(2,N))= -(FLY22(I,J) )  
             PLYSKY(IP)%FSKY(3,IADC(2,N))= -(FLY32(I,J) )
C stifness by ply
             PLYSKY(IP)%FSKY(4,IADC(2,N))= STI(I,J)*FAC(I,2) 
c                                      
             PLYSKY(IP)%FSKY(1,IADC(3,N))= -(FLY13(I,J))  
             PLYSKY(IP)%FSKY(2,IADC(3,N))= -(FLY23(I,J) )
             PLYSKY(IP)%FSKY(3,IADC(3,N))= -(FLY33(I,J) )  
C stifness by ply
             PLYSKY(IP)%FSKY(4,IADC(3,N))= STI(I,J)*FAC(I,1)
             
             PLYSKY(IP)%FSKY(1,IADC(4,N))= -(FLY14(I,J) )  
             PLYSKY(IP)%FSKY(2,IADC(4,N))= -(FLY24(I,J) ) 
             PLYSKY(IP)%FSKY(3,IADC(4,N))= -(FLY34(I,J) )
C stifness by ply
             PLYSKY(IP)%FSKY(4,IADC(4,N))= STI(I,J)*FAC(I,2)
C   
         ENDDO
       ENDDO 
C  
      ENDIF  
C
      RETURN
      END
C      
