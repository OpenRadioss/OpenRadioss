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
Chd|  I2VIT10                       source/interfaces/interf/i2vit10.F
Chd|-- called by -----------
Chd|        INTTI2V                       source/interfaces/interf/intti2v.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2VIT10(
     1 NSN     ,NMN     ,MS      ,V       ,A       ,
     2 AR      ,VR      ,X       ,IRECT   ,NSV     ,
     3 MSR     ,IRTL    ,CRST    ,IRUPT   ,WEIGHT  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN,NMN,
     .   IRECT(4,*), MSR(*), NSV(*), IRTL(*),IRUPT(*),WEIGHT(*)
C     REAL ou REAL*8
      my_real
     .   A(3,*),V(3,*),AR(3,*),VR(3,*),X(3,*),MS(*),CRST(2,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIR, I, J, II,JJ,L,NN, ISK, ICOD
C     REAL ou REAL*8
      my_real
     .   H(4), SS, TT, AMX, AMY, AMZ, VMX, VMY, VMZ,SP,SM,TP,TM,MAS,
     .   FAC, V1X2, V2X1, V2X3, V3X2, V3X1, V1X3,XC0,YC0,ZC0
C=======================================================================
      NIR=4
C
      DO II=1,NSN
        I = NSV(II)
        L = IRTL(II)
C
        IF(I > 0 .AND. IRUPT(II) == 0) THEN
           SS=CRST(1,II)
           TT=CRST(2,II)
           SP=ONE + SS
           SM=ONE - SS
           TP=FOURTH*(ONE + TT)
           TM=FOURTH*(ONE - TT)
           H(1)=TM*SM
           H(2)=TM*SP
           H(3)=TP*SP
           H(4)=TP*SM
           AMX=ZERO
           AMY=ZERO
           AMZ=ZERO
           VMX=ZERO
           VMY=ZERO
           VMZ=ZERO    
C
           DO JJ=1,NIR
             J=IRECT(JJ,L)
             AMX=AMX+A(1,J)*H(JJ)
             AMY=AMY+A(2,J)*H(JJ)
             AMZ=AMZ+A(3,J)*H(JJ)
             VMX=VMX+V(1,J)*H(JJ)
             VMY=VMY+V(2,J)*H(JJ)
             VMZ=VMZ+V(3,J)*H(JJ)
           ENDDO
           A(1,I)=AMX
           A(2,I)=AMY
           A(3,I)=AMZ
           V(1,I)=VMX
           V(2,I)=VMY
           V(3,I)=VMZ
C
        ENDIF
      ENDDO
C----
      RETURN
      END
Chd|====================================================================
Chd|  I2ROT10                       source/interfaces/interf/i2vit10.F
Chd|-- called by -----------
Chd|        INTTI2V                       source/interfaces/interf/intti2v.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2ROT10(
     1 NSN     ,NMN     ,MS      ,V       ,A       ,
     2 AR      ,VR      ,X       ,IRECT   ,NSV     ,
     3 MSR     ,IRTL    ,CRST    ,IRUPT   ,WEIGHT  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN,NMN,
     .   IRECT(4,*), MSR(*), NSV(*), IRTL(*),IRUPT(*),WEIGHT(*)
C     REAL ou REAL*8
      my_real
     .   A(3,*),V(3,*),AR(3,*),VR(3,*),X(3,*),MS(*),CRST(2,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIR, I, J, II,JJ,L,NN, ISK, ICOD
C     REAL ou REAL*8
      my_real
     .   H(4), SS, TT, AMX, AMY, AMZ, VMX, VMY, VMZ,SP,SM,TP,TM,MAS,
     .   FAC, V1X2, V2X1, V2X3, V3X2, V3X1, V1X3,XC0,YC0,ZC0
C=======================================================================
      NIR=4
C
      DO II=1,NSN
        I = NSV(II)
        L = IRTL(II)
C
        IF(I > 0 .AND. IRUPT(II) == 0) THEN
          SS=CRST(1,II)                             
          TT=CRST(2,II)                             
          SP=ONE + SS                               
          SM=ONE - SS                               
          TP=FOURTH*(ONE + TT)                        
          TM=FOURTH*(ONE - TT)                        
          H(1)=TM*SM                                
          H(2)=TM*SP                                
          H(3)=TP*SP                                
          H(4)=TP*SM                                
C
          XC0 = X(1,I)                              
          YC0 = X(2,I)                              
          ZC0 = X(3,I)                              
C
          AMX=ZERO                                    
          AMY=ZERO                                    
          AMZ=ZERO                                    
          VMX=ZERO                                    
          VMY=ZERO                                    
          VMZ=ZERO                                    
          DO JJ=1,4                                 
            J=IRECT(JJ,L)                           
            AMX=AMX+AR(1,J)*H(JJ)                   
            AMY=AMY+AR(2,J)*H(JJ)                   
            AMZ=AMZ+AR(3,J)*H(JJ)                   
            VMX=VMX+VR(1,J)*H(JJ)                   
            VMY=VMY+VR(2,J)*H(JJ)                   
            VMZ=VMZ+VR(3,J)*H(JJ)                   
            XC0=XC0 - X(1,J) * H(JJ)                
            YC0=YC0 - X(2,J) * H(JJ)                
            ZC0=ZC0 - X(3,J) * H(JJ)                
          ENDDO                                     
          AR(1,I)=AMX                               
          AR(2,I)=AMY                               
          AR(3,I)=AMZ                               
          VR(1,I)=VMX                               
          VR(2,I)=VMY                               
          VR(3,I)=VMZ                               
          V1X2=VMX * YC0                            
          V2X1=VMY * XC0                            
          V2X3=VMY * ZC0                            
          V3X2=VMZ * YC0                            
          V3X1=VMZ * XC0                            
          V1X3=VMX * ZC0                            

          A(1,I)= A(1,I) + AMY * ZC0 - AMZ * YC0    
     .          - HALF*(VMY*V2X1+VMZ*V3X1)           
          A(2,I)= A(2,I) + AMZ * XC0 - AMX * ZC0    
     .          - HALF*(VMZ*V3X2+VMX*V1X2)           
          A(3,I)= A(3,I) + AMX * YC0 - AMY * XC0    
     .          - HALF*(VMX*V1X3+VMY*V2X3)           
          V(1,I)= V(1,I) + V2X3 - V3X2              
          V(2,I)= V(2,I) + V3X1 - V1X3              
          V(3,I)= V(3,I) + V1X2 - V2X1              
        ENDIF                                       
      ENDDO
C----
      RETURN
      END
