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
Chd|  CBAFINT_PLY                   source/properties/composite_options/stack/cbafint_ply.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CBAFINT_PLY(JFT,JLT,NPT,NG,NPLAT,IPLAT,CDET,THKLY,TH12,
     .                        VOL,FF0,BM,BC,TC,SIG_IPLY,VNI,AREA,
     .                        VF,VFI,IXC) 
C--------------------------------------------------------------------------------------------------
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C	CALCUL DES FORCES INTERNES AUX NOEUDS 
C	ENTREES : NEL,MNT,NPT,NP,NIXC,IXC,HZ,OFF,
C                 CDET,EPAIS,BUFCON,BM,,,BC
C	SORTIES : VF
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C--------------------------------------------------------------------------------------------------
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,NPT,NPLAT,IPLAT(*),NG,IXC(NIXC,*)
      my_real 
c     .   FF(5,MVSIZ,NPT),CDET(*),VOL(MVSIZ,NPT), 
     .   FF0(MVSIZ,5,NPT),CDET(*),VOL(*), 
     .   BM(MVSIZ,36),BC(MVSIZ,40),
     .   THKLY(MVSIZ,*),VF(MVSIZ,12,NPT),TH12(*),
     .   TC(MVSIZ,2,2),SIG_IPLY(MVSIZ,3,NPT),
     .   AREA(MVSIZ),VNI(4,4),
     .   VFI(MVSIZ,12,NPT)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER EP,I,J,JSUP,JINF,IPLY,K,NPLAT0
      my_real
     .  C1,C2,BC1,BC3,BC5,BCX,BCY,FL1,FL2,FL3,
     .  FF(MVSIZ,5,NPT)
     
C--------------------------------------------------------------------------------------------------$
!!        NPLAT0 = JLT
          NPLAT0 = NPLAT
       DO J=1,NPT
#include "vectorize.inc"
        DO I=JFT,NPLAT0 
          EP=IPLAT(I)
          FF(EP,1,J)=FF0(EP,1,J) 
          FF(EP,2,J)=FF0(EP,2,J) 
          FF(EP,3,J)=FF0(EP,3,J) 
          FF(EP,5,J)=FF0(EP,4,J) 
          FF(EP,4,J)=FF0(EP,5,J) 
C          
cc 	  C2=VOL(EP)/NPT
!!          C2=VOL(EP)
          C2=THKLY(EP,J)*CDET(EP)
          C1=TH12(EP)*CDET(EP)
          BC1=BC(EP,1)*FF(EP,4,J)
          BC3=BC(EP,3)*FF(EP,4,J)
          BC5=BC(EP,5)*FF(EP,4,J)
C----------NOEUD 1 ------------
          VF(EP,1,J) = VF(EP,1,J) + C2*(BM(EP,1)*FF(EP,1,J))
          VF(EP,2,J) = VF(EP,2,J) + C2*(BM(EP,5)*FF(EP,2,J))
          VF(EP,3,J) = VF(EP,3,J) + C2*(BC1+BC(EP,2)*FF(EP,5,J))
C integration complete du cisaillement
cc          VF(1,EP,J) = VF(1,EP,J) + C2*(BM(EP,5)*FF(3,EP,J))
cc          VF(2,EP,J) = VF(2,EP,J) + C2*(BM(EP,1)*FF(3,EP,J)) 
          
C         
          VF(EP,7,J) = VF(EP,7,J) + C2*(BM(EP,3)*FF(EP,1,J))
          VF(EP,8,J) = VF(EP,8,J) + C2*(BM(EP,7)*FF(EP,2,J))
C integration complete du cisaillement
cc          VF(7,EP,J) = VF(7,EP,J) + C2*(BM(7,EP)*FF(3,EP,J))
cc          VF(8,EP,J) = VF(8,EP,J) + C2*(BM(3,EP)*FF(3,EP,J))                 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
          VF(EP,9,J) = VF(EP,9,J) + C2*(BC(EP,13)*FF(EP,4,J)
     .                            + BC(EP,14)*FF(EP,5,J))
C----------NOEUD 2 ------------
          VF(EP,4,J) = VF(EP,4,J) + C2*(BM(EP,2)*FF(EP,1,J))
          VF(EP,5,J) = VF(EP,5,J) + C2*(BM(EP,6)*FF(EP,2,J))
          VF(EP,6,J) = VF(EP,6,J) + C2*(BC(EP,7)*FF(EP,4,J)
     .                            + BC(EP,8)*FF(EP,5,J)) 
C integration complete du cisaillement
cc          VF(4,EP,J) = VF(4,EP,J) + C2*(BM(EP,6)*FF(3,EP,J))
cc          VF(5,EP,J) = VF(5,EP,J) + C2*(BM(EP,2)*FF(3,EP,J)) 
C
          VF(EP,10,J) =-VF(EP,7,J)
          VF(EP,11,J) =-VF(EP,8,J)
          VF(EP,12,J) =-VF(EP,9,J) 
       ENDDO  
#include "vectorize.inc"
       DO I=NPLAT0+1,JLT
          EP=IPLAT(I)
C
          FF(EP,1,J)=FF0(EP,1,J) 
          FF(EP,2,J)=FF0(EP,2,J) 
          FF(EP,3,J)=FF0(EP,3,J) 
          FF(EP,5,J)=FF0(EP,4,J) 
          FF(EP,4,J)=FF0(EP,5,J)
C          
!! 	  C2=VOL(EP)          
          C2=THKLY(EP,J)*CDET(EP)
          C1=TH12(EP)*CDET(EP)
	  BCX=TC(EP,1,1)*FF(EP,4,J)+TC(EP,1,2)*FF(EP,5,J)
	  BCY=TC(EP,2,1)*FF(EP,4,J)+TC(EP,2,2)*FF(EP,5,J)
C	  J=1 K0=0 K1=0 K2=0
	  VF(EP,1,J) = VF(EP,1,J) + C2*(BM(EP,1)*FF(EP,1,J)+
     1                                  BM(EP,2)*FF(EP,2,J)+
     3                                  BC(EP,1)*BCX+
     4                                  BC(EP,2)*BCY )
c     5                          +C1*(BMF(EP,1)*MM(1,EP)+
c     6                               BMF(EP,2)*MM(2,EP)+
c     7                               BMF(EP,3)*MM(3,EP))

	  VF(EP,2,J) = VF(EP,2,J  )+ C2*(BM(EP,4)*FF(EP,1,J)+
     2                                   BM(EP,5)*FF(EP,2,J)+
     4                                   BC(EP,3)*BCX+
     5                                   BC(EP,4)*BCY )
c     6                         +C1*(BMF(EP,4,EP)*MM(1,EP)+
c     7                              BMF(EP,5,EP)*MM(2,EP)+
c     8                              BMF(EP,6,EP)*MM(3,EP))

C          I1=3 I2=3
	  VF(EP,3,J)=VF(EP,3,J)+ C2*(BM(EP,7)*FF(EP,1,J)+
     2                               BM(EP,8)*FF(EP,2,J)+
     4                               BC(EP,5)*BCX+
     5                               BC(EP,6)*BCY )
c     6                         +C1*(BMF(EP,7)*MM(1,EP)+
c     7                              BMF(EP,8)*MM(2,EP)+
c     8                              BMF(EP,9)*MM(3,EP))
C	 J=2 K0=2 K1=3 K2=5 I1=4 I2=6
	  VF(EP,4,J) = VF(EP,4,J)+C2*(BM(EP,10)*FF(EP,1,J)+
     2                                BM(EP,11)*FF(EP,2,J)+
     4                                BC(EP,11)*BCX+
     5                                BC(EP,12)*BCY )
c     6                         +C1*(BMF(EP,10)*MM(1,EP)+
c     7                              BMF(EP,11)*MM(2,EP)+
c     8                              BMF(EP,12)*MM(3,EP))

C          I1=5 I2=7
	  VF(EP,5,J)=VF(EP,5,J)+C2*(BM(EP,13)*FF(EP,1,J)+
     2                              BM(EP,14)*FF(EP,2,J)+
     4                              BC(EP,13)*BCX+
     5                              BC(EP,14)*BCY )
c     6                         +C1*(BMF(EP,13)*MM(1,EP)+
c     7                              BMF(EP,14)*MM(2,EP)+
c     8                              BMF(EP,15)*MM(3,EP))
C          I1=6 I2=8
	  VF(EP,6,J)=VF(EP,6,J)+C2*(BM(EP,16)*FF(EP,1,J)+
     2                              BM(EP,17)*FF(EP,2,J)+
     4                              BC(EP,15)*BCX+
     5                              BC(EP,16)*BCY )
c     6                         +C1*(BMF(EP,16)*MM(1,EP)+
c     7                              BMF(EP,17)*MM(2,EP)+
c     8                              BMF(EP,18)*MM(3,EP))
C	 J=3 K0=4  K1=6 K2=10 I1=7 I2=11
	  VF(EP,7,J)=VF(EP,7,J)+C2*(BM(EP,19)*FF(EP,1,J)+
     2                              BM(EP,20)*FF(EP,2,J)+
     4                              BC(EP,21)*BCX+
     5                              BC(EP,22)*BCY )
c     6                         +C1*(BMF(EP,19)*MM(1,EP)+
c     7                              BMF(EP,20)*MM(2,EP)+
c     8                              BMF(EP,21)*MM(3,EP))
C          I1=8  I2=12
	  VF(EP,8,J)=VF(EP,8,J)+C2*(BM(EP,22)*FF(EP,1,J)+
     2                              BM(EP,23)*FF(EP,2,J)+
     4                              BC(EP,23)*BCX+
     5                              BC(EP,24)*BCY )
c     6                         +C1*(BMF(EP,22)*MM(1,EP)+
c     7                              BMF(EP,23)*MM(2,EP)+
c     8                              BMF(EP,24)*MM(3,EP))
C          I1=9 I2=13
	  VF(EP,9,J)=VF(EP,9,J)+C2*(BM(EP,25)*FF(EP,1,J)+
     2                              BM(EP,26)*FF(EP,2,J)+
     4                              BC(EP,25)*BCX+
     5                              BC(EP,26)*BCY )
cc     6                         +C1*(BMF(EP,25)*MM(1,EP)+
c     7                              BMF(EP,26)*MM(2,EP)+
c     8                              BMF(EP,27)*MM(3,EP))
C	 J=4  K0=6 K1=9 K2=15 I=1 I1=10 I2=16
	  VF(EP,10,J)=VF(EP,10,J)+C2*(BM(EP,28)*FF(EP,1,J)+
     2                                BM(EP,29)*FF(EP,2,J)+
     4                                BC(EP,31)*BCX+
     5                                BC(EP,32)*BCY )
c     6                         +C1*(BMF(EP,28)*MM(1,EP)+
c     7                              BMF(EP,29)*MM(2,EP)+
c     8                              BMF(EP,30)*MM(3,EP))
C          I1=11  I2=17
	  VF(EP,11,J)=VF(EP,11,J)+C2*(BM(EP,31)*FF(EP,1,J)+
     2                                BM(EP,32)*FF(EP,2,J)+
     4                                BC(EP,33)*BCX+
     5                                BC(EP,34)*BCY )
c     6                         +C1*(BMF(EP,31)*MM(1,EP)+
c     7                              BMF(EP,32)*MM(2,EP)+
c     8                              BMF(EP,33)*MM(3,EP))
C          I1=12 I2=18
	  VF(EP,12,J)=VF(EP,12,J)+C2*(BM(EP,34)*FF(EP,1,J)+
     2                                BM(EP,35)*FF(EP,2,J)+
     4                                BC(EP,35)*BCX+
     5                                BC(EP,36)*BCY )
c     6                         +C1*(BMF(EP,34)*MM(1,EP)+
c     7                              BMF(EP,35)*MM(2,EP)+
c     8                              BMF(EP,36)*MM(3,EP))
C 
       ENDDO 
      ENDDO   
C
C  interface
       DO IPLY = 1 , NPT - 1 
        JINF = IPLY  
        JSUP = IPLY + 1 
        DO EP=JFT,JLT 
C         
         FL1 = CDET(EP)*SIG_IPLY(EP,2,IPLY)
         FL2 = CDET(EP)*SIG_IPLY(EP,1,IPLY)
         FL3 = CDET(EP)*SIG_IPLY(EP,3,IPLY)
C----------NOEUD 1 ------------
         VFI(EP,1,JSUP) = VFI(EP,1,JSUP) + FL1*VNI(1,NG) 
         VFI(EP,2,JSUP) = VFI(EP,2,JSUP) + FL2*VNI(1,NG)
         VFI(EP,3,JSUP) = VFI(EP,3,JSUP) + FL3*VNI(1,NG) 
C ----------NOEUD 2 ------------        
         VFI(EP,4,JSUP) = VFI(EP,4,JSUP) + FL1*VNI(2,NG) 
         VFI(EP,5,JSUP) = VFI(EP,5,JSUP) + FL2*VNI(2,NG)
         VFI(EP,6,JSUP) = VFI(EP,6,JSUP) + FL3*VNI(2,NG) 
C----------NOEUD 3 ------------
         VFI(EP,7,JSUP) = VFI(EP,7,JSUP) + FL1*VNI(3,NG) 
         VFI(EP,8,JSUP) = VFI(EP,8,JSUP) + FL2*VNI(3,NG)
         VFI(EP,9,JSUP) = VFI(EP,9,JSUP) + FL3*VNI(3,NG)         
C----------NOEUD 4 ------------
         VFI(EP,10,JSUP) = VFI(EP,10,JSUP) + FL1*VNI(4,NG) 
         VFI(EP,11,JSUP) = VFI(EP,11,JSUP) + FL2*VNI(4,NG)
         VFI(EP,12,JSUP) = VFI(EP,12,JSUP) + FL3*VNI(4,NG)
cc -----------ply inf
C----------NOEUD 1 ------------
         VFI(EP,1,JINF) = VFI(EP,1,JINF) - FL1*VNI(1,NG) 
         VFI(EP,2,JINF) = VFI(EP,2,JINF) - FL2*VNI(1,NG)
         VFI(EP,3,JINF) = VFI(EP,3,JINF) - FL3*VNI(1,NG) 
C ----------NOEUD 2 ------------        
         VFI(EP,4,JINF) = VFI(EP,4,JINF) - FL1*VNI(2,NG) 
         VFI(EP,5,JINF) = VFI(EP,5,JINF) - FL2*VNI(2,NG)
         VFI(EP,6,JINF) = VFI(EP,6,JINF) - FL3*VNI(2,NG) 
C----------NOEUD 3 ------------
         VFI(EP,7,JINF) = VFI(EP,7,JINF) - FL1*VNI(3,NG) 
         VFI(EP,8,JINF) = VFI(EP,8,JINF) - FL2*VNI(3,NG)
         VFI(EP,9,JINF) = VFI(EP,9,JINF) - FL3*VNI(3,NG)         
C----------NOEUD 4 ------------
         VFI(EP,10,JINF) = VFI(EP,10,JINF) - FL1*VNI(4,NG) 
         VFI(EP,11,JINF) = VFI(EP,11,JINF) - FL2*VNI(4,NG)
         VFI(EP,12,JINF) = VFI(EP,12,JINF) - FL3*VNI(4,NG)
        ENDDO 
       ENDDO     
      RETURN
      END
