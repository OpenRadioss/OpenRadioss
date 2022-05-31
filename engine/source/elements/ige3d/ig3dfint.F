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
Chd|  IG3DFINT                      source/elements/ige3d/ig3dfint.F
Chd|-- called by -----------
Chd|        IG3DUFORC3                    source/elements/ige3d/ig3duforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IG3DFINT(PM, MXT, KXIG3D, SIG, NCTRL, MATB, FX, FY, FZ, 
     1                    VOL, BtDBA, DBA, SSP_EQ,STIG,NEL)
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
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "nsvis_c.inc"
#include      "fluid_scr_c.inc"
#include      "param_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NCTRL, KXIG3D(NIXIG3D,*), MXT(*),NEL
      my_real
     .   SIG(NEL,6), VOL(*), PM(NPROPM,*)
      my_real,
     .  DIMENSION(3*NCTRL,MVSIZ) :: MATB
      my_real
     .  FX(MVSIZ,*),FY(MVSIZ,*),FZ(MVSIZ,*),BtDBA(3*NCTRL,MVSIZ),
     .  DBA(6,MVSIZ),STIG(MVSIZ,*),SSP_EQ(*),SUMX,SUMY,SUMZ
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, INCTRL
      my_real
     .   AA,WIP(MVSIZ),STIN(MVSIZ,NCTRL)
C-----------------------------------------------
C            _
C           /   t
C     F = _/ [B]  sig dvol
C
C                   NEL
C         /--------------------\           
C   [B] = [dNi/dx  dNk/dx  ..  ] \  3
C         [dNi/dy  dNk/dy  ..  ] |  *
C         [dNi/dz  dNk/dz  ..  ] |  N
C         [dNj/dx  dNl/dx  ..  ] |  C
C         [dNj/dy  dNl/dy  ..  ] |  T
C         [dNj/dz  dNl/dz  ..  ] |  R
C         [  ..      ..    ..  ] /  L
C
      DO I=LFT,LLT
       SUMX=ZERO 
       SUMY=ZERO
       SUMZ=ZERO
       DO J=1,NCTRL
        IF( J <= KXIG3D(3,I+NFT) ) THEN
         INCTRL = (J-1)*3
         SUMX=SUMX+ABS(MATB(INCTRL+1,I))
         SUMY=SUMY+ABS(MATB(INCTRL+2,I))
         SUMZ=SUMZ+ABS(MATB(INCTRL+3,I))
        ENDIF
       ENDDO
       DO J=1,NCTRL
        IF( J <= KXIG3D(3,I+NFT) ) THEN
         INCTRL = (J-1)*3
         FX(I,J)=FX(I,J)-VOL(I)*(MATB(INCTRL+1,I)*SIG(I,1)
     +                         + MATB(INCTRL+2,I)*SIG(I,4)  
     +                         + MATB(INCTRL+3,I)*SIG(I,6))

         FY(I,J)=FY(I,J)-VOL(I)*(MATB(INCTRL+1,I)*SIG(I,4)
     +                         + MATB(INCTRL+2,I)*SIG(I,2)
     +                         + MATB(INCTRL+3,I)*SIG(I,5))

         FZ(I,J)=FZ(I,J)-VOL(I)*(MATB(INCTRL+1,I)*SIG(I,6)
     +                         + MATB(INCTRL+2,I)*SIG(I,5) 
     +                         + MATB(INCTRL+3,I)*SIG(I,3))
         STIN(I,J)= HALF*VOL(I)*
     .      (ABS(MATB(INCTRL+1,I))*SUMX 
     .     + ABS(MATB(INCTRL+2,I))*SUMY 
     .     + ABS(MATB(INCTRL+3,I))*SUMZ)
         AA=PM(89,MXT(I))*SSP_EQ(I)*SSP_EQ(I)
         STIG(I,J)=STIG(I,J)+STIN(I,J)*AA
        ENDIF
       ENDDO
      ENDDO


      RETURN
      END

Chd|====================================================================
Chd|  IG3DAVERAGE                   source/elements/ige3d/ig3dfint.F
Chd|-- called by -----------
Chd|        IG3DUFORC3                    source/elements/ige3d/ig3duforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IG3DAVERAGE(SIG  ,SIGG  ,VOL0 ,VOL0G,RHO  ,
     2                       EINT ,EINTG ,RHOG ,VOL  ,VOLG ,
     3                       EPLAS,EPLASG,G_PLA,EPSD ,EPSDG,
     4                       NEL  )
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
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "nsvis_c.inc"
#include      "fluid_scr_c.inc"
#include      "param_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER G_PLA,NEL
      my_real
     .   SIG(NEL,6), SIGG(NEL,6), VOL(*), VOLG(*) , VOL0(*),
     .   VOL0G(*), EINT(*)  , RHO(*), EINTG(*), RHOG(*),
     .   EPLAS(*), EPLASG(*), EPSD(*),EPSDG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   WIP(MVSIZ)
C-----------------------------------------------
c
      DO I=LFT,LLT  
       WIP(I) = VOL(I)/VOLG(I)                                       
       SIGG(I,1) = SIGG(I,1) + WIP(I) * SIG(I,1)		     
       SIGG(I,2) = SIGG(I,2) + WIP(I) * SIG(I,2)		     
       SIGG(I,3) = SIGG(I,3) + WIP(I) * SIG(I,3)		     
       SIGG(I,4) = SIGG(I,4) + WIP(I) * SIG(I,4)		     
       SIGG(I,5) = SIGG(I,5) + WIP(I) * SIG(I,5)		     
       SIGG(I,6) = SIGG(I,6) + WIP(I) * SIG(I,6)		     
       RHOG(I)   = RHOG(I)   + WIP(I) * RHO(I)                       
       EINTG(I)  = EINTG(I)  + EINT(I) * VOL0(I)/VOL0G(I)
      ENDDO
      IF (ISRAT > 0) THEN                                                    
        DO I=LFT,LLT                                                         
          EPSDG(I) = EPSDG(I) + WIP(I) * EPSD(I)                                
        ENDDO                                                                
      ENDIF 
      IF (G_PLA > 0) THEN                                                    
        DO I=LFT,LLT                                                          
          EPLASG(I)  = EPLASG(I)  + WIP(I) * EPLAS(I)                              
        ENDDO                                                                 
      ENDIF
c
C-----------
      RETURN
      END
