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
Chd|  S20FINT3                      source/elements/solid/solide20/s20fint3.F
Chd|-- called by -----------
Chd|        S16FORC3                      source/elements/thickshell/solide16/s16forc3.F
Chd|        S20FORC3                      source/elements/solid/solide20/s20forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S20FINT3(NPE,SIG, PX, PY, PZ,SSP_EQ,
     2                    FX,FY,FZ,VOL,QVIS,STIG,STIN,
     3                    EINT,RHO,Q,EPLAS,EPSD,EPSDG,
     4                    SIGG,EINTG,RHOG,QG,EPLASG,WI,VOLG,
     5                    VOL0,VOL0G,G_PLA,NEL,CONDE,
     6                    DELTAX,CONDEG)
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
      INTEGER NPE,G_PLA,NEL
      my_real
     .  PX(MVSIZ,*),PY(MVSIZ,*),PZ(MVSIZ,*),SSP_EQ(*),
     .  FX(MVSIZ,*), FY(MVSIZ,*), FZ(MVSIZ,*),
     .  SIG(NEL,6),VOL(*),QVIS(*),EINT(*),RHO(*),Q(*),EPLASG(*),EPLAS(*),
     .  SIGG(NEL,6),EINTG(LLT),RHOG(*),QG(*),WI,STIG(MVSIZ,*),EPSD(*),
     .  EPSDG(*),STIN(MVSIZ,*),VOLG(*),VOL0(*),VOL0G(*),CONDE(MVSIZ),
     .  DELTAX(MVSIZ), CONDEG(MVSIZ,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "nsvis_c.inc"
#include      "scr01_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N
C     REAL
      my_real
     .   S1(MVSIZ),S2(MVSIZ),S3(MVSIZ),S4(MVSIZ),S5(MVSIZ),S6(MVSIZ)
      my_real
     .   FINT,AA,WIP(MVSIZ)
C=======================================================================
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
        QG(I)     = QG(I)     + WIP(I) * Q(I)                         
        S1(I)=(SIG(I,1)+SVIS(I,1)-QVIS(I))*VOL(I)		   
        S2(I)=(SIG(I,2)+SVIS(I,2)-QVIS(I))*VOL(I)		   
        S3(I)=(SIG(I,3)+SVIS(I,3)-QVIS(I))*VOL(I)		   
        S4(I)=(SIG(I,4)+SVIS(I,4))*VOL(I)			   
        S5(I)=(SIG(I,5)+SVIS(I,5))*VOL(I)			   
        S6(I)=(SIG(I,6)+SVIS(I,6))*VOL(I)			   
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
C
      DO N=1,NPE
        DO I=LFT,LLT
          STIN(I,N)= VOL(I)*
     .     (PX(I,N)*PX(I,N) + PY(I,N)*PY(I,N) + PZ(I,N)*PZ(I,N))
          AA=RHO(I)*SSP_EQ(I)*SSP_EQ(I)
          STIG(I,N)=STIG(I,N)+STIN(I,N)*AA
          FX(I,N)=FX(I,N)-(S1(I)*PX(I,N)+S4(I)*PY(I,N)+S6(I)*PZ(I,N))
          FY(I,N)=FY(I,N)-(S2(I)*PY(I,N)+S5(I)*PZ(I,N)+S4(I)*PX(I,N))
          FZ(I,N)=FZ(I,N)-(S3(I)*PZ(I,N)+S6(I)*PX(I,N)+S5(I)*PY(I,N))
        ENDDO
      ENDDO
      IF(NODADT_THERM == 1) THEN
        DO N=1,NPE
           DO I=LFT,LLT
              CONDE(I) = CONDE(I)*DELTAX(I)*DELTAX(I)/FOUR
              CONDEG(I,N)= CONDEG(I,N)+ CONDE(I)*
     .     (PX(I,N)*PX(I,N) + PY(I,N)*PY(I,N) + PZ(I,N)*PZ(I,N))
           ENDDO
        ENDDO
      ENDIF
C-----------
      RETURN
      END
