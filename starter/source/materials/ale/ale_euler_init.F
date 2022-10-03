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
Chd|  ALE_EULER_INIT                  source/materials/ale/ale_euler_init.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ALE_EULER_INIT(MLAW_TAG,IPM,PM )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE ELBUFTAG_MOD    
C============================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr06_c.inc"
#include      "scr17_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(MLAW_TAG_), DIMENSION(NUMMAT), INTENT(INOUT)  :: MLAW_TAG
      INTEGER, DIMENSION(NPROPMI,NUMMAT), INTENT(IN) :: IPM
      my_real, DIMENSION(NPROPM,NUMMAT), INTENT(INOUT) :: PM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II,ILAW,uID1,uID2,MID1,MID2,ILAW1,ILAW2,JALE,JTUR,IMAT,MAT_ID
      INTEGER,EXTERNAL :: NINTRI   
      CHARACTER TITR*nchartitle             
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      
      
c-----------------------------------------      
c     SOUND SPEED BUFFER  
c-----------------------------------------  
      DO II=1,NUMMAT-1   
        ILAW = IPM(2,II)          
        JALE = NINT(PM(72,II))          
        IF(JALE /=0 )THEN               
          IF(ILAW==1 .OR.
     .       ILAW==2 .OR.
     .       ILAW==3 .OR.
     .       ILAW==4 .OR.
     .       ILAW==5 .OR.
     .       ILAW==6 .OR.
     .       ILAW==7 .OR.
     .       ILAW==8 .OR.
     .       ILAW==9 .OR.
     .       ILAW==10 .OR.
     .       ILAW==11 .OR.
     .       ILAW==13 .OR.
     .       ILAW==16 .OR.
     .       ILAW==17 .OR.
     .       ILAW==18 .OR.
     .       ILAW==20 .OR.
     .       ILAW==21 .OR.
     .       ILAW==22 .OR.
     .       ILAW==23 .OR.
     .       ILAW==26 .OR.
     .       ILAW==29 .OR.
     .       ILAW==30 .OR.
     .       ILAW==31 .OR.
     .       ILAW==36 .OR.
     .       ILAW==37 .OR.
     .       ILAW==41 .OR.
     .       ILAW==44 .OR.
     .       ILAW==46 .OR.
     .       ILAW==47 .OR.
     .       ILAW==49 .OR.
     .       ILAW>=50     )THEN  
        
               MLAW_TAG(II)%L_SSP    = 1 
               
               IF(ILAW==20)THEN
                 uID1                  = NINT(PM(21,II))
                 uID2                  = NINT(PM(22,II))
                 MID1                  = NINTRI(uID1,IPM,NPROPMI,NUMMAT,1)
                 MID2                  = NINTRI(uID2,IPM,NPROPMI,NUMMAT,1)
                 ILAW1                 = IPM(2,MID1)
                 ILAW2                 = IPM(2,MID2)               
                 MLAW_TAG(MID1)%L_SSP = 1 ! boundary layer material (ilaw updated later in sgrtail.F)
                 MLAW_TAG(MID2)%L_SSP = 1 ! boundary layer material (ilaw updated later in sgrtail.F) 
               ENDIF  
                            
          ENDIF                        
        ENDIF 
      ENDDO!next I
      
c-----------------------------------------      
c     MULTIMAT : ERROR IF LAGRANGIAN     
c-----------------------------------------      
      DO IMAT=1,NUMMAT-1       
        ILAW = IPM(2,IMAT)
       	JALE = NINT(PM(72,IMAT))   	
	IF (JALE == 0) THEN
          IF (ILAW == 20 .OR. ILAW == 37 .OR. ILAW == 51 .OR. ILAW == 151) THEN
            CALL FRETITL2(TITR,IPM(NPROPMI-LTITR+1,IMAT),LTITR)
            MAT_ID = IPM(1,IMAT)
            CALL ANCMSG(MSGID=101, MSGTYPE=MSGERROR, ANMODE=ANINFO, I1=MAT_ID, C1=TITR, I2=ILAW)
          ENDIF
        ENDIF  
      ENDDO  
      
c-----------------------------------------      
c     GLOBAL FLAGS 
c-----------------------------------------          
      DO IMAT=1,NUMMAT-1                                    
        ILAW = IPM(2,IMAT)                                                 
        JALE = NINT(PM(72,IMAT))          
        IF (JALE == 0 .AND. ILAW/=18 .AND. ILAW/=11) THEN  
          ILAG=1                                          
        ELSEIF(JALE == 1)THEN                            
          IALE=1                                          
        ELSEIF(JALE == 2)THEN                            
          IEULER=1  
        ELSEIF(JALE == 3) THEN
          ILAG=1                                       
        ENDIF   
      ENDDO  
      
c-----------------------------------------      
c     UPWIND 
c-----------------------------------------          
      DO IMAT=1,NUMMAT-1                                                                                     
        JALE = NINT(PM(72,IMAT))  
        IF (JALE /= 0) THEN                                
          PM(15,IMAT) = UPWMG                                 
          PM(16,IMAT) = UPWOG                                 
        ENDIF
      ENDDO          

c-----------------------------------------      
c     TURBULENCY 
c-----------------------------------------          
      DO IMAT=1,NUMMAT-1                                    
        ILAW = IPM(2,IMAT)                          
        JTUR = NINT(PM(70,IMAT))
        IF (ILAW /= 50) ITURB = MAX(ITURB ,JTUR)
      ENDDO  
      
c-----------------------------------------               
      RETURN
      END SUBROUTINE ALE_EULER_INIT
