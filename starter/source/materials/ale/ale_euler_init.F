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
Chd|  ALE_EULER_INIT                source/materials/ale/ale_euler_init.F
Chd|-- called by -----------
Chd|        HM_READ_PART                  source/model/assembling/hm_read_part.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        NINTRI                        source/system/nintrr.F        
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ALE_EULER_INIT(MLAW_TAG,IPM,PM, IGEO,TITR,TITR1,TITR2,IGTYP,ID,ILAW,MID,IMID,PID,IPID,JALE_FROM_PROP,JALE_FROM_MAT)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine is setting global parameter for ALE/EULER framework
C    %L_SSP           : buffer size to allocate sound speed in element buffer
C    UPWMG            : global parameter for 'M'omentum convection : upwind
C    UPWOG            : global parameter for 'O'ther convections (energy, mass) : upwind
C    IALE,IEULER,ILAG : global flag to detect if ALE, EULER, or none of them was defined in the input file
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE ELBUFTAG_MOD
      USE ALEFVM_MOD , only:ALEFVM_Param
C-----------------------------------------------
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
#include      "conv_c.inc"
#include      "fluid_scr_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(MLAW_TAG_), DIMENSION(NUMMAT), INTENT(INOUT)  :: MLAW_TAG
      CHARACTER*nchartitle,INTENT(IN) :: TITR,TITR1,TITR2
      INTEGER,INTENT(INOUT)::IGEO(NPROPGI,NUMGEO)      
      INTEGER,INTENT(IN) :: ID,IMID,PID,IPID,IGTYP,ILAW
      INTEGER,INTENT(INOUT) :: MID,JALE_FROM_PROP,JALE_FROM_MAT
      INTEGER, DIMENSION(NPROPMI,NUMMAT), INTENT(INOUT) :: IPM
      my_real, DIMENSION(NPROPM,NUMMAT), INTENT(INOUT) :: PM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II,uID1,uID2,MID1,MID2,ILAW1,ILAW2,JALE,JTUR,IMAT,MAT_ID,IGFLU
      INTEGER JTHE,STAT,CODCONV,CODREZO,IEXPAN
      INTEGER,EXTERNAL :: NINTRI   
      CHARACTER*5 CHAR_PROP,CHAR_MAT   
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      LOGICAL,EXTERNAL :: LOI_FLUID                 
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
        
        !################################################################!
        !  EULER & ALE : CHECK CONSISTENCY BETWEEN MAT and PROP          !
        !################################################################!  
        ! JALE_FROM_MAT = PM(72)        0:lagrange   1:ale    2:euler 
        ! JALE_FROM_PROP = IGEO(62)     0:lagrange   1:ale    2:euler
        
        !/ALE/MAT or /EULER/MAT
        JALE_FROM_MAT = 0
        IF(IMID > 0)THEN
          JALE_FROM_MAT = PM(72,IMID)
          !character string for possible error message
          IF(JALE_FROM_MAT == 1)THEN
            CHAR_MAT = 'ALE  '
          ELSEIF(JALE_FROM_MAT == 2)THEN
            CHAR_MAT = 'EULER'
          ENDIF
        ELSE
          JALE_FROM_MAT = 0
        ENDIF
        
        !/PROP/SOLID
        JALE_FROM_PROP = 0
        !/PROP_TYPE14 (IALE_FLAG)
        IF(IPID > 0)THEN
          JALE_FROM_PROP = IGEO(62,IPID)
          !character string for possible error message          
          IF(JALE_FROM_PROP == 1)THEN
            CHAR_PROP = 'ALE  '
          ELSEIF(JALE_FROM_PROP == 2)THEN
            CHAR_PROP = 'EULER'
          ENDIF          
        ELSE
          JALE_FROM_PROP = 0
        ENDIF
               
        !display error message if MATERIAL and PROPERTY have inconsistent definitions     
        IF(JALE_FROM_MAT > 0 .AND. JALE_FROM_PROP > 0)THEN
         IF(ILAW==77)THEN
             !law77 is not compatible with ALE or EULER framework
             CALL ANCMSG(MSGID=1120,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND_1,I1=MID,C1=TITR2 )                        
         ELSE
           IF(JALE_FROM_MAT /= JALE_FROM_PROP)THEN
             CALL ANCMSG(MSGID=130,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND_1,
     .                   I1=ID, C1=TITR,
     .                   I2=PID,C2=TITR1,C3=CHAR_PROP,
     .                   I3=MID,C4=TITR2,C5=CHAR_MAT )      
           ENDIF
         ENDIF
        ENDIF        
        
        !################################################################!   !(14)'SOLID'     
        !  EULERIAN AND ALE CASES : ALLOWS ONLY IGTYP=14 & IGTYP=15      !   !(14)'FLUID'
        !################################################################!   !(15)'POROUS'                
        IF(JALE_FROM_MAT > 0 .OR. JALE_FROM_PROP > 0)THEN
           IF (ILAW  ==  151 .AND. N2D /= 0) THEN
              !Allow tria for 2D law 151
              IF(IGTYP/=14.AND.IGTYP/=15.AND.IGTYP/=1)THEN  
                 CALL ANCMSG(MSGID=42,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID) 
                 MID=0 
                 PM(1:NPROPM,IMID) =  ZERO
                 IPM(1:NPROPMI,IMID) = 0                  
              ENDIF
           ELSE
              IF(IGTYP/=14.AND.IGTYP/=15)THEN  
                 !material set to void for normal termination otherwise engine will set value to non allocated arrays.
                 CALL ANCMSG(MSGID=42,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=ID) 
                 !material set to void for normal termination otherwise engine will set value to non allocated arrays.
                 MID=0 
                 PM(1:NPROPM,IMID) =  ZERO
                 IPM(1:NPROPMI,IMID) = 0                  
              ENDIF
           ENDIF
        ENDIF        
                
        !################################################################!
        !  MULTIMATERIAL CANNOT BE DEFINED IN LAGRANGIAN FRAMEWORK       !
        !################################################################!  
        !display error message if MATERIAL 20,37,51,151 are set with lagrangian framework
        IF(JALE_FROM_MAT == 0 .AND. JALE_FROM_PROP == 0)THEN
          IF(ILAW==20 .OR. ILAW==37 .OR. ILAW==51 .OR. ILAW==151)THEN
            CALL FRETITL2(TITR1,IGEO(NPROPGI-LTITR+1,IPID),LTITR)
            CALL FRETITL2(TITR2,IPM(NPROPMI-LTITR+1,IMID),LTITR)
            CALL ANCMSG(MSGID=101, MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .                  I1=ID, C1=TITR,
     .                  I2=PID,C2=TITR1,
     .                  I3=MID,C3=TITR2, 
     .                  I4=ILAW)          
          ENDIF
        ENDIF

        !################################################################!
        !  /HEAT/MAT : thermal global flag  ITHEM & ITHERM_FE            !
        !################################################################!  
        ! JALE_FROM_MAT = PM(72)        0:lagrange   1:ale    2:euler 
        ! JALE_FROM_PROP = IGEO(62)     0:lagrange   1:ale    2:euler 
        ! ITHERM : 1 => there are elements which require temperature at centroids
        ! ITHERM_FE : 1 => there are elements which require for temperature at nodes
        JTHE = NINT(PM(71,IMID))
        IF( JTHE > 0 )THEN
          IF(JALE_FROM_PROP>0 .OR. JALE_FROM_MAT>0)THEN
            ITHERM = 1
          ELSE
            ITHERM_FE = 1
          ENDIF
        ENDIF
        
        !################################################################!
        !  /THERM/STRESS                                                 !
        !################################################################!  
        IEXPAN=IPM(218,IMID)
        IF(IEXPAN > 0)THEN
          IF(JALE_FROM_PROP>0 .OR. JALE_FROM_MAT>0)THEN
             CALL ANCMSG(MSGID=1723,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=IMAT,C1=TITR)    
          ENDIF       
        ENDIF
        
        !################################################################!
        !  SOUND SPEED BUFFER  
        !################################################################!       
        IF(JALE_FROM_PROP > 0 .OR. JALE_FROM_MAT > 0)THEN               
          MLAW_TAG(IMID)%L_SSP = 1 
          IF(ILAW == 20)THEN
            uID1 = NINT(PM(21,IMID))
            uID2 = NINT(PM(22,IMID))
            MID1 = NINTRI(uID1,IPM,NPROPMI,NUMMAT,1)
            MID2 = NINTRI(uID2,IPM,NPROPMI,NUMMAT,1)
            ILAW1 = IPM(2,MID1)
            ILAW2 = IPM(2,MID2)                        
            ILAW2 = IPM(2,MID2)   
            PM(15,MID1) = UPWMG
            PM(15,MID2) = UPWMG
            PM(16,MID1) = UPWOG
            PM(16,MID2) = UPWOG                        
            MLAW_TAG(MID1)%L_SSP = 1 ! boundary layer material (ilaw updated later in sgrtail.F)
            MLAW_TAG(MID2)%L_SSP = 1 ! boundary layer material (ilaw updated later in sgrtail.F) 
          ENDIF                                                    
        ENDIF 
        
        !################################################################!     
        !  SPECIFIC TREATMENT LAW11 (BOUNDARY MATERIAL)
        !################################################################!                
        IF(JALE_FROM_PROP > 0)THEN
          IF(ILAW == 11)THEN
            IF(PM(92,IMID) == ZERO)THEN
              PM(92,IMID) = ONE
            ENDIF
          ENDIF
        ENDIF  
      
        !################################################################!     
        !  GLOBAL FLAGS 
        !################################################################!                
        IF ((JALE_FROM_PROP == 0 .AND. JALE_FROM_MAT == 0).AND. ILAW/=18 .AND. ILAW/=11) THEN  
          ILAG=1                                          
        ELSEIF(JALE_FROM_PROP == 1 .OR. JALE_FROM_MAT == 1)THEN                            
          IALE=1                                          
        ELSEIF(JALE_FROM_PROP == 2 .OR. JALE_FROM_MAT == 2)THEN                            
          IEULER=1  
        ELSE
          ILAG=1                                       
        ENDIF   
              
        !################################################################!     
        !  UPWIND  (backward compatibility)
        !################################################################!         
        IF (JALE_FROM_PROP /= 0 .OR. JALE_FROM_MAT /= 0) THEN                                
          PM(15,IMID) = UPWMG                                 
          PM(16,IMID) = UPWOG                                 
        ENDIF    

        !################################################################!      
        !  TURBULENCY 
        !################################################################!                                                            
        JTUR = NINT(PM(70,IMID))
        IF (ILAW /= 50) ITURB = MAX(ITURB ,JTUR)

        !################################################################!      
        !  CONVERTION-REZONING CODES/FLAGS (Reynolds transport theorem)
        !################################################################!-     
        CODCONV=0  !convection : local flags related to current material law (CODV are global values)
        CODREZO=0  !rezoning   : local flags
      
        !default convection
        IF(JALE_FROM_PROP /= 0 .OR. JALE_FROM_MAT /= 0)THEN
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
                !local flags
                CODCONV = CODCONV + 11
                !global flags
                CODV(1) = 01
                CODV(2) = 01
          ENDIF
        ENDIF
      
        !turbulency
        IF(JTUR > 0) THEN
          !local flags
          CODCONV = CODCONV + 1100
          !global flags
          CODV(3)=1
          CODV(4)=1
        ENDIF
        
        !specific convection (massic fraction, and lee-tarver parameter)
        IF(ILAW == 37 .OR. ILAW == 41)THEN
          !local flags
          CODCONV = CODCONV + 10000
          !global flags
          CODV(5)=1
        ENDIF
      
        !default rezoning(remapping) for deviatoric stress
        IF( (JALE_FROM_PROP /= 0 .OR. JALE_FROM_MAT /= 0) .AND. (
     .     ILAW == 2  .OR.
     .     ILAW == 3  .OR.
     .     ILAW == 4  .OR.
     .     ILAW == 7  .OR.
     .     ILAW == 8  .OR.
     .     ILAW == 9  .OR.
     .     ILAW == 10 .OR.
     .     ILAW == 16 .OR.
     .     ILAW == 21 .OR.
     .     ILAW == 22 .OR.
     .     ILAW == 23 .OR.
     .     ILAW == 26 .OR.
     .     ILAW == 28 .OR.
     .     ILAW == 29 .OR.
     .     ILAW == 30 .OR.
     .     ILAW == 31 .OR.
     .     ILAW == 36 .OR.
     .     ILAW == 44 .OR.
     .     ILAW == 49 .OR.
     .     ILAW == 97       )  )THEN
             CODREZO=11
         ENDIF
       
         IF( (JALE_FROM_PROP /= 0 .OR. JALE_FROM_MAT /= 0)  .AND. ILAW == 1)THEN
           CODREZO=1
         ENDIF
         
        !---ALEFVM (obsolete)---! 
        IF( (JALE_FROM_PROP /= 0 .OR. JALE_FROM_MAT /= 0) .AND. ALEFVM_Param%IEnabled == 1)THEN
         !Momentum Convection : %MOM(1:3,:)
          !local flags
          CODCONV = CODCONV + 11100000
          !global flags
          CODV(6) = 1
          CODV(7) = 1
          CODV(8) = 1                    
        ENDIF
               
        !storing values in material buffer for backward compatibility
        PM(10,IMID)=CODCONV+EM01
        PM(11,IMID)=CODREZO+EM01
        
       !################################################################!   
       !  CFD CONSISTENCY (OBSOLETE OPTION /CAA)                        !
       !################################################################!
       IGFLU=0
       IF(IPID > 0)IGFLU=IGEO(36,IPID)
       !### CAA without fluid material (CAA is obsolete)
       IF(ICAA /= 0)THEN
         IF(    JALE_FROM_PROP==1 .OR. JALE_FROM_PROP==2
     .     .OR. JALE_FROM_MAT==1  .OR. JALE_FROM_MAT==2 )THEN !ALE or EULER
           IF(.NOT.loi_fluid(ilaw)) THEN
             CALL ANCMSG(MSGID=37,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_1,
     .                   I1=ID  ,C1=TITR ,
     .                   I2=MID ,C2=TITR2,
     .                   I3=ILAW)
           ENDIF
         ENDIF
       ENDIF
       !### /PROP/FLUID without FLUID material 
       IF(  (IGFLU==1) .AND. .NOT.(loi_fluid(ilaw)) .AND. (ICAA == 0) .AND. (IGTYP == 14) )THEN
          CALL ANCMSG(MSGID=38,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND_1,
     .                I1=ID  ,C1=TITR ,
     .                I2=PID ,C2=TITR1,
     .                I3=MID ,C3=TITR2,
     .                I4=ILAW )
       ENDIF
      !-------------------------------------        
     

       !################################################################!   
       !  CFD CONSISTENCY (OBSOLETE OPTION /CAA)                        !
       !################################################################!
       IGFLU=0
       IF(IPID > 0)IGFLU=IGEO(36,IPID)
       !### CAA without fluid material (CAA is obsolete)
       IF(ICAA /= 0)THEN
         IF(    JALE_FROM_PROP==1 .OR. JALE_FROM_PROP==2
     .     .OR. JALE_FROM_MAT==1  .OR. JALE_FROM_MAT==2 )THEN !ALE or EULER
           IF(.NOT.loi_fluid(ilaw)) THEN
             CALL ANCMSG(MSGID=37,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_1,
     .                   I1=ID  ,C1=TITR ,
     .                   I2=MID ,C2=TITR2,
     .                   I3=ILAW)
           ENDIF
         ENDIF
       ENDIF
       !### /PROP/FLUID without FLUID material 
       IF(  (IGFLU==1) .AND. .NOT.(loi_fluid(ilaw)) .AND. (ICAA == 0) .AND. (IGTYP == 14) )THEN
          CALL ANCMSG(MSGID=38,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND_1,
     .                I1=ID  ,C1=TITR ,
     .                I2=PID ,C2=TITR1,
     .                I3=MID ,C3=TITR2,
     .                I4=ILAW )
       ENDIF
      !-------------------------------------
      
                
      RETURN
      END SUBROUTINE ALE_EULER_INIT
