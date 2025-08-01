Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    hm_read_pfluid         ../starter/source/loads/general/pfluid/hm_read_pfluid.F
!||--- called by ------------------------------------------------------
!||    lectur                 ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    hm_get_floatv          ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_floatv_dim      ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
!||    hm_get_intv            ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_get_string          ../starter/source/devtools/hm_reader/hm_get_string.F
!||    hm_option_read_key     ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_option_start        ../starter/source/devtools/hm_reader/hm_option_start.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod     ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    r2r_mod                ../starter/share/modules1/r2r_mod.F
!||    submodel_mod           ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_PFLUID(NUMLOADP ,ILOADP  ,LLOADP ,FACLOADP ,NPC      ,
     .                          SENSORS  ,IGRSURF ,UNITAB ,IFRAME   ,LSUBMODEL)
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE R2R_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE SENSOR_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARFIELD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "r2r_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER NPC(SNPC),IFRAME(LISKN,NUMFRAM+1),NUMLOADP, ILOADP(SIZLOADP,NLOADP), LLOADP(SLLOADP)
      my_real FACLOADP(LFACLOAD,NLOADP)
C-----------------------------------------------
      TYPE (SURF_)   ,TARGET, DIMENSION(NSURF)   :: IGRSURF
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(NSUBMOD)
      TYPE (SENSORS_) ,INTENT(IN) :: SENSORS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real FCX,FCY,FAC_FCX,FAC_FCY,FCX1,FCY1,FCX2,FCY2
      INTEGER J,K, SUB_INDEX, SUB_ID, UID, ID, IFLAGUNIT,NN,IAD,TSENS,ISENS,IS,ISU,
     .        FUN_HSP,FUN_CX,FUN_VEL,IFRA1,IFRA2,ICFIELD8,ICFIELD12,ICFIELD9,ICFIELD13,
     .        NCUR_HSP,NCUR_VEL,NCUR_CX
      CHARACTER MESS*40,char_X*1, char_Y*1, char_Z*1, char_XX*2, char_YY*2, char_ZZ*2
      CHARACTER(LEN=NCHARFIELD) :: XYZ, XYZ1
      CHARACTER(LEN=NCHARTITLE) :: TITR
      LOGICAL IS_AVAILABLE

      DATA char_X/'X'/
      DATA char_Y/'Y'/
      DATA char_Z/'Z'/
      DATA char_XX/'XX'/
      DATA char_YY/'YY'/
      DATA char_ZZ/'ZZ'/
      DATA MESS/'FLUID PRESSURE LOAD DEFINITION          '/
C----------------------------------------------------------------------------------        
C   C o m m e n t s
C----------------------------------------------------------------------------------   
C                         /LOAD/PFLUID     /LOAD/PBLAST    
C
C     LLOADP(1,K)   :    K=1,NLOADP_F     K=1+NLOADP_F,NLOADP_F+NLOADP_B  :    . . .  SEGMENT STORAGE (surface)
C     ILOADP(1,K)   :    K=1,NLOADP_F     K=1+NLOADP_F,NLOADP_F+NLOADP_B  :    . . .  INTEGER STORAGE (option parameters)
C     FACLOADP(1,K) :    K=1,NLOADP_F     K=1+NLOADP_F,NLOADP_F+NLOADP_B  :    . . .  REAL STORAGE    (option parameters)     
C
C----------------------------------------------------------------------------------   
C                     /LOAD/PFLUID                                     /LOAD/PBLAST  
C                       K=1,NLOADP_F                                     K=1+NLOADP_F,NLOADP_F+NLOADP_B  
C----------------------------------------------------------------------------------        
C     ILOADP(1:SIZLOADP,K) : INTEGER STORAGE (option parameters)
C----------------------------------------------------------------------------------      
C                     K=1,NLOADP_F                                    K=1+NLOADP_F,NLOADP_F+NLOADP_B 
C
C     ILOADP(1,K) :   4*NN (4 * Nb of segments)                       4*NN 
C     ILOADP(2,K) :   ID                                              IS
C     ILOADP(3,K) :   -                                               -
C     ILOADP(4,K) :   IAD  Address of segments in LLOADP              IAD         
C     ILOADP(5,K) :   2                                               3         
C     ILOADP(6,K) :   Sensor id                                       IZ_UPDATE 
C     ILOADP(7,K) :   FCT_HSP function No                             IABAC
C     ILOADP(8,K) :   Dir_hsp (1, 2, 3)                               ID                              
C     ILOADP(9,K) :   frahsp id                                       ITA_SHIFT               
C     ILOADP(10,K):   FCT_CX function No                              NDT                             
C     ILOADP(11,K):   FCT_VEL function No                             IMODEL
C     ILOADP(12,K):   Dir_vel (1, 2, 3)                               -
C     ILOADP(13,K):   fravel id                                       -
C----------------------------------------------------------------------------------   
C                     /LOAD/PFLUID                                     /LOAD/PBLAST  
C                       K=1,NLOADP_F                                     K=1+NLOADP_F,NLOADP_F+NLOADP_B   
C----------------------------------------------------------------------------------   
C     LLOADP(1:SLLOADP,K) : SEGMENT STORAGE (surface)
C----------------------------------------------------------------------------------      
C                     K=1,NLOADP_F                                    K=1+NLOADP_F,NLOADP_F+NLOADP_B 
C    
C     LLOADP(1,K) :   NODE1                                           NODE1            
C     LLOADP(2,K) :   NODE2                                           NODE2            
C     LLOADP(3,K) :   NODE3                                           NODE3            
C     LLOADP(4,K) :   NODE4                                           NODE4             
C----------------------------------------------------------------------------------   
C                     /LOAD/PFLUID                                     /LOAD/PBLAST  
C                       K=1,NLOADP_F                                     K=1+NLOADP_F,NLOADP_F+NLOADP_B   
C----------------------------------------------------------------------------------        
C     FACLOADP(LFACLOAD,K) : REAL STORAGE    (option parameters) 
C----------------------------------------------------------------------------------      
C    
C     FACLOADP( 1,K) = Fscaley_hsp                                    TDET                                      
C     FACLOADP( 2,K) = ONE/Ascalex_hsp                                XDET     
C     FACLOADP( 3,K) = Fscaley_pc                                     YDET                                        
C     FACLOADP( 4,K) = ONE/Ascalex_pc                                 ZDET     
C     FACLOADP( 5,K) = Fscaley_vel                                    WTNT                                        
C     FACLOADP( 6,K) = ONE/Ascalex_vel                                P0_REF   
C     FACLOADP( 7,K) = -                                  ZERO ! TA_SHIFT 
C     FACLOADP( 8,K) = -                                              Nx_SURF
C     FACLOADP( 9,K) = -                                              Ny_SURF
C     FACLOADP(10,K) = -                                              Nz_SURF
C     FACLOADP(11,K) = -                                              HC
C     FACLOADP(12,K) = -                                              alpha_zc
C     FACLOADP(13,K) = -                                              TSTOP 
C----------------------------------------------------------------------------------        
C   S o u r c e   L i n e s
C----------------------------------------------------------------------------------        
      NN = 0 
C--------------------------------------------------
C START BROWSING MODEL /LOAD/PFLUID
C--------------------------------------------------
      CALL HM_OPTION_START('/LOAD/PFLUID')
C--------------------------------------------------
C BROWSING MODEL 1..NLOADP_F
C--------------------------------------------------
      DO K=1,NLOADP_F
C--------------------------------------------------
C EXTRACT DATAS OF /LOAD/PFLUID... LINE
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID = ID,
     .                          UNIT_ID = UID,
     .                          SUBMODEL_ID = SUB_ID,
     .                          SUBMODEL_INDEX = SUB_INDEX,
     .                          OPTION_TITR = TITR)

        IFLAGUNIT = 0
        DO J=1,UNITAB%NUNITS
          IF (UNITAB%UNIT_ID(J) == UID) THEN
            IFLAGUNIT = 1
            EXIT
          ENDIF
        ENDDO
        IF (UID /= 0.AND.IFLAGUNIT  == 0) THEN
          CALL ANCMSG(MSGID=659, ANMODE= ANINFO, MSGTYPE=MSGERROR,
     .                I2=UID,I1=ID,C1='FLUID PRESSURE LOAD',C2='FLUID PRESSURE LOAD',C3= TITR)  
        ENDIF
C-----------        
        ILOADP(2,K) = ID  !load identifier
C-----------        
        IAD = NUMLOADP + 1                                                      
        FUN_HSP = 0                                                                  
        FUN_CX = 0                                                                   
        FUN_VEL = 0                                                                  
        ICFIELD9 = 0                                                                 
        ICFIELD13 = 0          
        !Reader (based on cfg file)                                                      
        CALL HM_GET_INTV('surf_ID',ISU,IS_AVAILABLE,LSUBMODEL)                       
        CALL HM_GET_INTV('sens_ID',ISENS,IS_AVAILABLE,LSUBMODEL)                     
        !-----------  
        !get face nodes N1,N2,N3,N4                                                               
        IS=0                                                                         
        DO J=1,NSURF                                                                 
          IF (ISU  == IGRSURF(J)%ID) IS=J                                            
        ENDDO                                                                        
        IF(IS /= 0)THEN                                                              
          NN=IGRSURF(IS)%NSEG                                                        
          DO J=1,NN                                                                  
            LLOADP(IAD+4*(J-1))  =IGRSURF(IS)%NODES(J,1)                             
            LLOADP(IAD+4*(J-1)+1)=IGRSURF(IS)%NODES(J,2)                             
            LLOADP(IAD+4*(J-1)+2)=IGRSURF(IS)%NODES(J,3)                             
            IF(IGRSURF(IS)%ELTYP(J)  == 7)THEN                                       
             LLOADP(IAD+4*(J-1)+3)=0                                                 
            ELSE                                                                     
             LLOADP(IAD+4*(J-1)+3)=IGRSURF(IS)%NODES(J,4)                            
            ENDIF                                                                    
          ENDDO                                                                      
        ENDIF                                                                        
                 ILOADP(1,K)=4*NN                                                             
        !-----------  
        !possible Sensor                                                               
                 TSENS=0                                                                      
        DO J=1,SENSORS%NSENSOR                                                               
            IF(ISENS /= 0) THEN                                                
                      IF (ISENS == SENSORS%SENSOR_TAB(J)%SENS_ID) TSENS=J
            ENDIF                                                              
          ENDDO                                                                
          IF((TSENS == 0).AND.(ISENS /= 0))THEN                               
                  CALL ANCMSG(MSGID=930, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1, I1=ID, C1=TITR, I2=ISENS)                                                
          ENDIF                                                                
        !-----------  
        !Input file - Reader                                                               
        CALL HM_GET_INTV('fct_hsp',NCUR_HSP,IS_AVAILABLE,LSUBMODEL)                  
        CALL HM_GET_FLOATV('Ascalex_hsp',FCX,IS_AVAILABLE,LSUBMODEL,UNITAB)          
        CALL HM_GET_FLOATV_DIM('Ascalex_hsp',FAC_FCX,IS_AVAILABLE,LSUBMODEL,UNITAB)  
        CALL HM_GET_FLOATV('Fscaley_hsp',FCY,IS_AVAILABLE,LSUBMODEL,UNITAB)          
        CALL HM_GET_FLOATV_DIM('Fscaley_hsp',FAC_FCY,IS_AVAILABLE,LSUBMODEL,UNITAB)  
        IF (FCX == ZERO) FCX = FAC_FCX ! default value                                          
        IF (FCY == ZERO) FCY = FAC_FCY ! default value                                     
        !-----------   
        !check function identifier if provided                                                              
        IF(NCUR_HSP /= 0)THEN                                                      
          DO J=1,NFUNCT                                                              
            IF(NPC(NFUNCT+J+1)  == NCUR_HSP)FUN_HSP=J                                
          ENDDO                                                                      
          IF(FUN_HSP  == 0)THEN                                                      
            CALL ANCMSG(MSGID=929, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1, I1=ID, C1=TITR, I2=NCUR_HSP)                                                 
          ENDIF                                                                      
        ENDIF                                                                        
        !-----------   
        !Reader for XYZ field                                                             
        XYZ = ''                                                                     
        CALL HM_GET_STRING('Dir_hsp',XYZ,ncharfield,IS_AVAILABLE)                    
        !-----------                                                                 
        ICFIELD8 = 0                                                                 
        IF(XYZ(1:1)  == char_X)ICFIELD8=1                                            
        IF(XYZ(1:1)  == char_Y)ICFIELD8=2                                            
        IF(XYZ(1:1)  == char_Z)ICFIELD8=3    
        ! XYZ mandatory if hydrostatic pressure (HSP) expected                                        
        IF (NCUR_HSP /= 0)THEN                                                       
          IF(XYZ(1:1) /= char_X  .AND. XYZ(1:1) /= char_Y  .AND. XYZ(1:1) /= char_Z) THEN                                            
             CALL ANCMSG(MSGID=927, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1, I1=ID, C1=TITR, C2=XYZ)                                                     
          ENDIF                                                                      
        ENDIF                                                                        
                                                                                                                                                                         
        !-----------    
        !possible frame                                                             
        CALL HM_GET_INTV('frahsp_ID',IFRA1,IS_AVAILABLE,LSUBMODEL)                   
        !-----------                                                       
        IF(IFRA1 == 0 .AND. SUB_INDEX /= 0)THEN                                      
          CALL ANCMSG(MSGID=1712, ANMODE=ANINFO, MSGTYPE=MSGWARNING,                                          
     .                I1=ID, I2=SUB_ID, C1='/LOAD/PFLUID - frahsp_ID=0', C2=TITR)                                            
        ENDIF                                                                        
                                                                                     
        DO J=0,NUMFRAM                                                               
          IF(IFRA1  == IFRAME(4,J+1)) THEN                                           
            ICFIELD9=J+1                                                             
          ENDIF                                                                      
        ENDDO                                                                        
        IF(IFRA1 == 0) ICFIELD9 = 1                                                  
        IF (IFRA1  /= 0 .AND. ICFIELD9 == 0) THEN                                    
          CALL ANCMSG(MSGID=928, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1, I1=ID, C1=TITR, I2=IFRA1)                                                      
        ENDIF                                                                        
        !-----------    
        !Reader                                                             
        CALL HM_GET_INTV('fct_pc',NCUR_CX,IS_AVAILABLE,LSUBMODEL)                    
        CALL HM_GET_FLOATV('Ascalex_pc',FCX1,IS_AVAILABLE,LSUBMODEL,UNITAB)          
        CALL HM_GET_FLOATV_DIM('Ascalex_pc',FAC_FCX,IS_AVAILABLE,LSUBMODEL,UNITAB)   
        CALL HM_GET_FLOATV('Fscaley_pc',FCY1,IS_AVAILABLE,LSUBMODEL,UNITAB)          
        CALL HM_GET_FLOATV_DIM('Fscaley_pc',FAC_FCY,IS_AVAILABLE,LSUBMODEL,UNITAB)   
        IF (FCX1 == ZERO) FCX1 = FAC_FCX                                             
        IF (FCY1 == ZERO) FCY1 = FAC_FCY                                             
        !-----------    
        !check user function identifier                                                            
        IF (NCUR_CX /= 0)THEN                                                        
          DO J=1,NFUNCT                                                              
            IF(NPC(NFUNCT+J+1)  == NCUR_CX)FUN_CX=J                                  
          ENDDO                                                                      
          IF(FUN_CX  == 0)THEN                                                       
            CALL ANCMSG(MSGID=929, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1, I1=ID, C1=TITR, I2=NCUR_CX)                                                  
          ENDIF                                                                      
        ENDIF                                                                        
        !-----------   
        !reader                                                              
        CALL HM_GET_INTV('fct_vel',NCUR_VEL,IS_AVAILABLE,LSUBMODEL)                  
        CALL HM_GET_FLOATV('Ascalex_vel',FCX2,IS_AVAILABLE,LSUBMODEL,UNITAB)         
        CALL HM_GET_FLOATV_DIM('Ascalex_vel',FAC_FCX,IS_AVAILABLE,LSUBMODEL,UNITAB)  
        CALL HM_GET_FLOATV('Fscaley_vel',FCY2,IS_AVAILABLE,LSUBMODEL,UNITAB)         
        CALL HM_GET_FLOATV_DIM('Fscaley_vel',FAC_FCY,IS_AVAILABLE,LSUBMODEL,UNITAB)  
        IF (FCX2 == ZERO) FCX2 = FAC_FCX                                             
        IF (FCY2 == ZERO) FCY2 = FAC_FCY                                             
        !-----------  
        !check user function identifier                                                               
        IF (NCUR_VEL /= 0)THEN                                                       
          DO J=1,NFUNCT                                                              
            IF(NPC(NFUNCT+J+1)  == NCUR_VEL)FUN_VEL=J                                
          ENDDO                                                                      
          IF(FUN_VEL  == 0)THEN                                                      
            CALL ANCMSG(MSGID=929, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1, I1=ID, C1=TITR, I2=NCUR_VEL)                                                 
          ENDIF                                                                      
        ENDIF                                                                        
        !-----------                                                                 
        CALL HM_GET_STRING('Dir_vel',XYZ1,ncharfield,IS_AVAILABLE)                   
        !-----------                                                                 
        ICFIELD12 = 0                                                                
        IF(XYZ1(1:1)  == char_X)ICFIELD12=1                                          
        IF(XYZ1(1:1)  == char_Y)ICFIELD12=2                                          
        IF(XYZ1(1:1)  == char_Z)ICFIELD12=3                                          
        IF (NCUR_VEL /= 0)THEN                                                       
          IF(XYZ1(1:1) /= char_X  .AND. XYZ1(1:1) /= char_Y .AND. XYZ1(1:1)  /= char_Z) THEN                                           
             CALL ANCMSG(MSGID=927, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1, I1=ID, C1=TITR, C2=XYZ1)                                                    
          ENDIF                                                                      
        ENDIF                                                                        

        !-----------                                                                                      
        CALL HM_GET_INTV('fravel_ID',IFRA2,IS_AVAILABLE,LSUBMODEL)                   
        !-----------                                                                 
        IF(IFRA2 == 0 .AND. SUB_INDEX /= 0)THEN                                      
          CALL ANCMSG(MSGID=1712, ANMODE=ANINFO, MSGTYPE=MSGWARNING,                                          
     .                I1 = ID, I2 = SUB_ID, C1 = '/LOAD/PFLUID - fravel_ID=0', C2 = TITR)                                        
        ENDIF                                                                        
        DO J=0,NUMFRAM                                                               
          IF(IFRA2  == IFRAME(4,J+1)) THEN                                           
            ICFIELD13=J+1                                                            
          ENDIF                                                                      
        ENDDO                                                                        
        IF(IFRA2 == 0) ICFIELD13 = 1                                                 
        IF (IFRA2 /= 0  .AND. ICFIELD13 == 0) THEN                                   
          CALL ANCMSG(MSGID=928, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1, I1=ID, C1=TITR, I2=IFRA2)                                                      
        ENDIF                                                                        

        ILOADP(5,K)=2 !type of load 2:PFLUID
               
        !STORAGE : ILOADP (integer) - FACLOADP (real)       
        IF(IS /= 0)THEN  
          ILOADP  ( 4,K) = IAD                                                        
          ILOADP  ( 6,K) = ISENS                                                      
          ILOADP  ( 7,K) = FUN_HSP                                                    
          ILOADP  ( 8,K) = ICFIELD8                                                   
          ILOADP  ( 9,K) = ICFIELD9                                                   
          ILOADP  (10,K) = FUN_CX                                                     
          ILOADP  (11,K) = FUN_VEL                                                    
          ILOADP  (12,K) = ICFIELD12                                                  
          ILOADP  (13,K) = ICFIELD13                                                  

          FACLOADP( 1,K) = FCY                                                        
          FACLOADP( 2,K) = ONE/FCX                                                    
          FACLOADP( 3,K) = FCY1                                                       
          FACLOADP( 4,K) = ONE/FCX1                                                   
          FACLOADP( 5,K) = FCY2                                                       
          FACLOADP( 6,K) = ONE/FCX2                                                   
          !Multidomains -> numbering common segments (one single time)                
            IF (IDDOM > 0) NN = NN-ISURF_R2R(1,IS)                                      
        ENDIF    

        !OUTPUT STARTER LISTING FILE
        WRITE (IOUT,2002)                                                                                                
        WRITE (IOUT,'(I10,2X,I10,9X,A1,2X,I10,2X,I10,2X,1PG20.13,2X,1PG20.13)')ISU,IFRA1,XYZ(1:1),NCUR_HSP,ISENS,FCX,FCY 
        WRITE (IOUT,2003)                                                                                                
        WRITE (IOUT,'(I10,4X,1PG20.13,4X,1PG20.13)')NCUR_CX,FCX1,FCY1                                                    
        WRITE (IOUT,2004)                                                                                                
        WRITE (IOUT,'(I10,9X,A1,2X,I10,2X,1PG20.13,2X,1PG20.13)')IFRA2,XYZ1(1:1),NCUR_VEL,FCX2,FCY2                      

        NUMLOADP = NUMLOADP + 4*NN

      ENDDO ! next K (next /LOAD/PFLUID)
C--------------------------------------------------------------------------------C
      RETURN
C--------------------------------------------------------------------------------C      
 2002 FORMAT(//
     .'     PFLUID LOAD  '/
     .'     ------------------  '/
     .'   SURFACE   FRAME_HSP   DIR_HSP    FUNC_HSP      SENSOR',
     .'           SCALE_X               SCALE_Y')

 2003 FORMAT(//
     .'   FUNC_CX             SCALE_X                 SCALE_Y')

 2004 FORMAT(//
     .' FRAME_VEL   DIR_VEL    FUNC_VEL',
     .'           SCALE_X               SCALE_Y')
C-----------------------------------------------
      END SUBROUTINE HM_READ_PFLUID
