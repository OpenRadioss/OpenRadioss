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
Chd|  LECLAS                        source/loads/laser/leclas.F   
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        LECLAS1                       source/loads/laser/leclas1.F  
Chd|        PRELECLAS                     source/loads/laser/lpreleclas.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        RESTMOD                       share/modules1/restart_mod.F  
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE LECLAS(LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE RESTMOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "sysunit.inc"
#include      "tabsiz_c.inc"
#include      "titr_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(SUBMODEL_DATA),INTENT(IN) ::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real              :: FI, ALPHA,CHALEUR,DAR,TSCALE,XK0,RDK,HNUK,TIMESCAL
      my_real              :: FAC_M, FAC_L, FAC_T
      INTEGER              :: K1, K2, I, J, MFK, IFK, NL, NC, IFUNC, IAFUNC, K, ID, ITYP
      INTEGER              :: UID, IFLAGUNIT, STAT
      CHARACTER*ncharkey   :: KEY,KEY2
      CHARACTER*nchartitle :: TITR
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   P r e - C o n d i t i o n
C-----------------------------------------------
      CALL PRELECLAS(SILAS,SXLAS,LSUBMODEL)  
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IF(.NOT.ALLOCATED(ILAS))ALLOCATE (ILAS(SILAS)    ,STAT=stat)  
      IF(.NOT.ALLOCATED(XLAS))ALLOCATE (XLAS(SXLAS)    ,STAT=stat)       
      IF(NLASER == 0)RETURN
      WRITE(ISTDO,'(A)')TITRE(49)      
      IF (SILAS > 0) ILAS = 0              
      IF (SXLAS > 0) XLAS = ZERO            
C-----------------------------------------------
      I=0
      TIMESCAL=ZERO
      ITYP=0
      K1 = 1
      K2 = 1
      WRITE(IOUT,'(/,A)')  ' LASER BEAM IMPACT'
      WRITE(IOUT,'(A)')    ' -----------------'
      LIDFS = 0

      CALL HM_OPTION_START('/DFS/LASER')

      
      DO K=1,NLASER
      
        TITR = ''
        KEY  = ''

        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID   = ID,
     .                          OPTION_TITR = TITR  ,
     .                          UNIT_ID     = UID   ,
     .                          KEYWORD2    = KEY   )

          !---------------------------------!
          !             UNITS               !
          !---------------------------------!	
          DO J=1,NUNITS
            IF (UNITAB%UNIT_ID(J) == UID) THEN    
              FAC_M = UNITAB%FAC_M(J)
              FAC_L = UNITAB%FAC_L(J)
              FAC_T = UNITAB%FAC_T(J)
              IFLAGUNIT = 1
              EXIT
            ENDIF
          ENDDO   

          !---------------------------------!
          !             READING             !
          !---------------------------------!	
          I=I+1

          CALL HM_GET_FLOATV('SLAS'  ,FI       ,IS_AVAILABLE, LSUBMODEL, UNITAB)        
          CALL HM_GET_INTV  ('fct_IDLAS'  ,IFUNC    ,IS_AVAILABLE, LSUBMODEL)                
          CALL HM_GET_FLOATV('STAR' ,ALPHA    ,IS_AVAILABLE, LSUBMODEL, UNITAB)        
          CALL HM_GET_INTV  ('fct_IDTAR' ,IAFUNC   ,IS_AVAILABLE, LSUBMODEL)                
          !CALL HM_GET_INTV  ('N/A'    ,ITYP     ,IS_AVAILABLE, LSUBMODEL)                
          !CALL HM_GET_FLOATV('N/A'    ,TIMESCAL ,IS_AVAILABLE, LSUBMODEL, UNITAB)        

          CALL HM_GET_FLOATV('Hn'     ,HNUK     ,IS_AVAILABLE, LSUBMODEL, UNITAB)        
          CALL HM_GET_FLOATV('VCp'     ,CHALEUR  ,IS_AVAILABLE, LSUBMODEL, UNITAB)        
          CALL HM_GET_FLOATV('K0'      ,XK0      ,IS_AVAILABLE, LSUBMODEL, UNITAB)        
          CALL HM_GET_FLOATV('Rd'      ,RDK      ,IS_AVAILABLE, LSUBMODEL, UNITAB)        
          CALL HM_GET_FLOATV('Ks'      ,DAR      ,IS_AVAILABLE, LSUBMODEL, UNITAB)        

          CALL HM_GET_INTV  ('Np'      ,NL       ,IS_AVAILABLE, LSUBMODEL)                
          CALL HM_GET_INTV  ('Nc'      ,NC       ,IS_AVAILABLE, LSUBMODEL)                
 
          IF (TIMESCAL == ZERO) TIMESCAL = ONE
          !---------------------------------!
          !        LISTING PRINTOUT         !
          !---------------------------------!
          IF(ITYP==0)THEN
          WRITE(IOUT,'(/,A,I10,/)')
     .                    ' LASER COLUMN TYPE 0',I
          WRITE(IOUT,'(A,1PG20.13)')
     .                    ' LASER INTENSITY FACTOR. . . . . =',FI
          WRITE(IOUT,FMT=FMW_A_I)   
     .                    ' LASER INTENSITY FUNCTION. . . . =',IFUNC
          WRITE(IOUT,'(A,1PG20.13)')
     .                    ' TARGET REFLEXION FACTOR . . . . =',ALPHA
          WRITE(IOUT,FMT=FMW_A_I)   
     .                    ' TARGET REFLEXION FUNCTION . . . =',IAFUNC
          WRITE(IOUT,'(A,1PG20.13)')
     .                    ' LASER FREQUENCY H*NU/K. . . . . =',HNUK
          WRITE(IOUT,'(A,1PG20.13)')
     .                    ' LATENT HEAT(MELTING+VAPOR.) . . =',CHALEUR
          WRITE(IOUT,'(A,1PG20.13)')
     .                    ' INVERSE BREMSSTRAHLUNG XK0. . . =',XK0
          WRITE(IOUT,'(A,1PG20.13)')
     .                    ' INVERSE BREMSSTRAHLUNG RD/K . . =',RDK
          WRITE(IOUT,'(A,1PG20.13)')
     .                    ' COMPLEMENT ABSORPTION IN VAPOUR =',DAR
          WRITE(IOUT,FMT=FMW_A_I)   
     .                    ' TARGET ELEMENT. . . . . . . . . =',NC
          WRITE(IOUT,'(A,1PG20.13)')
     .                    ' TIME SCALE FACTOR . . . . . . . =',TIMESCAL
          WRITE(IOUT,FMT=FMW_A_I)   
     .                    ' NUMBER OF PLASMA ELEMENT. . . . =',NL
          WRITE(IOUT,'(A)')      
     .           ' LIST OF PLASMA ELEMENT(FROM LASER TO TARGET) :'
          ELSE
            WRITE(IOUT,'(/,A,I10,/)')
     .                    ' LASER COLUMN TYPE 1',I
            WRITE(IOUT,'(A,1PG20.13)')
     .                    ' LASER INTENSITY FACTOR. . . . . =',FI
            WRITE(IOUT,'(A,I10)')   
     .                    ' LASER INTENSITY FUNCTION. . . . =',IFUNC
            WRITE(IOUT,'(A,1PG20.13)')
     .                    ' FULL VAPO. ENERGY(/unit Volume) =',CHALEUR
            WRITE(IOUT,'(A,I10)')   
     .                    ' NUMBER OF ELEMENT. . . . . . . .=',NL
            WRITE(IOUT,'(A)')      
     .           ' LIST OF PLASMA ELEMENT(FROM LASER TO TARGET) :'
          ENDIF

          !------------------------------------!
          !            UNITS                   !
          !------------------------------------!
          !CHALEUR = CHALEUR * FAC_L*FAC_L/FAC_T/FAC_T        ! J/kg/K
          !XK0     = XK0 * FAC_L**5                           ! m**5
          !DAR     = DAR * FAC_L**5                           ! m**5/mole**2

          !------------------------------------!
          !            STORAGE                 !
          !------------------------------------!
          XK0 = XK0 * RDK**3.5

          ILAS(K1)   = NL
          ILAS(K1+3) = IFUNC
          ILAS(K1+4) = IAFUNC
          ILAS(K1+5) = ITYP

          XLAS(K2)  =CHALEUR
          XLAS(K2+1)=FI
          XLAS(K2+2)=ALPHA

          XLAS(K2+3)=XK0
          XLAS(K2+4)=HNUK
          XLAS(K2+5)=DAR/(HNUK**2)
          XLAS(K2+6) = ONE / TIMESCAL

          CALL LECLAS1(NL,ILAS(K1+6),NC ,LSUBMODEL)         
          IFK = 6 + 2*(NL+1)
          MFK = 7          
          K1 = K1 + IFK
          K2 = K2 + MFK
        
      ENDDO

C-----------------------------------------------
      RETURN
      END
