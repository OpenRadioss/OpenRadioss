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
Chd|  HM_READ_RAND                  source/general_controls/computation/hm_read_rand.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        ALEAT                         source/general_controls/computation/aleat.F
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        RANDOM_MOD                    share/modules1/random_mod.F   
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_RAND(X     ,IGRNOD   ,ITAB,IRAND,ALEA,XSEED,
     .                        UNITAB,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE UNITAB_MOD
      USE RANDOM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "random_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(*),IRAND(*)
      my_real
     .   X(3,*),ALEA(*),XSEED(*)
C-----------------------------------------------
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  NGR2USR
      my_real
     .   ALEAT
      EXTERNAL NGR2USR,ALEAT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ID,IS,IGRS,IALL,NRANDG,SUB_ID
      CHARACTER KEY*ncharkey
!
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
      LOGICAL IS_AVAILABLE
C======================================================================|
      IALL   = 0
      NRANDG = 0
      SEED  =ZERO
      XALEA =ZERO
C--------------------------------------------------
C START BROWSING MODEL RBODY
C--------------------------------------------------
      IS_AVAILABLE = .FALSE.
      CALL HM_OPTION_START('/RANDOM')
C-----------------------------      
      DO I=1,NRAND
        IRAND(I) = 0
        ALEA(I) = ZERO
        XSEED(I) = ZERO
        ! --------------------------------
        ! check if -rxalea or -rseed command line option are used
        IF(.NOT.RAND_STRUCT%CMD) THEN
C--------------------------------------------------
C EXTRACT DATAS OF /RANDOM
C--------------------------------------------------
            CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                           KEYWORD2 = KEY,
     .                           SUBMODEL_ID = SUB_ID)
C
            IF(SUB_ID == 0) THEN
              CALL HM_GET_FLOATV('XALEA',ALEA(I),IS_AVAILABLE,LSUBMODEL,UNITAB)
              CALL HM_GET_FLOATV('SEED',XSEED(I),IS_AVAILABLE,LSUBMODEL,UNITAB)
              CALL HM_GET_INTV('GRNOD_ID',ID,IS_AVAILABLE,LSUBMODEL)
C
              IRAND(I) = ID
              IF (ALEA(I) > ZERO) THEN
                SEED  = XSEED(I)
                IF(KEY(1:5) == 'GRNOD') THEN
                  NRANDG = NRANDG+1
                  INGR2USR => IGRNOD(1:NGRNOD)%ID
                  IRAND(NRANDG) = NGR2USR(ID,INGR2USR,NGRNOD)
                  IF (IRAND(NRANDG) == 0) THEN
                    CALL ANCMSG(MSGID=173,
     .                        MSGTYPE=MSGERROR,
     .                          ANMODE=ANINFO,
     .                          C1='RANDOM NOISE',
     .                          C2='NODE',
     .                          I1=ID)
                    NRANDG = NRANDG-1
                  ENDIF
                ELSE
                  IALL = IALL+1
                  XALEA = ALEA(I)
                ENDIF
              ENDIF
            ENDIF
        ! --------------------------------
        ELSE
        ! -rxalea or -rseed command line option are used
            IALL = IALL+1
            IRAND(I) = 0
            ALEA(I) = RAND_STRUCT%ALEA_NBR
            XSEED(I) = RAND_STRUCT%SEED_NBR
            SEED = XSEED(I)
            XALEA = ALEA(I)
        ENDIF
        ! --------------------------------
      ENDDO
      IF ((NRANDG == 0 .AND. XALEA > ZERO).OR.(NRANDG > 0 .AND. IALL == 0) ) THEN
        IF(.NOT.RAND_STRUCT%CMD) WRITE(IOUT,1000)
      ENDIF
C---
      IF (NRANDG == 0 .AND. XALEA > ZERO) THEN
C-----------------------
C       All nodes
C-----------------------
        DO I=1,NUMNOD
          X(1,I)=X(1,I)+XALEA*ALEAT()
          X(2,I)=X(2,I)+XALEA*ALEAT()
          X(3,I)=X(3,I)+XALEA*ALEAT()
        ENDDO
C
        WRITE (IOUT,'(8X,A)')'NODE GROUP           :          ALL NODES'
        WRITE (IOUT,1100) XALEA
        IF (SEED /= ZERO) WRITE (IOUT,1200) SEED
        IF (IPRI >= 4) THEN
          WRITE (IOUT,1400)
          DO I=1,NUMNOD
            WRITE(IOUT,1500)ITAB(I),X(1,I),X(2,I),X(3,I)
          ENDDO
        ENDIF
      ELSEIF (NRANDG > 0 .AND. IALL == 0) THEN
C-----------------------
C       Node groups only
C-----------------------
        DO I=1,NRANDG
          IGRS = IRAND(I)
          SEED = XSEED(I)
          DO J=1,IGRNOD(IGRS)%NENTITY
            IS=IGRNOD(IGRS)%ENTITY(J)
            X(1,IS)=X(1,IS)+ALEA(I)*ALEAT()
            X(2,IS)=X(2,IS)+ALEA(I)*ALEAT()
            X(3,IS)=X(3,IS)+ALEA(I)*ALEAT()
          ENDDO
          WRITE (IOUT,1050) IGRNOD(IGRS)%ID
          WRITE (IOUT,1100) ALEA(I)
          IF (SEED /= ZERO) WRITE (IOUT,1200) SEED
        ENDDO
        IF (IPRI >= 4) THEN
          WRITE (IOUT,1400)
          DO I=1,NRANDG
            IGRS = IRAND(I)
            DO J=1,IGRNOD(IGRS)%NENTITY
              IS=IGRNOD(IGRS)%ENTITY(J)
              WRITE(IOUT,1500) ITAB(IS),X(1,IS),X(2,IS),X(3,IS)
            ENDDO
          ENDDO
        ENDIF
      ENDIF
C-----------------------
      RETURN
 999  CALL FREERR(3)
 1000 FORMAT(//
     .'     RANDOM NOISE       '/
     .'     ------------       ')
 1050 FORMAT(/8X,'NODE GROUP           :          ID    = ',I10)
 1100 FORMAT( 8X,'MAXIMUM RANDOM NOISE :          XALEA = ',1PG20.13)
 1200 FORMAT( 8X,'RANDOM SEQUENCE      :          SEED  = ',1PG20.13)
 1400 FORMAT(/8X,'NEW NODE COORDINATES',20X,'X',24X,'Y',24X,'Z')
 1500 FORMAT( 7X,I10,3(5X,E20.13))
C---
      RETURN
      END SUBROUTINE HM_READ_RAND
