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
Chd|  SET_ADMESH                    source/model/remesh/set_admesh.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE SET_ADMESH(IPART ,IPADMESH,PADMESH,UNITAB,LSUBMODEL)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE HM_OPTION_READ_MOD
      USE SUBMODEL_MOD
      USE UNITAB_MOD
C----------------------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "remesh_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPART(LIPART1,*), IPADMESH(KIPADMESH,*)
C     REAL ou REAL*8
      my_real
     .       PADMESH(KPADMESH,*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ID,
     .  N,IP,I,J,INILEV,NPART_ADM,ID_IP
C     REAL ou REAL*8
      my_real
     .  ANGL,ANGLDEGR,THKERR
      CHARACTER MESS*40,TITR*nchartitle,KEY*ncharkey,KEY2*ncharkey

      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C-----------------------------------------------
      DATA MESS /'OPTIONS FOR ADAPTIVE MESHING DEFINITION '/
C-----------------------------------------------
      WRITE(IOUT,1000)
C------
C--------------------------------------------------
C    READING /ADMESH/GLOBAL
C--------------------------------------------------

C--------* START BROWSING MODEL ADMESH OPTIONS *------
C
      CALL HM_OPTION_START('/ADMESH/GLOBAL')

      DO N =1,NADMESHG
        TITR = ''

        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_TITR = TITR,
     .                       KEYWORD2 = KEY,
     .                       KEYWORD3 = KEY2)

C
          IS_AVAILABLE = .FALSE.

          CALL HM_GET_INTV('LEVEL',LEVELMAX,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('Iadmrule',IADMRULE,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('Istatcnd',ISTATCND,IS_AVAILABLE,LSUBMODEL)
C
C--------* EXTRACT DATAS (REAL VALUES) *------
C
          CALL HM_GET_FLOATV('Tdelay',DTADMESH,IS_AVAILABLE,LSUBMODEL,UNITAB)
C

      ENDDO
C------

      WRITE(IOUT,1100) LEVELMAX,IADMRULE,DTADMESH,ISTATCND
C------
      WRITE(IOUT,1200)

C--------------------------------------------------
C    READING /ADMESH/SET
C--------------------------------------------------

      CALL HM_OPTION_START('/ADMESH/SET')

      DO N =1,NADMESHSET
         TITR = ''

         CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       OPTION_TITR = TITR,
     .                       KEYWORD2 = KEY)

C
          IS_AVAILABLE = .FALSE.

          CALL HM_GET_INTV('LEVEL',INILEV,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('NIP',NPART_ADM,IS_AVAILABLE,LSUBMODEL)
C
C--------* EXTRACT DATAS (REAL VALUES) *------
C
          CALL HM_GET_FLOATV('ANGLE2',ANGLDEGR,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Thkerr',THKERR,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
C
          IF(ANGLDEGR <= ZERO .OR.ANGLDEGR >= HUNDRED80)THEN
            CALL ANCMSG(MSGID=649,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID,
     .                  C1=TITR)
          END IF
          ANGL=ANGLDEGR*PI/HUNDRED80
C
C         pour creer les structures LSH4UPL, PSH4UPL, LSH3UPL, PSH3UPL
          IF(THKERR > ZERO) IADMERRT=1
          IF(THKERR == ZERO)THKERR=EP30
C
C
          DO I=1,NPART_ADM

             CALL HM_GET_INT_ARRAY_INDEX('PartIds1',ID_IP,I,IS_AVAILABLE,LSUBMODEL)

             IF(ID_IP.NE.0)THEN
                IP=0
                DO J=1,NPART
                  IF(IPART(4,J).EQ.ID_IP)THEN
                    IP=J
                    GOTO 50
                  END IF
                END DO
 50            CONTINUE
C
          	IF(IP.NE.0)THEN
          	  IF(IPART(10,IP).NE.0)THEN
                    CALL ANCMSG(MSGID=644,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO,
     .                     I1=IPART(4,IP))
          	  END IF
          	  IPART(10,IP)  =LEVELMAX
                  IPADMESH(1,IP)=MIN(INILEV,LEVELMAX)
          	  PADMESH(1,IP) =COS(ANGL)
          	  PADMESH(2,IP) =THKERR

                  WRITE(IOUT,1250) IPART(4,IP),
     .                             IPADMESH(1,IP),
     .                             ANGLDEGR,THKERR

          	ELSE
                  CALL ANCMSG(MSGID=646,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=ID_IP)
          	END IF
             END IF

          ENDDO

      ENDDO
C

      RETURN
C-------------------------------------
  999 CALL FREERR(1)

 1000 FORMAT(    /1X,'   ADAPTIVE MESHING         ' /
     .            1X,' --------------------       '// )
 1100 FORMAT(//
     .    '    GLOBAL PARAMETERS FOR ADAPTIVE MESHING    ' //
     .    '    ======================================    ' //
     .    '    NUMBER OF MAXIMUM LEVELS . . .  . . . . . ',I5/,
     .    '    TWO TO ONE ADAPTIVE RULE (0:NO/1:YES) . . ',I5/,
     .    '    TIME BETWEEN 2 CHECKS FOR ADAPTATION. . . ',1PG20.13/,
     .    '    TIME STEP BASED ON THE COARSE MESH (0:NO/1:YES). . ',I5)
 1200 FORMAT(//
     .    '    SETTINGS PER PART FOR ADAPTIVE MESHING    ' //
     .    '    ======================================    ' //)
 1250 FORMAT(
     .    '    PART ID  . . . . . . . . . . .  . . . . . ',I5/,
     .    '    INITIAL NUMBER OF LEVELS . . .  . . . . . ',I5/,
     .    '    ANGLE CRITERIA  . . . . . . . . . . . . . ',1PG20.13/,
     .    '    CRITERIA ON THICKNESS ERROR . . . . . . . ',1PG20.13/)
      RETURN
      END
