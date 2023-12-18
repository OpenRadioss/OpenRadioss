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
Chd|  HM_READ_PROP19                source/properties/shell/hm_read_prop19.F
Chd|-- called by -----------
Chd|        HM_READ_PROPERTIES            source/properties/hm_read_properties.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PROP19(IG, IGTYP, ISTACK, GEO, IGEO, PM, IPM, UNITAB, 
     .     IDRAPEID, LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "drape_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_), INTENT(IN) ::UNITAB
      INTEGER, INTENT(INOUT) :: IGEO(NPROPGI)
      INTEGER, INTENT(IN) :: IPM(NPROPMI,*), IDRAPEID(*), IG, IGTYP, ISTACK
      my_real, INTENT(INOUT) :: GEO(NPROPG)
      my_real, INTENT(IN) ::  PM(NPROPM,*)
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: IMID, J, DRAPE_ID, ID
      my_real :: THICKT, FAC_GEN, PUN
      CHARACTER(LEN = nchartitle) :: TITR, TITR1, IDTITL
      LOGICAL :: IS_AVAILABLE, IS_ENCRYPTED, LFOUND
C-------------------------------------------------
C     B e g i n n i n g   o f  S u b r o u t i n e
C-------------------------------------------------
      PUN = 0.1
      IGEO(1) = IG
      IGEO(11) = IGTYP
      GEO(12) = IGTYP + PUN
      
      IS_AVAILABLE = .FALSE.
      IS_ENCRYPTED = .FALSE.
!     Encryption flag
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)      
!     Line 1
      CALL HM_GET_INTV('material', IMID, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('thickness1', GEO(1), IS_AVAILABLE, LSUBMODEL, UNITAB)  
      CALL HM_GET_FLOATV('orientangle', GEO(2), IS_AVAILABLE, LSUBMODEL, UNITAB)  
      CALL HM_GET_INTV('grsh4n_ID', IGEO(40), IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('grsh3n_ID', IGEO(41), IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('integrationpoints', IGEO(4), IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('orientangle2', GEO(212), IS_AVAILABLE, LSUBMODEL, UNITAB)  
!     Line 2
      CALL HM_GET_INTV('table', DRAPE_ID, IS_AVAILABLE, LSUBMODEL)
      
      IF (IGEO(4) <= 0) IGEO(4) = 1   ! default value
      IGEO(44) = IGEO(4) ! Used with drape for OUPT
      IF (GEO(212) == ZERO) THEN
         CALL HM_GET_FLOATV_DIM('orientangle2',FAC_GEN, IS_AVAILABLE, LSUBMODEL, UNITAB)
         GEO(212) = NINTY * FAC_GEN ! default value   
      ENDIF

      IF (IS_ENCRYPTED) THEN
         WRITE(IOUT, 1000) IG
      ELSE
         WRITE(IOUT, 2000) IG, IMID, GEO(1), GEO(2), IGEO(40), IGEO(41), IGEO(4),
     .        GEO(212), DRAPE_ID
      ENDIF
     
      GEO(2) = GEO(2) * PI / HUNDRED80
      GEO(212) = GEO(212) * PI / HUNDRED80
      IGEO(48) = 0 

!     *********************************************************    !
!     Check for layer materials compatibility moved to "lecgeo"
!     *********************************************************    !
      LFOUND = .FALSE.
      DO J=1,NUMMAT-1
         IF (IPM(1,J) == IMID) THEN
            IGEO(101) = J
            LFOUND = .TRUE.
            EXIT
         ENDIF
      ENDDO
      IF (.NOT. LFOUND) THEN
         CALL FRETITL2(TITR, IGEO(NPROPGI-LTITR+1), LTITR)
         IF (ISTACK == 0) THEN
            CALL ANCMSG(MSGID = 31, MSGTYPE = MSGERROR, ANMODE = ANINFO_BLIND_2,
     .           I1 = IG, C1 = TITR, I2 = IMID)  
         ELSE
            CALL ANCMSG(MSGID = 1151, MSGTYPE = MSGERROR, ANMODE = ANINFO_BLIND_2,
     .           I1 = IG, C1 = TITR, I2 = IMID) 
         ENDIF
         IGEO(101) = 1   
      ELSE 
!     ***********************************    ! 
!     check for right drape id definition
!     ***********************************    ! 
         IF (NDRAPE > 0 .OR. DRAPE_ID > 0) THEN
            LFOUND = .FALSE.
            DO J=1,NDRAPE
               IF (DRAPE_ID > 0 .AND. DRAPE_ID == IDRAPEID(J)) THEN
                  IGEO(48) = DRAPE_ID
                  LFOUND = .TRUE.
                  EXIT
               ENDIF 
            ENDDO
            IF (.NOT. LFOUND) THEN
               IF (DRAPE_ID > 0)THEN
                  CALL FRETITL2(TITR, IGEO(NPROPGI-LTITR+1), LTITR)
                  CALL ANCMSG(MSGID = 1153, MSGTYPE = MSGERROR, ANMODE = ANINFO,
     .                 I1 = IG, C1 = TITR, I2 = DRAPE_ID)
               ENDIF
            ENDIF
         ENDIF                  
      ENDIF
      IF (IGEO(4) > 10) THEN
         CALL ANCMSG(MSGID = 788, MSGTYPE = MSGERROR, ANMODE = ANINFO,
     .        I1 = IG, C1 = IDTITL)
         CALL ARRET(2)
      ENDIF
      RETURN
 1000 FORMAT(
     &    5X,' LAYER  SHELL PROPERTY SET'/,
     &    5X,'------------------------------------'/,
     &    5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     &    5X,'CONFIDENTIAL DATA'//)
 2000 FORMAT(
     & 5X,'LAYER PROPERTY SET ',/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . . . .  . .=',I10/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . . . .  . .=',I10/,
     & 5X,'LAYER THICKNESS . . . . . . . . . . . . . . .  .=',1PG20.13/,
     & 5X,'INCRMENTAL ANGLE (DIR 1,PROJ(DIR 1 / SHELL). . .=',1PG20.13/,      
     & 5X,'SHELL 4-nodes GROUP IDENTIFIER. . . . . . .  . .=',I10/,
     & 5X,'SHELL 3-nodes GROUP IDENTIFIER. . . . . . .  . .=',I10/,
     & 5X,'NUMBER OF INTEGRATION POINTS THROUGH LAYER.  . .=',I10/,
     & 5X,'ORTHOTROPY ANGLE BETWEEN (DIR 1,DIR 2). . . . . =',1PG20.13/,
     & 5X,'DRAPE IDENTIFIER  . . . . . . . . . . . . .  . .=',I10/)
      END SUBROUTINE HM_READ_PROP19
