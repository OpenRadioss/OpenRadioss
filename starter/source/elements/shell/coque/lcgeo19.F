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
!||    lcgeo19                  ../starter/source/elements/shell/coque/lcgeo19.F
!||--- called by ------------------------------------------------------
!||    lecstack_ply             ../starter/source/properties/composite_options/stack/lecstack_ply.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    fretitl2                 ../starter/source/starter/freform.F
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||--- uses       -----------------------------------------------------
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE LCGEO19(GEO   ,IGEO    ,PM ,IPM ,UNITAB,IUNIT,
     .                   ISTACK,IDRAPEID,LSUBMODEL)
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
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
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IGEO(NPROPGI),IPM(NPROPMI,*),ISTACK,IDRAPEID(*),IUNIT
      my_real
     .   GEO(NPROPG), PM(NPROPM,*)
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER ISS*3
      INTEGER IPMAT,IMID,MLAWLY1, IG,J,JREC
      my_real
     .    THICKT
      INTEGER ISH3N,DRAPE_ID
      INTEGER ID
      CHARACTER(LEN=NCHARTITLE)::TITR,TITR1
      LOGICAL :: IS_AVAILABLE, IS_ENCRYPTED
      LOGICAL :: FOUND


      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)

      ISH3N = IGEO(18)
      CALL HM_GET_INTV('mat_ID', IMID, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('t', GEO(1), IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('delta_phi', GEO(2), IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV('grsh4n_ID', IGEO(40), IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('grsh3n_ID', IGEO(41), IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Npt_ply', IGEO(4), IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('alpha_i', GEO(212), IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_INTV('drape_ID', DRAPE_ID, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('def_orth', IGEO(49), IS_AVAILABLE, LSUBMODEL)

      IG = IGEO(1)
      IF (IGEO(4) <= 0) IGEO(4) = 1
      IF (GEO(212) == ZERO) GEO(212) = NINTY
      IF (IS_ENCRYPTED) THEN
         WRITE(IOUT,1000)IG
 1000    FORMAT(
     &    5X,' LAYER  SHELL PROPERTY SET'/,
     &    5X,'------------------------------------'/,
     &    5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     &    5X,'CONFIDENTIAL DATA'//)
      ELSE     
          WRITE(IOUT,2000)IG,IMID,GEO(1),GEO(2),IGEO(40),IGEO(41),IGEO(4),
     .   GEO(212),DRAPE_ID,IGEO(49)
      ENDIF     
      GEO(2)=GEO(2)*PI/HUNDRED80
      GEO(212)=GEO(212)*PI/HUNDRED80
      IGEO(48) = 0  !  keeped for DRAPE IDENTIFICATION NUMBER
C ------------------------------------
C
C  check for layer materials compatibility moved to "lecgeo"
C
      FOUND = .FALSE.
      IF(IMID > 0) THEN
        DO J=1,NUMMAT
           IF (IPM(1,J) == IMID) THEN
              IGEO(101) = J
              FOUND = .TRUE.
              EXIT
           ENDIF
        ENDDO
      ENDIF  
      IF (.NOT. FOUND) THEN
         CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1),LTITR)
         IF(ISTACK == 0) THEN
            CALL ANCMSG(MSGID=31,
     .           MSGTYPE=MSGERROR,
     .           ANMODE=ANINFO_BLIND_2,
     .           I1=IG,
     .           C1=TITR,
     .           I2=IMID)  
         ELSE
            CALL ANCMSG(MSGID=1151,
     .           MSGTYPE=MSGERROR,
     .           ANMODE=ANINFO_BLIND_2,
     .           I1=IG,
     .           C1=TITR,
     .           I2=IMID) 
            
         ENDIF  
         IGEO(101) = 1   
      ENDIF
C
C  check for wright drape id definition
C
      IF (NDRAPE > 0 .OR. DRAPE_ID > 0) THEN
         FOUND = .FALSE.
        DO J=1,NDRAPE
          IF (DRAPE_ID > 0 .AND. DRAPE_ID == IDRAPEID(J)) THEN
             IGEO(48) = DRAPE_ID
             FOUND = .TRUE.
             EXIT
           ENDIF 
        ENDDO
        IF (.NOT. FOUND) THEN
           IF (DRAPE_ID > 0)THEN
              CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1),LTITR)
              CALL ANCMSG(MSGID=1153,
     .             MSGTYPE=MSGERROR,
     .             ANMODE=ANINFO,
     .             I1=IG,
     .             C1=TITR,
     .             I2=DRAPE_ID)
           ENDIF
        ENDIF
      ENDIF ! IF (NDRAPE > 0)
c-----------
      RETURN
c-----------
 2000 FORMAT(/,
     & 5X,'PLY PROPERTY SET ',/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . . . .  . .=',I10/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . . . .  . .=',I10/,
     & 5X,'LAYER THICKNESS . . . . . . . . . . . . . . .  .=',1PG20.13/,
     & 5X,'INCREMENTAL ANGLE (DIR 1,PROJ(DIR 1 / SHELL). . .=',1PG20.13/,      
     & 5X,'SHELL 4-nodes GROUP IDENTIFIER. . . . . . .  . .=',I10/,
     & 5X,'SHELL 3-nodes GROUP IDENTIFIER. . . . . . .  . .=',I10/,
     & 5X,'NUMBER OF INTEGRATION POINTS THROUGH LAYER.  . .=',I10/,
     & 5X,'ORTHOTROPY ANGLE BETWEEN (DIR 1,DIR 2). . . . . =',1PG20.13/,
     & 5X,'DRAPE IDENTIFIER  . . . . . . . . . . . . .  . .=',I10/,
     & 5X,'DEF_ORTH FLAG . . . . . . . . . . . . . . . . . =',I10/)
      END
