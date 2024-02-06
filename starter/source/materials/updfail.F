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
Chd|  UPDFAIL                       source/materials/updfail.F    
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FAIL_FUN2SYS                  source/materials/tools/fail_fun2sys.F
Chd|        MATTAB_USR2SYS                source/materials/tools/mattab_usr2sys.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      module updfail_mod
      contains
!
      SUBROUTINE UPDFAIL(MAT_PARAM ,NUMMAT ,NFUNCT ,NTABLE ,FUNC_ID ,TABLE ,
     .                   FAIL_FRACTAL,NGRSHEL   ,NGRSH3N,IGRSH4N,IGRSH3N,
     .                   IXC    ,IXTG      ,NUMELC ,NUMELTG )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MATPARAM_DEF_MOD
      USE TABLE_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE RANDOM_WALK_DEF_MOD
      USE RANDOM_WALK_DMG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NUMMAT     !< total number of material models 
      INTEGER ,INTENT(IN) :: NFUNCT     !< total number of system functions   
      INTEGER ,INTENT(IN) :: NTABLE     !< total number of system function tables   
      INTEGER ,INTENT(IN) :: NGRSHEL    !< number of 4n shell element groups
      INTEGER ,INTENT(IN) :: NGRSH3N    !< number of 3n shell element groups
      INTEGER ,INTENT(IN) :: NUMELC     !< number of system 4n shell elements
      INTEGER ,INTENT(IN) :: NUMELTG    !< number of system 3n shell elements
      INTEGER ,DIMENSION(NFUNCT)        ,INTENT(IN) :: FUNC_ID
      INTEGER ,DIMENSION(NIXC,NUMELC)   ,INTENT(IN) :: IXC
      INTEGER ,DIMENSION(NIXTG,NUMELTG) ,INTENT(IN) :: IXTG
      TYPE(TTABLE), DIMENSION(NTABLE)   ,INTENT(IN) :: TABLE
      TYPE(MATPARAM_STRUCT_) ,DIMENSION(NUMMAT), INTENT(INOUT), TARGET :: MAT_PARAM
      TYPE (GROUP_)  ,DIMENSION(NGRSHEL)  :: IGRSH4N
      TYPE (GROUP_)  ,DIMENSION(NGRSH3N)  :: IGRSH3N
      TYPE (FAIL_FRACTAL_) ,INTENT(INOUT) :: FAIL_FRACTAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IMAT,MAT_ID,IR,NFAIL,IFRAC,NFAIL_FRACTAL
C=======================================================================
!     replace function and table Ids by system numbers in matparam%fail structure
!------------------------------------------------
      NFAIL_FRACTAL = 0
!
      DO IMAT=1,NUMMAT
        NFAIL  = MAT_PARAM(IMAT)%NFAIL
        IF (NFAIL > 0) THEN
          DO IR = 1,NFAIL
c
            ! Function(s) User IDs into internal IDs 
            IF (MAT_PARAM(IMAT)%FAIL(IR)%NFUNC > 0) THEN
              CALL FAIL_FUN2SYS(MAT_PARAM(IMAT)%FAIL(IR) ,
     .                          MAT_PARAM(IMAT)%TITLE    ,
     .                          MAT_PARAM(IMAT)%MAT_ID   ,
     .                          NFUNCT  ,FUNC_ID  )
            END IF
c
            ! Table(s) User IDs into internal IDs            
            IF (MAT_PARAM(IMAT)%FAIL(IR)%NTABLE > 0) THEN
              CALL MATTAB_USR2SYS(MAT_PARAM(IMAT)%TITLE    ,
     .                            MAT_PARAM(IMAT)%MAT_ID, 
     .                            NTABLE ,TABLE ,
     .                            MAT_PARAM(IMAT)%FAIL(IR)%NTABLE ,
     .                            MAT_PARAM(IMAT)%FAIL(IR)%TABLE  )
            END IF
c
            ! /FAIL/TAB2 temperature dependency check
            IF (MAT_PARAM(IMAT)%FAIL(IR)%IRUPT == 41) THEN 
              ! Check if temperature dependency is defined twice: in EPSF table and FCT_TEMP
              IF ((TABLE(MAT_PARAM(IMAT)%FAIL(IR)%TABLE(1))%NDIM == 3) .AND. 
     .            (MAT_PARAM(IMAT)%FAIL(IR)%IFUNC(4) > 0)) THEN 
                CALL ANCMSG(MSGID=3055, 
     .                      MSGTYPE=MSGWARNING, 
     .                      ANMODE=ANINFO_BLIND,
     .                      I1=MAT_PARAM(IMAT)%MAT_ID,
     .                      C1=MAT_PARAM(IMAT)%TITLE)   
              ENDIF 
              ! Check if temperature dependency is defined twice: in INST table and FCT_TEMP
              IF (MAT_PARAM(IMAT)%FAIL(IR)%TABLE(2) > 0) THEN 
                IF ((TABLE(MAT_PARAM(IMAT)%FAIL(IR)%TABLE(2))%NDIM == 3) .AND. 
     .              (MAT_PARAM(IMAT)%FAIL(IR)%IFUNC(4) > 0)) THEN 
                  CALL ANCMSG(MSGID=3056, 
     .                        MSGTYPE=MSGWARNING, 
     .                        ANMODE=ANINFO_BLIND,
     .                        I1=MAT_PARAM(IMAT)%MAT_ID,
     .                        C1=MAT_PARAM(IMAT)%TITLE)   
                ENDIF 
              ENDIF
            ENDIF 
!           count number of /fail/fractal_dmg models
            IF (MAT_PARAM(IMAT)%FAIL(IR)%IRUPT == 12) THEN
              NFAIL_FRACTAL = NFAIL_FRACTAL + 1
            END IF
c
          ENDDO
        ENDIF
      ENDDO
!------------------------------------------------
!     fractal damage model initialization
!------------------------------------------------
      FAIL_FRACTAL%NFAIL = NFAIL_FRACTAL
      ALLOCATE(FAIL_FRACTAL%FRACTAL(NFAIL_FRACTAL))
      
      IF (NFAIL_FRACTAL > 0) THEN
        IFRAC = 0
        DO IMAT=1,NUMMAT
          NFAIL  = MAT_PARAM(IMAT)%NFAIL
          IF (NFAIL > 0) THEN
            MAT_ID = MAT_PARAM(IMAT)%MAT_ID
            DO IR = 1,NFAIL
              IF (MAT_PARAM(IMAT)%FAIL(IR)%IRUPT == 12) THEN
                IFRAC = IFRAC + 1
                CALL RANDOM_WALK_DMG(FAIL_FRACTAL%FRACTAL(IFRAC),MAT_PARAM(IMAT)%FAIL(IR),
     .               NGRSHEL,NGRSH3N,IGRSH4N,IGRSH3N,
     .               IMAT   ,NIXC   ,IXC    ,NIXTG  ,IXTG  ,NUMELC ,NUMELTG)
              END IF
            ENDDO
          ENDIF
        ENDDO
      ENDIF
c-----------
      RETURN
      END
      END MODULE updfail_mod
