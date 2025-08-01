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
!||    r2r_domdec      ../starter/source/coupling/rad2rad/r2r_domdec.F
!||--- called by ------------------------------------------------------
!||    lectur          ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    ancmsg          ../starter/source/output/message/message.F
!||    ifrontplus      ../starter/source/spmd/node/frontplus.F
!||    nlocal          ../starter/source/spmd/node/ddtools.F
!||    plist_ifront    ../starter/source/spmd/node/ddtools.F
!||    usr2sys         ../starter/source/system/sysfus.F
!||--- uses       -----------------------------------------------------
!||    message_mod     ../starter/share/message_module/message_mod.F
!||    r2r_mod         ../starter/share/modules1/r2r_mod.F
!||    restmod         ../starter/share/modules1/restart_mod.F
!||====================================================================
      SUBROUTINE R2R_DOMDEC(IEXTER,IGRNOD,FRONTB_R2R,DT_R2R,FLAG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE RESTMOD
        USE MESSAGE_MOD
        USE R2R_MOD
        USE GROUPDEF_MOD
        USE INOUTFILE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IEXTER(NR2R,*),FLAG,FRONTB_R2R(SFRONTB_R2R,*)
        my_real
     .     DT_R2R(4,*)
        TYPE (GROUP_)  , DIMENSION(NGRNOD)   :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER REF,I,J,K,L,IDG,NJ,NJS,ERR,DOMLEN,FL_EXIT,SPLIST
        CHARACTER NAM*150,CHRUN*4,MESS*40
        INTEGER, DIMENSION(:), ALLOCATABLE :: PLIST
C-----------------------------------------------

C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
        INTEGER USR2SYS
        INTEGER NLOCAL
        INTEGER :: LEN_TMP_NAME
        CHARACTER(len=4096) :: TMP_NAME
C-----------------------------------------------

        REF = 991982

        WRITE(CHRUN,'(I4.4)') IRUN
        IF (IPID==0) THEN
          NAM = TRIM(DOM_NAME)//'_'//CHRUN//'.domdec'
          DOMLEN = LEN_TRIM(DOM_NAME)+12
        ELSE
C-----------only 1 subdomain
          NAM=DOM_NAME(ISUBDOM(8,1):ISUBDOM(8,1)+
     .                 ISUBDOM(7,1)-1)//'_'//CHRUN//'.domdec'
          DOMLEN = ISUBDOM(7,1)+12
        ENDIF

        TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//NAM(1:LEN_TRIM(NAM))
        LEN_TMP_NAME = OUTFILE_NAME_LEN+LEN_TRIM(NAM)
        OPEN(UNIT=REF,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .        ACCESS='SEQUENTIAL',FORM='FORMATTED',STATUS='UNKNOWN')

        IF (FLAG == 1) THEN
C-------------------------------------
          ALLOCATE(PLIST(NSPMD))
          PLIST(1:NSPMD) = -1
C------ Information for FSI
          WRITE(REF,1303,IOSTAT=ERR) FLG_FSI
C------ Information for speedup computation
          DO I=1,4
            WRITE(REF,1304,IOSTAT=ERR) DT_R2R(I,1)
          END DO
C------
          DO I=1,NR2RLNK
            IDG = IEXTER(1,I)
C--  70 -> nlocal link - nodes already defined in link type 4
            IF (IEXTER(5,I) /= 70) THEN
              DO J=1,IGRNOD(IDG)%NENTITY
                NJ = IGRNOD(IDG)%ENTITY(J)
                FL_EXIT = 0
                SPLIST=0
                CALL PLIST_IFRONT(PLIST,NJ,SPLIST)
C
                WRITE(REF,1302,IOSTAT=ERR) ITAB(NJ),SPLIST
                DO L=1,SPLIST
                  K = PLIST(L)
                  WRITE(REF,1303,IOSTAT=ERR) (K)
                  IF (ERR/=0) THEN
                    CALL ANCMSG(MSGID=950,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1)
                    FL_EXIT = 1
                    EXIT
                  ENDIF
                ENDDO
                IF (FL_EXIT==1) EXIT
              END DO
            ENDIF
          END DO
C
          CLOSE(UNIT=REF,STATUS='KEEP')
          DEALLOCATE(PLIST)

        ELSEIF (FLAG == 0) THEN
C------ Information for FSI
          READ(REF,1303,IOSTAT=ERR) FLG_FSI
C------ Information for speedup computation
          DO I=1,4
            READ(REF,1304,IOSTAT=ERR) DT_R2R(I,1)
          END DO
C
          DO I=1,NR2RLNK
            IDG = IEXTER(1,I)
C--  70 -> nlocal link - nodes already defined in link type 4
            IF (IEXTER(5,I) /= 70) THEN
              DO J=1,IGRNOD(IDG)%NENTITY
                FL_EXIT = 0
                READ(REF,1302,IOSTAT=ERR) NJS,SPLIST
                NJ = USR2SYS(NJS,ITABM1,MESS,0)
                DO K=1,NSPMD
                  FRONTB_R2R(NJ,K) = IDG
                END DO
                DO L=1,SPLIST
                  READ(REF,1303,IOSTAT=ERR) K
                  IF (ERR/=0) THEN
                    CALL ANCMSG(MSGID=950,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1)
                    FL_EXIT = 1
                    EXIT
                  ENDIF
                  FRONTB_R2R(NJ,K) = -1
                  IF (NLOCAL(NJ,K)/=1) CALL IFRONTPLUS(NJ,K)
                ENDDO
                IF (FL_EXIT==1) EXIT
              END DO
            ELSE
              DO J=1,IGRNOD(IDG)%NENTITY
                NJ = IGRNOD(IDG)%ENTITY(J)
                DO K=1,NSPMD
                  IF (FRONTB_R2R(NJ,K) > 0) FRONTB_R2R(NJ,K) = FRONTB_R2R(NJ,K) + NGRNOD*IDG
                END DO
              END DO
            ENDIF
          END DO
          CLOSE(UNIT=REF,STATUS='KEEP')

        ELSE
C------------------------------------
          CLOSE(UNIT=REF,STATUS='DELETE')

C-------------------------------------
        ENDIF

C--------------------------------------------------------------C
        RETURN

1302    FORMAT( 1X,I20,I9)
1303    FORMAT( 1X,I9)
1304    FORMAT( 1X,E9.4)

C--------------------------------------------------------------C

        RETURN

C-----------

        RETURN
      END SUBROUTINE R2R_DOMDEC
