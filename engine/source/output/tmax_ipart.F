Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  TMAX_IPART                    source/output/tmax_ipart.F    
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OUTMAX_MOD                    ../common_source/modules/outmax_mod.F
Chd|====================================================================
      SUBROUTINE TMAX_IPART(IPARG ,IPART ,IPARTS ,IPARTC ,IPARTG ,H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE OUTMAX_MOD
      USE H3D_MOD
      USE MESSAGE_MOD
      USE NAMES_AND_TITLES_MOD, ONLY: NCHARKEY
C-----------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, DIMENSION(NPARG,NGROUP) ,INTENT(IN):: IPARG
      INTEGER, DIMENSION(LIPART1,NPART),INTENT(IN):: IPART
      INTEGER ,DIMENSION(NUMELS),INTENT(IN):: IPARTS
      INTEGER ,DIMENSION(NUMELC),INTENT(IN):: IPARTC
      INTEGER ,DIMENSION(NUMELTG),INTENT(IN):: IPARTG
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, N ,NELC , NELTG , IP , NPRT , IPRT
      INTEGER IH3D,NG,ITY,NFT,IKEY,K_LEN,ID_INPUT,ID,NH3D_P

      INTEGER :: NKPART(NPART,3),NOPART(3)
      CHARACTER(LEN=NCHARKEY) :: KEY2
      CHARACTER(LEN=NCHARKEY) :: KEY3
      CHARACTER(LEN=NCHARKEY) :: KEY4
      CHARACTER(LEN=NCHARKEY) :: KEY5
C-----------------------------------------------
C   S o u r c e   L i n e s
C----------------------------------------------- 
C------- IPART_OK(NGROUP,1) for GPS_PART,IPART_OK(NGROUP,2) for GPSRA_PART   
         IF (ALLOCATED(IPART_OK))  DEALLOCATE(IPART_OK)
         ALLOCATE(IPART_OK(NGROUP,2))
         IPART_OK = 0
        IF ((LMAX_NSIG+LMAX_NSTRA)>0) THEN
          IF (LMAX_NSIG>0) ALLOCATE(IGPSTAG(NUMNOD))
          IF (LMAX_NSTRA>0)ALLOCATE(IGPSTRATAG(NUMNOD))
          NKPART(1:NPART,1:3) = 0
          NOPART(1:3) = 1
          NCY_GPS = 10
          NCY_GPSTR = 10
          DO ID_INPUT=1,H3D_DATA%N_INPUT_H3D
c         
            KEY2 = H3D_DATA%INPUT_LIST(ID_INPUT)%KEY2
            KEY3 = H3D_DATA%INPUT_LIST(ID_INPUT)%KEY3
            KEY4 = H3D_DATA%INPUT_LIST(ID_INPUT)%KEY4
            KEY5 = H3D_DATA%INPUT_LIST(ID_INPUT)%KEY5
            IF ( KEY2=='PART' ) THEN
              ID = 3
              NOPART(ID) = 0
            ELSEIF ( KEY3=='GPS'.AND. KEY4=='TMAX') THEN
              ID = 1
              NOPART(ID) = 0
              IF (KEY5/=' ') READ (KEY5(3:12),'(I10)',ERR=100) NCY_GPS 
            ELSEIF ( KEY3=='GPSTRAIN'.AND. KEY4=='TMAX') THEN
              ID = 2
              NOPART(ID) = 0
              IF (KEY5/=' ') READ (KEY5(3:12),'(I10)',ERR=100) NCY_GPSTR 
            ELSE
              ID = 0
            END IF
            IF ( ID > 0 ) THEN
              NH3D_P = H3D_DATA%INPUT_LIST(ID_INPUT)%NB_PART
              IF (NH3D_P==0) THEN
                NKPART(1:NPART,ID) = 1
              ELSE
                DO J=1,NH3D_P
                  IP = H3D_DATA%INPUT_LIST(ID_INPUT)%PART_LIST(J)
                  DO K=1,NPART
                    IF(IP == IPART(4,K)) NKPART(K,ID)=1
                  END DO
                END DO
              END IF
            END IF
          END DO 
c           
          DO ID=1,3
            IF ( NOPART(ID)>0 ) NKPART(1:NPART,ID)=1
          END DO
          DO ID=1,2
            NKPART(1:NPART,ID)=NKPART(1:NPART,3)*NKPART(1:NPART,ID)
          END DO
          DO NG=1,NGROUP
             IF (IPARG(8,NG)==1) CYCLE                                 
             NFT=IPARG(3,NG)+1
             ITY=IPARG(5,NG)
C           
             IPRT = 0
             SELECT CASE (ITY)
               CASE(1)
                 IPRT = IPARTS(NFT)
               CASE(3)
                 IPRT = IPARTC(NFT)
               CASE(7)
                 IPRT = IPARTG(NFT)
             END SELECT
             IF(IPRT>0) IPART_OK(NG,1:2) = NKPART(IPRT,1:2)
          END DO
        END IF

      RETURN
100   CALL ANCMSG(MSGID=277,c1=KEY2//'/'//KEY3//'/'//KEY4//'/'//KEY5,ANMODE=ANINFO)
      END
