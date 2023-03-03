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
Chd|  CREATE_SEATBELT               source/tools/seatbelts/create_seatbelt.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        NEW_SEATBELT                  source/tools/seatbelts/new_seatbelt.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SEATBELT_MOD                  ../common_source/modules/seatbelt_mod.F
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|====================================================================
      SUBROUTINE CREATE_SEATBELT(IXR,ITAB,KNOD2EL1D,NOD2EL1D,IPM,
     .                           X,SENSOR_TAB,BUFMAT,PM,GEO,
     .                           IDDLEVEL,KNOD2ELC,NOD2ELC,IXC,IGEO,
     .                           ISKN    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE SEATBELT_MOD
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "com01_c.inc"
#include      "my_allocate.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IDDLEVEL,IXR(NIXR,*),ITAB(*),KNOD2EL1D(*),NOD2EL1D(*),IPM(NPROPMI,*),
     .        KNOD2ELC(*),NOD2ELC(*),IXC(NIXC,*)
      INTEGER, INTENT(INOUT) :: IGEO(NPROPGI,NUMGEO),ISKN(SISKWN)
      my_real X(3,*),BUFMAT(*),PM(NPROPM,*),GEO(NPROPG,*)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,NOD_START,SEATBELT_ID,COMPT,ELEM_CUR,
     .        FLAG,NNOD,MTYP,MID,NDIR,
     .        I1,I2,IADBUF,TAG_PRINT,ISENS_LOC(2),IPID,OFFC,OFFR,NB_ELEM,NODE,
     .        NB_2D_SEATBELT,COMPT_BELT_END,COMPT_FRAM,NEXT_NODE,NODE_CUR,COMPT_2D,MID_2D,NODE_LONGI,
     .        IADBUF_2D,FUNC1,FUNC2,ISK,N1,N2,SEATBELT_ELEM_FOUND,IMOV,IECROU,NB_ELEM_1D,NB_BRANCH,
     .        BRANCH_CPT
      my_real DIST2,LMIN,RHO,XK,XC,AREA,LONGI_DIRECTION(3),EDGE_DIRECTION(3),SCAL,E11,E22,G12,DET,
     .        N12,N21,NU,FSCALE1,FSCALE2,A11,A22,A12,C1,SSP,RHO0,YOUNG,FSCALET,KMAX,A1C,A2C
C
      INTEGER , DIMENSION(:), ALLOCATABLE:: TAG_RES,TAG_SHELL,TAG_NOD,CC_ELEM,CPT_MAT,TAG_MAT_2D,
     .                                      TAG_NOD_SHELL,TAG_NOD_SPRING,FRAM_TAB,TAG_FRAM_SEATBELT,
     .                                      NNOD_FRAM_SEATBELT,BELT_END_NFRAM,BELT_END_ADDR,TAG_PROP_2D,
     .                                      BRANCH_TAB
      my_real , DIMENSION(:), ALLOCATABLE:: AV_LEN_MAT,AV_AREA_MAT,ELEMSIZE_MAT,BELT_END_SECTION,
     .                                      SECTION_MAT
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
C
C-----------------------------------------------
C--   Check of sensor (not made in hm_read_slipring or hm_read_retractor as sensor are not yet read)
C
      IF (IDDLEVEL == 0) THEN
C
        DO I=1,NSLIPRING
          ISENS_LOC(1) = 0
          IF(SLIPRING(I)%SENSID > 0)THEN
            DO K=1,NSENSOR
              IF(SLIPRING(I)%SENSID == SENSOR_TAB(K)%SENS_ID) ISENS_LOC(1) = K
            ENDDO
            IF(ISENS_LOC(1) == 0) THEN
              CALL ANCMSG(MSGID=2002,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    C1='SENSOR',
     .                    I1=SLIPRING(I)%ID,I2=SLIPRING(I)%SENSID)
            ELSE
              SLIPRING(I)%SENSID = ISENS_LOC(1)
            ENDIF
          ENDIF
        ENDDO
C
        DO I=1,NRETRACTOR
          ISENS_LOC(1:2) = 0
          DO J=1,2
            IF(RETRACTOR(I)%ISENS(J) > 0)THEN
              DO K=1,NSENSOR
                IF(RETRACTOR(I)%ISENS(J) == SENSOR_TAB(K)%SENS_ID) ISENS_LOC(J) = K
              ENDDO
              IF(ISENS_LOC(J) == 0) THEN
                CALL ANCMSG(MSGID=2028,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      C1='SENSOR',
     .                      I1=RETRACTOR(I)%ID,I2=RETRACTOR(I)%ISENS(J))
              ELSE
                RETRACTOR(I)%ISENS(J) = ISENS_LOC(J)
              ENDIF
            ENDIF
          ENDDO
        ENDDO
C
      ENDIF
C
C-----------------------------------------------
C
C--   Loop to find elements of the seatbelt from starting node for each slipiring/retractor
C--   Need to check bifurcation in seatbelt and to tag elements on same cpu for domdec
C
C-----------------------------------------------
C
      MY_ALLOCATE(TAG_NOD_SHELL,NUMNOD)
      MY_ALLOCATE(TAG_PROP_2D,NUMGEO)
      TAG_NOD_SHELL(1:NUMNOD) = 0
      TAG_PROP_2D(1:NUMGEO) = 0
      DO I=1,NUMELC
        MID = IXC(1,I)
        MTYP = IPM(2,MID)
        IPID = IXC(6,I)
        IF (MTYP == 119) THEN
          DO J=2,5
            TAG_NOD_SHELL(IXC(J,I)) = TAG_NOD_SHELL(IXC(J,I)) + 1
          ENDDO
C-        tag of prop type 9 to 1 to set IP=24 or -2 if conflict with non seatbelt elements
          IF (TAG_PROP_2D(IPID)==0) TAG_PROP_2D(IPID) = 1
          IF (TAG_PROP_2D(IPID)==-1) TAG_PROP_2D(IPID) = -2
        ELSEIF (IGEO(11,IPID)==9) THEN
C-        tag of prop type 9 to -2 for error message if conflict with non seatbelt elements
          IF (TAG_PROP_2D(IPID)==0) TAG_PROP_2D(IPID) = -1
          IF (TAG_PROP_2D(IPID)==1) TAG_PROP_2D(IPID) = -2
        ENDIF
      ENDDO
C
      NB_ELEM_1D = 0
      MY_ALLOCATE(TAG_NOD_SPRING,NUMNOD)
      TAG_NOD_SPRING(1:NUMNOD) = 0
      DO I=1,NUMELR
        MID = IXR(5,I)
        IF (MID > 0) THEN
          MTYP = IPM(2,MID)
          IF (MTYP == 114) THEN
            NB_ELEM_1D = NB_ELEM_1D + 1
            DO J=2,3
              TAG_NOD_SPRING(IXR(J,I)) = TAG_NOD_SPRING(IXR(J,I)) + 1
            ENDDO
          ENDIF
        ENDIF
      ENDDO
C
C----------------------------------------------------------------------------
C---  Check of /PROP/TYPE9 - IP flag and skew
C----------------------------------------------------------------------------
C
      DO I=1,NUMGEO
C-      automatic setting of ip = 24
        IF (IGEO(14,I) /= 24) THEN
          IF (TAG_PROP_2D(I) == 1) THEN
            IGEO(14,I) = 24
            CALL ANCMSG(MSGID=2076,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=IGEO(1,I)) 
            ISK = IGEO(2,I)
            IF (ISK > 0) THEN
C-            skew must be skew/mov or skew/fix
              IMOV = ISKN(LISKN*(ISK-1)+5)
              IF (IMOV == 0) THEN
                CALL ANCMSG(MSGID=2082,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      I1=IGEO(1,I)) 
              ENDIF
            ENDIF
          ELSEIF (TAG_PROP_2D(I) == -2) THEN
             CALL ANCMSG(MSGID=2077,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=IGEO(1,I)) 
          ENDIF
        ENDIF
C-      check of nodes 1 and 2 of skew - must be on the same element
        IF (TAG_PROP_2D(I)==1) THEN
          ISK = IGEO(2,I)
          IF (ISK > 0) THEN
            IMOV = ISKN(LISKN*(ISK-1)+5)
            IF (IMOV > 0) THEN
              N1 = ISKN(LISKN*(ISK-1)+1)
              N2 = ISKN(LISKN*(ISK-1)+2)
              SEATBELT_ELEM_FOUND = 0
              DO K=KNOD2ELC(N1)+1,KNOD2ELC(N1+1)
                ELEM_CUR = NOD2ELC(K)
                MID = IXC(1,ELEM_CUR)
                MTYP = IPM(2,MID)
                IF (MTYP==119) THEN
                  DO J=2,5
                    IF (IXC(J,ELEM_CUR)==N2) SEATBELT_ELEM_FOUND = 1
                  ENDDO
                ENDIF
              ENDDO
              IF (SEATBELT_ELEM_FOUND == 0) THEN
                CALL ANCMSG(MSGID=2083,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      I1=IGEO(1,I),I2=ISKN(LISKN*(ISK-1)+4))
              ENDIF
            ENDIF             
          ENDIF
        ENDIF
      ENDDO
C
      DEALLOCATE(TAG_PROP_2D)
C
C----------------------------------------------------------------------------
C---  Loop on elements on edges of seatbelt
C----------------------------------------------------------------------------
C
C--   if nshell = 1 and nspring = 1 -> node in corner of 2D belt
C--   if nshell = 2 and nspring = 1 -> node on edge of 2D belt
C--   if nshell = 0 and nspring = 1 -> node at end of 1D belt
C
      MY_ALLOCATE(TAG_NOD,NUMNOD)
      TAG_NOD(1:NUMNOD) = 0
      COMPT_BELT_END = 0
      COMPT_FRAM = 0
      DO I=1,NUMNOD
        IF ((TAG_NOD_SHELL(I) < 2).AND.(TAG_NOD_SPRING(I)==1).AND.(TAG_NOD(I)==0)) THEN
          COMPT_BELT_END = COMPT_BELT_END + 1
          COMPT_FRAM = COMPT_FRAM + 1
          TAG_NOD(I) = 1
          IF (TAG_NOD_SHELL(I) == 1) THEN
            NEXT_NODE = I
            DO WHILE(NEXT_NODE > 0)
              NODE_CUR = NEXT_NODE
              NEXT_NODE = 0
              DO K=KNOD2ELC(NODE_CUR)+1,KNOD2ELC(NODE_CUR+1)
                ELEM_CUR = NOD2ELC(K)
                MID = IXC(1,ELEM_CUR)
                MTYP = IPM(2,MID)
                IF (MTYP==119) THEN
                  DO J=2,5
                    IF ((TAG_NOD_SPRING(IXC(J,ELEM_CUR))==1).AND.(TAG_NOD(IXC(J,ELEM_CUR))==0)) THEN
C--                   next node on transverse edge of seatbelt
                      NEXT_NODE = IXC(J,ELEM_CUR)
                      TAG_NOD(NEXT_NODE) = 1
                      COMPT_FRAM = COMPT_FRAM + 1
                    ENDIF
                  ENDDO
                ENDIF
              ENDDO       
            ENDDO     
          ENDIF
        ENDIF
      ENDDO
C
      TAG_NOD(1:NUMNOD) = 0
      MY_ALLOCATE(BELT_END_NFRAM,COMPT_BELT_END)
      MY_ALLOCATE(BELT_END_ADDR,COMPT_BELT_END)
      MY_ALLOCATE(FRAM_TAB,COMPT_FRAM)
      MY_ALLOCATE(BELT_END_SECTION,COMPT_BELT_END)
      BELT_END_NFRAM(1:COMPT_BELT_END) = 0
      BELT_END_ADDR(1:COMPT_BELT_END) = 0
      BELT_END_SECTION(1:COMPT_BELT_END) = ZERO
      FRAM_TAB(1:COMPT_FRAM) = 0
      COMPT_BELT_END = 0
      COMPT_FRAM = 0
      DO I=1,NUMNOD
        IF ((TAG_NOD_SHELL(I) < 2).AND.(TAG_NOD_SPRING(I)==1).AND.(TAG_NOD(I)==0)) THEN
          COMPT_BELT_END = COMPT_BELT_END + 1
          COMPT_FRAM = COMPT_FRAM + 1
          TAG_NOD(I) = 1
          BELT_END_NFRAM(COMPT_BELT_END) = 1
          BELT_END_ADDR(COMPT_BELT_END) = COMPT_FRAM
          FRAM_TAB(COMPT_FRAM) = I
          IF (TAG_NOD_SHELL(I) == 1) THEN
C
C--         determination of longitudinal direction using spring connected to corner of seatblet  
            DO K=KNOD2EL1D(I)+1,KNOD2EL1D(I+1)
              IF (NOD2EL1D(K) > NUMELT+NUMELP) THEN
                ELEM_CUR = NOD2EL1D(K)-NUMELT-NUMELP
                MID = IXR(5,ELEM_CUR)
                IF (MID > 0) THEN
                  MTYP = IPM(2,MID)
                  IF ((MTYP == 114).AND.(IXR(2,ELEM_CUR)/= I)) THEN 
                    NODE_LONGI = IXR(2,ELEM_CUR)
                  ELSEIF (MTYP == 114) THEN
                    NODE_LONGI = IXR(3,ELEM_CUR)
                  ENDIF
                ENDIF
              ENDIF
            ENDDO
            DIST2 = (X(1,I)-X(1,NODE_LONGI))**2+(X(2,I)-X(2,NODE_LONGI))**2+(X(3,I)-X(3,NODE_LONGI))**2                  
            LONGI_DIRECTION(1) = (X(1,I)-X(1,NODE_LONGI))/SQRT(MAX(EM20,DIST2))
            LONGI_DIRECTION(2) = (X(2,I)-X(2,NODE_LONGI))/SQRT(MAX(EM20,DIST2))
            LONGI_DIRECTION(3) = (X(3,I)-X(3,NODE_LONGI))/SQRT(MAX(EM20,DIST2))
C
            NEXT_NODE = I
            DO WHILE(NEXT_NODE > 0)
              NODE_CUR = NEXT_NODE
              NEXT_NODE = 0
              DO K=KNOD2ELC(NODE_CUR)+1,KNOD2ELC(NODE_CUR+1)
                ELEM_CUR = NOD2ELC(K)
                MID = IXC(1,ELEM_CUR)
                MTYP = IPM(2,MID)
                IF (MTYP==119) THEN
                  DO J=2,5
                    IF ((TAG_NOD_SPRING(IXC(J,ELEM_CUR))==1).AND.(TAG_NOD(IXC(J,ELEM_CUR))==0)) THEN
C--                   next node on transverse edge of seatbelt
                      NEXT_NODE = IXC(J,ELEM_CUR)
                      TAG_NOD(NEXT_NODE) = 1
                      COMPT_FRAM = COMPT_FRAM + 1
                      FRAM_TAB(COMPT_FRAM) = NEXT_NODE
                    ENDIF
                  ENDDO
                ENDIF
              ENDDO
              IF (NEXT_NODE > 0) THEN
C--             seatbelt section is incremented
                DIST2 = (X(1,NODE_CUR)-X(1,NEXT_NODE))**2+(X(2,NODE_CUR)-X(2,NEXT_NODE))**2
     .                 +(X(3,NODE_CUR)-X(3,NEXT_NODE))**2
                EDGE_DIRECTION(1) = (X(1,NODE_CUR)-X(1,NEXT_NODE))/SQRT(MAX(EM20,DIST2))
                EDGE_DIRECTION(2) = (X(2,NODE_CUR)-X(2,NEXT_NODE))/SQRT(MAX(EM20,DIST2))
                EDGE_DIRECTION(3) = (X(3,NODE_CUR)-X(3,NEXT_NODE))/SQRT(MAX(EM20,DIST2))
                SCAL = LONGI_DIRECTION(1)*EDGE_DIRECTION(1)+LONGI_DIRECTION(2)*EDGE_DIRECTION(2)
     .                +LONGI_DIRECTION(3)*EDGE_DIRECTION(3)
                DIST2 = DIST2*(ONE-SCAL*SCAL)
                IPID = IXC(6,ELEM_CUR)
                BELT_END_SECTION(COMPT_BELT_END) = BELT_END_SECTION(COMPT_BELT_END) + SQRT(MAX(EM20,DIST2))*GEO(1,IPID)
              ENDIF       
            ENDDO
            BELT_END_NFRAM(COMPT_BELT_END) = COMPT_FRAM - BELT_END_ADDR(COMPT_BELT_END) + 1  
          ENDIF
        ENDIF
      ENDDO
C
C      DO I=1,COMPT_BELT_END
C        DO J=1,BELT_END_NFRAM(I)
C          print *,"-->",I,ITAB(FRAM_TAB(BELT_END_ADDR(I)+J-1))
C        ENDDO
C        print *,"SECTION",BELT_END_SECTION(I)
C      ENDDO
C
      DEALLOCATE(TAG_NOD_SPRING,TAG_NOD_SHELL)
C
      MY_ALLOCATE(TAG_RES,NUMELR)
      MY_ALLOCATE(TAG_FRAM_SEATBELT,COMPT_BELT_END)
      MY_ALLOCATE(NNOD_FRAM_SEATBELT,COMPT_BELT_END)
      TAG_NOD(1:NUMNOD) = 0
      TAG_RES(1:NUMELR) = 0
      SEATBELT_ID = 0
      FLAG = 0
      NB_2D_SEATBELT = 0
      TAG_FRAM_SEATBELT(1:COMPT_BELT_END) = 0
      NNOD_FRAM_SEATBELT(1:COMPT_BELT_END) = 0
C
C----------------------------------------------------------------------------
C---  Loop on seatblet elements in longitudinal direction
C----------------------------------------------------------------------------
C
      IF (COMPT_BELT_END == 0) THEN
        CALL ANCMSG(MSGID=2099,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1)
      ENDIF
C
      MY_ALLOCATE(BRANCH_TAB,2*NB_ELEM_1D)
C
      DO I=1,COMPT_BELT_END
C
C--     Check of nodes
C
        IF (TAG_NOD(FRAM_TAB(BELT_END_ADDR(I)))==0) THEN
          SEATBELT_ID = SEATBELT_ID + 1
          NNOD = 0
C
          IF (BELT_END_NFRAM(I) > 1) NB_2D_SEATBELT = NB_2D_SEATBELT + 1
C
          DO J=1,BELT_END_NFRAM(I)
C
            NNOD = NNOD + 1
            NOD_START = FRAM_TAB(BELT_END_ADDR(I)+J-1)
            NDIR = 0
C
            DO K=KNOD2EL1D(NOD_START)+1,KNOD2EL1D(NOD_START+1)
              IF (NOD2EL1D(K) > NUMELT+NUMELP) THEN
                ELEM_CUR = NOD2EL1D(K)-NUMELT-NUMELP
                MID = IXR(5,ELEM_CUR)
                IF (MID > 0) THEN
                  MTYP = IPM(2,MID)
                  IF (MTYP == 114) THEN
C--                 Loop on belt elements
                    NB_BRANCH = 0
                    BRANCH_CPT = 0
                    CALL NEW_SEATBELT(IXR,ITAB,KNOD2EL1D,NOD2EL1D,NOD_START,
     .                                  ELEM_CUR,TAG_RES,TAG_NOD,SEATBELT_ID,FLAG,
     .                                  NNOD,IPM,NB_ELEM_1D,NB_BRANCH,BRANCH_TAB,
     .                                  BRANCH_CPT)

C--                 Loop on subranch (only if no sliprings and no retractors)
                    DO WHILE(NB_BRANCH > 0)
                      NOD_START = BRANCH_TAB(2*(BRANCH_CPT-NB_BRANCH)+1) 
                      ELEM_CUR = BRANCH_TAB(2*(BRANCH_CPT-NB_BRANCH)+2) 
                      NB_BRANCH = NB_BRANCH -1
                      CALL NEW_SEATBELT(IXR,ITAB,KNOD2EL1D,NOD2EL1D,NOD_START,
     .                                  ELEM_CUR,TAG_RES,TAG_NOD,SEATBELT_ID,FLAG,
     .                                  NNOD,IPM,NB_ELEM_1D,NB_BRANCH,BRANCH_TAB,
     .                                  BRANCH_CPT)
                    ENDDO
C
                  ENDIF
                ENDIF
              ENDIF
            ENDDO
C
          ENDDO
C
          TAG_FRAM_SEATBELT(I) = SEATBELT_ID
          NNOD_FRAM_SEATBELT(I) = NNOD
C
        ELSEIF(BELT_END_NFRAM(I) > 1) THEN
C--       check of frames (2D sliprings)
          COMPT = 0
          DO J=1,BELT_END_NFRAM(I)
            IF (TAG_NOD(FRAM_TAB(BELT_END_ADDR(I))) /= 0) COMPT = COMPT + 1
          ENDDO
          IF (COMPT /= BELT_END_NFRAM(I)) THEN
            CALL ANCMSG(MSGID=2073,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=SLIPRING(I)%ID)
          ENDIF
C
        ENDIF
C
      ENDDO
C
      DEALLOCATE(BRANCH_TAB)
C
C----------------------------------------------------------------------------
C---  Filling of seatbelt structure
C----------------------------------------------------------------------------
C
      N_SEATBELT = SEATBELT_ID
      IF (IDDLEVEL == 0) MY_ALLOCATE(SEATBELT_TAB,N_SEATBELT)
      MY_ALLOCATE(TAG_MAT_2D,NUMMAT)
      TAG_MAT_2D(1:NUMMAT) = 0
      IF (NB_2D_SEATBELT > 0) THEN
        MY_ALLOCATE(TAG_SHELL,NUMELC)
        MY_ALLOCATE(SECTION_MAT,NUMMAT)
        TAG_SHELL(1:NUMELC) = 0
        SECTION_MAT(1:NUMMAT) = ZERO
      ENDIF
C
      DO I=1,N_SEATBELT
        COMPT = 0
        COMPT_2D = 0
        SEATBELT_TAB(I)%NFRAM = 1
        SEATBELT_TAB(I)%NNOD = 0
        SEATBELT_TAB(I)%ELEM_SIZE = ZERO
        DO J=1,COMPT_BELT_END
          IF (TAG_FRAM_SEATBELT(J)==I) THEN
            SEATBELT_TAB(I)%NNOD = SEATBELT_TAB(I)%NNOD + NNOD_FRAM_SEATBELT(J)
            SEATBELT_TAB(I)%NFRAM = BELT_END_NFRAM(J)
            SEATBELT_TAB(I)%SECTION = BELT_END_SECTION(J)
          ENDIF
        ENDDO  
        DO J=1,NUMELR
          IF (TAG_RES(J) == I) THEN
C--         count of 1d elements of the seatbelt
            COMPT = COMPT + 1
            MID = IXR(5,J)
            IF (TAG_MAT_2D(MID)==0) TAG_MAT_2D(MID) = -MID
C--         count and tag of 2d elements of the seatbelt
            DO K=2,3
              NODE = IXR(K,J)
              DO L=KNOD2ELC(NODE)+1,KNOD2ELC(NODE+1)
                ELEM_CUR = NOD2ELC(L)
                MID_2D = IXC(1,ELEM_CUR)
                MTYP = IPM(2,MID_2D)
                IF (MTYP==119) THEN
                  IF (TAG_SHELL(ELEM_CUR)==0) THEN
                    TAG_SHELL(ELEM_CUR) = I
                    COMPT_2D = COMPT_2D + 1
                    TAG_MAT_2D(MID) = MID_2D
                    IF (SECTION_MAT(MID_2D) == ZERO) THEN
                      SECTION_MAT(MID_2D) = SEATBELT_TAB(I)%SECTION
                    ELSEIF (ABS(SEATBELT_TAB(I)%SECTION-SECTION_MAT(MID_2D)) > EM05) THEN
                      CALL ANCMSG(MSGID=2075,
     .                          MSGTYPE=MSGERROR,
     .                          ANMODE=ANINFO_BLIND_1,
     .                          I1=IPM(1,MID_2D))
                    ENDIF
                  ENDIF
                ENDIF
              ENDDO
            ENDDO 
          ENDIF
        ENDDO 
        SEATBELT_TAB(I)%NSPRING = COMPT
        SEATBELT_TAB(I)%NSHELL = COMPT_2D
        IF (IDDLEVEL == 0) MY_ALLOCATE(SEATBELT_TAB(I)%SPRING,COMPT)
        COMPT = 0
        DO J=1,NUMELR
          IF (TAG_RES(J) == I) THEN
            COMPT = COMPT + 1
            SEATBELT_TAB(I)%SPRING(COMPT) = J
          ENDIF
        ENDDO    
      ENDDO
C
      DEALLOCATE(BELT_END_NFRAM,BELT_END_SECTION,BELT_END_ADDR,FRAM_TAB,TAG_RES,TAG_FRAM_SEATBELT,NNOD_FRAM_SEATBELT)
C
C----------------------------------------------------------------------------
C---  Computation of elem_size from retractor
C----------------------------------------------------------------------------
C
      DO I=1,NRETRACTOR
        SEATBELT_ID = TAG_NOD(RETRACTOR(I)%NODE(1))
        RETRACTOR(I)%INACTI_NNOD_MAX = SEATBELT_TAB(SEATBELT_ID)%NNOD
        IF (IDDLEVEL == 0) MY_ALLOCATE(RETRACTOR(I)%INACTI_NODE,SEATBELT_TAB(SEATBELT_ID)%NNOD)
        SEATBELT_TAB(SEATBELT_ID)%ELEM_SIZE =  MAX(SEATBELT_TAB(SEATBELT_ID)%ELEM_SIZE,RETRACTOR(I)%ELEMENT_SIZE)        
      ENDDO
C
C----------------------------------------------------------------------------
C---  Computation of default lmin and default critical damping
C----------------------------------------------------------------------------
C
      MY_ALLOCATE(CPT_MAT,NUMMAT)
      MY_ALLOCATE(AV_LEN_MAT,NUMMAT)
      MY_ALLOCATE(AV_AREA_MAT,NUMMAT)
      MY_ALLOCATE(ELEMSIZE_MAT,NUMMAT)
      COMPT = 0
      CPT_MAT(1:NUMMAT) = 0
      AV_LEN_MAT(1:NUMMAT) = ZERO
      AV_AREA_MAT(1:NUMMAT) = ZERO
      ELEMSIZE_MAT(1:NUMMAT) = ZERO
C
      DO I=1,N_SEATBELT
        DO J=1,SEATBELT_TAB(I)%NSPRING
          ELEM_CUR = SEATBELT_TAB(I)%SPRING(J)
          IPID = IXR(1,ELEM_CUR)
          I1 = IXR(2,ELEM_CUR)
          I2 = IXR(3,ELEM_CUR)
          MID= IXR(5,ELEM_CUR)
          ELEMSIZE_MAT(MID) = MAX(ELEMSIZE_MAT(MID),SEATBELT_TAB(I)%ELEM_SIZE)
          DIST2 = (X(1,I1)-X(1,I2))**2+(X(2,I1)-X(2,I2))**2+(X(3,I1)-X(3,I2))**2
          IF (DIST2 > ZERO) THEN
            AV_LEN_MAT(MID) = AV_LEN_MAT(MID) + SQRT(DIST2)
            AV_AREA_MAT(MID) = AV_AREA_MAT(MID) + GEO(1,IPID)
            CPT_MAT(MID) = CPT_MAT(MID) + 1
          ENDIF
        ENDDO
      ENDDO
C
      TAG_PRINT = 0
      DO MID=1,NUMMAT
        IADBUF   = IPM(7,MID)
        IF (CPT_MAT(MID) > 0) THEN
          LMIN = BUFMAT(IADBUF+119-1)
          IF (LMIN == ZERO) THEN
C--         default lmin = 1% of average length
            BUFMAT(IADBUF+119-1) = EM02 * (AV_LEN_MAT(MID) / CPT_MAT(MID))
            IF (TAG_PRINT == 0) WRITE(IOUT,1000)
            TAG_PRINT = 1
            WRITE(IOUT,'(5X,I10,8X,G16.9)') IPM(1,ABS(TAG_MAT_2D(MID))),BUFMAT(IADBUF+119-1)
          ENDIF
C--       storage of retrator eleme size
          BUFMAT(IADBUF+126-1) = ELEMSIZE_MAT(MID)
        ENDIF       
      ENDDO
C
      TAG_PRINT = 0
      DO MID=1,NUMMAT
        IADBUF   = IPM(7,MID)
        IF (CPT_MAT(MID) > 0) THEN
          XC = BUFMAT(IADBUF+70)
          XK = BUFMAT(IADBUF+64)
          IECROU = INT(BUFMAT(IADBUF+76))
          IF (XC == ZERO) THEN
C--         default damping is 30% of critical damping
            RHO = PM(1,MID)
            AREA = AV_AREA_MAT(MID) / CPT_MAT(MID)
            XC = ZEP3 * SQRT(RHO*AREA*XK) * (AV_LEN_MAT(MID) / CPT_MAT(MID))
            BUFMAT(IADBUF+70) = XC
            IF (TAG_PRINT == 0) WRITE(IOUT,1100)
            TAG_PRINT = 1
            WRITE(IOUT,'(5X,I10,8X,G16.9)') IPM(1,ABS(TAG_MAT_2D(MID))),BUFMAT(IADBUF+70) 
          ENDIF
          BUFMAT(IADBUF+71) = 0.1*XC  
          BUFMAT(IADBUF+72) = 0.1*XC         
C--       for 2D_seatbelt mass is applied on shell - rho set to 0 - rho is stored int UPARAM(128) for elementary time step
          IF ((TAG_MAT_2D(MID) > 0).AND.(IDDLEVEL==0)) THEN
            BUFMAT(IADBUF+127-1) = ONE
            BUFMAT(IADBUF+128-1) = 0.9*PM(1,MID)
            PM(1,MID) = EM20
            BUFMAT(IADBUF+71) = 0.3*XC  
            BUFMAT(IADBUF+72) = 0.3*XC
            IF (IECROU==10) THEN
C--           specific non linear formulation for 2d seatblets
              IECROU = 12
              BUFMAT(IADBUF+76) = IECROU + EM01          
            ENDIF
          ENDIF       
        ENDIF       
      ENDDO
C
      DEALLOCATE(CPT_MAT,AV_LEN_MAT,AV_AREA_MAT,ELEMSIZE_MAT,TAG_MAT_2D)
C
C----------------------------------------------------------------------------
C---  Update of mat119 variables after section computation
C----------------------------------------------------------------------------
C
      IF ((NB_2D_SEATBELT > 0).AND.(IDDLEVEL==0)) THEN
        TAG_PRINT = 0
        DO MID=1,NUMMAT
          MTYP = IPM(2,MID)
          IADBUF = IPM(7,MID)
          IF (MTYP == 119) THEN
            FUNC1 = IPM(227,MID)
            FUNC2 = IPM(228,MID)
C--         RHO = MPUL/S)
            RHO0=PM(1,MID)/SECTION_MAT(MID)
C--         E11 = K/S
            E11 = BUFMAT(IADBUF)/SECTION_MAT(MID)
            E22 = BUFMAT(IADBUF+1)
            FSCALET = BUFMAT(IADBUF+12)
            IF (E22 == EM20) E22 = FSCALET*E11
            N12 = BUFMAT(IADBUF+2)
            IF (FUNC1 == 0) THEN
              N21 = N12*E22/E11
              KMAX = MAX(E11,E22)
            ELSE
              N21 = N12*FSCALET
              KMAX = MAX(ONE,FSCALET)*BUFMAT(IADBUF+21)/SECTION_MAT(MID)
            ENDIF
            NU  = SQRT(N12*N21)
            G12 = BUFMAT(IADBUF+5)
            IF (G12 == EM20) G12 = E11/(TWO*(ONE + N12))
            DET = ONE / (ONE - N12*N21)
            A11 = E11 * DET
            A22 = E22 * DET
            A12 = A11 * N21
            C1  = KMAX * DET
C--         coating
            A1C = BUFMAT(IADBUF+13)
            A2C = BUFMAT(IADBUF+14)
            C1 = MAX(A11,A22,A1C)
            SSP = SQRT(C1/RHO0)
            IF(DET<=ZERO) THEN
              CALL ANCMSG(MSGID=307,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=IPM(1,MID),
     .                    C1='SEATBELT MATERIAL')
            ENDIF
            FSCALE1 = BUFMAT(IADBUF+10)/SECTION_MAT(MID)
            FSCALE2 = BUFMAT(IADBUF+11)/SECTION_MAT(MID)
C--         update of UPARAM
            BUFMAT(IADBUF) = E11
            BUFMAT(IADBUF+1) = E22
            BUFMAT(IADBUF+3) = N21
            BUFMAT(IADBUF+4) = NU
            BUFMAT(IADBUF+5) = G12
            BUFMAT(IADBUF+6) = A11
            BUFMAT(IADBUF+7) = A22
            BUFMAT(IADBUF+8) = A12
            BUFMAT(IADBUF+10) = FSCALE1
            BUFMAT(IADBUF+11) = FSCALE2
            BUFMAT(IADBUF+16) = SSP
C--         update of PM
            PM(1,MID)=RHO0
            PM(89,MID)=RHO0
            PM(20,MID) = KMAX/(ONE - NU**2)
            PM(21,MID) = NU
            PM(22,MID) = HALF*KMAX/(ONE + NU)
            PM(24,MID) = KMAX/(ONE - NU**2)
            PM(32,MID) = C1
C--         Need to be store in PM for hourglass forces computation
            PM(33,MID) = E11
            PM(34,MID) = E22
            PM(35,MID) = N12
            PM(36,MID) = N21
            PM(37,MID) = G12
            PM(38,MID) = G12
            PM(39,MID) = G12

C--         printout
            IF (TAG_PRINT == 0) WRITE(IOUT,1200)
            TAG_PRINT = 1
            WRITE(IOUT,'(5X,I10,8X,G16.9,G16.9,G16.9,G16.9)') IPM(1,MID),SECTION_MAT(MID),E11,
     .                                                        E22,G12
          ENDIF       
        ENDDO
      ENDIF
C
      IF (NB_2D_SEATBELT > 0) DEALLOCATE(SECTION_MAT)
C
      IF (NSPMD > 1) THEN
C
C----------------------------------------------------------------------------
C---    DOMDEC - all elements of 1 seatbelt on the same proc
C----------------------------------------------------------------------------
C
        OFFC = NUMELS + NUMELQ    
        OFFR = NUMELS + NUMELQ + NUMELC + NUMELP + NUMELT
C
        DO I=1,N_SEATBELT
C
          IF (SEATBELT_TAB(I)%NFRAM == 1) THEN
C--         1D SEATBELT
            MY_ALLOCATE(CC_ELEM,SEATBELT_TAB(I)%NSPRING)
            CC_ELEM(1:SEATBELT_TAB(I)%NSPRING) = 0
            COMPT = 0
            DO J=1,SEATBELT_TAB(I)%NSPRING
              COMPT = COMPT + 1
              CC_ELEM(COMPT) = OFFR + SEATBELT_TAB(I)%SPRING(J)
            ENDDO
            NB_ELEM = COMPT
C
          ELSEIF (SEATBELT_TAB(I)%NFRAM > 1) THEN
C--         2D SEATBELT
            NB_ELEM = SEATBELT_TAB(I)%NSPRING + SEATBELT_TAB(I)%NSHELL
            MY_ALLOCATE(CC_ELEM,NB_ELEM)
            CC_ELEM(1:NB_ELEM) = 0
            COMPT = 0
            DO J=1,SEATBELT_TAB(I)%NSPRING
              COMPT = COMPT + 1
              CC_ELEM(COMPT) = OFFR + SEATBELT_TAB(I)%SPRING(J)
            ENDDO
            DO J=1,NUMELC
              IF (TAG_SHELL(J) == I) THEN
                COMPT = COMPT + 1
                CC_ELEM(COMPT) = OFFC + J
              ENDIF
            ENDDO
C
          ENDIF
C
          CALL C_PREVENT_DECOMPOSITION(NB_ELEM,CC_ELEM)
          DEALLOCATE(CC_ELEM)
C
        ENDDO
C
      ENDIF
C
      IF (NB_2D_SEATBELT > 0) DEALLOCATE(TAG_SHELL)
C             
      RETURN
C
1000  FORMAT(/
     . '      SEATBELTS DEFAULT LMIN COMPUTATION '/
     . '      ---------------------------------- '/
     . '         MAT ID   DEFAULT LMIN '/)
C
1100  FORMAT(/
     . '      SEATBELTS DEFAULT DAMPING COMPUTATION '/
     . '      ---------------------------------- '/
     . '         MAT ID   DEFAULT DAMPING '/)
C
1200  FORMAT(/
     . '      2D SEATBELTS SECTION COMPUTATION '/
     . '      ---------------------------------- '/
     . '         MAT ID   SEATBELT SECTION      E11              E22              G12'/)
C
      END SUBROUTINE CREATE_SEATBELT

