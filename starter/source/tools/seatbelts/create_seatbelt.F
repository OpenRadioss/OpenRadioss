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
Chd|  CREATE_SEATBELT               source/tools/seatbelts/create_seatbelt.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        NEW_SEATBELT                  source/tools/seatbelts/new_seatbelt.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SEATBELT_MOD                  ../common_source/modules/seatbelt_mod.F
Chd|====================================================================
      SUBROUTINE CREATE_SEATBELT(IXR,ITAB,KNOD2EL1D,NOD2EL1D,IPM,
     .                           X,LSENSOR,BUFMAT,PM,GEO,
     .                           IDDLEVEL,KNOD2ELC,NOD2ELC,IXC)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE SEATBELT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "com01_c.inc"
C     NSUBMOD
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXR(NIXR,*),ITAB(*),KNOD2EL1D(*),NOD2EL1D(*),IPM(NPROPMI,*),LSENSOR(NSENSI,*),IDDLEVEL,
     .        KNOD2ELC(*),NOD2ELC(*),IXC(NIXC,*)
      my_real X(3,*),BUFMAT(*),PM(NPROPM,*),GEO(NPROPG,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,EL_START,NOD_START,NOD_MID,SEATBELT_ID,COMPT,ELEM_CUR,
     .        N1,FLAG,NNOD,OFFSET,MTYP,MID,NDIR,
     .        I1,I2,IADBUF,TAG_PRINT,ISENS_LOC(2),IPID,NUM_SEATBELT,OFFC,OFFR,NB_ELEM,NODE,
     .        NB_2D_SEATBELT,TAG_SLIPRING_SEATBELT(NSLIPRING),
     .        TAG_RETRACTOR_SEATBELT(NRETRACTOR),NNOD_SLIPRING_SEATBELT(NSLIPRING),NNOD_RETRACTOR_SEATBELT(NRETRACTOR)
      my_real DIST2,LMIN,RHO,XK,XC,AREA
C
      INTEGER , DIMENSION(:), ALLOCATABLE:: TAG_RES,TAG_SHELL,TAG_NOD,CC_ELEM,CPT_MAT,TAG_MAT_2D
      my_real , DIMENSION(:), ALLOCATABLE:: AV_LEN_MAT,AV_AREA_MAT,ELEMSIZE_MAT
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
              IF(SLIPRING(I)%SENSID == LSENSOR(1,K)) ISENS_LOC(1) = K
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
                IF(RETRACTOR(I)%ISENS(J) == LSENSOR(1,K)) ISENS_LOC(J) = K
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
C
      ALLOCATE(TAG_RES(NUMELR),TAG_NOD(NUMNOD))
      TAG_RES(1:NUMELR) = 0
      TAG_NOD(1:NUMNOD) = 0
      SEATBELT_ID = 0
      FLAG = 0
      NB_2D_SEATBELT = 0
      NNOD_SLIPRING_SEATBELT(1:NSLIPRING) = 0
      NNOD_RETRACTOR_SEATBELT(1:NRETRACTOR) = 0
C
C-----Loop on sliprings --
C
      DO I=1,NSLIPRING
C
C--     Check of nodes
C
        IF (TAG_NOD(SLIPRING(I)%FRAM(1)%NODE(2))==0) THEN
          SEATBELT_ID = SEATBELT_ID + 1
          NNOD = 0
C
          IF (SLIPRING(I)%NFRAM > 1) NB_2D_SEATBELT = NB_2D_SEATBELT + 1
C
          DO J=1,SLIPRING(I)%NFRAM
C
            NOD_MID = SLIPRING(I)%FRAM(J)%NODE(2)
            NDIR = 0
C
            DO K=KNOD2EL1D(NOD_MID)+1,KNOD2EL1D(NOD_MID+1)
              IF (NOD2EL1D(K) > NUMELT+NUMELP) THEN
                ELEM_CUR = NOD2EL1D(K)-NUMELT-NUMELP
                MID = IXR(5,ELEM_CUR)
                IF (MID > 0) THEN
                  MTYP = IPM(2,MID)
                  IF (MTYP == 114) THEN
C--               Loop on belt in one direction
                    CALL NEW_SEATBELT(IXR,ITAB,KNOD2EL1D,NOD2EL1D,NOD_MID,
     .                                ELEM_CUR,TAG_RES,TAG_NOD,SEATBELT_ID,FLAG,
     .                                NNOD,IPM)
C 
                    NDIR = NDIR + 1
                    IF(NDIR > 2) CALL ANCMSG(MSGID=2005,
     .                                       MSGTYPE=MSGERROR,
     .                                       ANMODE=ANINFO,
     .                                       I1=ITAB(NOD_MID))
                  ELSE
                    CALL ANCMSG(MSGID=2032,
     .                          MSGTYPE=MSGERROR,
     .                          ANMODE=ANINFO,
     .                          I1=SLIPRING(I)%ID,I2=IXR(NIXR,ELEM_CUR))
                  ENDIF
                ENDIF
              ENDIF
C
            ENDDO
C
          ENDDO
C
          TAG_SLIPRING_SEATBELT(I) = SEATBELT_ID
          NNOD_SLIPRING_SEATBELT(I) = NNOD
C
        ELSEIF(SLIPRING(I)%NFRAM > 1) THEN
C--       check of frames (2D sliprings)
          COMPT = 0
          DO J=1,SLIPRING(I)%NFRAM
            IF (TAG_NOD(SLIPRING(I)%FRAM(J)%NODE(2)) /= 0) COMPT = COMPT + 1
          ENDDO
          IF (COMPT /= SLIPRING(I)%NFRAM) THEN
            CALL ANCMSG(MSGID=2053,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=SLIPRING(I)%ID)
          ENDIF
C
        ENDIF
C
      ENDDO
C
C----- Loop on retractors --
C
      DO I=1,NRETRACTOR
C
C--     Check of nodes if needed
C
        N1 = RETRACTOR(I)%NODE(1)
        IF(ALLOCATED(RETRACTOR(I)%INACTI_NODE)) DEALLOCATE(RETRACTOR(I)%INACTI_NODE)
C
        IF (TAG_NOD(N1)==0) THEN
          SEATBELT_ID = SEATBELT_ID + 1
          NNOD = 0
          NDIR = 0
C
          DO K=KNOD2EL1D(N1)+1,KNOD2EL1D(N1+1)
            IF (NOD2EL1D(K) > NUMELT+NUMELP) THEN
              ELEM_CUR = NOD2EL1D(K)-NUMELT-NUMELP
              MID = IXR(5,ELEM_CUR)
              IF (MID > 0) THEN
                MTYP = IPM(2,MID)
                IF (MTYP == 114) THEN
C--               Loop on belt in one direction
                  CALL NEW_SEATBELT(IXR,ITAB,KNOD2EL1D,NOD2EL1D,N1,
     .                              ELEM_CUR,TAG_RES,TAG_NOD,SEATBELT_ID,FLAG,
     .                              NNOD,IPM)
C 
                  NDIR = NDIR + 1
                  IF(NDIR > 2) CALL ANCMSG(MSGID=2005,
     .                                     MSGTYPE=MSGERROR,
     .                                     ANMODE=ANINFO,
     .                                     I1=ITAB(NOD_MID))
                ELSE
                  CALL ANCMSG(MSGID=2033,
     .                        MSGTYPE=MSGERROR,
     .                        ANMODE=ANINFO,
     .                        I1=RETRACTOR(I)%ID,I2=IXR(NIXR,ELEM_CUR))
                ENDIF
              ENDIF
            ENDIF
          ENDDO
C
          NNOD_RETRACTOR_SEATBELT(I) = NNOD
          TAG_RETRACTOR_SEATBELT(I) = SEATBELT_ID
C
        ELSE
C
          TAG_RETRACTOR_SEATBELT(I) = SEATBELT_ID
C
        ENDIF
C
      ENDDO
C
C----------------------------------------------------------------------------
C---  Filling of seatbelt structure
C----------------------------------------------------------------------------
C
      N_SEATBELT = SEATBELT_ID
      ALLOCATE(SEATBELT_TAB(N_SEATBELT))
      DO I=1,N_SEATBELT
        COMPT = 0
        DO J=1,NUMELR
          IF (TAG_RES(J) == I) COMPT = COMPT + 1
        ENDDO 
        SEATBELT_TAB(I)%NSPRING = COMPT
        ALLOCATE(SEATBELT_TAB(I)%SPRING(COMPT))
        COMPT = 0
        DO J=1,NUMELR
          IF (TAG_RES(J) == I) THEN
            COMPT = COMPT + 1
            SEATBELT_TAB(I)%SPRING(COMPT) = J
          ENDIF
        ENDDO
        SEATBELT_TAB(I)%NNOD = 0
        SEATBELT_TAB(I)%ELEM_SIZE = ZERO
        DO J=1,NSLIPRING
          IF (TAG_SLIPRING_SEATBELT(J)==I) THEN
            SEATBELT_TAB(I)%NNOD = SEATBELT_TAB(I)%NNOD + NNOD_SLIPRING_SEATBELT(J)
            SEATBELT_TAB(I)%NFRAM = SLIPRING(J)%NFRAM
          ENDIF
        ENDDO
        DO J=1,NRETRACTOR
          IF (TAG_RETRACTOR_SEATBELT(J)==I) SEATBELT_TAB(I)%NNOD = SEATBELT_TAB(I)%NNOD + NNOD_RETRACTOR_SEATBELT(J)
        ENDDO         
      ENDDO
C
C----------------------------------------------------------------------------
C---  Computation of elem_size from retractor
C----------------------------------------------------------------------------
C
      DO I=1,NRETRACTOR
        SEATBELT_ID = TAG_RETRACTOR_SEATBELT(I)
C--     Node N1 must be counter
        RETRACTOR(I)%INACTI_NNOD_MAX = SEATBELT_TAB(SEATBELT_ID)%NNOD + 1
        ALLOCATE(RETRACTOR(I)%INACTI_NODE(SEATBELT_TAB(SEATBELT_ID)%NNOD + 1))
        SEATBELT_TAB(SEATBELT_ID)%ELEM_SIZE =  MAX(SEATBELT_TAB(SEATBELT_ID)%ELEM_SIZE,RETRACTOR(I)%ELEMENT_SIZE)        
      ENDDO
C
C----------------------------------------------------------------------------
C---  Computation of default lmin and default critical damping
C----------------------------------------------------------------------------
C
      ALLOCATE(CPT_MAT(NUMMAT),AV_LEN_MAT(NUMMAT),AV_AREA_MAT(NUMMAT),ELEMSIZE_MAT(NUMMAT),TAG_MAT_2D(NUMMAT))
      COMPT = 0
      CPT_MAT(1:NUMMAT) = 0
      AV_LEN_MAT(1:NUMMAT) = ZERO
      AV_AREA_MAT(1:NUMMAT) = ZERO
      ELEMSIZE_MAT(1:NUMMAT) = ZERO
      TAG_MAT_2D(1:NUMMAT) = 0

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
          IF (SEATBELT_TAB(I)%NFRAM > 1) TAG_MAT_2D(MID) = 1
        ENDDO
      ENDDO
C
      TAG_PRINT = 0
      DO MID=1,NUMMAT
        IADBUF   = IPM(7,MID)
        LMIN = BUFMAT(IADBUF+119-1)
        IF (CPT_MAT(MID) > 0) THEN
          IF (LMIN == ZERO) THEN
C--         default lmin = 1% of average length
            BUFMAT(IADBUF+119-1) = EM02 * (AV_LEN_MAT(MID) / CPT_MAT(MID))
            IF (TAG_PRINT == 0) WRITE(IOUT,1000)
            TAG_PRINT = 1
            WRITE(IOUT,'(5X,I10,8X,G16.9)') IPM(1,MID),BUFMAT(IADBUF+119-1)
          ENDIF
C--       storage of retrator eleme size
          BUFMAT(IADBUF+125-1) = ELEMSIZE_MAT(MID)
        ENDIF       
      ENDDO
C
      TAG_PRINT = 0
      DO MID=1,NUMMAT
        IADBUF   = IPM(7,MID)
        XC = BUFMAT(IADBUF+70)
        IF (CPT_MAT(MID) > 0) THEN
          IF (XC == ZERO) THEN
C--         default damping is 30% of critical damping
            RHO = PM(1,MID)
            AREA = AV_AREA_MAT(MID) / CPT_MAT(MID)
            XK = BUFMAT(IADBUF+64)
            BUFMAT(IADBUF+70) = ZEP3 * SQRT(RHO*AREA*XK) * (AV_LEN_MAT(MID) / CPT_MAT(MID))
            IF (TAG_PRINT == 0) WRITE(IOUT,1100)
            TAG_PRINT = 1
            WRITE(IOUT,'(5X,I10,8X,G16.9)') IPM(1,MID),BUFMAT(IADBUF+70) 
          ENDIF
C--       for 2D_seatbelt mass is apllied on shell
          IF (TAG_MAT_2D(MID) == 1) PM(1,MID) = EM20       
        ENDIF       
      ENDDO
C
      DEALLOCATE(CPT_MAT,AV_LEN_MAT,AV_AREA_MAT,ELEMSIZE_MAT)
C
      IF (NSPMD > 1) THEN
C
C----------------------------------------------------------------------------
C---    DOMDEC - all elements of 1 seatbelt on the same proc
C----------------------------------------------------------------------------
C
        IF (NB_2D_SEATBELT > 0) ALLOCATE(TAG_SHELL(NUMELC))
C
        OFFC = NUMELS + NUMELQ    
        OFFR = NUMELS + NUMELQ + NUMELC + NUMELP + NUMELT
C
        DO I=1,N_SEATBELT
C
          IF (SEATBELT_TAB(I)%NFRAM == 1) THEN
C--         1D SEATBELT
            ALLOCATE(CC_ELEM(SEATBELT_TAB(I)%NSPRING))
            CC_ELEM(1:SEATBELT_TAB(I)%NSPRING) = 0
            COMPT = 0
            DO J=1,SEATBELT_TAB(I)%NSPRING
              COMPT = COMPT + 1
              CC_ELEM(COMPT) = OFFR + SEATBELT_TAB(I)%SPRING(J)
            ENDDO
C
          ELSEIF (SEATBELT_TAB(I)%NFRAM > 1) THEN
C--         2D SEATBELT
            TAG_SHELL(1:NUMELC) = 0
C-- -       all shells connected to seatbelt are stick on same CPU (not only seatblet shells)
            NB_ELEM = SEATBELT_TAB(I)%NSPRING
            DO J=1,SEATBELT_TAB(I)%NSPRING
              DO K=2,3
                NODE = IXR(K,SEATBELT_TAB(I)%SPRING(J))
                DO L=KNOD2ELC(NODE)+1,KNOD2ELC(NODE+1)
                  ELEM_CUR = NOD2ELC(L)
                  IF (TAG_SHELL(ELEM_CUR) == 0) THEN
                    TAG_SHELL(ELEM_CUR) = 1
                    NB_ELEM = NB_ELEM + 1
                  ENDIF
                ENDDO
              ENDDO
            ENDDO
C
            ALLOCATE(CC_ELEM(NB_ELEM))
            CC_ELEM(1:NB_ELEM) = 0
            COMPT = 0
            DO J=1,SEATBELT_TAB(I)%NSPRING
              COMPT = COMPT + 1
              CC_ELEM(COMPT) = OFFR + SEATBELT_TAB(I)%SPRING(J)
            ENDDO
            DO J=1,NUMELC
              IF (TAG_SHELL(J) == 1) THEN
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
      RETURN
C
1000  FORMAT(/
     . '      SEATBELTS DEFAULT LMIN COMPUTATION '/
     . '      ---------------------------------- '/
     . '         MAT ID     DEFAULT LMIN '/)
C
1100  FORMAT(/
     . '      SEATBELTS DEFAULT DAMPING COMPUTATION '/
     . '      ---------------------------------- '/
     . '         MAT ID     DEFAULT DAMPING '/)
C
      END SUBROUTINE CREATE_SEATBELT

