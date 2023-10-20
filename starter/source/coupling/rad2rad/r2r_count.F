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
Chd|  R2R_COUNT                     source/coupling/rad2rad/r2r_count.F
Chd|-- called by -----------
Chd|        R2R_PRELEC                    source/coupling/rad2rad/r2r_prelec.F
Chd|-- calls ---------------
Chd|        INCOQ3                        source/interfaces/inter3d1/incoq3.F
Chd|        INSOL3                        source/interfaces/inter3d1/insol3.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        NOD2EL_MOD                    share/modules1/nod2el_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F
Chd|        RESTMOD                       share/modules1/restart_mod.F
Chd|====================================================================
      SUBROUTINE R2R_COUNT(PASSE,IPARTS,
     2           IPARTC,IPARTG,IGRPP_R2R ,PM_STACK , IWORKSH,
     3           IGRNOD,IGRSURF,IGRSLIN,IGRBRIC,IXS10,
     4           IXS20,IXS16)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE RESTMOD
        USE NOD2EL_MOD
        USE R2R_MOD
        USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IPARTS(*),IPARTC(*),IPARTG(*),PASSE,IGRPP_R2R(2,*),
     .     IWORKSH(*),IXS10(*), IXS16(*), IXS20(*)
        my_real
     .    PM_STACK(*)
C-----------------------------------------------
        TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
        TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
        TYPE (SURF_)   , DIMENSION(NSLIN)   :: IGRSLIN
        TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,L,IAD,IP,CUR_ID,CUR_TYP,TAG1,TAG2,COMPT,CCPL
        INTEGER ID_ELC,ID_ELTG,ID_ELS,IRECT(4,1),COMPT2,COMPT3
        INTEGER CCPL_T4_EXPO,CCPL_T4_IMPO,OFF,TAG10,TAG20
        my_real AREA
C=======================================================================
C---Pre-counting of nodes/elements/surfaces/lines kept after split------
C=======================================================================

        OFF = NPART + NUMNOD

C--------------------------------------------------------------------C
C------Precounting of nb of taged nodes of GRNOD---------------------C
C--------------------------------------------------------------------C
        DO I=1,NGRNOD

          COMPT = 0
          CCPL = 0
          CCPL_T4_EXPO = 0
          CCPL_T4_IMPO = 0
          DO J=1,IGRNOD(I)%NENTITY
            CUR_ID = IGRNOD(I)%ENTITY(J)
            IF (TAGNO(CUR_ID+NPART)>=0) COMPT=COMPT+1
            IF (TAGNO(CUR_ID+NPART)>1) CCPL=CCPL+1
            IF (TAGNO(CUR_ID+NPART)<=0) CCPL=CCPL+1
            IF ((TAGNO(CUR_ID+NPART)==2).AND.(TAGNO(CUR_ID+OFF)==-1)) CCPL_T4_IMPO = CCPL_T4_IMPO + 1
            IF ((TAGNO(CUR_ID+NPART)==2).AND.(TAGNO(CUR_ID+OFF)==1))  CCPL_T4_EXPO = CCPL_T4_EXPO + 1
          END DO
          IGRNOD(I)%R2R_ALL   = COMPT    ! temporary storage before split
          IGRNOD(I)%R2R_SHARE = CCPL     ! temporary storage before split
          IGRPP_R2R(1,I)  = CCPL_T4_EXPO
          IGRPP_R2R(2,I)  = CCPL_T4_IMPO
        ENDDO

C--------------------------------------------------------------------C
C------Precounting of internal and external surfaces of the domain---C
C--------------------------------------------------------------------C

        IF (PASSE==0) THEN
          ALLOCATE(ISURF_R2R(5,NSURF))
          DO I=1,NSURF
            COMPT = 0
            DO J=1,IGRSURF(I)%NSEG
              IF (IGRSURF(I)%ELTYP(J) == 0) THEN
C    -> case of surfaces defined by segments -> identification of elements attached to segments <--
                DO L=1,4
                  IRECT(L,1)=IGRSURF(I)%NODES(J,L)
                END DO
                CALL INSOL3(X,IRECT,IXS,0,ID_ELS,1,
     .               AREA,0,KNOD2ELS,NOD2ELS,0,
     .               IXS10,IXS16,IXS20)
                CALL INCOQ3(IRECT,IXC,IXTG ,0,ID_ELC,
     .               ID_ELTG,1,GEO,PM,KNOD2ELC ,
     .               KNOD2ELTG,NOD2ELC,NOD2ELTG,THKE,2,IGEO,
     .               PM_STACK , IWORKSH)
C    -> temporary storage if element found type in segment type <--
                IF (ID_ELS/=0) THEN
                  IGRSURF(I)%ELTYP(J) = 11
                  IGRSURF(I)%ELEM(J)  = ID_ELS
                ENDIF
                IF (ID_ELC/=0) THEN
                  IGRSURF(I)%ELTYP(J) = 13
                  IGRSURF(I)%ELEM(J)  = ID_ELC
                ENDIF
                IF (ID_ELTG/=0) THEN
                  IGRSURF(I)%ELTYP(J) = 17
                  IGRSURF(I)%ELEM(J)  = ID_ELTG
                ENDIF
              ENDIF
C    -> counting of segments initially in the domain <--
              CUR_ID  = IGRSURF(I)%ELEM(J)
              CUR_TYP = IGRSURF(I)%ELTYP(J)
              IP = 0
              IF (CUR_TYP>10) CUR_TYP=CUR_TYP-10
              IF (CUR_TYP==1) IP = IPARTS(CUR_ID)
              IF (CUR_TYP==3) IP = IPARTC(CUR_ID)
              IF (CUR_TYP==7) IP = IPARTG(CUR_ID)
              IF (IP>0) THEN
                IF (TAGNO(IP)==1) COMPT=COMPT+1
              ENDIF
            END DO
            ISURF_R2R(1,I) = 0
            ISURF_R2R(2,I) = 0
            ISURF_R2R(3,I) = COMPT
          END DO
        ENDIF

C-------At each pass - number of added segments is counted--------------C

        DO I=1,NSURF
          COMPT = 0
          CCPL = 0
          CCPL_T4_EXPO = 0
          CCPL_T4_IMPO = 0
          DO J=1,IGRSURF(I)%NSEG
            CUR_ID = IGRSURF(I)%ELEM(J)
            CUR_TYP= IGRSURF(I)%ELTYP(J)
            IP = 0
            IF (CUR_TYP>10) CUR_TYP=CUR_TYP-10
            IF (CUR_TYP==1) IP = IPARTS(CUR_ID)
            IF (CUR_TYP==3) IP = IPARTC(CUR_ID)
            IF (CUR_TYP==7) IP = IPARTG(CUR_ID)
            IF (IP>0) THEN
              IF (TAGNO(IP)==0) THEN
                IF (CUR_TYP==1) IP = TAG_ELS(CUR_ID+NPART)
                IF (CUR_TYP==3) IP = TAG_ELC(CUR_ID+NPART)
                IF (CUR_TYP==7) IP = TAG_ELG(CUR_ID+NPART)
                IF (IP>0) COMPT=COMPT+1
                IF (IP==1) CCPL_T4_IMPO=CCPL_T4_IMPO+1
              ELSE
                IF (CUR_TYP==1) IP = TAG_ELS(CUR_ID+NPART)
                IF (CUR_TYP==3) IP = TAG_ELC(CUR_ID+NPART)
                IF (CUR_TYP==7) IP = TAG_ELG(CUR_ID+NPART)
                IF (IP>0) CCPL=CCPL+1
                IF (IP==1) CCPL_T4_EXPO=CCPL_T4_EXPO+1
              ENDIF
            ENDIF
          END DO
          ISURF_R2R(1,I) = ISURF_R2R(3,I) + COMPT
          ISURF_R2R(2,I) = ISURF_R2R(3,I) - CCPL
          ISURF_R2R(4,I) = CCPL_T4_EXPO
          ISURF_R2R(5,I) = CCPL_T4_IMPO
        END DO

C--------------------------------------------------------------------C
C------Precounting of internal and external lines of the domain------C
C--------------------------------------------------------------------C

        IF (PASSE==0) THEN
          ALLOCATE(ISLIN_R2R(2,NSLIN))
        ENDIF

C-------At each pass - number of added lined is counted--------------C

        DO I=1,NSLIN
          COMPT = 0
          CCPL = 0
          DO J=1,IGRSLIN(I)%NSEG
            TAG1 = TAGNO(IGRSLIN(I)%NODES(J,1)+NPART)
            TAG2 = TAGNO(IGRSLIN(I)%NODES(J,2)+NPART)
            IF ((TAG1==1).AND.(TAG2/=-1)) THEN
              COMPT=COMPT+1
            ELSEIF ((TAG1/=-1).AND.(TAG2==1)) THEN
              COMPT=COMPT+1
            ELSEIF ((TAG1/=-1).AND.(TAG2/=-1)) THEN
              CCPL=CCPL+1
            ENDIF
          END DO

          ISLIN_R2R(1,I) = COMPT + CCPL
          ISLIN_R2R(2,I) = COMPT
        END DO

C--------------------------------------------------------------------C
C------Precounting of grbric and external lines of the domain--------C
C--------------------------------------------------------------------C

        IF (PASSE==0) THEN
          ALLOCATE(IGRBRIC_R2R(5,NGRBRIC))
          DO I=1,NGRBRIC
            COMPT = 0
            DO J=1,IGRBRIC(I)%NENTITY
C    -> counting of elements initially in the domain  <--
              CUR_ID  = IGRBRIC(I)%ENTITY(J)
              IF (TAGNO(IPARTS(CUR_ID))==1) COMPT=COMPT+1
            END DO
            IGRBRIC_R2R(1,I) = 0
            IGRBRIC_R2R(2,I) = 0
            IGRBRIC_R2R(3,I) = COMPT
          END DO
        ENDIF

C-------At each pass - number of added elements is counted------------C

        DO I=1,NGRBRIC
          COMPT = 0
          CCPL = 0
          CCPL_T4_EXPO = 0
          CCPL_T4_IMPO = 0
          DO J=1,IGRBRIC(I)%NENTITY
            CUR_ID  = IGRBRIC(I)%ENTITY(J)
            IP = IPARTS(CUR_ID)
            IF (IP>0) THEN
              IF (TAGNO(IP)==0) THEN
                IF (TAG_ELS(CUR_ID+NPART)>0) COMPT=COMPT+1
                IF (TAG_ELS(CUR_ID+NPART)==1) CCPL_T4_IMPO=CCPL_T4_IMPO+1
              ELSE
                IF (TAG_ELS(CUR_ID+NPART)>0) CCPL=CCPL+1
                IF (TAG_ELS(CUR_ID+NPART)==1) CCPL_T4_EXPO=CCPL_T4_EXPO+1
              ENDIF
            ENDIF
          END DO
          IGRBRIC_R2R(1,I) = IGRBRIC_R2R(3,I) + COMPT
          IGRBRIC_R2R(2,I) = IGRBRIC_R2R(3,I) - CCPL
          IGRBRIC_R2R(4,I) = CCPL_T4_EXPO
          IGRBRIC_R2R(5,I) = CCPL_T4_IMPO
        END DO

C-----------
        RETURN
      END SUBROUTINE R2R_COUNT
