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
Chd|  TAGELEM_R2R                   source/coupling/rad2rad/tagelem_r2r.F
Chd|-- called by -----------
Chd|        R2R_SPLIT                     source/coupling/rad2rad/r2r_split.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TAGELEM_R2R(NUMEL,IPART,TAGBUF,NPART)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NUMEL,IPART(*),TAGBUF(*),NPART
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER J,L
C=======================================================================

        DO J=1,NUMEL
          IF (TAGBUF(IPART(J)) == 1)THEN
            TAGBUF(J+NPART)=TAGBUF(J+NPART)+1
          ENDIF
        ENDDO

C-----------
        RETURN
      END SUBROUTINE TAGELEM_R2R

Chd|====================================================================
Chd|  TAG_ELEM_VOID_R2R             source/coupling/rad2rad/tagelem_r2r.F
Chd|-- called by -----------
Chd|        R2R_PRELEC                    source/coupling/rad2rad/r2r_prelec.F
Chd|-- calls ---------------
Chd|        MODIF_TAG                     source/coupling/rad2rad/routines_r2r.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        NOD2EL_MOD                    share/modules1/nod2el_mod.F   
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        RESTMOD                       share/modules1/restart_mod.F  
Chd|====================================================================
      SUBROUTINE TAG_ELEM_VOID_R2R(NB,IPARTS,IPARTC,IPARTG,
     2           IPARTSP,VAL,CONT,MODIF,ITAGL,F2,FLAG,EANI2,
     3           IGRSURF,IGRNOD,GR_ID)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE RESTMOD
        USE R2R_MOD
        USE NOD2EL_MOD
        USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com04_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IPARTS(*),IPARTC(*),IPARTG(*),NB,VAL,GR_ID,
     .          FLAG,CONT,MODIF,IPARTSP(*),F2,ITAGL(*),EANI2(*)
C-----------------------------------------------
        TYPE (GROUP_), DIMENSION(NGRNOD) :: IGRNOD
        TYPE (SURF_) :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER J,K,L,NI,N1,EL_ID,FACE(4),ITAG,SUM,CUR_ID,ELTAG,CUR_10,
     .          CUR_20,CUR_16,OFFSET
C=======================================================================

        IF (FLAG==0) THEN

C--------------------------------------------------------------------C
C---------------FLAG = 0 --> tag of the elements of the surface------C
C--------------------------------------------------------------------C

          DO J=1,NB
            FACE(1) = IGRSURF%NODES(J,1)
            FACE(2) = IGRSURF%NODES(J,2)
            FACE(3) = IGRSURF%NODES(J,3)
            FACE(4) = IGRSURF%NODES(J,4)
            IF (FACE(4)==0) FACE(4)=FACE(3)
            NI = FACE(1)
            ELTAG = 0

C------------------------>   faces of solids  <----------------------C
            DO L = KNOD2ELS(NI)+1,KNOD2ELS(NI+1)
              CUR_ID = NOD2ELS(L)
              DO K = 1,4
                ITAGL(FACE(K)) = 0
              END DO
              DO K = 2,9
                ITAGL(IXS(NIXS*(CUR_ID-1)+K)) = 1
              END DO
              IF (EANI2(CUR_ID)==10) THEN
                OFFSET = NIXS*NUMELS
                CUR_10 = CUR_ID-NUMELS8
                DO K=1,6
                  ITAGL(IXS(OFFSET+6*(CUR_10-1)+K)) = 1
                ENDDO
              ELSEIF (EANI2(CUR_ID)==20) THEN
                OFFSET = NIXS*NUMELS+6*NUMELS10
                CUR_20 = CUR_ID-(NUMELS8+NUMELS10)
                DO K=1,12
                  ITAGL(IXS(OFFSET+12*(CUR_20-1)+K)) = 1
                ENDDO
              ELSEIF (EANI2(CUR_ID)==16) THEN
                OFFSET = NIXS*NUMELS+6*NUMELS10+12*NUMELS20
                CUR_16 = CUR_ID-(NUMELS8+NUMELS10+NUMELS20)
                DO K=1,8
                  ITAGL(IXS(OFFSET+8*(CUR_16-1)+K)) = 1
                ENDDO
              ENDIF
              SUM=ITAGL(FACE(1))+ITAGL(FACE(2))+ITAGL(FACE(3))+ITAGL(FACE(4))
              IF (SUM==4) ELTAG = 1
              IF ((TAG_ELS(CUR_ID+NPART)<(1+CONT)).AND.
     .           (TAGNO(IPARTS(CUR_ID))/=VAL).AND.(SUM==4)) THEN
                CALL MODIF_TAG(TAG_ELS(CUR_ID+NPART),1+CONT+F2,MODIF)
              ENDIF
            END DO

C------------------------>  shells  <--------------------------------C
            DO L = KNOD2ELC(NI)+1,KNOD2ELC(NI+1)
              CUR_ID = NOD2ELC(L)
              DO K = 1,4
                ITAGL(FACE(K)) = 0
              END DO
              DO K = 2,5
                ITAGL(IXC(NIXC*(CUR_ID-1)+K)) = 1
              END DO
              SUM=ITAGL(FACE(1))+ITAGL(FACE(2))+ITAGL(FACE(3))+ITAGL(FACE(4))
              IF (SUM==4) ELTAG = 1
              IF ((TAG_ELC(CUR_ID+NPART)<(1+CONT)).AND.
     .           (TAGNO(IPARTC(CUR_ID))/=VAL).AND.(SUM==4)) THEN
                CALL MODIF_TAG(TAG_ELC(CUR_ID+NPART),1+CONT+F2,MODIF)
              ENDIF
            END DO
C------------------------>   sh3n  <---------------------------------C
            DO L = KNOD2ELTG(NI)+1,KNOD2ELTG(NI+1)
              CUR_ID = NOD2ELTG(L)
              DO K = 1,4
                ITAGL(FACE(K)) = 0
              END DO
              DO K = 2,4
                ITAGL(IXTG(NIXTG*(CUR_ID-1)+K)) = 1
              END DO
              SUM=ITAGL(FACE(1))+ITAGL(FACE(2))+ITAGL(FACE(3))+ITAGL(FACE(4))
              IF (SUM==4) ELTAG = 1
              IF ((TAG_ELG(CUR_ID+NPART)<(1+CONT)).AND.
     .           (TAGNO(IPARTG(CUR_ID))/=VAL).AND.(SUM==4)) THEN
                CALL MODIF_TAG(TAG_ELG(CUR_ID+NPART),1+CONT+F2,MODIF)
              ENDIF
            END DO

C------------------------>   segments without elements  <------------C
            IF (ELTAG==0) THEN
              DO K = 1,4
                IF (TAGNO(FACE(K)+NPART)==-1) THEN
                  CALL MODIF_TAG(TAGNO(FACE(K)+NPART),0,MODIF)
                ENDIF
              END DO
            ENDIF
C
          END DO

        ELSE

C--------------------------------------------------------------------C
C---------------FLAG = 1 --> tag of the nodes of the surface---------C
C--------------------------------------------------------------------C

          DO J=1,NB
            NI = IGRNOD(GR_ID)%ENTITY(J)
C------------------------>   faces of solids  <----------------------C
            DO L = KNOD2ELS(NI)+1,KNOD2ELS(NI+1)
              CUR_ID = NOD2ELS(L)
              IF ((TAG_ELS(CUR_ID+NPART)<(1+CONT)).AND.
     .           (TAGNO(IPARTS(CUR_ID))/=VAL)) THEN
                CALL MODIF_TAG(TAG_ELS(CUR_ID+NPART),1+CONT,MODIF)
              ENDIF
            END DO
C------------------------>   shells  <-------------------------------C
            DO L = KNOD2ELC(NI)+1,KNOD2ELC(NI+1)
              CUR_ID = NOD2ELC(L)
              IF ((TAG_ELC(CUR_ID+NPART)<(1+CONT)).AND.
     .           (TAGNO(IPARTC(CUR_ID))/=VAL)) THEN
                CALL MODIF_TAG(TAG_ELC(CUR_ID+NPART),1+CONT,MODIF)
              ENDIF
            END DO
C------------------------>  sh3n  <----------------------------------C
            DO L = KNOD2ELTG(NI)+1,KNOD2ELTG(NI+1)
              CUR_ID = NOD2ELTG(L)
              IF ((TAG_ELG(CUR_ID+NPART)<(1+CONT)).AND.
     .           (TAGNO(IPARTG(CUR_ID))/=VAL)) THEN
                CALL MODIF_TAG(TAG_ELG(CUR_ID+NPART),1+CONT,MODIF)
              ENDIF
            END DO
C------------------------>  SPH particles <--------------------------C
            IF (NUMSPH>0) THEN
              CUR_ID = NOD2SP(NI)
              IF ((TAG_ELSP(CUR_ID+NPART)<(1+CONT)).AND.
     .           (TAGNO(IPARTSP(CUR_ID))/=VAL)) THEN
                CALL MODIF_TAG(TAGNO(NI+NPART),2*(1+CONT),MODIF)
              ENDIF
            ENDIF
          END DO

        ENDIF

C-----------
        RETURN
      END SUBROUTINE TAG_ELEM_VOID_R2R

Chd|====================================================================
Chd|  TAG_ELEM_VOID_R2R_LIN         source/coupling/rad2rad/tagelem_r2r.F
Chd|-- called by -----------
Chd|        R2R_PRELEC                    source/coupling/rad2rad/r2r_prelec.F
Chd|-- calls ---------------
Chd|        MODIF_TAG                     source/coupling/rad2rad/routines_r2r.F
Chd|        R2R_VOID_1D                   source/coupling/rad2rad/r2r_void.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        NOD2EL_MOD                    share/modules1/nod2el_mod.F   
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        RESTMOD                       share/modules1/restart_mod.F  
Chd|====================================================================
      SUBROUTINE TAG_ELEM_VOID_R2R_LIN(NB,IPARTS,
     2           IPARTC,IPARTG,IPARTT,IPARTP,IPARTR,VAL,CONT,
     3           MODIF,WARN,IGRSLIN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE RESTMOD
        USE R2R_MOD
        USE NOD2EL_MOD
        USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IPARTS(*),IPARTC(*),IPARTG(*),IPARTR(*),
     .          NB,VAL,CONT,MODIF,IPARTT(*),IPARTP(*),WARN
        TYPE (SURF_) :: IGRSLIN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER J,K,L,CUR_ID,CUR_TYP,NI1,NI2,FLAG
        INTEGER OFFSET,CUR_P,CUR_10,CUR_20,CUR_16
C=======================================================================

C--------------------------------------------------------------------C
C---------------------tag of elements of the line--------------------C
C--------------------------------------------------------------------C

        DO J=1,NB
          NI1 = IGRSLIN%NODES(J,1)
          NI2 = IGRSLIN%NODES(J,2)
C------------------------>   shells  <-------------------------------C
          DO L = KNOD2ELC(NI1)+1,KNOD2ELC(NI1+1)
            CUR_ID = NOD2ELC(L)
            FLAG = 0
            IF ((TAG_ELC(CUR_ID+NPART)<(1+CONT)).AND.
     .         (TAGNO(IPARTC(CUR_ID))/=VAL)) THEN
              DO K=2,5
                IF (IXC(NIXC*(CUR_ID-1)+K) == NI2) FLAG = 1
              ENDDO
            ENDIF
            IF (FLAG == 1)
     .          CALL MODIF_TAG(TAG_ELC(CUR_ID+NPART),1+CONT,MODIF)
          END DO
C------------------------>  sh3n  <----------------------------------C
          DO L = KNOD2ELTG(NI1)+1,KNOD2ELTG(NI1+1)
            CUR_ID = NOD2ELTG(L)
            FLAG = 0
            IF ((TAG_ELG(CUR_ID+NPART)<(1+CONT)).AND.
     .         (TAGNO(IPARTG(CUR_ID))/=VAL)) THEN
              DO K=2,4
                IF (IXTG(NIXTG*(CUR_ID-1)+K) == NI2) FLAG = 1
              ENDDO
            ENDIF
            IF (FLAG == 1)
     .          CALL MODIF_TAG(TAG_ELG(CUR_ID+NPART),1+CONT,MODIF)
          END DO
C------------------------> TRUSS / BEAM / SPRINGS <-----------------C
          DO L = KNOD2EL1D(NI1)+1,KNOD2EL1D(NI1+1)
            CUR_ID = NOD2EL1D(L)
            FLAG = 0
            IF (CUR_ID<=NUMELT) THEN
C---------->               TRUSS
              IF ((TAG_ELT(CUR_ID+NPART)<(1+CONT)).AND.
     .        (TAGNO(IPARTT(CUR_ID))/=VAL)) THEN
                DO K=2,3
                  IF (IXT(NIXT*(CUR_ID-1)+K) == NI2) FLAG = 1
                ENDDO
              ENDIF
              IF (FLAG == 1) CALL R2R_VOID_1D(IPARTT(CUR_ID),IPART)
              IF (FLAG == 1)
     .            CALL MODIF_TAG(TAG_ELT(CUR_ID+NPART),1+CONT,MODIF)
            ELSEIF (CUR_ID<=(NUMELT+NUMELP)) THEN
C---------->               BEAM
              CUR_P = CUR_ID-NUMELT
              IF ((TAG_ELP(CUR_P+NPART)<(1+CONT)).AND.
     .        (TAGNO(IPARTP(CUR_P))/=VAL)) THEN
                DO K=2,3
                  IF (IXP(NIXP*(CUR_P-1)+K) == NI2) FLAG = 1
                ENDDO
              ENDIF
              IF (FLAG == 1) CALL R2R_VOID_1D(IPARTP(CUR_P),IPART)
              IF (FLAG == 1)
     .            CALL MODIF_TAG(TAG_ELP(CUR_P+NPART),1+CONT,MODIF)
            ELSE
C---------->               SPRINGS (not yet compatible)
              CUR_P = CUR_ID-NUMELT-NUMELP
              IF ((TAG_ELR(CUR_P+NPART)<(1+CONT)).AND.
     .        (TAGNO(IPARTR(CUR_P))/=VAL)) THEN
                DO K=2,3
                  IF (IXR(NIXR*(CUR_P-1)+K) == NI2) FLAG = 1
                ENDDO
              ENDIF
              IF (FLAG == 1) WARN = 1
            ENDIF
          END DO
C------------------------>   faces of solids  <---------------------C
          DO L = KNOD2ELS(NI1)+1,KNOD2ELS(NI1+1)
            CUR_ID = NOD2ELS(L)
            FLAG = 0
            IF ((TAG_ELS(CUR_ID+NPART)<(1+CONT)).AND.
     .         (TAGNO(IPARTS(CUR_ID))/=VAL)) THEN
              DO K=2,9
                IF(IXS(NIXS*(CUR_ID-1)+K) == NI2) FLAG = 1
              ENDDO
              IF (EANI(CUR_ID)==10) THEN
                OFFSET = NIXS*NUMELS
                CUR_10 = CUR_ID-NUMELS8
                DO K=1,6
                  IF(IXS(OFFSET+6*(CUR_10-1)+K) == NI2) FLAG = 1
                ENDDO
              ELSEIF (EANI(CUR_ID)==20) THEN
                OFFSET = NIXS*NUMELS+6*NUMELS10
                CUR_20 = CUR_ID-(NUMELS8+NUMELS10)
                DO K=1,12
                  IF(IXS(OFFSET+12*(CUR_20-1)+K) == NI2) FLAG = 1
                ENDDO
              ELSEIF (EANI(CUR_ID)==16) THEN
                OFFSET = NIXS*NUMELS+6*NUMELS10+12*NUMELS20
                CUR_16 = CUR_ID-(NUMELS8+NUMELS10+NUMELS20)
                DO K=1,8
                  IF(IXS(OFFSET+8*(CUR_16-1)+K) == NI2) FLAG = 1
                ENDDO
              ENDIF
            ENDIF
            IF (FLAG == 1)
     .          CALL MODIF_TAG(TAG_ELS(CUR_ID+NPART),1+CONT,MODIF)
          END DO
        END DO

C-----------
        RETURN
      END SUBROUTINE TAG_ELEM_VOID_R2R_LIN
