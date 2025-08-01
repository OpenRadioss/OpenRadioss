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
!||    mod_sptri   ../starter/source/elements/sph/sptri.F
!||--- called by ------------------------------------------------------
!||    sptri       ../starter/source/elements/sph/sptri.F
!||====================================================================
      module MOD_SPTRI
      implicit none
      INTEGER, DIMENSION(:), ALLOCATABLE :: WREDUCE
      END MODULE MOD_SPTRI

!||====================================================================
!||    sptri         ../starter/source/elements/sph/sptri.F
!||--- called by ------------------------------------------------------
!||    lectur        ../starter/source/starter/lectur.F
!||    spinih        ../starter/source/elements/sph/spinih.F
!||--- calls      -----------------------------------------------------
!||    ancmsg        ../starter/source/output/message/message.F
!||    arret         ../starter/source/system/arret.F
!||    spbuc31       ../starter/source/elements/sph/spbuc31.F
!||    spclasv       ../starter/source/elements/sph/spclasv.F
!||--- uses       -----------------------------------------------------
!||    message_mod   ../starter/share/message_module/message_mod.F
!||    mod_sptri     ../starter/source/elements/sph/sptri.F
!||====================================================================
      SUBROUTINE SPTRI(KXSP,IXSP,NOD2SP,X,SPBUF,
     .                 LPRTSPH,LONFSPH,IPARTSP,SZ_INTP_DIST,MAX_INTP_DIST_PART,
     .                 PRE_SEARCH)
C-----------------------------------------------
      USE MESSAGE_MOD
      USE MOD_SPTRI
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "sphcom.inc"
#include      "buckr_c.inc"
C-----------------------------------------------
C     PRE_SEARCH = 0 -> full search of neigbours 
C     PRE_SEARCH = 1 -> pre-search of neigbours for computation of max interparticle dist 
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXSP(NISP,*), IXSP(KVOISPH,*), NOD2SP(*),
     .        LPRTSPH(2,0:NPART) ,LONFSPH(*)
      INTEGER ,INTENT(IN) :: IPARTSP(NUMSPH),PRE_SEARCH,SZ_INTP_DIST
      my_real
     .        SPBUF(NSPBUF,*), X(3,*)
      my_real ,INTENT(INOUT) :: MAX_INTP_DIST_PART(SZ_INTP_DIST)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,J,K, IREDUCE, NVOIS, IERROR,JVOIS(NUMSPH), JSTOR(NUMSPH), JPERM(NUMSPH)
      INTEGER NS, WASPACT(NUMSPH), IPRT
      my_real DVOIS(NUMSPH), BMINMA(6), MYSPATRUE,XMAX, YMAX, ZMAX
C-----------------------------------------------
      NVOIS = 0
C-----------------------------------------------      
      ALLOCATE(WREDUCE(NUMSPH),STAT=IERROR)
      IF(IERROR/=0) THEN
        CALL ANCMSG(MSGID=19,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .              C1='(SPH)')
        CALL ARRET(2)
      END IF
      IREDUCE=0
      WREDUCE(1:NUMSPH)=0
C-----------------------------------------------      
      IF(NSPHIO==0)THEN
        NSPHACT=0
        DO N=1,NUMSPH
          IF(KXSP(2,N)/=0)THEN
            NSPHACT=NSPHACT+1
            WASPACT(NSPHACT)=N
          ENDIF
        ENDDO
      ELSE
        NSPHACT=0
        DO IPRT=1,NPART
          DO K=LPRTSPH(2,IPRT-1)+1,LPRTSPH(1,IPRT)
           NSPHACT=NSPHACT+1
           WASPACT(NSPHACT)=LONFSPH(K)
          ENDDO
        ENDDO
      END IF
C
      DO NS=1,NSPHACT
        N=WASPACT(NS)
        KXSP(5,N)=0
      END DO
C-----------------------------------------------
      BMINMA(1) = -EP30
      BMINMA(2) = -EP30 
      BMINMA(3) = -EP30
      BMINMA(4) =  EP30
      BMINMA(5) =  EP30
      BMINMA(6) =  EP30
C
      XMIN=EP30
      XMAX=-EP30
      YMIN=EP30
      YMAX=-EP30
      ZMIN=EP30
      ZMAX=-EP30     
C
C        Bucket sort. DBUC + MIN / MAX
C
      DBUC=ZERO
      DO NS=1,NSPHACT
        N=WASPACT(NS)
        DBUC=MAX(DBUC,SPBUF(1,N))
C
        J=KXSP(3,N)
        XMIN= MIN(XMIN,X(1,J))
        YMIN= MIN(YMIN,X(2,J))
        ZMIN= MIN(ZMIN,X(3,J))
        XMAX= MAX(XMAX,X(1,J))
        YMAX= MAX(YMAX,X(2,J))
        ZMAX= MAX(ZMAX,X(3,J))
      END DO
C
      BMINMA(1) = MAX(BMINMA(1),XMAX) 
      BMINMA(2) = MAX(BMINMA(2),YMAX) 
      BMINMA(3) = MAX(BMINMA(3),ZMAX) 
      BMINMA(4) = MIN(BMINMA(4),XMIN) 
      BMINMA(5) = MIN(BMINMA(5),YMIN) 
      BMINMA(6) = MIN(BMINMA(6),ZMIN)
C
      DBUC=DBUC*SQRT(ONE +SPATRUE)*ONEP0001
      BMINMA(1) = BMINMA(1)+DBUC
      BMINMA(2) = BMINMA(2)+DBUC
      BMINMA(3) = BMINMA(3)+DBUC
      BMINMA(4) = BMINMA(4)-DBUC
      BMINMA(5) = BMINMA(5)-DBUC
      BMINMA(6) = BMINMA(6)-DBUC
C-----------------------------------------------
      CALL SPBUC31(X      ,KXSP  ,IXSP  ,NOD2SP,
     .             SPBUF  ,WASPACT,JVOIS,JSTOR ,JPERM  ,
     .             DVOIS  ,IREDUCE,WREDUCE,BMINMA,IPARTSP ,
     .             SZ_INTP_DIST,MAX_INTP_DIST_PART,PRE_SEARCH)
C-----------------------------------------------
C     re-tri voisins (voisins vrais, voisins dans la zone de securite).
C
      IF (PRE_SEARCH==0) THEN
      MYSPATRUE=SPATRUE
C     /---------------/
C      CALL MY_BARRIER
C     /---------------/
      CALL SPCLASV(X     ,SPBUF ,KXSP    ,IXSP   ,NOD2SP   ,
     1             WASPACT,MYSPATRUE,IREDUCE,WREDUCE)
C     /---------------/
C      CALL MY_BARRIER
C     /---------------/
      IF(MYSPATRUE<SPATRUE)SPATRUE=MYSPATRUE
      ELSE     
        DEALLOCATE(WREDUCE)       
      ENDIF

      RETURN
      END
