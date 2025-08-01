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
!||    admlcnt                ../starter/source/model/remesh/admlist.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    my_exit                ../starter/source/output/analyse/analyse.c
!||--- uses       -----------------------------------------------------
!||    format_mod             ../starter/share/modules1/format_mod.F90
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    reader_old_mod         ../starter/share/modules1/reader_old_mod.F90
!||====================================================================
      SUBROUTINE ADMLCNT(NIX     ,IX      ,NUMEL       ,IPARTEL ,IPART   ,
     .                   KELTREE ,ELTREE  ,KSONTREE    ,NSONTREE,KLEVTREE,
     .                   NLIST   ,MESS    ,IX1         ,IX2     ,INDEX   ,
     .                   KK      ,NEL     ,TYPE    ,ID      ,TITR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE FORMAT_MOD , ONLY : FMT_10I
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
      USE READER_OLD_MOD , ONLY : LINE, IREC
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIX, IX(NIX,*), NUMEL, IPARTEL(*), IPART(LIPART1,*),
     .        KELTREE, ELTREE(KELTREE,*), KSONTREE, NSONTREE, KLEVTREE,
     .        NLIST,IX1(*), IX2(*), INDEX(*), KK, NEL, LEVEL
      CHARACTER MESS*40
      INTEGER ID
      CHARACTER(LEN=NCHARTITLE) :: TITR,TYPE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LIST(NLIST), IL, JREC, J10(10)
      INTEGER I, J, NOLD, K, IWORK(70000)
      INTEGER LELT, NE, IP, NLEV
C-----------------------------------------------
C CONSTITUTION DE LIST
C-----------------------------------------------
      IL   =0
C
      JREC=IREC
      JREC=JREC+1
      READ(IIN,REC=JREC,ERR=999,FMT='(A)')LINE
      DO WHILE(LINE(1:1)/='/')
        READ(LINE,ERR=999,FMT=FMT_10I) J10
        DO I=1,10
          IF(J10(I)/=0)THEN
            IL=IL+1
            LIST(IL)=J10(I)
          END IF
        ENDDO
        JREC=JREC+1
        READ(IIN,REC=JREC,ERR=999,FMT='(A)')LINE
      ENDDO
C-----------------------
C TRI DE LIST EN ORDRE CROISSANT
C AVEC SUPPRESSION DES No DOUBLES
C-----------------------
      CALL MY_ORDERS(0,IWORK,LIST,INDEX,NLIST,1)
      DO I=1,NLIST
        INDEX(NLIST+I) = LIST(INDEX(I))
      ENDDO
      K=1
      NOLD = INDEX(NLIST+1)
      DO I=1,NLIST
        IF(NOLD/=INDEX(NLIST+I))K=K+1
        LIST(K) = INDEX(NLIST+I)
        NOLD    = INDEX(NLIST+I)
      ENDDO
      NEL=K
C-----------------------
C TRI DE IX EN ORDRE CROISSANT si KK = 0
C-----------------------
      IF(KK==0)THEN
        DO I=1,NUMEL
          IX2(I) = IX(NIX,I)
        ENDDO
        CALL MY_ORDERS(0,IWORK,IX2,INDEX,NUMEL,1)
        DO I=1,NUMEL
          IX1(I) = IX2(INDEX(I))
        ENDDO
        DO I=1,NUMEL
          IX2(I) = INDEX(I)
        ENDDO
      ENDIF
C-----------------------
C RECHERCHE DES ELEMENTS DE LIST() DANS IX()
C  ALGO < NLIST+NUMEL
C-----------------------
      I=1
      J=1
      DO I=1,NEL
        DO WHILE(LIST(I)>IX1(J).AND.J<NUMEL)
          J=J+1
        ENDDO
        IF(LIST(I)==IX1(J))THEN
          LIST(I)=IX2(J)
        ELSE
           CALL ANCMSG(MSGID=70,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 C1=TYPE,
     .                 I1=ID,
     .                 C2=TITR,
     .                 I2=LIST(I))
           RETURN
        ENDIF
      ENDDO
C-----------------------
C
C-----------------------
      LELT = 0
C
      DO I=1,NEL

        NE=LIST(I)

        IP=IPARTEL(NE)
        NLEV =IPART(10,IP)

        LEVEL    =ELTREE(KLEVTREE,NE)
        IF(LEVEL < 0) LEVEL=-(LEVEL+1)

        LELT=LELT+NSONTREE**(NLEV-LEVEL)

      END DO
C
      NEL=LELT
C-----------------------
      RETURN
 999  CALL FREERR(1)
      CALL MY_EXIT(2)


      RETURN
      END     
!||====================================================================
!||    admlist                ../starter/source/model/remesh/admlist.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    my_exit                ../starter/source/output/analyse/analyse.c
!||--- uses       -----------------------------------------------------
!||    format_mod             ../starter/share/modules1/format_mod.F90
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    reader_old_mod         ../starter/share/modules1/reader_old_mod.F90
!||====================================================================
      SUBROUTINE ADMLIST(NIX     ,IX      ,NUMEL   ,IPARTEL     ,IPART   ,
     .                   KELTREE ,ELTREE  ,KSONTREE,NSONTREE    ,KLEVTREE,
     .                   NLIST   ,MESS    ,IX1     ,IX2         ,INDEX   ,
     .                   KK      ,NEL     ,NELT    ,TYPE        ,ID      ,
     .                   TITR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
      USE FORMAT_MOD , ONLY : FMT_10I
      USE READER_OLD_MOD , ONLY : LINE, IREC
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "remesh_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIX, IX(NIX,*), NUMEL, IPARTEL(*), IPART(LIPART1,*),
     .        KELTREE, ELTREE(KELTREE,*), KSONTREE, NSONTREE, KLEVTREE,
     .        NLIST, IX1(*), IX2(*), INDEX(*), KK, NEL, NELT(*)
      CHARACTER MESS*40
      INTEGER ID
      CHARACTER(LEN=NCHARTITLE) :: TITR,TYPE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LIST(NLIST), IL, JREC, J10(10)
      INTEGER I, J, NOLD, K, IWORK(70000)
      INTEGER LELT, LELT1, LELT2, NE, KE, IP, LEVEL, NLEV,
     .        LELTMP, NELTMP(NSONTREE**(LEVELMAX+1))
C-----------------------------------------------
C CONSTITUTION DE LIST
C-----------------------------------------------
      IL   =0
C
      JREC=IREC
      JREC=JREC+1
      READ(IIN,REC=JREC,ERR=999,FMT='(A)')LINE
      DO WHILE(LINE(1:1)/='/')
        READ(LINE,ERR=999,FMT=FMT_10I) J10
        DO I=1,10
          IF(J10(I)/=0)THEN
            IL=IL+1
            LIST(IL)=J10(I)
          END IF
        ENDDO
        JREC=JREC+1
        READ(IIN,REC=JREC,ERR=999,FMT='(A)')LINE
      ENDDO
C-----------------------
C TRI DE LIST EN ORDRE CROISSANT
C AVEC SUPPRESSION DES No DOUBLES
C-----------------------
      CALL MY_ORDERS(0,IWORK,LIST,INDEX,NLIST,1)
      DO I=1,NLIST
        INDEX(NLIST+I) = LIST(INDEX(I))
      ENDDO
      K=1
      NOLD = INDEX(NLIST+1)
      DO I=1,NLIST
        IF(NOLD/=INDEX(NLIST+I))K=K+1
        LIST(K) = INDEX(NLIST+I)
        NOLD    = INDEX(NLIST+I)
      ENDDO
      NEL=K
C-----------------------
C TRI DE IX EN ORDRE CROISSANT si KK = 0
C-----------------------
      IF(KK==0)THEN
        DO I=1,NUMEL
          IX2(I) = IX(NIX,I)
        ENDDO
        CALL MY_ORDERS(0,IWORK,IX2,INDEX,NUMEL,1)
        DO I=1,NUMEL
          IX1(I) = IX2(INDEX(I))
        ENDDO
        DO I=1,NUMEL
          IX2(I) = INDEX(I)
        ENDDO
      ENDIF
C-----------------------
C RECHERCHE DES ELEMENTS DE LIST() DANS IX()
C  ALGO < NLIST+NUMEL
C-----------------------
      I=1
      J=1
      DO I=1,NEL
        DO WHILE(LIST(I)>IX1(J).AND.J<NUMEL)
          J=J+1
        ENDDO
        IF(LIST(I)==IX1(J))THEN
          LIST(I)=IX2(J)
        ELSE
           CALL ANCMSG(MSGID=70, MSGTYPE=MSGERROR, ANMODE=ANINFO, C1=TYPE, I1=ID, C2=TITR, I2=LIST(I))
           RETURN
        ENDIF
      ENDDO
C-----------------------
C
C-----------------------
      LELT = 0
C
      DO I=1,NEL
        NE=LIST(I)

        IP=IPARTEL(NE)
        NLEV =IPART(10,IP)

        IF(NLEV==0)THEN
          LELT=LELT+1
          NELT(LELT)=NE
        ELSE

          LELTMP   =1
          NELTMP(1)=NE

          LEVEL    =ELTREE(KLEVTREE,NE)
          IF(LEVEL < 0) LEVEL=-(LEVEL+1)

          LELT1=0
          LELT2=LELTMP

          DO WHILE(LEVEL < NLEV)
            DO KE=LELT1+1,LELT2
              DO K=0,NSONTREE-1
                LELTMP=LELTMP+1
                NELTMP(LELTMP)=ELTREE(KSONTREE,NELTMP(KE))+K
              END DO
            END DO

            LELT1=LELT2
            LELT2=LELTMP

            LEVEL=LEVEL+1
          END DO
           
          DO KE=LELT1+1,LELT2
            LELT=LELT+1
            NELT(LELT)=NELTMP(KE)
          END DO

        END IF
      END DO
C
      NEL=LELT
C-----------------------
      RETURN
 999  CALL FREERR(1)
      CALL MY_EXIT(2)


      RETURN
      END     
