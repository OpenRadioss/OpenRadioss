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
Chd|  CHECKRBY                      source/constraints/general/rbody/checkrby.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ANODSET                       source/output/analyse/analyse_node.c
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        KINSET                        source/constraints/general/kinset.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE CHECKRBY(RBY     ,NPBY    ,LPBY    ,ITAB    ,
     2                    IKINE   ,IDDLEVEL,NOM_OPT ,NUMSL   )
C-------------------------------------
C     LECTURE STRUCTURE RIGIDES IFORM8=2
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "analyse_name.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPBY(NNPBY,*), LPBY(*), ITAB(*)
      INTEGER IKINE(*), IDDLEVEL, NUMSL
C     REAL
      my_real
     .   RBY(NRBY,*)
      INTEGER NOM_OPT(LNOPT1,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,L,K,KK,M,N,NSL,NSKEW,IC,
     .   ISPHER,IDIR,P,IG,ID,ICDG,NSL_XTRA,
     .   NRB,NUMSL_TMP
      CHARACTER TITR*nchartitle
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: TABSL
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX,IKINE1
      INTEGER IWORK(70000),IOLD
      INTEGER IFLAGI1,IFLAGDBL,IRB
C-----------------------------------
C   NPBY( 1,N) = main NODE
C   NPBY( 2,N) = NUMBER OF SECND NODES
C   NPBY( 3,N) = ICDG
C   NPBY( 4,N) = ISENS gw117
C   NPBY( 5,N) = FLAG SPHERICAL INERTIA
C   NPBY( 6,N) = IDENTIFICATEUR
C   NPBY( 7,N) = 1 ON(1) OFF(0)
C   NPBY( 8,N) = ISU
C   NPBY( 9,N) = NSKEW
C   NPBY(10,N) = IEXPAMS (AMS - Hidden)
C                 = 1 (default) : AMS expansion ; = 2 (Hidden) : No expansion
C   NPBY(11,N) = IAD => secnd nodes LPBY(IAD+1:IAD+NSN)
C   NPBY(12,N) = RBODY LEVEL
C   NPBY(13,N) = RBODY IFLAG
C   NPBY(14,N) = NUMBER OF XTRA_NODE with Iflag=1
C   NPBY(15,N) = NUMBER OF XTRA_NODE with Iflag=2
C   NPBY(16,N) = NUMBER OF XTRA_NODE with Iflag=3
C=======================================================================
      IF (NUMSL > 0) THEN
        ALLOCATE(TABSL(2,NUMSL))
        ALLOCATE(INDEX(3*NUMSL))
        TABSL=0
        INDEX=0
      END IF
      N=0
      K=0
      KK=0
      NRB = 0
C
      ALLOCATE(IKINE1(3*NUMNOD))
      DO I=1,3*NUMNOD
        IKINE1(I) = 0
      ENDDO
C
      DO N=1,NRBYKIN
        NSL=NPBY(2,N)
        NSL_XTRA=NPBY(14,N)+NPBY(15,N)+NPBY(16,N)
        ISPHER = NPBY(5,N)
        ICDG = NPBY(3,N)
        ID=NOM_OPT(1,N)
c
        DO J=1,NSL-NSL_XTRA
          CALL ANODSET(LPBY(J+K), CHECK_RB_S)
          TABSL(1,J+KK)=ITAB(LPBY(J+K))
          TABSL(2,J+KK)=N
        ENDDO
C
        IF(IDDLEVEL==0)THEN
         IF(IKREM == 0)THEN
          DO J=1,NSL
           DO IDIR=1,6
             CALL KINSET(8,ITAB(LPBY(J+K)),IKINE(LPBY(J+K)),IDIR,0,
     .                   IKINE1(LPBY(J+K)))
           ENDDO
          ENDDO
         ELSE
          DO J=1,NSL
           DO IDIR=1,6
             CALL KINSET(128,ITAB(LPBY(J+K)),IKINE(LPBY(J+K)),IDIR,0,
     .                   IKINE1(LPBY(J+K)))
           ENDDO
          ENDDO
         ENDIF
        ENDIF
C
        K=K+NSL
        KK=KK+NSL-NSL_XTRA
      ENDDO
      NUMSL_TMP=KK
C-------------------------------------
C Bilan secnd nodes doubles (sans les XTRA_NODES qui sont teste avant)
C-------------------------------------
      IF (NRBYKIN > 1) THEN
      IWORK=0
      IFLAGDBL=0
      DO I=1,NUMSL_TMP
        INDEX(I)=I
      END DO
      CALL MY_ORDERS(0,IWORK,TABSL,INDEX,NUMSL_TMP,2)
      IF (NUMSL_TMP > 0) THEN
        IOLD=-1
        DO I=1,NUMSL_TMP
        IF (TABSL(1,INDEX(I))==IOLD) THEN
          IF (IFLAGDBL==0) THEN
            IFLAGI1=I-1
          END IF
          IFLAGDBL=1
        ELSE
          IF (IFLAGDBL/=0) THEN
            DO J=IFLAGI1,I-1
              IRB=TABSL(2,INDEX(J))
              ID=NOM_OPT(1,IRB)
              CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,IRB),LTITR)
              CALL ANCMSG(MSGID=1026,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    PRMOD=MSG_CUMU)
            END DO
            CALL ANCMSG(MSGID=1026,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=TABSL(1,INDEX(IFLAGI1)),
     .                  PRMOD=MSG_PRINT)
            IFLAGDBL=0
          END IF
        END IF
        IOLD=TABSL(1,INDEX(I))     
        END DO
      END IF
      END IF
C
C------------------------------------
      IF(ALLOCATED(TABSL))DEALLOCATE(TABSL)
      IF(ALLOCATED(INDEX))DEALLOCATE(INDEX)
      IF(ALLOCATED(IKINE1)) DEALLOCATE(IKINE1)
C------------------------------------
      RETURN
C
      END SUBROUTINE CHECKRBY
C
