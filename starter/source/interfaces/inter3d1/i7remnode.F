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
Chd|  I7REMNODE                     source/interfaces/inter3d1/i7remnode.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        I25REMNOR                     source/interfaces/inter3d1/i7remnode.F
Chd|        I7REMNODE_BUILD               source/interfaces/inter3d1/i7remnode.F
Chd|        I7REMNODE_INIT                source/interfaces/inter3d1/i7remnode.F
Chd|        MY_EXIT                       source/output/analyse/analyse.c
Chd|        UPGRADE_REMNODE               source/interfaces/interf1/upgrade_remnode.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I7REMNODE(
     1         IREMNODE,NOINT   ,TITR     ,INTBUF_TAB ,NUMNOD ,
     1         X       ,NRTM    ,IRECT    ,NSV        ,NSN    ,
     2         ITAB    ,GAP_S   ,GAP_M    ,GAPMIN     ,GAPMAX ,
     3         GAP_S_L ,GAP_M_L ,IGAP     ,GAP        ,DRAD    ,
     4         NREMNODE,NTY     ,IPARI   ,I_MEM_REM   ,GAPM_MX ,
     5         GAPS_MX ,GAPM_L_MX,GAPS_L_MX ,ILEV     ,NBINFLG ,
     6         MBINFLG ,DGAPLOAD)  
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "vect07_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IREMNODE, NOINT, NSN, NRTM, NUMNOD,IGAP, NREMNODE ,NTY, I_MEM_REM, ILEV
      INTEGER IRECT(4,NRTM),ITAB(*),NSV(NSN),IPARI(*),NBINFLG(*),MBINFLG(*)
      my_real
     .        GAPMIN, GAPMAX, GAP, DRAD, GAPM_MX, GAPS_MX, GAPM_L_MX, GAPS_L_MX
      my_real , INTENT(IN) :: DGAPLOAD
      my_real
     .        X(3,*),GAP_S(*),GAP_M(*),GAP_S_L(*),GAP_M_L(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IFIRST,ILAST,ISELF_IMPACTANT
      INTEGER, DIMENSION(:),ALLOCATABLE ::
     .        KNOD2SEG,NOD2SEG,NODDEL,TAGNOD,ID_NOD,TAGSECND,ITAGSEG,NOD2EXPAND
      my_real
     .        MINSEG
      my_real, DIMENSION(:),ALLOCATABLE ::
     .        DIST1,GAPV,GAPSECND,GAP_S_L_TMP
C-----------------------------------------------
C
      ALLOCATE(KNOD2SEG(NUMNOD+1),NOD2SEG(4*NRTM),NODDEL(NUMNOD),
     .         TAGNOD(NUMNOD),ID_NOD(NUMNOD),NOD2EXPAND(NUMNOD),ITAGSEG(NRTM))
      ALLOCATE(DIST1(NUMNOD),GAPV(NUMNOD),TAGSECND(NUMNOD),
     .         GAPSECND(NUMNOD),GAP_S_L_TMP(NUMNOD))
C
      KNOD2SEG(1:NUMNOD+1) = 0
      TAGSECND (1:NUMNOD)   = 0
      ID_NOD  (1:NUMNOD)   = 0
      NODDEL  (1:NUMNOD)   = 0
      TAGNOD  (1:NUMNOD)   = 0
      NOD2EXPAND(1:NUMNOD) = 0
      NOD2SEG (1:4*NRTM)   = 0
      ITAGSEG (1:NRTM)     = 0
      GAPV    (1:NUMNOD)   = ZERO
      GAPSECND (1:NUMNOD)   = ZERO
      DIST1   (1:NUMNOD)   = EP30
C
C-----------------------------------------------
      CALL I7REMNODE_INIT(ISELF_IMPACTANT,NTY  ,
     1         X       ,NRTM    ,IRECT    ,NSV     ,NSN     ,NUMNOD ,
     2         ITAB    ,GAP_S   ,GAP_M    ,GAPMIN  ,GAPMAX  ,
     3         GAP_S_L ,GAP_M_L ,IGAP     ,INTBUF_TAB%KREMNODE,INTBUF_TAB%REMNODE ,
     4         GAP     ,DRAD    ,NREMNODE ,ILEV    ,NBINFLG ,
     5         MBINFLG ,IPARI   ,I_MEM_REM,GAPM_MX ,GAPS_MX ,
     6         GAPM_L_MX ,GAPS_L_MX ,KNOD2SEG,NOD2SEG,TAGSECND,
     7         GAPSECND   ,GAP_S_L_TMP,MINSEG )  
C
      IF(ISELF_IMPACTANT==0) RETURN
C
C-----------------------------------------------
      IFIRST=1
      ILAST =NRTM
      I_MEM_REM=1
      DO WHILE(I_MEM_REM==1)
        I_MEM_REM=0
        CALL I7REMNODE_BUILD(IFIRST,ILAST,
     1         X       ,NRTM    ,IRECT    ,NSV     ,NSN    ,
     2         ITAB    ,GAP_S   ,GAP_M    ,GAPMIN  ,GAPMAX ,
     3         GAP_S_L ,GAP_M_L ,IGAP     ,INTBUF_TAB%KREMNODE,INTBUF_TAB%REMNODE ,
     4         GAP     ,DRAD    ,NREMNODE ,NOD2EXPAND,
     5         I_MEM_REM,GAPM_MX ,GAPS_MX ,
     6         GAPM_L_MX ,GAPS_L_MX ,KNOD2SEG,NOD2SEG,TAGSECND,
     7         GAPSECND ,NODDEL  ,TAGNOD   ,ID_NOD   ,DIST1  ,
     8         GAPV    ,GAP_S_L_TMP,ITAGSEG ,MINSEG  ,DGAPLOAD)   
C
        IF (I_MEM_REM == 1) THEN
          NREMNODE = NREMNODE + 5*NRTM
          IF(NREMNODE < 0)THEN
            IF(NTY == 7)THEN
              CALL ANCMSG(MSGID=1736,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANSTOP,
     .                I1=NOINT,
     .                C1=TITR)
            ELSEIF(NTY ==25)THEN
              CALL ANCMSG(MSGID=1737,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANSTOP,
     .                I1=NOINT,
     .                C1=TITR)
            END IF
            DO I=IFIRST,ILAST
C              end Starter properly without restart file.
               INTBUF_TAB%KREMNODE(I+1)=INTBUF_TAB%KREMNODE(IFIRST) 
            END DO
            IREMNODE = IREMNODE + 1
            CALL MY_EXIT(2)
          END IF
          CALL UPGRADE_REMNODE(IPARI,NREMNODE,INTBUF_TAB,NTY)
        ENDIF
      END DO
      IREMNODE = IREMNODE + 1
C-----------------------------------------------
      IF(NTY==25)THEN
        CALL I25REMNOR(NRTM    ,IRECT    ,NSV     ,NSN    ,NUMNOD ,
     2                 INTBUF_TAB%KREMNODE,INTBUF_TAB%REMNODE  ,INTBUF_TAB%KREMNOR ,
     .                                                   INTBUF_TAB%REMNOR ,IPARI  ,
     3                 TAGSECND )
      END IF  
C-----------------------------------------------
      DEALLOCATE(KNOD2SEG,NOD2SEG,NODDEL,ID_NOD,TAGNOD,ITAGSEG)
      DEALLOCATE(DIST1,GAPV,TAGSECND,GAPSECND,GAP_S_L_TMP)
C-----------------------------------------------
      RETURN
      END  
Chd|====================================================================
Chd|  I7REMNODE_INIT                source/interfaces/inter3d1/i7remnode.F
Chd|-- called by -----------
Chd|        I7REMNODE                     source/interfaces/inter3d1/i7remnode.F
Chd|-- calls ---------------
Chd|        BITGET                        source/interfaces/inter3d1/bitget.F
Chd|====================================================================
      SUBROUTINE I7REMNODE_INIT(ISELF_IMPACTANT,NTY    ,
     1         X       ,NRTM    ,IRECT    ,NSV     ,NSN    ,NUMNOD ,
     2         ITAB    ,GAP_S   ,GAP_M    ,GAPMIN  ,GAPMAX ,
     3         GAP_S_L ,GAP_M_L ,IGAP     ,KREMNODE,REMNODE ,
     4         GAP     ,DRAD    ,NREMNODE ,ILEV    ,NBINFLG ,
     5         MBINFLG ,IPARI   ,I_MEM_REM,GAPM_MX ,GAPS_MX ,
     6         GAPM_L_MX ,GAPS_L_MX ,KNOD2SEG,NOD2SEG,TAGSECND,
     7         GAPSECND   ,GAP_S_L_TMP,MINSEG )  
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect07_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISELF_IMPACTANT, NTY, NSN, NRTM, NUMNOD,IGAP, NREMNODE , I_MEM_REM, ILEV
      INTEGER IRECT(4,*),ITAB(*),NSV(*),KREMNODE(*),REMNODE(*),
     .        IPARI(*),KNOD2SEG(*),NOD2SEG(4*NRTM),TAGSECND(*),NBINFLG(*),MBINFLG(*)
      my_real
     .        GAPMIN, GAPMAX, GAP, DRAD, GAPM_MX, GAPS_MX, GAPM_L_MX, GAPS_L_MX, MINSEG
      my_real
     .        X(3,*),GAP_S(*),GAP_M(*),GAP_S_L(*),GAP_M_L(*), GAPSECND(*), GAP_S_L_TMP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,N,CPT,KMAX,IMS1,IMS2,ISS1,ISS2
      INTEGER, DIMENSION(:),ALLOCATABLE :: TAGNOD
C-----------------------------------------------
      INTEGER BITGET
      EXTERNAL BITGET
C-----------------------------------------------
c Build inverse connectivity for segments
C-----------------------------------------------
C
C     NOD2SEG(1:4*NRTM)    = 0
C     KNOD2SEG(1:NUMNOD+1) = 0
C     TAGSECND(1:NUMNOD)    = 0
C     GAPSECND(1:NUMNOD)    = ZERO
C
C
      DO I=1,NSN
        TAGSECND(NSV(I)) = I
      ENDDO
C
      MINSEG = EP30
C
      ISELF_IMPACTANT = 0
      IF(NTY /= 25 .OR. (NTY == 25 .AND. ILEV /=2))THEN
        DO I=1,NRTM
          DO J=1,4
            IF( TAGSECND(IRECT(J,I))  /= 0 ) THEN
              ISELF_IMPACTANT = 1
            END IF
          ENDDO
        END DO
      ELSE ! NTY == 25 and S1-S2
        DO I=1,NRTM
          DO J=1,4
            N = TAGSECND(IRECT(J,I))
            IF( N  /= 0 ) THEN
              IMS1 = BITGET(MBINFLG(I),0)
              IMS2 = BITGET(MBINFLG(I),1)
              ISS1 = BITGET(NBINFLG(N),0)
              ISS2 = BITGET(NBINFLG(N),1)
              IF(((IMS1 == 1 .and. ISS2==1).or.
     .            (IMS2 == 1 .and. ISS1==1)))THEN
                ISELF_IMPACTANT = 1
              END IF
            END IF
          ENDDO
        END DO
      END IF
C
      IF (ISELF_IMPACTANT == 0) RETURN
C
      DO I=1,NRTM
        IF( IRECT(3,I) .NE. IRECT(4,I) ) THEN 

         MINSEG = MIN( MINSEG,
     .               (X(1,IRECT(1,I))-X(1,IRECT(2,I)))**2 + 
     .               (X(2,IRECT(1,I))-X(2,IRECT(2,I)))**2 +
     .               (X(3,IRECT(1,I))-X(3,IRECT(2,I)))**2 ,
     .               (X(1,IRECT(2,I))-X(1,IRECT(3,I)))**2 + 
     .               (X(2,IRECT(2,I))-X(2,IRECT(3,I)))**2 +
     .               (X(3,IRECT(2,I))-X(3,IRECT(3,I)))**2 ,
     .               (X(1,IRECT(3,I))-X(1,IRECT(4,I)))**2 + 
     .               (X(2,IRECT(3,I))-X(2,IRECT(4,I)))**2 +
     .               (X(3,IRECT(3,I))-X(3,IRECT(4,I)))**2 ,
     .               (X(1,IRECT(4,I))-X(1,IRECT(1,I)))**2 + 
     .               (X(2,IRECT(4,I))-X(2,IRECT(1,I)))**2 +
     .               (X(3,IRECT(4,I))-X(3,IRECT(1,I)))**2 )
        ELSEIF( IRECT(3,I) == IRECT(4,I) )THEN 

         MINSEG = MIN( MINSEG,
     .               (X(1,IRECT(1,I))-X(1,IRECT(2,I)))**2 + 
     .               (X(2,IRECT(1,I))-X(2,IRECT(2,I)))**2 +
     .               (X(3,IRECT(1,I))-X(3,IRECT(2,I)))**2 ,
     .               (X(1,IRECT(2,I))-X(1,IRECT(3,I)))**2 + 
     .               (X(2,IRECT(2,I))-X(2,IRECT(3,I)))**2 +
     .               (X(3,IRECT(2,I))-X(3,IRECT(3,I)))**2 ,
     .               (X(1,IRECT(3,I))-X(1,IRECT(1,I)))**2 + 
     .               (X(2,IRECT(3,I))-X(2,IRECT(1,I)))**2 +
     .               (X(3,IRECT(3,I))-X(3,IRECT(1,I)))**2 )

        ENDIF
      ENDDO
      MINSEG  = SQRT(MINSEG)
C
      DO I=1,NRTM
        CPT  = 0
        KMAX = 4
        IF(IRECT(KMAX,I) == 0   .OR. 
     .    IRECT(3,I) == IRECT(4,I) ) KMAX = 3
        DO K=1,KMAX
          IF(TAGSECND(IRECT(K,I))  /= 0) CPT = CPT + 1
        END DO
        IF (CPT .NE. 0 ) THEN
          DO K=1,KMAX
            N = IRECT(K,I)
            KNOD2SEG(N) = KNOD2SEG(N) + 1
          END DO
        ENDIF
      END DO
C
      DO I=1,NUMNOD
        KNOD2SEG(I+1) = KNOD2SEG(I+1) + KNOD2SEG(I)
      END DO
C
      DO N=NUMNOD,1,-1
        KNOD2SEG(N+1)=KNOD2SEG(N)
      END DO
      KNOD2SEG(1)=0
C
      DO I=1,NRTM
        KMAX = 4
        CPT = 0
        IF(IRECT(KMAX,I) == 0   .OR. 
     .      IRECT(3,I) == IRECT(4,I) ) KMAX = 3
        DO K=1,KMAX
          IF(TAGSECND(IRECT(K,I))  /= 0) CPT = CPT + 1
        END DO
        IF (CPT .NE. 0) THEN
          DO K=1,KMAX
            N = IRECT(K,I)
            KNOD2SEG(N) = KNOD2SEG(N) + 1
            NOD2SEG(KNOD2SEG(N)) = I
          END DO
        ENDIF
      END DO
C
      DO N=NUMNOD,1,-1
        KNOD2SEG(N+1)=KNOD2SEG(N)
      END DO
      KNOD2SEG(1)=0
C-----------------------------------------------
C  Searching Algorith Connected nodes : D < SQRT(2.) * GAP
C-----------------------------------------------
      IF(IGAP >= 1)THEN
        DO I=1,NSN
          GAPSECND(NSV(I)) = GAP_S(I)
        ENDDO
      ENDIF
C-----------------------------------------------
      IF(IGAP==3)THEN
        DO I=1,NSN
          GAP_S_L_TMP(NSV(I)) = GAP_S_L(I)
        ENDDO
      END IF
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  I7REMNODE_BUILD               source/interfaces/inter3d1/i7remnode.F
Chd|-- called by -----------
Chd|        I7REMNODE                     source/interfaces/inter3d1/i7remnode.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I7REMNODE_BUILD(IFIRST,ILAST,
     1         X       ,NRTM    ,IRECT    ,NSV     ,NSN    ,
     2         ITAB    ,GAP_S   ,GAP_M    ,GAPMIN  ,GAPMAX ,
     3         GAP_S_L ,GAP_M_L ,IGAP     ,KREMNODE,REMNODE ,
     4         GAP     ,DRAD    ,NREMNODE ,NOD2EXPAND,
     5         I_MEM_REM,GAPM_MX ,GAPS_MX ,
     6         GAPM_L_MX ,GAPS_L_MX ,KNOD2SEG,NOD2SEG,TAGSECND,
     7         GAPSECND ,NODDEL  ,TAGNOD   ,ID_NOD    ,DIST1  ,
     8         GAPV    ,GAP_S_L_TMP,ITAGSEG,MINSEG    ,DGAPLOAD)  
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect07_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFIRST, ILAST, NSN, NRTM, IGAP, NREMNODE , I_MEM_REM
      INTEGER IRECT(4,*),ITAB(*),NSV(*),KREMNODE(*),REMNODE(*)
      INTEGER KNOD2SEG(*),NOD2SEG(4*NRTM),NODDEL(*),TAGNOD(*),ID_NOD(*),NOD2EXPAND(*),
     .        TAGSECND(*),ITAGSEG(NRTM)
       my_real
     .        GAPMIN, GAPMAX, GAP, DRAD, GAPM_MX, GAPS_MX, GAPM_L_MX, GAPS_L_MX, MINSEG
      my_real , INTENT(IN) :: DGAPLOAD
      my_real
     .        X(3,*),GAP_S(*),GAP_M(*),GAP_S_L(*),GAP_M_L(*)
      my_real 
     .        DIST1(*), GAPV(*), GAPSECND(*), GAP_S_L_TMP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,SEG,ISEG,LEVEL,CPT,NBSEG,SEG1,L,CPT1,N,
     .        NE,CPT_TOTAL,NS,LREMNORMAX
      INTEGER LISTSEG(NRTM),LISTSEGTMP(NRTM),LISTSEGTOTAL(NRTM),CPTOPER
      INTEGER :: JMAX,KMAX
      my_real
     .        MINDIST,DMAX,DIST,X0,Y0,Z0,X1,Y1,Z1,LMAX
C-----------------------------------------------
C  Searching Algorith Connected nodes : D < SQRT(2.) * GAP
C-----------------------------------------------
      CPTOPER   = 0
      CPT       = 0
      CPT_TOTAL = 0
C
      DO I=IFIRST,ILAST

        IF(IGAP==0)THEN
          DMAX  = SQRT(TWO) * MAX(GAP+DGAPLOAD,DRAD)
        ELSEIF(IGAP==1 .OR. IGAP==2) THEN
          DMAX  = SQRT(TWO) * MAX(GAP_M(I)+GAPS_MX+DGAPLOAD,DRAD)
        ELSEIF(IGAP==3) THEN
          DMAX  = SQRT(TWO) * MAX(MIN(GAP_M(I)+GAPS_MX,GAP_M_L(I)+GAPS_L_MX)+DGAPLOAD,DRAD)
        END IF

        CPTOPER = 0
        LEVEL   = 1
        SEG     = I
        ITAGSEG(SEG) = LEVEL
        NBSEG     =1
        MINDIST   = ZERO
        JMAX      = 4
        CPT_TOTAL = 0
        IF((IRECT(JMAX,SEG) == 0) .OR. IRECT(3,SEG) == IRECT(4,SEG) ) JMAX = 3
        DO J=1,JMAX
          TAGNOD(IRECT(J,SEG)) = 1
          DIST1(IRECT(J,SEG)) = ZERO
        ENDDO
        LISTSEG(1)=SEG
        DO WHILE( (MINDIST + MINSEG) <= DMAX .AND. NBSEG /= 0)
          LEVEL   = LEVEL + 1
          MINDIST = EP30
          CPT     = 0

          DO ISEG=1,NBSEG
            SEG  = LISTSEG(ISEG)

            JMAX = 4
            IF((IRECT(JMAX,SEG) == 0) .OR. IRECT(3,SEG) == IRECT(4,SEG) ) JMAX = 3

            TAGNOD(IRECT(1:JMAX,SEG))=2
            
            DO J=1,JMAX
C 
C             If we already started from this node AT THIS LEVEL
C             <=> all paths from this node have been explored yet AT THIS LEVEL
              IF(NOD2EXPAND(IRECT(J,SEG))/=0) CYCLE
C 
              NOD2EXPAND(IRECT(J,SEG))=1
              DO K=KNOD2SEG(IRECT(J,SEG))+1,KNOD2SEG(IRECT(J,SEG)+1)
                SEG1 = NOD2SEG(K)

                KMAX = 4
                IF((IRECT(KMAX,SEG1) == 0) .OR. IRECT(3,SEG1) == IRECT(4,SEG1) ) KMAX = 3

                IF(ITAGSEG(SEG1) == 0 .OR. ITAGSEG(SEG1) == LEVEL) THEN
                  IF(ITAGSEG(SEG1) == 0)THEN
                    CPT = CPT + 1
                    LISTSEGTMP(CPT)=SEG1
                  ENDIF
                  ITAGSEG(SEG1)=LEVEL
c 
                  DO L=1,KMAX
                    
                    IF(TAGSECND(IRECT(L,SEG1))== 0 .OR.
     .                 TAGNOD(IRECT(L,SEG1)) == 2)CYCLE

                    DIST1(IRECT(L,SEG1))=MIN(DIST1(IRECT(L,SEG1)),
     .                DIST1(IRECT(J,SEG))+
     .                  SQRT((X(1,IRECT(L,SEG1)) - X(1,IRECT(J,SEG)))**2 + 
     .                       (X(2,IRECT(L,SEG1)) - X(2,IRECT(J,SEG)))**2 +
     .                       (X(3,IRECT(L,SEG1)) - X(3,IRECT(J,SEG)))**2 ))
                    MINDIST=MIN(MINDIST,DIST1(IRECT(L,SEG1)))
                    IF(TAGNOD(IRECT(L,SEG1)) == 0) THEN
                      CPTOPER = CPTOPER + 1
                      TAGNOD(IRECT(L,SEG1)) = 1
                      ID_NOD(CPTOPER)=IRECT(L,SEG1)
                    ENDIF 

                    IF(IGAP==1 .OR. IGAP==2)THEN
                      GAPV(IRECT(L,SEG1))=GAPSECND(IRECT(L,SEG1))+GAP_M(I)
                      GAPV(IRECT(L,SEG1))=MIN(GAPMAX,GAPV(IRECT(L,SEG1)))
                      GAPV(IRECT(L,SEG1))=MAX(GAPMIN,GAPV(IRECT(L,SEG1)))
                      GAPV(IRECT(L,SEG1))=MAX(DRAD,GAPV(IRECT(L,SEG1))+DGAPLOAD)
                    ELSEIF(IGAP==3)THEN
                      GAPV(IRECT(L,SEG1))=GAPSECND(IRECT(L,SEG1))+GAP_M(I)
                      GAPV(IRECT(L,SEG1))=
     .                   MIN(GAP_S_L_TMP(IRECT(L,SEG1))+GAP_M_L(I),GAPV(I))
                      GAPV(IRECT(L,SEG1))=MIN(GAPMAX,GAPV(IRECT(L,SEG1)))
                      GAPV(IRECT(L,SEG1))=MAX(GAPMIN,GAPV(IRECT(L,SEG1)))
                      GAPV(IRECT(L,SEG1))=MAX(DRAD,GAPV(IRECT(L,SEG1))+DGAPLOAD)
                    END IF

                  ENDDO
                ENDIF
                 
              ENDDO 
C       
            ENDDO

            TAGNOD(IRECT(1:4,SEG))=1

          ENDDO
C
C         Reset Starting Nodes
          DO ISEG=1,NBSEG
            SEG  = LISTSEG(ISEG)
            JMAX = 4
            IF((IRECT(JMAX,SEG) == 0) .OR. IRECT(3,SEG) == IRECT(4,SEG) ) JMAX = 3
            DO J=1,JMAX
              NOD2EXPAND(IRECT(J,SEG))=0
            END DO
          END DO
C
          NBSEG     = CPT
          IF(NBSEG ==0)EXIT
          DO J=1,CPT
            LISTSEG(J)    =LISTSEGTMP(J)
            LISTSEGTMP(J) = 0
            LISTSEGTOTAL(J+CPT_TOTAL) = LISTSEG(J)
          ENDDO
          CPT_TOTAL = CPT_TOTAL + CPT

        ENDDO
        IF (LEVEL == 1) THEN
          KREMNODE(I+1) = KREMNODE(I)
          CYCLE
        ENDIF
C
        DIST1(IRECT(1,I)) = EP30
        DIST1(IRECT(2,I)) = EP30
        DIST1(IRECT(3,I)) = EP30
        DIST1(IRECT(4,I)) = EP30
C
        CPT1 = 0 
        IF(IGAP==0)THEN
          DO L=1,CPTOPER 
            IF(DIST1(ID_NOD(L)) <= DMAX)THEN
              CPT1 = CPT1 + 1
              NODDEL(CPT1) = ID_NOD(L)
            ENDIF
          ENDDO
        ELSE
          DO L=1,CPTOPER 
            IF(DIST1(ID_NOD(L)) <= SQRT(TWO)*GAPV(ID_NOD(L)))THEN
              CPT1 = CPT1 + 1
              NODDEL(CPT1) = ID_NOD(L)
            ENDIF
          ENDDO
        END IF
        IF(KREMNODE(I)+CPT1.GT.NREMNODE) THEN
C
C         Go to UPGRADE_REMNODE  ...
          IFIRST=I
C
          DO L=1,CPTOPER
            DIST1(ID_NOD(L)) = EP30
          ENDDO
          DO L=1,CPT_TOTAL
            TAGNOD(IRECT(1:4, LISTSEGTOTAL(L))) = 0
            ITAGSEG(LISTSEGTOTAL(L)) = 0
            LISTSEGTOTAL(L) = 0
          ENDDO
          TAGNOD(IRECT(1:4,I)) = 0
          ITAGSEG(I) = 0

          I_MEM_REM=1
          RETURN
        ENDIF
C
        KREMNODE(I+1) = KREMNODE(I)+CPT1
        DO L=1,CPT1
          REMNODE(KREMNODE(I)+L) = NODDEL(L)
        ENDDO
        DO L=1,CPTOPER
          DIST1(ID_NOD(L)) = EP30
        ENDDO
        DO L=1,CPT_TOTAL
          TAGNOD(IRECT(1:4, LISTSEGTOTAL(L))) = 0
          ITAGSEG(LISTSEGTOTAL(L)) = 0
          LISTSEGTOTAL(L)          = 0
        ENDDO
        TAGNOD(IRECT(1:4,I)) = 0
        ITAGSEG(I)           = 0

      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  I25REMNOR                     source/interfaces/inter3d1/i7remnode.F
Chd|-- called by -----------
Chd|        I7REMNODE                     source/interfaces/inter3d1/i7remnode.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I25REMNOR(
     1         NRTM    ,IRECT    ,NSV     ,NSN    ,NUMNOD ,
     2         KREMNODE,REMNODE  ,KREMNOR ,REMNOR  ,IPARI ,
     3         TAGSECND )  
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect07_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NRTM, NUMNOD
      INTEGER IRECT(4,*),NSV(*),KREMNODE(*),REMNODE(*),
     .        KREMNOR(*),REMNOR(*),IPARI(*),TAGSECND(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,N,NS,LREMNORMAX
C-----------------------------------------------
c T25 :: Build inverse connectivity for vertices
C-----------------------------------------------
C
      DO I=1,NRTM
        K = KREMNODE(I)+1
        L = KREMNODE(I+1)         
        DO J=K,L
           NS = TAGSECND(REMNODE(J))
           KREMNOR(NS) = KREMNOR(NS)+1
        ENDDO
      ENDDO
C
      DO N=1,NSN
         KREMNOR(N+1) = KREMNOR(N+1) + KREMNOR(N)
      END DO
C
      DO N=NSN,1,-1
         KREMNOR(N+1)=KREMNOR(N)
      END DO
      KREMNOR(1)=0
C
      DO I=1,NRTM
        K = KREMNODE(I)+1
        L = KREMNODE(I+1)         
        DO J=K,L
           N = TAGSECND(REMNODE(J))
           KREMNOR(N) = KREMNOR(N)+1
           REMNOR(KREMNOR(N)) = I
        ENDDO
      ENDDO
C
      DO N=NSN,1,-1
        KREMNOR(N+1)=KREMNOR(N)
      END DO
      KREMNOR(1)=0
C
C----- Compute maximum number of MAIN segments banned for all SECONDARY nodes---
      LREMNORMAX = 0
      DO N=1,NSN
         L = KREMNOR(N+1)-KREMNOR(N)
         IF( L.GT.LREMNORMAX) THEN
           LREMNORMAX = L
         ENDIF
      ENDDO
      IPARI(82) = LREMNORMAX

      RETURN
      END
Chd|====================================================================
Chd|  REMN_I2                       source/interfaces/inter3d1/i7remnode.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ADDINT2M                      source/interfaces/inter3d1/i7remnode.F
Chd|        ADDINT2S                      source/interfaces/inter3d1/i7remnode.F
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ANOTHERN2                     source/interfaces/inter3d1/i7remnode.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE REMN_I2(IPARI   ,INTBUF_TAB   ,ITAB, NOM_OPT,NREMOV,IFLAG  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), ITAB(*),NREMOV(*)
      INTEGER NOM_OPT(LNOPT1,*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr15_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,NTY
      INTEGER ILEV,II,J,NMN,NSN,NRTS,NRTM,
     .        NLINS,NLINM,IWOUT,INCOM,NM,N2,IFLAG,NRE,ip,IACT,
     .        IF7,IF24,IF25,NN2
      INTEGER, DIMENSION(:),ALLOCATABLE :: TAGN,TAGD
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR

C-----------------------------------------------
C----like Irem_gap of int7, creat list of SECONDARY nodes to be removed per M_seg
C---------IFLAG=0 : DIM
      ALLOCATE(TAGN(NUMNOD),TAGD(NUMNOD))
      TAGN(1:NUMNOD)=0
C----------only during dimensioning
      IF (IFLAG==0) THEN
       IACT=0  
        DO N=1,NINTER
         NTY=IPARI(7,N)
         IF7 =IPARI(54,N)
         IF24=IPARI(63,N)
         IF25=IPARI(83,N)
         IF(NTY==7 .AND. IF7>0 )THEN
          IACT=1
          CYCLE
         ENDIF
         IF(NTY==24 .AND. IF24>0 )THEN
          IACT=1
          CYCLE
         ENDIF
         IF(NTY==25 .AND. IF25>0 )THEN
          IACT=1
          CYCLE
         ENDIF
        ENDDO
       IF (IACT==0) THEN
        DO N=1,NINTER
         NREMOV(N) = 0
        ENDDO
        RETURN
       END IF
      END IF !(IFLAG==0) THEN
C-------tag int2 nodes------  
       DO N=1,NINTER
         NTY=IPARI(7,N)
         IF(NTY.EQ.2)THEN
          NRTS  =IPARI(3,N)
          NRTM  =IPARI(4,N)
          NSN   =IPARI(5,N)
          NMN   =IPARI(6,N)
          ILEV  =IPARI(20,N)
C
          DO II=1,NSN        
            J=INTBUF_TAB(N)%NSV(II)
            TAGN(J)=-N
          ENDDO
          DO II=1,NMN        
            J=INTBUF_TAB(N)%MSR(II)
            IF (TAGN(J)>0) THEN
             TAGN(J)=N+NINTER
            ELSE
             TAGN(J)=N
            END IF
          ENDDO
         ENDIF
       ENDDO
C-----
       DO N=1,NINTER
        NTY=IPARI(7,N)
        NSN   =IPARI(5,N)
        NRTS  =IPARI(3,N)
        NRTM  =IPARI(4,N)
        IF7   =IPARI(54,N)
        IF24  =IPARI(63,N)
        IF25  =IPARI(83,N)
        IF (IFLAG==0) NREMOV(N) = 0

        ID=NOM_OPT(1,N)
        CALL FRETITL2(TITR,
     .       NOM_OPT(LNOPT1-LTITR+1,N),LTITR)
C----- --           
        IF((NTY==7.AND.IF7>0).OR.(NTY==24.AND.IF24>0).OR.(NTY==25.AND.IF25>0))THEN
          IF (IFLAG==0) NREMOV(N) = IPARI(62,N)
          IF (IFLAG==1 .AND. IPARI(63,N) /= 2 ) CYCLE
          IF (IFLAG==1) INTBUF_TAB(N)%KREMNODE(1) = 0
          DO II=1,NRTM
           IF (IFLAG==1) INTBUF_TAB(N)%KREMNODE(II+1) =INTBUF_TAB(N)%KREMNODE(II)
           TAGD(1:NUMNOD)=0
           DO J=1,4
            NM = INTBUF_TAB(N)%IRECTM(4*(II-1)+J)
C--------if it's a MAIN node (type2) ->add SECONDARY node(tagged) in the list
            IF (TAGN(NM)>0) THEN            
             N2 = TAGN(NM)
             IF (N2 >NINTER) THEN
              NN2=N2-NINTER
              CALL ADDINT2S(NN2     ,INTBUF_TAB  ,IPARI   ,II      ,NM     ,
     1                 NREMOV(N),INTBUF_TAB(N)%REMNODE,INTBUF_TAB(N)%KREMNODE,
     2                 TAGD,IFLAG)    
C-----------Another N2 (NN2 will be updated after ANOTHERN2 )
              CALL ANOTHERN2(NN2     ,NINTER  ,INTBUF_TAB  ,IPARI   ,NM     )
c              IF (N2==0) Message out                 
              CALL ADDINT2S(NN2     ,INTBUF_TAB  ,IPARI   ,II      ,NM     ,
     1                 NREMOV(N),INTBUF_TAB(N)%REMNODE,INTBUF_TAB(N)%KREMNODE,
     2                 TAGD,IFLAG)    
             ELSE
              CALL ADDINT2S(N2     ,INTBUF_TAB  ,IPARI   ,II      ,NM     ,
     1                 NREMOV(N),INTBUF_TAB(N)%REMNODE,INTBUF_TAB(N)%KREMNODE,
     2                 TAGD,IFLAG)    
             END IF     
C--------if it's a SECONDARY node (type2) ->add MAIN nodes(tagged) in the list
            ELSEIF (TAGN(NM)<0) THEN
             N2 = -TAGN(NM)
              CALL ADDINT2M(N2     ,INTBUF_TAB  ,IPARI   ,II      ,NM     ,
     1                   NREMOV(N),INTBUF_TAB(N)%REMNODE,INTBUF_TAB(N)%KREMNODE,
     2                   TAGD,IFLAG)    
            END IF
           END DO !DO J=1,4
          END DO 
C------------update of IPARI(62,N) is out of subroutine    
         IF (IFLAG==0.AND.(NREMOV(N)>IPARI(62,N))) THEN
          NRE = NREMOV(N)-IPARI(62,N)
             CALL ANCMSG(MSGID=1053,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=NRE,
     .                   I3=NOM_OPT(1,N2))
         END IF
        END IF!(NTY==7.OR.NTY==24.OR.NTY==25)
       END DO
      DEALLOCATE(TAGN,TAGD)
C----
      RETURN
      END
Chd|====================================================================
Chd|  ADDINT2S                      source/interfaces/inter3d1/i7remnode.F
Chd|-- called by -----------
Chd|        REMN_I2                       source/interfaces/inter3d1/i7remnode.F
Chd|-- calls ---------------
Chd|        ADD_NS                        source/interfaces/inter3d1/i7remnode.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE ADDINT2S(N2     ,INTBUF_TAB  ,IPARI   ,MI      ,NM     ,
     2                    NREMOV ,IREMOV  ,IADREM ,TAGD,IFLAG   )                  
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N2, MI, NM, NREMOV,IPARI(NPARI,*),
     .        IREMOV(*),IFLAG,IADREM(*),TAGD(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C----IFLAG=0 -> dim   , IREMOV : Global node num. (SECONDARY) 
C     REAL
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J  ,NEW,NSN,IADA,ip
      INTEGER N, NTY
C-----------------------------------------------
        NEW = NREMOV
        NSN   =IPARI(5,N2)
        IF (IFLAG==1) IADA=IADREM(MI)+1
        CALL ADD_NS(NSN,INTBUF_TAB(N2)%IRECTM,INTBUF_TAB(N2)%NSV,
     1              INTBUF_TAB(N2)%IRTLM,NREMOV,IREMOV,IADA,NM,TAGD,IFLAG   )                  
        NEW = NREMOV-NEW
        IF (IFLAG==1) IADREM(MI+1)=IADREM(MI+1)+NEW
C----
      RETURN
      END
Chd|====================================================================
Chd|  ADD_NS                        source/interfaces/inter3d1/i7remnode.F
Chd|-- called by -----------
Chd|        ADDINT2S                      source/interfaces/inter3d1/i7remnode.F
Chd|-- calls ---------------
Chd|        INSERT_A                      source/interfaces/inter3d1/i7remnode.F
Chd|        INTAB                         source/interfaces/inter3d1/i24tools.F
Chd|====================================================================
      SUBROUTINE ADD_NS(NSN    ,IRECT   ,NSV    ,IRTL     ,NREMOV  ,
     2                 IREMOV  ,IADA    ,NM     ,TAGD,IFLAG    )                  
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4,*),NSV(*),IRTL(*), NM, NREMOV,
     .        IREMOV(*),IFLAG,NSN,IADA,TAGD(*)
C----IFLAG=0 -> dim   , IREMOV : Global node num. (SECONDARY) 
C     REAL
C-----------------------------------------------
C   External function
C-----------------------------------------------
      LOGICAL INTAB
      EXTERNAL INTAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II ,I,J  ,IL, L
C-----------------------------------------------
        DO II=1,NSN
          I=NSV(II)
          L=IRTL(II)
          IF (TAGD(I)>0) CYCLE
          IF (INTAB(4,IRECT(1,L),NM)) THEN
           IF (IFLAG==0) THEN
            NREMOV =NREMOV+1
            TAGD(I)=1
           ELSE
            CALL INSERT_A(NREMOV,IREMOV,I  ,IADA)
            TAGD(I)=1
           END IF 
          ENDIF
        END DO !II=1,NSN
C----
      RETURN
      END
Chd|====================================================================
Chd|  ADDINT2M                      source/interfaces/inter3d1/i7remnode.F
Chd|-- called by -----------
Chd|        REMN_I2                       source/interfaces/inter3d1/i7remnode.F
Chd|-- calls ---------------
Chd|        ADD_NM                        source/interfaces/inter3d1/i7remnode.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE ADDINT2M(N2     ,INTBUF_TAB  ,IPARI   ,MI      ,NS     ,
     2                    NREMOV ,IREMOV  ,IADREM ,TAGD,IFLAG   )                  
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N2, MI, NS, NREMOV,IPARI(NPARI,*),
     .        IREMOV(*),IFLAG,IADREM(*),TAGD(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C----IFLAG=0 -> dim   , IREMOV : Global node num. (SECONDARY) 
C     REAL
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J  ,NEW,NSN,IADA
      INTEGER N, NTY
C-----------------------------------------------
        NEW = NREMOV
        NSN   =IPARI(5,N2)
        IF (IFLAG==1) IADA=IADREM(MI)+1
        CALL ADD_NM(NSN,INTBUF_TAB(N2)%IRECTM,INTBUF_TAB(N2)%NSV,
     1              INTBUF_TAB(N2)%IRTLM,NREMOV,IREMOV,IADA,NS,TAGD,IFLAG  )                  
        NEW = NREMOV-NEW
        IF (IFLAG==1)IADREM(MI+1)=IADREM(MI+1)+NEW
C----
      RETURN
      END
Chd|====================================================================
Chd|  ADD_NM                        source/interfaces/inter3d1/i7remnode.F
Chd|-- called by -----------
Chd|        ADDINT2M                      source/interfaces/inter3d1/i7remnode.F
Chd|-- calls ---------------
Chd|        INSERT_A                      source/interfaces/inter3d1/i7remnode.F
Chd|====================================================================
      SUBROUTINE ADD_NM(NSN     ,IRECT   ,NSV    ,IRTL     ,NREMOV  ,
     2                  IREMOV  ,IADA    ,NS     ,TAGD  ,IFLAG    )                  
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4,*),NSV(*),IRTL(*), NS, NREMOV,
     .        IREMOV(*),IFLAG,NSN,IADA  ,TAGD(*)  
C----IFLAG=0 -> dim   , IREMOV : Global node num. (SECONDARY) 
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II ,I,J  ,IL, L,NM,NNOD
C-----------------------------------------------
        DO II=1,NSN
          I=NSV(II)
          IF (NS==I) THEN
           NNOD=4
           L=IRTL(II)
           IF (IRECT(4,L)==IRECT(3,L)) NNOD=3
           DO J=1,NNOD
            NM =IRECT(J,L)
            IF (IFLAG==0.AND.TAGD(NM)==0) THEN
             NREMOV =NREMOV+1
             TAGD(NM)=1
            ELSEIF (TAGD(NM)==0) THEN
             CALL INSERT_A(NREMOV,IREMOV,NM  ,IADA)
             TAGD(NM)=1
            END IF 
           END DO
          ENDIF
        END DO !II=1,NSN
C----
      RETURN
      END
Chd|====================================================================
Chd|  ANOTHERN2                     source/interfaces/inter3d1/i7remnode.F
Chd|-- called by -----------
Chd|        REMN_I2                       source/interfaces/inter3d1/i7remnode.F
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE ANOTHERN2(N2     ,NINTER  ,INTBUF_TAB  ,IPARI   ,NM     )                  
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N2, NM, IPARI(NPARI,*),NINTER

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C     REAL
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J  ,NEW,II
      INTEGER N, NTY, INCOM,NMN
C-----------------------------------------------
       NEW=0
       DO N=1,N2-1
         NTY=IPARI(7,N)
         IF(NTY.EQ.2)THEN
          NMN   =IPARI(6,N)
          INCOM  =IPARI(11,N)
C
          DO II=1,NMN        
            J=INTBUF_TAB(N)%MSR(II)
            IF (NM==J) THEN
             NEW = N
             CYCLE
            END IF
          ENDDO
         ENDIF
         IF (NEW>0) CYCLE
       ENDDO
       N2=NEW
C----
      RETURN
      END
Chd|====================================================================
Chd|  INSERT_A                      source/interfaces/inter3d1/i7remnode.F
Chd|-- called by -----------
Chd|        ADD_NM                        source/interfaces/inter3d1/i7remnode.F
Chd|        ADD_NM1                       source/interfaces/inter3d1/i7remnode.F
Chd|        ADD_NS                        source/interfaces/inter3d1/i7remnode.F
Chd|        ADD_NSFIC                     source/interfaces/inter3d1/i7remnode.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INSERT_A(N,IC,IA,ID)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N ,IC(*),ID,IA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IT,IC_CP
C
C----add IA--at IC(ID)--------------------------
      IF (ID > N+1 ) RETURN
      DO I =N+1,ID+1,-1
       IC(I) = IC(I-1)
      ENDDO
      IC(ID)=IA
      N = N + 1
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  REMN_I2_EDG                   source/interfaces/inter3d1/i7remnode.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ADD_NSFIC                     source/interfaces/inter3d1/i7remnode.F
Chd|        DIM_IEDGN2                    source/interfaces/inter3d1/i7remnode.F
Chd|        IND_IEDGN2                    source/interfaces/inter3d1/i7remnode.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE REMN_I2_EDG(IPARI   ,INTBUF_TAB   ,ITAB, NREMOV,IFLAG  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), ITAB(*),NREMOV(*),IFLAG

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,NTY,NN,NE2,IE,IE1,IE2,I,NNREM
      INTEGER II,J,NMN,NSN,NRTS,NRTM,IADA,IEDGE,NSNE,NRTSE
      INTEGER, DIMENSION(:),ALLOCATABLE :: TAGN
      INTEGER, DIMENSION(:,:),ALLOCATABLE :: IEDGN2
C-----------------------------------------------
C----like Irem_gap of int7, creat list of SECONDARY nodes to be removed per M_seg
C---------IFLAG=0 : DIM
      ALLOCATE(TAGN(NUMNOD))
C-----
       DO N=1,NINTER
        NTY=IPARI(7,N)
        NSN   =IPARI(5,N)
        NRTM  =IPARI(4,N)
        IEDGE =IPARI(59,N)
        IF (IFLAG==0) NREMOV(N) = IPARI(62,N)
        NRTSE = IPARI(52,N)
        IF (NREMOV(N) ==0.OR.IEDGE==0) CYCLE
        NSNE  = IPARI(55,N)
        TAGN(1:NUMNOD)=0
        DO J=1,INTBUF_TAB(N)%KREMNODE(NRTM+1)
         NN = INTBUF_TAB(N)%REMNODE(J)
         TAGN(NN)=1
        END DO
        CALL DIM_IEDGN2(NE2,NSNE,INTBUF_TAB(N)%IS2SE,INTBUF_TAB(N)%IRTSE,TAGN)
        IF (NE2==0) CYCLE
        ALLOCATE(IEDGN2(3,NE2))
        CALL IND_IEDGN2(NE2,NSNE,INTBUF_TAB(N)%IS2SE,INTBUF_TAB(N)%IRTSE,TAGN,IEDGN2)
        CALL ADD_NSFIC(NE2,NRTM,NSNE,INTBUF_TAB(N)%IS2SE,NREMOV(N),
     +       INTBUF_TAB(N)%KREMNODE,INTBUF_TAB(N)%REMNODE,IEDGN2,IFLAG)
        DEALLOCATE(IEDGN2)
       END DO
      DEALLOCATE(TAGN)
C----
      RETURN
      END
Chd|====================================================================
Chd|  DIM_IEDGN2                    source/interfaces/inter3d1/i7remnode.F
Chd|-- called by -----------
Chd|        REMN_I2_EDG                   source/interfaces/inter3d1/i7remnode.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DIM_IEDGN2(NE2,NSNE,IS2SE,IRTSE,TAGN)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NE2,NSNE,IS2SE(2,*),IRTSE(5,*),TAGN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IK1(4),IK2(4),NS1,NS2,IED,I,J,IE1,IE2,IE
      DATA IK1 /1,2,3,4/
      DATA IK2 /2,3,4,1/
C-----
        NE2 =0
      DO I=1,NSNE
       IE1 = IS2SE(1,I)
       IE2 = IS2SE(2,I)
       IF (IE1 > 0) THEN
         IE = IE1
         IED=IRTSE(5,IE)
         NS1= IRTSE(IK1(IED),IE)
         NS2= IRTSE(IK2(IED),IE)
       ELSEIF(IE2 > 0) THEN
         IE = IE2
         IED=IRTSE(5,IE)
         NS1= IRTSE(IK2(IED),IE)
         NS2= IRTSE(IK1(IED),IE)
       ELSE
        print *,'problem EDGE **** I,IE1,IE2=',I,IE1,IE2
       END IF
        IF (TAGN(NS1)>0.AND.TAGN(NS2)>0) NE2 = NE2 + 1
      END DO ! I=1,NSNE        
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  IND_IEDGN2                    source/interfaces/inter3d1/i7remnode.F
Chd|-- called by -----------
Chd|        REMN_I2_EDG                   source/interfaces/inter3d1/i7remnode.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IND_IEDGN2(NE2,NSNE,IS2SE,IRTSE,TAGN,IEDGN2)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NE2,NSNE,IS2SE(2,*),IRTSE(5,*),TAGN(*),IEDGN2(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IK1(4),IK2(4),NS1,NS2,IED,I,J,IE1,IE2,IE
      DATA IK1 /1,2,3,4/
      DATA IK2 /2,3,4,1/
C-----
        NE2 =0
      DO I=1,NSNE
       IE1 = IS2SE(1,I)
       IE2 = IS2SE(2,I)
       IF (IE1 > 0) THEN
         IE = IE1
         IED=IRTSE(5,IE)
         NS1= IRTSE(IK1(IED),IE)
         NS2= IRTSE(IK2(IED),IE)
       ELSEIF(IE2 > 0) THEN
         IE = IE2
         IED=IRTSE(5,IE)
         NS1= IRTSE(IK2(IED),IE)
         NS2= IRTSE(IK1(IED),IE)
       ELSE
        print *,'problem EDGE **** I,IE1,IE2=',I,IE1,IE2
       END IF
         IF (TAGN(NS1)>0.AND.TAGN(NS2)>0) THEN
          NE2 = NE2 + 1
          IEDGN2(1,NE2) = NS1
          IEDGN2(2,NE2) = NS2
          IEDGN2(3,NE2) = I
         END IF
      END DO ! I=1,NSNE        
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  ADD_NSFIC                     source/interfaces/inter3d1/i7remnode.F
Chd|-- called by -----------
Chd|        REMN_I2_EDG                   source/interfaces/inter3d1/i7remnode.F
Chd|-- calls ---------------
Chd|        INSERT_A                      source/interfaces/inter3d1/i7remnode.F
Chd|        INTAB                         source/interfaces/inter3d1/i24tools.F
Chd|====================================================================
      SUBROUTINE ADD_NSFIC(NE2,NRTM,NSNE,IS2SE,NREMOV,KREMNODE,
     +                     REMNODE,IEDGN2,IFLAG)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTM,NSNE,IS2SE(2,*),NREMOV,KREMNODE(*),REMNODE(*),
     +        IEDGN2(3,*),IFLAG,NE2
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IE,IE1,IE2,I,NNREM,NN,KREMOV_OLD
      INTEGER NS1,NS2,IED,J,II,IADA,NS,NEW,NR0,IADN
C-----------------------------------------------
C   External function
C-----------------------------------------------
      LOGICAL INTAB
      EXTERNAL INTAB
C-----
       IF (IFLAG==0) THEN
        DO II=1,NRTM
         IF (KREMNODE(II+1)>KREMNODE(II)) THEN
           NNREM = KREMNODE(II+1) - KREMNODE(II)
           IADA = KREMNODE(II)+1
          DO J=1,NE2
           NS1= IEDGN2(1,J)
           NS2= IEDGN2(2,J)
           IF (INTAB(NNREM,REMNODE(IADA),NS1)
     1         .OR.INTAB(NNREM,REMNODE(IADA),NS2)) THEN
             NREMOV =NREMOV + 1
           END IF
          END DO !J=1,NE2
         END IF !(KREMNODE(II+1)>KREMNODE(II)) THEN
        END DO !II=1,NRTM
       ELSE
        NEW = 0
        DO II=1,NRTM
          NR0=NREMOV
          KREMOV_OLD = KREMNODE(II)
          KREMNODE(II) = KREMNODE(II) + NEW
          IF (KREMNODE(II+1)>KREMOV_OLD) THEN
           NNREM = KREMNODE(II+1) - KREMOV_OLD
           IADA = KREMNODE(II)+1
           IADN = KREMNODE(II+1)+NEW+1 
           DO J=1,NE2
            NS1= IEDGN2(1,J)
            NS2= IEDGN2(2,J)
            NN = IEDGN2(3,J) + NUMNOD
            IF (INTAB(NNREM,REMNODE(IADA),NS1)
     1         .OR.INTAB(NNREM,REMNODE(IADA),NS2)) THEN
C------if one of edge nodes in %REMNODE(iad+1:iad+NNREM+1), add fictive nodes  
             CALL INSERT_A(NREMOV,REMNODE,NN  ,IADN)
             NEW = NEW + 1
            END IF !(INTAB(NSREM,INTBUF_TAB(N)%REMNODE(IADA),NS1)           
           END DO ! J=1,NE2
           END IF !(KREMNODE(II+1)>KREMNODE(II)) THEN
          END DO !II=1,NRTM
          KREMNODE(NRTM+1) = KREMNODE(NRTM+1) + NEW
       END IF !(IFLAG==0) THEN
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  REMN_I2OP                     source/interfaces/inter3d1/i7remnode.F
Chd|-- called by -----------
Chd|        ININTR2                       source/interfaces/inter3d1/inintr2.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        PRE_I2                        source/interfaces/inter3d1/i7remnode.F
Chd|        UPGRADE_REMNODE2              source/interfaces/interf1/upgrade_remnode.F
Chd|        ZERONM_TAGD                   source/interfaces/inter3d1/i7remnode.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE REMN_I2OP(IPARI   ,INTBUF_TAB   ,ITAB, NOM_OPT,NREMOV)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), ITAB(*),NREMOV(*)
      INTEGER NOM_OPT(LNOPT1,*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr15_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "my_allocate.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,NTY,FLAGREMNODE
      INTEGER ILEV,II,J,NMN,NSN,NRTS,NRTM,LREMNORMAX,K,
     .        NLINS,NLINM,IWOUT,INCOM,NM,N2,IFLAG,NRE,ip,IACT,
     .        IF7,IF24,IF25,NN2,NNOD,M1,M2,M3,M4,NNREM,IBIT,NEW,
     .        KI,KL,JJ,IEDGE,NEDGE
      INTEGER, DIMENSION(:),ALLOCATABLE :: TAGD
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR

      INTEGER :: COMPTEUR,I2NODE_SIZE,I,L,L1,IS,IIS,NS,IADA
      INTEGER :: TYP25_USE
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: I2NODE,POINTS_I2N
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAGNOD

        
        INTEGER :: III,JJJ,NNOD_2
        INTEGER :: FIRST,LAST,NNREM_SAVE,FLAGREMNODE_SAV
        INTEGER :: OFFSET, NBR_INTRA,NBR_EXTRA,TOTAL_INSERTED
        INTEGER :: SIZE_INSERTED_NODE,OLDSIZE,MAX_INSERTED_NODE
        INTEGER, DIMENSION(:), ALLOCATABLE :: NBR_INSERT_II,ADRESS_II
        INTEGER, DIMENSION(:), ALLOCATABLE :: KREMNODE_SAVE,INSERTED_NODE,REMNODE,TMP
!       -------------------------------
!       FIRST : integer , first block of inserted nodes
!       LAST : integer , last block of inserted nodes
!       NNREM_SAVE : integer , internal counter
!       OFFSET : integer , internal offset for the REMNODE array
!       NBR_INTRA : integer , number of old nodes between 2 blocks
!       NBR_EXTRA : integer , number of old remaining nodes
!       TOTAL_INSERTED : integer , total number of inserted nodes
!       TYP25_USE : integer, interface typ25 marker --> TYP25_USE = 1 if
!                   an interface 25 is used, O otherwise!
!       NBR_INSERT_II : integer, dimension = NRTM , number of inserted nodes for each II segment
!       ADRESS_II : integer, dimension = NRTM , adress of the first inserted nodes for each II segment
!       KREMNODE_SAVE : integer, dimension = NRTM+1 , list of old nodes
!       SIZE_INSERTED_NODE : integer, size of the INSERTED_NODE array ; SIZE_INSERTED_NODE is an upper bound, 
!                            can be optimized!
!       INSERTED_NODE : integer, dimension = SIZE_INSERTED_NODE, list inserted nodes
!       REMNODE : integer, dimension = NRTM + TOTAL_INSERTED, new array with old and inserted nodes
!       -------------------------------
C-----------------------------------------------
C----like Irem_gap of int7, creat list of SECONDARY nodes to be removed per M_seg
C----------only during dimensioning
        IACT=0  
        TYP25_USE = 0
        DO N=1,NINTER
                NTY=IPARI(7,N)
                IF7 =IPARI(54,N)
                IF24=IPARI(63,N)
                IF25=IPARI(83,N)
                IF(NTY==7 .AND. IF7>0 )THEN
                        IACT=1
                        CYCLE
                ENDIF
                IF(NTY==24 .AND. IF24>0 )THEN
                        IACT=1
                        CYCLE
                ENDIF
                IF(NTY==25 .AND. IF25>0 )THEN
                        IACT=1
                        TYP25_USE = 1
                        CYCLE
                ENDIF
        ENDDO
        IF (IACT==0) THEN
                DO N=1,NINTER
                        NREMOV(N) = 0
                ENDDO
                RETURN
        END IF
        IF(TYP25_USE==1) THEN
                ALLOCATE( TAGNOD(NUMNOD) )
                TAGNOD(1:NUMNOD) = 0
        ENDIF
C-------tag int2 nodes------ 

! ********************************

!       I2NODE : | 1th node surf/MAIN | interface | SECONDARY  |
!                | 2nd node surf/MAIN | interface | SECONDARY  |
!                | 3tr node surf/MAIN | interface | SECONDARY  |
!                | 4th node surf/MAIN | interface | SECONDARY  |
!                |    SECONDARY node        | interface | -SECONDARY |

!      Compute the size of I2NODE array
        I2NODE_SIZE = 0
        DO N=1,NINTER
                NTY=IPARI(7,N)
                NREMOV(N) = 0
                IF(NTY.EQ.2)THEN
                        NSN   =IPARI(5,N)
                        DO II=1,NSN
                                L=INTBUF_TAB(N)%IRTLM(II)                                   
                                IF (INTBUF_TAB(N)%IRECTM(4*(L-1)+3)==INTBUF_TAB(N)%IRECTM(4*(L-1)+4)) THEN
                                        NNOD = 3
                                ELSE
                                        NNOD = 4
                                END IF
                                I2NODE_SIZE=I2NODE_SIZE + NNOD + 1
                        END DO !II=1,NSN
                ENDIF
        ENDDO
        IF (I2NODE_SIZE==0) RETURN
        ALLOCATE(I2NODE(I2NODE_SIZE,3))
        ALLOCATE(POINTS_I2N(NUMNOD,2))   
        ALLOCATE(TAGD(NUMNOD))

        CALL PRE_I2(IPARI  ,INTBUF_TAB ,I2NODE_SIZE, I2NODE,POINTS_I2N)
 

!   ------------------------------------------------

!           remnode :
!           1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
!
!           inserted nodes : 
!           3 nodes for ii=1 ; adress of the 1st one = 3 ; iad1(1) --> iad1(3)
!           1 nodes for ii=4 ; adress of the 1st one = 7 ; iad2(1) --> iad2(1)
!           6 nodes for ii=1 ; adress of the 1st one = 10 ; iad3(1) --> iad3(6)
!
!           First insertion iad1 :
!           1 | 2 | iad1(1) | 3 | 4 | 5 | 6 | 7 | 8 | 9
!           1 | 2 | iad1(2) | iad1(1) | 3 | 4 | 5 | 6 | 7 | 8 | 9
!           1 | 2 | iad1(3) | iad1(2) | iad1(1) | 3 | 4 | 5 | 6 | 7 | 8 | 9
!
!           iad2 :
!           1 | 2 | iad1(3) | iad1(2) | iad1(1) | 3 | iad2(1) | 4 | 5 | 6 | 7 | 8 | 9
!
!           iad3 :
!           1 | 2 | iad1(3) | iad1(2) | iad1(1) | 3 | iad2(1) | 4 | 5 | iad3(1) | 6 | 7 | 8 | 9
!           1 | 2 | iad1(3) | iad1(2) | iad1(1) | 3 | iad2(1) | 4 | 5 | iad3(2) | iad3(1) | 6 | 7 | 8 | 9
!                           ...
!
!           Optimization :
!           1st step :
!               count the total number of inserted nodes --> NNREM
!               get the number of the inserted nodes for each II segment --> NBR_INSERT_II
!               get the address of the first node for each II segment --> ADRESS_II
!               save the inserted node --> INSERTED_NODE
!           2nd step : 
!               allocate the new array REMNODE
!               get the first/last block of inserted node --> FIRST/LAST
!                   if ADRESS_II(FIRST) = 1 --> insert the NBR_INSERT_II(FIRST) node in REMNODE(1:NBR_INSERT_II(FIRST))
!                   if ADRESS_II(FIRST) > 1 --> copy the old node in the new array
!               copy the inserted nodes in the new array for each II segment
!               copy the old nodes in the new array for each II segment
!               check if every old nodes were copied and if not, copy them!
!
!           example : 
!               old remnode : 
!           1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9    
!               new remnode :           
!           1 | 2 | iad1(3) | iad1(2) | iad1(1) | 3 | iad2(1) | 4 | 5 | iad3(6) | ... | iad3(2) | iad3(1) | 6 | 7 | 8 | 9
!
!           ADRESS_II(FIRST) > 1 :
!               new remnode :           
!           1 | 2 | . | . | ...
!           for II=1, insert the iad1 nodes :
!           1 | 2 | iad1(2) | iad1(1) | . | . | ...
!           for II=1, copy the old nodes :
!           1 | 2 | iad1(2) | iad1(1) | 3 | . | . |
!           ...
!           1 | 2 | iad1(3) | iad1(2) | iad1(1) | 3 | iad2(1) | . | . | ...
!           for II=LAST, insert the iadlast nodes
!           1 | 2 | iad1(3) | iad1(2) | iad1(1) | 3 | iad2(1) | 4 | 5 | iad3(6) | ... | iad3(2) | iad3(1) | . | . | ...
!           if there are no old nodes to insert, the treatment is over
!           else insert the remaining old nodes 
!           1 | 2 | iad1(3) | iad1(2) | iad1(1) | 3 | iad2(1) | 4 | 5 | iad3(6) | ... | iad3(2) | iad3(1) | 6 | 7 | 8 | 9
!   ------------------------------------------------


C---------
        DO N=1,NINTER
                    NTY=IPARI(7,N)
                    NSN   =IPARI(5,N)
                    NRTS  =IPARI(3,N)
                    NRTM  =IPARI(4,N)
                    IF7   =IPARI(54,N)
                    IF24  =IPARI(63,N)
                    IF25  =IPARI(83,N)
                    IEDGE = IPARI(58,N)

                    ALLOCATE( NBR_INSERT_II(NRTM) )
                    ALLOCATE( ADRESS_II(NRTM) )
                    ALLOCATE( KREMNODE_SAVE(NRTM+1) )
                    NBR_INSERT_II(1:NRTM) = 0
                    ADRESS_II(1:NRTM) = 0
                    KREMNODE_SAVE(1:NRTM+1) = 0

                    ID=NOM_OPT(1,N)
                    CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,N),LTITR)
C----- --           
                    IF((NTY==7.AND.IF7>0).OR.(NTY==24.AND.IF24>0).OR.(NTY==25.AND.IF25>0))THEN
                            TAGD(1:NUMNOD)=2
                            JJJ = 0
                            FLAGREMNODE=IPARI(63,N)
                            FLAGREMNODE_SAV=IPARI(63,N)
C--------dim first  
                            NNREM = 0      
                            DO JJ=1,NSN
                                NS = INTBUF_TAB(N)%NSV(JJ)
                                IF (NS<=NUMNOD) TAGD(NS)=0
                            ENDDO
                            IFLAG =0
                            NREMOV(N) = IPARI(62,N)
                            IADA= 1
                            IF(NREMOV(N)>0) KREMNODE_SAVE(1:NRTM+1) = INTBUF_TAB(N)%KREMNODE(1:NRTM+1)

                            SIZE_INSERTED_NODE = 1
                            MAX_INSERTED_NODE = 1
                            DO II=1,NRTM
                                IF (INTBUF_TAB(N)%IRECTM(4*(II-1)+4)==INTBUF_TAB(N)%IRECTM(4*(II-1)+3)) THEN
                                    NNOD=3
                                ELSE
                                    NNOD=4
                                END IF           
                                DO J=1,NNOD
                                    NM = INTBUF_TAB(N)%IRECTM(4*(II-1)+J)
                                    IF (POINTS_I2N(NM,1)==0) CYCLE
                                    MAX_INSERTED_NODE = MAX( MAX_INSERTED_NODE,POINTS_I2N(NM,2)-POINTS_I2N(NM,1) )
                                    
                                ENDDO
                            ENDDO

                            IF(4*MAX_INSERTED_NODE * NRTM < 0 .OR. MAX_INSERTED_NODE * NRTM > 1 000 000) THEN
                              SIZE_INSERTED_NODE =  4 * NRTM
                            ELSE
                              SIZE_INSERTED_NODE =  4 * NRTM *MAX_INSERTED_NODE
                            ENDIF
C FOR TESTING ONLY: uncomment the following line to force reallocation
C                          SIZE_INSERTED_NODE = MAX_INSERTED_NODE
C

                            MY_ALLOCATE(INSERTED_NODE,SIZE_INSERTED_NODE) 

                            DO II=1,NRTM
                                NNREM_SAVE = NNREM
C
C          Do not add nodes already stored w/IREM_GAP
                                IF(FLAGREMNODE==2)THEN
                                    KI = INTBUF_TAB(N)%KREMNODE(II)+1
                                    KL = INTBUF_TAB(N)%KREMNODE(II+1) 
                                    DO J=KI,KL
                                        NS = INTBUF_TAB(N)%REMNODE(J)
                                        TAGD(NS)=1
                                    END DO
                                END IF
C
                                IF (INTBUF_TAB(N)%IRECTM(4*(II-1)+4)==INTBUF_TAB(N)%IRECTM(4*(II-1)+3)) THEN
                                    NNOD=3
                                ELSE
                                    NNOD=4
                                END IF   
                               
                                IF(JJJ + MAX_INSERTED_NODE > SIZE_INSERTED_NODE) THEN
C extend INSERTED_NODE if needed
                                  OLDSIZE = SIZE_INSERTED_NODE
                                  SIZE_INSERTED_NODE = SIZE_INSERTED_NODE  + MAX(NRTM,MAX_INSERTED_NODE)
                                  MY_ALLOCATE(TMP,SIZE_INSERTED_NODE)
                                  TMP(1:OLDSIZE) = INSERTED_NODE(1:OLDSIZE)
!                                 move_alloc deallocates TMP
                                  CALL MOVE_ALLOC(TMP,INSERTED_NODE)
                                ENDIF

                                DO J=1,NNOD
                                    NM = INTBUF_TAB(N)%IRECTM(4*(II-1)+J)
                                    IF (POINTS_I2N(NM,1)==0) CYCLE
                                    DO I=POINTS_I2N(NM,1),POINTS_I2N(NM,2)
                                        N2 = I2NODE(I,2)
                                        IS = I2NODE(I,3)
                                        IF (IS >0) THEN
                                            NS = INTBUF_TAB(N2)%NSV(IS)
                                            IF (TAGD(NS)==0) THEN
                                                NNREM = NNREM + 1  
                                                TAGD(NS)=1  
                                                JJJ = JJJ + 1
                                                INSERTED_NODE(JJJ) = NS        
                                            END IF
                                        ELSEIF (IS <0) THEN
                                            IIS = -IS
                                            L = INTBUF_TAB(N2)%IRTLM(IIS)
                                            NNOD_2 = 4
                                            IF( INTBUF_TAB(N2)%IRECTM(4*(L-1)+4)==INTBUF_TAB(N2)%IRECTM(4*(L-1)+3) ) NNOD_2 = 3
                                            DO III = 1,NNOD_2
                                                NM = INTBUF_TAB(N2)%IRECTM(4*(L-1)+III)
                                                IF(TAGD(NM)==0) THEN
                                                    NNREM = NNREM + 1
                                                    TAGD(NM)=1 
                                                    JJJ = JJJ + 1
                                                    INSERTED_NODE(JJJ) = NM   
                                                ENDIF
                                            ENDDO 
                                        END IF
                                    END DO
                                END DO !DO J=1,4
            
                                !   -------------------
                                !   number of inserted nodes
                                NBR_INSERT_II(II) = NNREM - NNREM_SAVE
                                KREMNODE_SAVE(II) = KREMNODE_SAVE(II+1) - KREMNODE_SAVE(II)
                                IADA = IADA + KREMNODE_SAVE(II)
                                !   adress of the first inserted node
                                ADRESS_II(II) = IADA
                                KREMNODE_SAVE(II) = IADA + NBR_INSERT_II(II) - 1
                                IADA = IADA + NBR_INSERT_II(II)
                                !   -------------------
                                
C-----reset    TAGD=0        
                                DO J=1,NNOD
                                        NM = INTBUF_TAB(N)%IRECTM(4*(II-1)+J)
                                        IF (POINTS_I2N(NM,1)==0) CYCLE
                                        DO I=POINTS_I2N(NM,1),POINTS_I2N(NM,2)
                                                N2 = I2NODE(I,2)
                                                IS = I2NODE(I,3)
                                                IF (IS >0) THEN
                                                        NS = INTBUF_TAB(N2)%NSV(IS)
                                                        IF (TAGD(NS)==1) TAGD(NS)=0
                                                ELSEIF (IS <0) THEN
                                                        IIS = -IS
                                                        CALL ZERONM_TAGD(IIS ,INTBUF_TAB(N2)%IRECTM,
     .                                                                   INTBUF_TAB(N2)%IRTLM,TAGD)
                                                END IF
                                        END DO
                                END DO !DO J=1,NNOD
                                IF(FLAGREMNODE==2)THEN
                                        DO J=KI,KL
                                                NS = INTBUF_TAB(N)%REMNODE(J)
                                                TAGD(NS)=0
                                        END DO
                                END IF
C
                            END DO !II=1,NRTM


                            IF(NNREM>0) THEN
                              
                                ! get the first and the last inserted node
                                FIRST = 0
                                LAST = 0
                                DO II = 1,NRTM
                                    IF(FIRST==0) THEN
                                        IF( NBR_INSERT_II(II)/=0 ) FIRST = II
                                    ENDIF
                                    IF(LAST==0) THEN
                                        IF( NBR_INSERT_II(NRTM+1-II)/=0 ) LAST = NRTM+1-II
                                    ENDIF
                                ENDDO
                                !       count the total number of inserted nodes
                                TOTAL_INSERTED = 0
                                DO II=1,NRTM
                                    TOTAL_INSERTED = TOTAL_INSERTED + NBR_INSERT_II(II)
                                ENDDO
                                !       allocate the buffer array
                                ALLOCATE( REMNODE(NREMOV(N)+TOTAL_INSERTED) )
       
                                J = 0
                                I = 0
                                OFFSET = 0
                                IF( FIRST>0 ) THEN
                                    !   insertion of the first chunk of node : if ADRESS_II(FIRST) > 1
                                    !   --> need to copy the old nodes 
                                    IF( ADRESS_II(FIRST)>1 ) THEN
                                        REMNODE(1:ADRESS_II(FIRST)-1) = INTBUF_TAB(N)%REMNODE(1:ADRESS_II(FIRST)-1)
                                        OFFSET = OFFSET + ADRESS_II(FIRST)-1
                                        I = I + ADRESS_II(FIRST)-1
                                    ENDIF
        
                                    DO II=FIRST,LAST
                                        !       insertion of the nodes
                                        IF( NBR_INSERT_II(II)>0 ) THEN
                                            DO JJ = 1,NBR_INSERT_II(II)
                                                J = J + 1
                                                REMNODE(OFFSET+NBR_INSERT_II(II)+1-JJ) = INSERTED_NODE(J)
                                            ENDDO
                                            OFFSET = OFFSET + NBR_INSERT_II(II)
                                        ENDIF
                                        IF(II<LAST.AND.NREMOV(N)>0) THEN
                                            ! copy of the old nodes
                                            NBR_INTRA = ADRESS_II(II+1) - ADRESS_II(II)-NBR_INSERT_II(II)
                                            IF( NBR_INTRA>0 )THEN
                                                DO JJ = 1,NBR_INTRA
                                                    I = I + 1
                                                    REMNODE(JJ+OFFSET) = INTBUF_TAB(N)%REMNODE(I)
                                                ENDDO
                                                OFFSET = OFFSET + NBR_INTRA
                                            ENDIF
                                        ENDIF
                                    ENDDO
                                ENDIF
                                !       copy of the old nodes for the LAST chunk

                                IF( I<NREMOV(N) ) THEN
                                    NBR_EXTRA = NREMOV(N) - I
                                    REMNODE(OFFSET+1:OFFSET+NBR_EXTRA) = INTBUF_TAB(N)%REMNODE(I+1:NREMOV(N))
                                ENDIF
                                !       update of NNREM and deallocation / allocation of the new array
                                NNREM = NNREM + NREMOV(N)
                                CALL UPGRADE_REMNODE2(N,NNREM,INTBUF_TAB(N),NTY)
                                INTBUF_TAB(N)%REMNODE(1:NNREM) = REMNODE(1:NNREM)
                                INTBUF_TAB(N)%KREMNODE(2:NRTM+1) = KREMNODE_SAVE(1:NRTM)
                                INTBUF_TAB(N)%KREMNODE(1)=0
C------------update of IPARI(62,N) is out of subroutine    
                                CALL ANCMSG(MSGID=1053,
     .                                  MSGTYPE=MSGWARNING,
     .                                  ANMODE=ANINFO_BLIND_1,
     .                                  I1=ID,
     .                                  C1=TITR,
     .                                  I2=NNREM,
     .                                  I3=NOM_OPT(1,N2))
C----------used for Iedge=1     
                                NREMOV(N) = NNREM
                            END IF !IF (NNREM>0) THEN
                            IF(ALLOCATED(REMNODE)) DEALLOCATE( REMNODE )  
                            IF(ALLOCATED(INSERTED_NODE)) DEALLOCATE( INSERTED_NODE )  
 
                            !   -------------------
        !   ------------------------------------------------
                    END IF!(NTY==7.OR.NTY==24.OR.NTY==25)
C
                    IF(NTY==25.AND.IF25>0.AND.NNREM>0)THEN
C
                            DO I=1,NSN
                                TAGNOD(INTBUF_TAB(N)%NSV(I))=I
                            END DO
C
C-----    Inverse table of REMNODE for sliding in int25 : SECONDARY node -> MAIN segments---
                            DO I=1,NRTM
                                K = INTBUF_TAB(N)%KREMNODE(I)+1
                                L = INTBUF_TAB(N)%KREMNODE(I+1)       
                                DO J=K,L
                                    NS = TAGNOD(INTBUF_TAB(N)%REMNODE(J))
                                    INTBUF_TAB(N)%KREMNOR(NS) = INTBUF_TAB(N)%KREMNOR(NS)+1
                                ENDDO
                            ENDDO
C
                            DO NS=1,NSN
                                INTBUF_TAB(N)%KREMNOR(NS+1) = INTBUF_TAB(N)%KREMNOR(NS+1) + INTBUF_TAB(N)%KREMNOR(NS)
                            END DO
C
                            DO NS=NSN,1,-1
                                INTBUF_TAB(N)%KREMNOR(NS+1)=INTBUF_TAB(N)%KREMNOR(NS)
                            END DO
                            INTBUF_TAB(N)%KREMNOR(1)=0
C
                            DO I=1,NRTM
                                K = INTBUF_TAB(N)%KREMNODE(I)+1
                                L = INTBUF_TAB(N)%KREMNODE(I+1)       
                                DO J=K,L
                                    NS = TAGNOD(INTBUF_TAB(N)%REMNODE(J))
                                    INTBUF_TAB(N)%KREMNOR(NS) = INTBUF_TAB(N)%KREMNOR(NS)+1
                                    INTBUF_TAB(N)%REMNOR(INTBUF_TAB(N)%KREMNOR(NS)) = I
                                ENDDO
                            ENDDO
C
                            DO NS=NSN,1,-1
                                INTBUF_TAB(N)%KREMNOR(NS+1)=INTBUF_TAB(N)%KREMNOR(NS)
                            END DO
                            INTBUF_TAB(N)%KREMNOR(1)=0
C
C-------  Compute maximum number of MAIN segments banned for all SECONDARY nodes---
                            LREMNORMAX = 0
                            DO NS=1,NSN
                                L = INTBUF_TAB(N)%KREMNOR(NS+1)-INTBUF_TAB(N)%KREMNOR(NS)
                                IF( L.GT.LREMNORMAX) THEN
                                    LREMNORMAX = L
                                ENDIF
                            ENDDO
                            IPARI(82,N) = LREMNORMAX
C
C-------  Reset IRTLM & PENE_OLD (cf INACTI)

                            DO NS=1,NSN
                                DO J=INTBUF_TAB(N)%KREMNOR(NS)+1,INTBUF_TAB(N)%KREMNOR(NS+1)
                                    L=INTBUF_TAB(N)%REMNOR(J)
                                    IF(INTBUF_TAB(N)%IRTLM(4*(NS-1)+1)==INTBUF_TAB(N)%MSEGLO(L))THEN
                                        INTBUF_TAB(N)%IRTLM(4*(NS-1)+1:4*(NS-1)+4)   =0
                                        INTBUF_TAB(N)%TIME_S(2*(NS-1)+1:2*(NS-1)+2)  =ZERO
                                        INTBUF_TAB(N)%PENE_OLD(5*(NS-1)+1:5*(NS-1)+5)=ZERO
                                    ENDIF
                                ENDDO
                            ENDDO

                            DO I=1,NSN
                                TAGNOD(INTBUF_TAB(N)%NSV(I))=0
                            END DO

                    END IF ! IF(NTY==25.AND.IF25>0)THEN

                    DEALLOCATE( NBR_INSERT_II )
                    DEALLOCATE( ADRESS_II )
                    DEALLOCATE( KREMNODE_SAVE ) 
C-------------------------------------------------
C       IremI2 for edge to edge contact type 25
C-----------------------------------------------

                    IF(NTY==25.AND.IF25>0.AND.IEDGE>0)THEN
                      CALL REMN_I2OP_EDG25(N          ,FLAGREMNODE_SAV ,IPARI   ,INTBUF_TAB   ,I2NODE,
     .                                     POINTS_I2N ,I2NODE_SIZE     ,NOM_OPT  ,ITAB        )
                    ENDIF      
        END DO 

        DEALLOCATE(TAGD,I2NODE,POINTS_I2N)
        IF(TYP25_USE==1) THEN
                DEALLOCATE( TAGNOD )
        ENDIF
C----
        RETURN
        END
Chd|====================================================================
Chd|  PRE_I2                        source/interfaces/inter3d1/i7remnode.F
Chd|-- called by -----------
Chd|        REMN_I2OP                     source/interfaces/inter3d1/i7remnode.F
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE PRE_I2(IPARI   ,INTBUF_TAB ,NSIZE, I2NODE,POINT_I2NODE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSIZE
      INTEGER IPARI(NPARI,*), I2NODE(NSIZE,3),POINT_I2NODE(NUMNOD,2)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,NTY
      INTEGER ILEV,II,J,NMN,NSN,NRTS,NRTM,
     .        NLINS,NLINM,IWOUT,INCOM,NM,N2,IFLAG,NRE,ip,IACT,
     .        IF7,IF24,IF25,NN2,NNOD,M1,M2,M3,M4

      INTEGER :: WORK(70000)
      INTEGER :: COMPTEUR,I2NODE_SIZE,I,L,L1
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: I2NODE_LOC

C-----------------------------------------------
        I2NODE_SIZE = NSIZE
! ********************************

!       I2NODE : | 1th node surf/MAIN | interface | SECONDARY  |
!                | 2nd node surf/MAIN | interface | SECONDARY  |
!                | 3tr node surf/MAIN | interface | SECONDARY  |
!                | 4th node surf/MAIN | interface | SECONDARY  |
!                |    SECONDARY node        | interface | -SECONDARY |

!      Compute the size of I2NODE array
!     Allocate the temporary I2NODE array to I2NODE_SIZE , 3
      ALLOCATE(I2NODE_LOC(I2NODE_SIZE,3))
      
       COMPTEUR=1
       DO N=1,NINTER
         NTY=IPARI(7,N)
         IF(NTY.EQ.2)THEN
          NRTS  =IPARI(3,N)
          NRTM  =IPARI(4,N)
          NSN   =IPARI(5,N)
          NMN   =IPARI(6,N)
          ILEV  =IPARI(20,N)
          DO II=1,NSN
            I=INTBUF_TAB(N)%NSV(II)
            L=INTBUF_TAB(N)%IRTLM(II) 
            L1 = 4*(L-1)
            M1 = INTBUF_TAB(N)%IRECTM(L1+1)            
            M2 = INTBUF_TAB(N)%IRECTM(L1+2)            
            M3 = INTBUF_TAB(N)%IRECTM(L1+3)            
            M4 = INTBUF_TAB(N)%IRECTM(L1+4)            
            ! 1
            I2NODE_LOC(COMPTEUR,1) = M1                                 ! node surf/MAIN
            I2NODE_LOC(COMPTEUR,2) = N                                  ! interface
            I2NODE_LOC(COMPTEUR,3) = II                                 ! SECONDARY_id
            COMPTEUR = COMPTEUR + 1 
            ! 2
            I2NODE_LOC(COMPTEUR,1) = M2                                 
            I2NODE_LOC(COMPTEUR,2) = N                                  
            I2NODE_LOC(COMPTEUR,3) = II                                 
            COMPTEUR = COMPTEUR + 1
            ! 3
            I2NODE_LOC(COMPTEUR,1) = M3    
            I2NODE_LOC(COMPTEUR,2) = N                                  
            I2NODE_LOC(COMPTEUR,3) = II                               
            COMPTEUR = COMPTEUR + 1
            ! 4
           IF (M4/=M3) THEN
            I2NODE_LOC(COMPTEUR,1) = M4    ! node surf/MAIN
            I2NODE_LOC(COMPTEUR,2) = N                                  ! interface
            I2NODE_LOC(COMPTEUR,3) = II                                 ! SECONDARY 
            COMPTEUR = COMPTEUR + 1
           END IF !(M4/=M3) THEN
            ! 5
            I2NODE_LOC(COMPTEUR,1) = I                                  ! SECONDARY node
            I2NODE_LOC(COMPTEUR,2) = N                                  ! interface
            I2NODE_LOC(COMPTEUR,3) = -II                                ! - SECONDARY 
            COMPTEUR = COMPTEUR + 1
          ENDDO
         ENDIF
       ENDDO

!       Sort the I2NODE array :
!               | NSM(1) | Inter(1) | SECONDARY(1)
!               | NSM(1) | Inter(1) | SECONDARY(20)
!               | NSM(1) | Inter(1) | SECONDARY(3)
!               | NSM(1) | Inter(2) | SECONDARY(1)
!               | NSM(2) | Inter(4) | SECONDARY(14)
!               | NSM(2) | Inter(5) | SECONDARY(18)
!               | NSM(3) | Inter(1) | SECONDARY(1)
!               |  ...   |   ...    |   ...      
!       Compute the pointer array POINT_I2NODE :
!               | 0 | 0 | if 0,0 --> node not in type2 interface
!               | 1 | 3 | 
!               | 4 | 5 | 
!               | 0 | 0 | 

       ALLOCATE( INDEX(2*I2NODE_SIZE) )
       DO I=1,2*I2NODE_SIZE
        INDEX(I)=I
       ENDDO
c       ALLOCATE(I2NODE(I2NODE_SIZE,3))
       CALL MY_ORDERS( 0, WORK, I2NODE_LOC(1,1), INDEX, I2NODE_SIZE , 1)    
       COMPTEUR = 1
c       ALLOCATE(POINT_I2NODE(NUMNOD,2))   
       POINT_I2NODE(1:NUMNOD,1:2) = 0
       POINT_I2NODE(I2NODE_LOC(INDEX(1),1),1) = 1
       DO I=1,I2NODE_SIZE
!       Initialization of I2NODE array
        I2NODE(COMPTEUR,1) = I2NODE_LOC(INDEX(I),1)
        I2NODE(COMPTEUR,2) = I2NODE_LOC(INDEX(I),2)
        I2NODE(COMPTEUR,3) = I2NODE_LOC(INDEX(I),3)
!       Initialization of POINT_I2NODE array
        IF(POINT_I2NODE(I2NODE_LOC(INDEX(I),1),1)==0) THEN 
          POINT_I2NODE(I2NODE_LOC(INDEX(I),1),1) = COMPTEUR
          IF (I>1) POINT_I2NODE(I2NODE_LOC(INDEX(I-1),1),2) = COMPTEUR - 1
        ENDIF
        COMPTEUR=COMPTEUR+1
       ENDDO
       POINT_I2NODE(I2NODE_LOC(INDEX(I2NODE_SIZE),1),2) = I2NODE_SIZE
!       Deallocate the temporary I2NODE_LOC array and the INDEX array
       DEALLOCATE(INDEX)
       DEALLOCATE(I2NODE_LOC)
! ********************************  
C----
      RETURN
      END
Chd|====================================================================
Chd|  ADD_NM1                       source/interfaces/inter3d1/i7remnode.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        INSERT_A                      source/interfaces/inter3d1/i7remnode.F
Chd|====================================================================
      SUBROUTINE ADD_NM1(IS     ,IRECT   ,IRTL     ,NREMOV  ,IREMOV  ,
     2                  IADA    ,TAGD   ,NEW       ,IFLAG    )                  
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4,*),IRTL(*), NREMOV,
     .        IREMOV(*),IFLAG,IADA  ,TAGD(*)  ,NEW,IS
C----IFLAG=0 -> dim   , IREMOV : Global node num. (SECONDARY) 
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II ,I,J  ,IL, L,NM,NNOD
C-----------------------------------------------
           I=IS
           NNOD=4
           L=IRTL(IS)
           IF (IRECT(4,L)==IRECT(3,L)) NNOD=3
           DO J=1,NNOD
            NM =IRECT(J,L)
            IF (IFLAG==0.AND.TAGD(NM)==0) THEN
             NREMOV =NREMOV+1
             TAGD(NM)=1
            ELSEIF (TAGD(NM)==0) THEN
             CALL INSERT_A(NREMOV,IREMOV,NM  ,IADA)
             NEW =NEW+1
             TAGD(NM)=1
            END IF 
           END DO
C----
      RETURN
      END
Chd|====================================================================
Chd|  ZERONM_TAGD                   source/interfaces/inter3d1/i7remnode.F
Chd|-- called by -----------
Chd|        REMN_I2OP                     source/interfaces/inter3d1/i7remnode.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ZERONM_TAGD(IS     ,IRECT   ,IRTL     ,TAGD   )                  
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4,*),IRTL(*), TAGD(*)  ,IS
C----IFLAG=0 -> dim   , IREMOV : Global node num. (SECONDARY) 
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II ,I,J  ,IL, L,NM,NNOD
C-----------------------------------------------
           I=IS
           NNOD=4
           L=IRTL(IS)
           IF (IRECT(4,L)==IRECT(3,L)) NNOD=3
           DO J=1,NNOD
            NM =IRECT(J,L)
            IF (TAGD(NM)==1) TAGD(NM)=0
           END DO
C----
      RETURN
      END
Chd|====================================================================
Chd|  REMN_I2_EDGOP                 source/interfaces/inter3d1/i7remnode.F
Chd|-- called by -----------
Chd|        ININTR2                       source/interfaces/inter3d1/inintr2.F
Chd|-- calls ---------------
Chd|        ADD_NSFIC1                    source/interfaces/inter3d1/i7remnode.F
Chd|        DIM_PTEDGN                    source/interfaces/inter3d1/i7remnode.F
Chd|        PRE_I2EDGE                    source/interfaces/inter3d1/i7remnode.F
Chd|        UPGRADE_REMNODE2              source/interfaces/interf1/upgrade_remnode.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE REMN_I2_EDGOP(IPARI   ,INTBUF_TAB   ,ITAB, NREMOV)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), ITAB(*),NREMOV(*),IFLAG

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,NTY,NN,NE2,IE,IE1,IE2,I,NNREM,NNREMEG
      INTEGER II,J,NMN,NSN,NRTS,NRTM,IADA,IEDGE,NSNE,NRTSE,IACT
      INTEGER, DIMENSION(:),ALLOCATABLE :: TAGN,TAGE,E2NODE
      INTEGER, DIMENSION(:,:),ALLOCATABLE :: PT_E2NODE
C-----------------------------------------------
C----like Irem_gap of int7, creat list of SECONDARY nodes to be removed per M_seg
       IACT=0  
       DO N=1,NINTER
        NTY=IPARI(7,N)
        NSNE  = IPARI(55,N)
        IF (NTY==24.AND.NREMOV(N) >0.AND.NSNE>0) IACT=1
       END DO
       IF (IACT==0) RETURN
C--------- DIM
      ALLOCATE(TAGN(NUMNOD))
       DO N=1,NINTER
        NTY=IPARI(7,N)
        NSN   =IPARI(5,N)
        NRTM  =IPARI(4,N)
        IEDGE =IPARI(59,N)
        NSNE  = IPARI(55,N)
        IF (NTY==24.AND.NREMOV(N) >0.AND.NSNE>0) THEN
         TAGN(1:NUMNOD)=0
         DO J=1,INTBUF_TAB(N)%KREMNODE(NRTM+1)
          NN = INTBUF_TAB(N)%REMNODE(J)
          TAGN(NN)=1
         END DO
         CALL DIM_PTEDGN(NE2,NSNE,INTBUF_TAB(N)%IS2SE,INTBUF_TAB(N)%IRTSE,TAGN)
         IF (NE2==0) CYCLE
         ALLOCATE(E2NODE(NE2),PT_E2NODE(NUMNOD,2))
         CALL PRE_I2EDGE(NE2,NSNE,INTBUF_TAB(N)%IS2SE,INTBUF_TAB(N)%IRTSE,TAGN,
     +                   E2NODE,PT_E2NODE)
C---- dim
         IFLAG = 0
         ALLOCATE(TAGE(NSNE))
         TAGE(1:NSNE)=0
         NNREMEG=0
         CALL ADD_NSFIC1(NRTM,NNREMEG,INTBUF_TAB(N)%KREMNODE,INTBUF_TAB(N)%REMNODE,
     +                  E2NODE,PT_E2NODE,TAGE,IFLAG)
         IF (NNREMEG>0) THEN
           NNREMEG = NNREMEG + NREMOV(N)
           CALL UPGRADE_REMNODE2(N,NNREMEG,INTBUF_TAB(N),NTY)
           IFLAG = 1
           CALL ADD_NSFIC1(NRTM,NREMOV(N),INTBUF_TAB(N)%KREMNODE,INTBUF_TAB(N)%REMNODE,
     +                     E2NODE,PT_E2NODE,TAGE,IFLAG)
         END IF !(NNREMEG>0) THEN
         DEALLOCATE(E2NODE,PT_E2NODE,TAGE)
        END IF !(NTY==24.AND.NREMOV(N) >0.AND.NSNE>0) THEN
       END DO
C-----
      DEALLOCATE(TAGN)
C----
      RETURN
      END
Chd|====================================================================
Chd|  DIM_PTEDGN                    source/interfaces/inter3d1/i7remnode.F
Chd|-- called by -----------
Chd|        REMN_I2_EDGOP                 source/interfaces/inter3d1/i7remnode.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DIM_PTEDGN(NSIZE,NSNE,IS2SE,IRTSE,TAGN)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSIZE,NSNE,IS2SE(2,*),IRTSE(5,*),TAGN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IK1(4),IK2(4),NS1,NS2,IED,I,J,IE1,IE2,IE
      DATA IK1 /1,2,3,4/
      DATA IK2 /2,3,4,1/
C-----
       NSIZE =0
      DO I=1,NSNE
       IE1 = IS2SE(1,I)
       IE2 = IS2SE(2,I)
       IF (IE1 > 0) THEN
         IE = IE1
         IED=IRTSE(5,IE)
         NS1= IRTSE(IK1(IED),IE)
         NS2= IRTSE(IK2(IED),IE)
       ELSEIF(IE2 > 0) THEN
         IE = IE2
         IED=IRTSE(5,IE)
         NS1= IRTSE(IK2(IED),IE)
         NS2= IRTSE(IK1(IED),IE)
       ELSE
        print *,'problem EDGE **** I,IE1,IE2=',I,IE1,IE2
       END IF
       IF (TAGN(NS1)>0.AND.TAGN(NS2)>0) THEN
        NSIZE = NSIZE + 2
       END IF
      END DO ! I=1,NSNE        
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  PRE_I2EDGE                    source/interfaces/inter3d1/i7remnode.F
Chd|-- called by -----------
Chd|        REMN_I2_EDGOP                 source/interfaces/inter3d1/i7remnode.F
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|====================================================================
      SUBROUTINE PRE_I2EDGE(NSIZE,NSNE,IS2SE,IRTSE,TAGN,E2NODE,PT_E2NODE)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSIZE,NSNE,IS2SE(2,*),IRTSE(5,*),TAGN(*),
     .        E2NODE(NSIZE),PT_E2NODE(NUMNOD,2)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IK1(4),IK2(4),NS1,NS2,IED,I,J,IE1,IE2,IE
      INTEGER :: WORK(70000)
      INTEGER :: COMPTEUR,L,L1
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: E2NODE_LOC
      DATA IK1 /1,2,3,4/
      DATA IK2 /2,3,4,1/
C-----
! ********************************

!       E2NODE : | 1th EDGE node ns1 |  fictive node  |
!                | 2nd EDGE node ns2 |  fictive node  |

!      Compute the size of E2NODE array
!     Allocate the temporary E2NODE array to I2NODE_SIZE , 2
      ALLOCATE(E2NODE_LOC(NSIZE,2))
      
       COMPTEUR=1
      DO I=1,NSNE
       IE1 = IS2SE(1,I)
       IE2 = IS2SE(2,I)
       IF (IE1 > 0) THEN
         IE = IE1
         IED=IRTSE(5,IE)
         NS1= IRTSE(IK1(IED),IE)
         NS2= IRTSE(IK2(IED),IE)
       ELSEIF(IE2 > 0) THEN
         IE = IE2
         IED=IRTSE(5,IE)
         NS1= IRTSE(IK2(IED),IE)
         NS2= IRTSE(IK1(IED),IE)
       ELSE
        print *,'problem EDGE **** I,IE1,IE2=',I,IE1,IE2
       END IF
       IF (TAGN(NS1)>0.AND.TAGN(NS2)>0) THEN
            E2NODE_LOC(COMPTEUR,1) = NS1                                 ! node surf/MAIN
            E2NODE_LOC(COMPTEUR,2) = I                                 ! SECONDARY_id
            COMPTEUR = COMPTEUR + 1 
            ! 2
            E2NODE_LOC(COMPTEUR,1) = NS2                                 ! node surf/MAIN
            E2NODE_LOC(COMPTEUR,2) = I                                 ! SECONDARY_id
            COMPTEUR = COMPTEUR + 1 
       END IF
      END DO ! I=1,NSNE  
       ALLOCATE( INDEX(2*NSIZE) )
       DO I=1,2*NSIZE
        INDEX(I)=I
       ENDDO
       CALL MY_ORDERS( 0, WORK, E2NODE_LOC(1,1), INDEX, NSIZE , 1)    
       COMPTEUR = 1
       PT_E2NODE(1:NUMNOD,1:2) = 0
       PT_E2NODE(E2NODE_LOC(INDEX(1),1),1) = 1
       DO I=1,NSIZE
!       Initialization of E2NODE array
        E2NODE(COMPTEUR) = E2NODE_LOC(INDEX(I),2)
!       Initialization of POINT_I2NODE array
        IF(PT_E2NODE(E2NODE_LOC(INDEX(I),1),1)==0) THEN 
          PT_E2NODE(E2NODE_LOC(INDEX(I),1),1) = COMPTEUR
          PT_E2NODE(E2NODE_LOC(INDEX(I-1),1),2) = COMPTEUR - 1
        ENDIF
        COMPTEUR=COMPTEUR+1
       ENDDO
       PT_E2NODE(E2NODE_LOC(INDEX(NSIZE),1),2) = NSIZE
!       Deallocate the temporary I2NODE_LOC array and the INDEX array
       DEALLOCATE(INDEX)
       DEALLOCATE(E2NODE_LOC)
! ********************************  
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  ADD_NSFIC1                    source/interfaces/inter3d1/i7remnode.F
Chd|-- called by -----------
Chd|        REMN_I2_EDGOP                 source/interfaces/inter3d1/i7remnode.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ADD_NSFIC1(NRTM,NREMOV,KREMNODE,REMNODE,E2NODE,PT_E2NODE,
     +                      TAGN,IFLAG)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTM,NREMOV,KREMNODE(*),REMNODE(*),
     +        E2NODE(*),PT_E2NODE(NUMNOD,2),TAGN(*),IFLAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IE,IE1,IE2,I,NNREM,NN,KREMOV_OLD,NM,NII
      INTEGER NS1,NS2,IED,J,II,IADA,NS,NEW,NR0,IADN
      INTEGER, DIMENSION(:), ALLOCATABLE :: KREMN_CP,REMN_CP
C-----
       IF (IFLAG==0) THEN
        DO II=1,NRTM
         DO I = KREMNODE(II)+1,KREMNODE(II+1)
          NS = REMNODE(I)
          IF (PT_E2NODE(NS,1)==0) CYCLE
          DO J = PT_E2NODE(NS,1),PT_E2NODE(NS,2)
             NN = E2NODE(J)
             IF (TAGN(NN)==0) THEN
              NREMOV = NREMOV + 1
              TAGN(NN) = 1
             END IF
          END DO !J=
         END DO
C reset  TAGN(NN) =0         
         DO I = KREMNODE(II)+1,KREMNODE(II+1)
          NS = REMNODE(I)
          IF (PT_E2NODE(NS,1)==0) CYCLE
          DO J = PT_E2NODE(NS,1),PT_E2NODE(NS,2)
             NN = E2NODE(J)
             TAGN(NN) = 0
          END DO !J=
         END DO
        END DO !II=1,NRTM
       ELSE
C-------- copy KREMNODE,REMNODE in *_CP and re-build KREMNODE,REMNODE to avoid insert       
        ALLOCATE(KREMN_CP(NRTM+1),REMN_CP(NREMOV))
        KREMN_CP(1:NRTM+1)=KREMNODE(1:NRTM+1)
        REMN_CP(1:NREMOV)=REMNODE(1:NREMOV)
        DO II=1,NRTM
C--------old first 
         NII = KREMN_CP(II+1)-KREMN_CP(II)      
         DO I = 1,NII
          REMNODE(KREMNODE(II)+I)=REMN_CP(KREMN_CP(II)+I)
         END DO
C--------fictive nodes
         NEW = KREMNODE(II) + NII
         DO I = KREMN_CP(II)+1,KREMN_CP(II+1)
          NS = REMN_CP(I)
          IF (PT_E2NODE(NS,1)==0) CYCLE
          DO J = PT_E2NODE(NS,1),PT_E2NODE(NS,2)
            NN = E2NODE(J)
            IF (TAGN(NN)==0) THEN
             NM = NN + NUMNOD
             NEW = NEW + 1
             REMNODE(NEW)=NM
             TAGN(NN)=1
            END IF 
          END DO !J=
         END DO !I = 
C reset  TAGN(NN) =0         
         DO I = KREMN_CP(II)+1,KREMN_CP(II+1)
          NS = REMN_CP(I)
          IF (PT_E2NODE(NS,1)==0) CYCLE
          DO J = PT_E2NODE(NS,1),PT_E2NODE(NS,2)
             NN = E2NODE(J)
             TAGN(NN) = 0
          END DO !J=
         END DO
         KREMNODE(II+1) = NEW
        END DO !II=1,NRTM
        DEALLOCATE(KREMN_CP,REMN_CP)
       END IF !(IFLAG==0) THEN
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  RI2_INT24P_INI                source/interfaces/inter3d1/i7remnode.F
Chd|-- called by -----------
Chd|        ININTR2                       source/interfaces/inter3d1/inintr2.F
Chd|-- calls ---------------
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        RM_CAND24                     source/interfaces/inter3d1/i7remnode.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE RI2_INT24P_INI(IPARI   ,INTBUF_TAB   ,ITAB, NOM_OPT,NREMOV)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), ITAB(*),NREMOV(*),IFLAG
      INTEGER NOM_OPT(LNOPT1,*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,I_STOK,NTY,ID
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C----remove pene_ini of int24 with Irem_i2 
C------INTBUF_TAB(N)%CAND_E,INTBUF_TAB(N)%CAND_N
       DO N=1,NINTER
        NTY=IPARI(7,N)
        IF (NTY==24.AND.NREMOV(N) >0) THEN
        ID=NOM_OPT(1,N)
        CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,N),LTITR)
         I_STOK = INTBUF_TAB(N)%I_STOK(1)
         CALL RM_CAND24(I_STOK,ID,TITR,INTBUF_TAB(N)%CAND_N,INTBUF_TAB(N)%CAND_E,
     +                  INTBUF_TAB(N)%KREMNODE,INTBUF_TAB(N)%REMNODE,
     +                  INTBUF_TAB(N)%NSV,INTBUF_TAB(N)%IRTLM,
     +                  INTBUF_TAB(N)%PENE_OLD,ITAB )
         INTBUF_TAB(N)%I_STOK(1) = I_STOK 
        END IF
       END DO
C----
      RETURN
      END
Chd|====================================================================
Chd|  RM_CAND24                     source/interfaces/inter3d1/i7remnode.F
Chd|-- called by -----------
Chd|        RI2_INT24P_INI                source/interfaces/inter3d1/i7remnode.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE RM_CAND24(I_STOK,ID,TITR,CAND_N,CAND_E,KREMNODE  ,REMNOD ,
     *                     NSV   ,IRTLM,PENE_OLD,ITAB  )
C
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
      INTEGER I_STOK,CAND_E(*),CAND_N(*),KREMNODE(*),IRTLM(2,*),REMNOD(*),
     *        NSV(*),ID,ITAB(*)
C     REAL
      my_real
     .   PENE_OLD(5,*)
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NE, I,NS,NI,I_RM(I_STOK),K,L,J,II_STOK,NRM
      INTEGER ITAG(NUMNOD)
C     REAL
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
            DO I=1,I_STOK
             I_RM(I) = 0
             NI = CAND_N(I)
             NE = CAND_E(I)
             NS = NSV(NI)
             K = KREMNODE(NE)+1
             L = KREMNODE(NE+1)  
             DO J=K,L
              IF (REMNOD(J)==NS) THEN
               I_RM(I) = 1
               CYCLE
              END IF
             END DO             
            END DO          
C--------reput   IRTLM(1,NI)=0,PEN_OLD(5,NI)=0 et compact CAND_N,CAND_E ,I_STOK
            II_STOK = 0       
            DO I=1,I_STOK
             IF (I_RM(I) == 1) THEN
              NI = CAND_N(I)
              IRTLM(1,NI) = 0
              PENE_OLD(5,NI)=ZERO
             ELSE
              II_STOK = II_STOK+1       
              CAND_N(II_STOK) = CAND_N(I)
              CAND_E(II_STOK) = CAND_E(I)
             END IF
            END DO
            NRM =  I_STOK-II_STOK   
C ----- message out
            IF (NRM >0) THEN            
                                CALL ANCMSG(MSGID=1637,
     .                                  MSGTYPE=MSGWARNING,
     .                                  ANMODE=ANINFO_BLIND_1,
     .                                  I1=ID,
     .                                  C1=TITR,
     .                                  I2=NRM)
             IF(IPRI>=5) THEN
              WRITE(IOUT,*) 'REMOVED SECONDARY NODE WITH INITIAL PENETRATION:'
              K = 0
              ITAG(1:NUMNOD)=0
              DO I=1,I_STOK
               IF (I_RM(I) == 1) THEN
                NI = CAND_N(I)
                NS = NSV(NI)
                IF (NS <= NUMNOD .AND. ITAG(NS)==0 ) THEN
                 K = K + 1
                 I_RM(K) = NS
                 ITAG(NS)=1
                END IF
               END IF
              END DO
              WRITE(IOUT,FMT=FMW_10I) (ITAB(I_RM(J)),J=1,NRM)
             END IF!(IPRI>=5) THEN
            END IF !(NRM >0) THEN            
            I_STOK= II_STOK 
C      
      RETURN
      END
