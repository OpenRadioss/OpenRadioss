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
Chd|  I24INISUR_NEI                 source/interfaces/inter3d1/i24inisu_nei.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        COMM_SEG_EN                   source/interfaces/inter3d1/i24inisu_nei.F
Chd|        MSG_ERR                       source/interfaces/inter3d1/i24tools.F
Chd|====================================================================
      SUBROUTINE I24INISUR_NEI(NRTM ,NSN,IRECT,IRTLM,MVOISIN,
     2                         NVOISIN,MSEGLO ,MSEGTYP,ITAB ,X ,
     3                         ID,TITR,IGEO )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTM,NSN,IRECT(4,NRTM),MVOISIN(4,NRTM),NVOISIN(8,NRTM),
     .        MSEGLO(NRTM),IRTLM(2,NSN),MSEGTYP(NRTM),ITAB(*),
     .        IGEO(NPROPGI,*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,IW,I1,I2,I3,M,NMAX,N_MAX,E_MAX,E_ID,N_EI,
     1        ITAG(NUMNOD),N_NI,NE0,NN0,NRTM_SH,NRTM0,
     2        J1,J2,J3,J4,K1,K2,L1,L2,KPERM1(4),KPERM2(4),IRR,
     3        NFT,JLT
      INTEGER, DIMENSION(:),ALLOCATABLE :: MVOI,NVOI
      INTEGER, DIMENSION(:,:),ALLOCATABLE :: EIDNOD
      my_real
     .   X(*)
      DATA KPERM1/1,3,5,7/
      DATA KPERM2/2,4,6,8/
C init IRTLM (done before) MSEGLO
C      IRTLM(1:2,1:NSN)=0
      DO I=1,NRTM
        MSEGLO(I)=I      
      ENDDO
C-----------shell segs have been duplicated w/ inverse order
C-----------for the moment all antisymmetry surface will be stored at the end
      DO I=1,NUMNOD
        ITAG(I)=0
      ENDDO
      DO I=1,NRTM
       DO J=1,3
        M=IRECT(J,I)
        ITAG(M)=ITAG(M)+1
       END DO
       IF (IRECT(4,I)/=IRECT(3,I))THEN
         M= IRECT(4,I)
         ITAG(M)=ITAG(M)+1
       END IF
      END DO
C-----MSEGTYP (<0 for i=NRTM0+1,NRTM0+NRTM_SH) -> IM2SH---------
C-----------max number of connected segment per node
      NMAX=0
      DO I=1,NUMNOD
       NMAX=MAX(NMAX,ITAG(I))
       ITAG(I)=0
      ENDDO
      ALLOCATE(MVOI(NMAX+10),NVOI(2*NMAX+10),EIDNOD(NMAX,NUMNOD))
      EIDNOD=0
C------------ini- E_ids of each node
      DO I=1,NRTM
       DO J=1,3
        M=IRECT(J,I)
        ITAG(M)=ITAG(M)+1
        EIDNOD(ITAG(M),M)=I
       END DO
       IF (IRECT(4,I)/=IRECT(3,I)) THEN
        M= IRECT(4,I)
        ITAG(M)=ITAG(M)+1
        EIDNOD(ITAG(M),M)=I
       END IF
      END DO
C------------MVOISIN-(seg number)-,NVOISIN (node number)---
      E_MAX=4
      N_MAX=8
      DO I=1,NRTM
       DO J=1,N_MAX
        NVOISIN(J,I)=0
       END DO
       DO J=1,E_MAX
        MVOISIN(J,I)=0
       END DO
      END DO
C
      DO I=1,NRTM
       N_EI=0
       N_NI=0
C----seg 1-2------
       I1 =IRECT(1,I)
       I2 =IRECT(2,I)
       CALL COMM_SEG_EN(ITAG(I1),EIDNOD(1,I1),ITAG(I2),EIDNOD(1,I2),
     1                  N_EI,MVOI ,I   ,I1 ,I2 ,IRECT,MSEGTYP  ,
     2                  N_NI,NVOI ,X  ,MVOISIN(1,I),NVOISIN(1,I),
     3                  IRR   )
       IF (IRR >0) CALL MSG_ERR(I1,I2,ITAB,IRR,ID,TITR)
C----seg 2-3------
       I1 =IRECT(2,I)
       I2 =IRECT(3,I)
       CALL COMM_SEG_EN(ITAG(I1),EIDNOD(1,I1),ITAG(I2),EIDNOD(1,I2),
     1                  N_EI,MVOI ,I   ,I1 ,I2 ,IRECT,MSEGTYP  ,
     2                  N_NI,NVOI ,X  ,MVOISIN(1,I),NVOISIN(1,I),
     3                  IRR   )
       IF (IRR >0) CALL MSG_ERR(I1,I2,ITAB,IRR,ID,TITR)
C----seg 3-4------
       I1 =IRECT(3,I)
       I2 =IRECT(4,I)
       CALL COMM_SEG_EN(ITAG(I1),EIDNOD(1,I1),ITAG(I2),EIDNOD(1,I2),
     1                  N_EI,MVOI ,I   ,I1 ,I2 ,IRECT,MSEGTYP  ,
     2                  N_NI,NVOI ,X  ,MVOISIN(1,I),NVOISIN(1,I),
     3                  IRR   )
       IF (IRR >0) CALL MSG_ERR(I1,I2,ITAB,IRR,ID,TITR)
C----seg 1-4------
       I1 =IRECT(4,I)
       I2 =IRECT(1,I)
       CALL COMM_SEG_EN(ITAG(I1),EIDNOD(1,I1),ITAG(I2),EIDNOD(1,I2),
     1                  N_EI,MVOI ,I   ,I1 ,I2 ,IRECT,MSEGTYP  ,
     2                  N_NI,NVOI ,X  ,MVOISIN(1,I),NVOISIN(1,I),
     3                  IRR   )
       IF (IRR >0) CALL MSG_ERR(I1,I2,ITAB,IRR,ID,TITR)

c sous-triangles
c        goto 6543
       DO K1=1,4
         K2=K1+1
         IF(K2==5)K2=1
         I1 = IRECT(K1,I)
         I2 = IRECT(K2,I)
         J  = MVOISIN(K1,I)
		 IF (J==0) CYCLE
         J1 = IRECT(1,J)
         J2 = IRECT(2,J)
         J3 = IRECT(3,J)
         J4 = IRECT(4,J)
         L1 = KPERM1(K1)
         L2 = KPERM2(K1)
C-------------tria do nothing : +,+ -> 1         
         IF (J3==J4) THEN
C         
         ELSEIF(J2==I2.and.J3==I1)THEN
c          sous-triangle 2
           NVOISIN(L1,I)=-NVOISIN(L1,I)
         ELSEIF(J3==I2.and.J4==I1)THEN
c          sous-triangle 3
           NVOISIN(L2,I)=-NVOISIN(L2,I)
         ELSEIF(J4==I2.and.J1==I1)THEN
c          sous-triangle 4
           NVOISIN(L1,I)=-NVOISIN(L1,I)
           NVOISIN(L2,I)=-NVOISIN(L2,I)
c        ELSE  sous-triangle 1
         ENDIF
       ENDDO
C
      END DO !I=1,NRTM
C      
      DEALLOCATE(MVOI,NVOI,EIDNOD)
C
c      DO I=1,NRTM
c       print *,'N_E(I),MSEGTYP(I),I=',N_E(I),MSEGTYP(I),I
c       print *,'MVOISIN(1,I)=',(MVOISIN(J,I),J=1,N_E(I))
c      END DO
c      DO I=1,NRTM
c       print *,'IRECT(j,I)=',(ITAB(IRECT(J,I)),J=1,4)
c       print *,'N_N(I),I=',N_N(I),I
c       print *,'NVOISIN(1,I)=',(ITAB(NVOISIN(J,I)),J=1,N_N(I))
c      END DO
      RETURN
      END
Chd|====================================================================
Chd|  COMM_SEG_EN                   source/interfaces/inter3d1/i24inisu_nei.F
Chd|-- called by -----------
Chd|        I24INISUR_NEI                 source/interfaces/inter3d1/i24inisu_nei.F
Chd|-- calls ---------------
Chd|        COMM_SEG_E                    source/interfaces/inter3d1/i24inisu_nei.F
Chd|        COMM_SEG_N                    source/interfaces/inter3d1/i24inisu_nei.F
Chd|        REMOVEIC1                     source/interfaces/inter3d1/i24tools.F
Chd|        RE_ORI                        source/interfaces/inter3d1/i24tools.F
Chd|        SEG_OPP                       source/interfaces/inter3d1/i24tools.F
Chd|====================================================================
      SUBROUTINE COMM_SEG_EN(N1,IED1,N2,IED2,NE,ICE,ISELF,
     .                       I1,I2,IRECT,IM2SH ,NN,ICN ,X  ,IE,IN ,IRR)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N1,IED1(*),N2,IED2(*),NE,ICE(*),ISELF,IASYM,IRR,
     .        I1,I2,IRECT(4,*),NN,ICN(*),IE(*),IN(*),IM2SH(*)
      my_real
     .   X(3,*)
C-----------------------------------------------
c FUNCTION: find neighbour segment and nodes which share the same nodes I1,I2
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   N1,IED1(N1)       - number and neighbour segment id list of node I1
c  I   N2,IED2(N2)       - number and neighbour segment id list of node I2
c  O   NE,ICE(NE)        - Number and neighbour segment id list of segment id ISELF
c  I   ISELF,I1,I2       - input segment id ISELF w/ nodes I1,I2 (commun nodes)
c  I   IRECT(4,*)        - connectivity of segment id *
c  I   X(3,*)            - node co-ordinates
c  O   NN,ICN(NN)        - Number and neighbour node list
c  O   IE(NE),IN(NN)     - final (reduced) neighbour segment,node arries
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NEW,K,M,NE0,NN0,DNE,IOP
C---------------------
       IRR = 0
       NE0=NE
       NN0=NN
       IASYM = IABS(IM2SH(ISELF))
C-------------neighbour segments-----------       
       CALL COMM_SEG_E(N1,IED1,N2,IED2,NE,ICE,ISELF,
     .                I1,I2,IRECT,IASYM )
C-------------treatement of multi-neighbours (T form,shell) segments->reduce to 1----------
       DNE = NE-NE0
       IF (DNE > 1) THEN
         CALL REMOVEIC1(DNE,ICE(NE0+1),ISELF,IRECT,X ,I1,I2,IASYM,IRR)
         NE=NE0+1
       END IF
       IF (ICE(NE)>0 )THEN
        CALL SEG_OPP(ISELF,ICE(NE),IRECT,X ,IOP)
        IF (IOP > 0 ) ICE(NE) = 0
       END IF !(ICE(NE)>0 )THEN
       CALL COMM_SEG_N(NE0,NE,ICE,NN,ICN,ISELF,I1,I2,IRECT)
C-------------after convention--------------
       IF ((NN-NN0)==2) CALL RE_ORI(I1,I2,ICN(NN0+1),X  )
C 
       IF ((NE-NE0) >1 .OR.(NN-NN0)> 2) THEN
C        print *,'!!!error (report to developer)!!!',(NE-NE0),(NN-NN0)
        IRR=12
       END IF 
       DO I=1+NE0,NE
        IE(I)=ICE(I)
       END DO
       DO I=1+NN0,NN
        IN(I)=ICN(I)
       END DO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  COMM_SEG_E                    source/interfaces/inter3d1/i24inisu_nei.F
Chd|-- called by -----------
Chd|        COMM_SEG_EN                   source/interfaces/inter3d1/i24inisu_nei.F
Chd|-- calls ---------------
Chd|        ADD_ID                        source/interfaces/inter3d1/i24tools.F
Chd|        INTAB                         source/interfaces/inter3d1/i24tools.F
Chd|        SAME_SEG                      source/interfaces/inter3d1/i24tools.F
Chd|====================================================================
      SUBROUTINE COMM_SEG_E(N1,IED1,N2,IED2,N,IC,ISELF,
     .                      I1,I2,IRECT,IASYM)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N1,IED1(*),N2,IED2(*),N,IC(*),ISELF,
     .        I1,I2,IRECT(4,*),IASYM
C-----------------------------------------------
c FUNCTION: find neighbour segment and which share the same nodes I1,I2
c        ---neighbour node arry will be buid after taking into account of treatment w/ IC 
c           (remains only one segment at the end)
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   N1,IED1(N1)       - number and neighbour segment id list of node I1
c  I   N2,IED2(N2)       - number and neighbour segment id list of node I2
c  O   N,IC(N)           - Number and neighbour segment id list of segment id ISELF
c  I   ISELF,I1,I2       - input segment id ISELF w/ nodes I1,I2 (commun nodes)
c  I   IRECT(4,*)        - connectivity of segment id *
C-----------------------------------------------
C   External function
C-----------------------------------------------
      LOGICAL INTAB,SAME_SEG
      EXTERNAL INTAB,SAME_SEG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NEW,K,M,LING,NE,NN
      DATA LING/0/
C----add commun ID--at end--------------------------
      IF (I1==I2) THEN
C----add 0 in IC as convention ----------------------------
        CALL ADD_ID(N,IC,LING)
      ELSE
       NE=N
       DO J=1,N2
        NEW=IED2(J)
        IF (NEW==ISELF.OR.NEW==IASYM) CYCLE
        IF (INTAB(N1,IED1,NEW)) THEN
         IF (.NOT.SAME_SEG(IRECT(1,ISELF),IRECT(1,NEW)))
     .      CALL ADD_ID(N,IC,NEW)
        END IF
       END DO 
C----add 0 for IC if find nothing -> consisting w/ ICN------------------------
       IF (NE==N) CALL ADD_ID(N,IC,LING)
      END IF !(I1==I2) THEN
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  COMM_SEG_N                    source/interfaces/inter3d1/i24inisu_nei.F
Chd|-- called by -----------
Chd|        COMM_SEG_EN                   source/interfaces/inter3d1/i24inisu_nei.F
Chd|-- calls ---------------
Chd|        ADD_N_ID                      source/interfaces/inter3d1/i24tools.F
Chd|====================================================================
      SUBROUTINE COMM_SEG_N(NE0,NE,ICE,NN,ICN,ISELF,I1,I2,IRECT)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NE0,NE,ICE(*),NN,ICN(*),ISELF,I1,I2,IRECT(4,*)
C-----------------------------------------------
c FUNCTION: find neighbour nodes and which share the same nodes I1,I2
c----------maximum 2 nodes will be chosen par two commun nodes I1,I2
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  O   NE0,NE,ICE(NE)    - Number and neighbour seg id
c  I   ISELF,I1,I2       - input segment id ISELF w/ nodes I1,I2 (commun nodes)
c  I   IRECT(4,*)        - connectivity of segment id *
c  O   NN,ICN(NN)        - Number and neighbour node list
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NEW,K,M,LING,NN0
      DATA LING/0/
C----add commun ID--at end--------------------------
      IF (I1==I2) THEN
C----add 0 in ICN as convention ----------------------------
        CALL ADD_N_ID(NN,ICN,LING)
        CALL ADD_N_ID(NN,ICN,LING)
      ELSE
       NN0=NN
       DO J=NE0+1,NE
        NEW=ICE(J)
        IF (NEW ==0) CYCLE
         DO K=1,3
          M=IRECT(K,NEW)
          IF (M/=I1.AND.M/=I2) CALL ADD_N_ID(NN,ICN,M)
         END DO
         M=IRECT(4,NEW)
         IF (M /= IRECT(3,NEW)) THEN
          IF (M/=I1.AND.M/=I2) CALL ADD_N_ID(NN,ICN,M)
         ELSE
C----add 0 for tria ----------------------------
          CALL ADD_N_ID(NN,ICN,LING)
         END IF
       END DO 
C----add 0 for IC if find nothing -> consisting w/ ICN------------------------
       IF (NN==NN0) THEN
        CALL ADD_N_ID(NN,ICN,LING)
        CALL ADD_N_ID(NN,ICN,LING)
       END IF
      END IF !(I1==I2) THEN
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  I24INI_GAP_N                  source/interfaces/inter3d1/i24inisu_nei.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I24INI_GAP_N(
     1 NRTM  ,IRECT ,IXS   ,GEO   ,IXC   ,IXTG  ,
     2 IXT   ,IXP   ,IPART  ,IPARTC   ,IPARTTG  ,
     3 THK   ,THK_PART,NVOISIN ,GAP_N ,GAP_M    ,
     4 NMN   ,MSR   ,GAPN_M,GAP_N0,INTPLY  ,
     5 GAPMAX_M ,IGEO,MSEGTYP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTM,IRECT(4,*), IXS(NIXS,*), IXC(NIXC,*),
     .   IXTG(NIXTG,*), IXT(NIXT,*), IXP(NIXP,*),
     .   IPART(LIPART1,*), IPARTC(*), IPARTTG(*),
     .   NVOISIN(8,*),MSR(*),NMN,INTPLY,IGEO(NPROPGI,*),
     .   MSEGTYP(*)
C     REAL ou REAL*8
      my_real
     .   GEO(NPROPG,*), THK(*),THK_PART(*),GAP_N(12,*),GAP_M(*),
     .   GAPN_M(*),GAP_N0(12,*),GAPMAX_M
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,IW,I1,I2,I3,MG,M,IP,IGTYP
      my_real
     .   WA(NUMNOD),DX
C init 
C
       DO I=1,NUMNOD
        WA(I)=ZERO
       END DO
C        
C------------------------------------
C     GAP NOEUDS IN WA
C------------------------------------
       DO I=1,NUMELC
         MG=IXC(6,I)
         IGTYP = IGEO(11,MG)
         IP = IPARTC(I)
         IF ( THK_PART(IP) .NE. ZERO .AND. IINTTHICK .EQ. 0) THEN
           DX=UNDEMI*THK_PART(IP)
         ELSEIF ( THK(I) .NE. ZERO .AND. IINTTHICK .EQ. 0) THEN
           DX=UNDEMI*THK(I)
         ELSEIF(IGTYP == 17) THEN
           DX=UNDEMI*THK(I)
         ELSE
           DX=UNDEMI*GEO(1,MG)
         ENDIF
         WA(IXC(2,I))=MAX(WA(IXC(2,I)),DX)
         WA(IXC(3,I))=MAX(WA(IXC(3,I)),DX)
         WA(IXC(4,I))=MAX(WA(IXC(4,I)),DX)
         WA(IXC(5,I))=MAX(WA(IXC(5,I)),DX)
       ENDDO
C
       DO I=1,NUMELTG
         MG=IXTG(5,I)
         IGTYP = IGEO(11,MG)
         IP = IPARTTG(I)
         IF ( THK_PART(IP) .NE. ZERO .AND. IINTTHICK .EQ. 0) THEN
           DX=UNDEMI*THK_PART(IP)
         ELSEIF ( THK(NUMELC+I) .NE. ZERO .AND. IINTTHICK .EQ. 0) THEN
           DX=UNDEMI*THK(NUMELC+I)
         ELSEIF(IGTYP == 17) THEN
           DX=UNDEMI*THK(NUMELC+I)
         ELSE
           DX=UNDEMI*GEO(1,MG)
         ENDIF
         WA(IXTG(2,I))=MAX(WA(IXTG(2,I)),DX)
         WA(IXTG(3,I))=MAX(WA(IXTG(3,I)),DX)
         WA(IXTG(4,I))=MAX(WA(IXTG(4,I)),DX)
       ENDDO
C--------exclude lines in master surfaces
C       DO I=1,NUMELT
C        MG=IXT(4,I)
C        DX=UNDEMI*SQRT(GEO(1,MG))
C        WA(IXT(2,I))=MAX(WA(IXT(2,I)),DX)
C        WA(IXT(3,I))=MAX(WA(IXT(3,I)),DX)
C       ENDDO
C
C       DO I=1,NUMELP
C        MG=IXP(5,I)
C        DX=UNDEMI*SQRT(GEO(1,MG))
C        WA(IXP(2,I))=MAX(WA(IXP(2,I)),DX)
C        WA(IXP(3,I))=MAX(WA(IXP(3,I)),DX)
C       ENDDO
C------------------------------------
C     INI GAP_N (4 + 8 voisins), GAP_M is modified taking into accound nodal gap
C------------------------------------
C -----due to the fact that if surf_M does not contain the defining w/ shell
C-------> should not take into account GAP_shell    
        DO I=1,NRTM
         IF (MSEGTYP(I)==0) THEN
           DO J=1,4
             M=IRECT(J,I)
             WA(M) = ZERO
           END DO
         END IF !(MSEGTYP(I)==0) THEN
        END DO  ! nrtm
C        
      DO I=1,NMN
       M = MSR(I)
       WA(M) = MIN(WA(M),GAPMAX_M)
       GAPN_M(I) = WA(M)
      END DO
C		 
      IF(INTPLY == 0) THEN
          DO I=1,NRTM
             GAP_M(I) = ZERO
             DO J=1,4
               M=IRECT(J,I)
               GAP_N(J,I)=WA(M)
               GAP_M(I) = MAX(GAP_M(I),WA(M))
             END DO
C          
             DO J= 1,8
              M=IABS(NVOISIN(J,I))
              IF (M > 0) THEN
                GAP_N(J+4,I)=WA(M)
              ELSE
                GAP_N(J+4,I)=ZERO
              END IF
            END DO
          END DO  ! nrtm
      ELSE
          DO I=1,NRTM
             GAP_M(I) = ZERO
             DO J=1,4
               M=IRECT(J,I)
               GAP_N(J,I)=WA(M)
               GAP_M(I) = MAX(GAP_M(I),WA(M))
               GAP_N0(J,I) = GAP_N(J,I)
             END DO
C          
             DO J= 1,8
              M=IABS(NVOISIN(J,I))
              IF (M >0) THEN
                GAP_N(J+4,I)=WA(M)
                GAP_N0(J+4,I) = WA(M)
              ELSE
                GAP_N(J+4,I)=ZERO
                GAP_N0(J+4,I) = ZERO
              END IF
            END DO
          END DO  ! nrtm
      ENDIF ! intply
C-----reset MSEGTYP(I)=0 for coating shell, engine uses MSEGTYP only for symmetry
        DO I=1,NRTM
        IF (MSEGTYP(I)>NRTM) MSEGTYP(I) =0 
        END DO  ! nrtm
C        
      RETURN
      END
