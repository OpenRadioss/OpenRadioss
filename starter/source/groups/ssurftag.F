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
Chd|  SSURFTAG                      source/groups/ssurftag.F      
Chd|-- called by -----------
Chd|        HM_READ_SURF                  source/groups/hm_read_surf.F  
Chd|-- calls ---------------
Chd|        SSURF10                       source/groups/ssurftag.F      
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        SURF_MOD                      share/modules1/surf_mod.F     
Chd|====================================================================
      SUBROUTINE SSURFTAG(IXS     ,IPARTS  ,NSEG0    ,IGRSURF ,TAGBUF,
     .                    NSEG    ,KNOD2ELS,NOD2ELS  ,IEXT    ,FLAG  ,
     .                    IXS10   ,IXS16   ,IXS20    ,IFRE    ,KEY   ,
     .                    KNOD2ELC,NOD2ELC ,KNOD2ELTG,NOD2ELTG,
     .                    IXC     ,IXTG    ,IPARTC   ,IPARTTG ,NINDX,
     .                    NINDX_SOL, NINDX_SOL10, INDX, INDX_SOL, INDX_SOL10,
     .                    SURF_ELM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE SURF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IPARTS(*),TAGBUF(*),
     .        KNOD2ELS(*),NOD2ELS(*),
     .        IXS10(6,*),IXS16(8,*),IXS20(12,*),
     .        KNOD2ELC(*),NOD2ELC(*),KNOD2ELTG(*),NOD2ELTG(*),
     .        IXC(NIXC,*),IXTG(NIXTG,*),IPARTC(*),IPARTTG(*)
      INTEGER IEXT,NSEG,FLAG,IFRE,NSEG0
      CHARACTER KEY*ncharkey
      INTEGER :: NINDX, NINDX_SOL, NINDX_SOL10
      INTEGER, DIMENSION(*) :: INDX,INDX_SOL, INDX_SOL10
      TYPE(PART_TYPE), DIMENSION(*) :: SURF_ELM
!
      TYPE (SURF_) :: IGRSURF
!       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*       
!       FLAG_GRBRIC : flag to initialize the INDX_SOL(10) arrays
!                     and optimize an old and expensive treatment in SSURFTAG
!                     = true for /SURF/GCBRIC
!                     = false  if /SURF/XXX is different from  /SURF/GCBRIC
!       ----------------
!       FLAG_GRBRIC = false (/SURF/XXX/ /= /SURF/GCBRIC) :
!       NINDX : number of tagged part 
!       INDX  : tagged part     
!       ----------------
!       FLAG_GRBRIC = true (/SURF/XXX/ = SURF/GCBRIC) :
!       NINDX_SOL(10) : number of the tagged solid(10) element
!                      --> need to split solid and solid10 
!                      for a treatment in the SSURFTAG routine
!                      only useful for /SURF/GRBRIC
!       INDX_SOL(10) : ID of the tagged solid(10) element
!                      --> need to split solid and solid10 
!                      for a treatment in the SSURFTAG routine
!                      only useful for /SURF/GRBRIC
!       SURF_ELM : PART_TYPE structure
!                  %NSOL(10) : number of element per part
!                  %SOL(10)_PART : ID of the element
!       ----------------
!       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,J,K,JS,KS,II,JJ,K1,K2,LL,FACE(4),FC10(3),NN,KK,I1,
     .        NI(4),NS(4),MI(4),MS(4),NMIN,MMIN,NF,MF,IPERM,N1,N2,I
      INTEGER NODTAG(NUMNOD),FASTAG(NUMELS),FACES(4,6),PWR(7),
     .        FACES10(3,6),NNS,ISHEL,ISEG
!     FLAG_PART : check for tagged part
      LOGICAL :: FLAG_PART      
!     NUM_PART : number of tagged part 
!     NUM_ELM : number of element in the tagged part
      INTEGER :: NUM_PART,NUM_ELM
      INTEGER :: ID_PART,JS_PART, JS_ELM        !       index
      DATA FACES/4,3,2,1,
     .           5,6,7,8,
     .           1,2,6,5,
     .           3,4,8,7,
     .           2,3,7,6,
     .           1,5,8,4/
      DATA FACES10/0,0,0,
     .             0,0,0,
     .             3,6,4,
     .             5,6,2,
     .             1,2,3,
     .             4,5,1/
      DATA PWR/1,2,4,8,16,32,64/
C=======================================================================
      FASTAG=0
C
      IF(IEXT==1)THEN
C
C       External surface only.
        DO JS=1,NUMELS8+NUMELS10
          IF(KEY(1:6)=='GRBRIC')THEN
            IF (TAGBUF(JS)==0) CYCLE          !case of tagged elems
          ELSE
            IF (TAGBUF(IPARTS(JS))==0) CYCLE  !case of tagged parts
          END IF
          DO JJ=1,6
            DO II=1,4
              NS(II)=IXS(FACES(II,JJ)+1,JS)
            END DO
C
C           keep only 1 occurrence of each node (triangles, degenerated cases...)
            DO K1=1,3
              DO K2=K1+1,4
                IF(NS(K2)==NS(K1))NS(K2)=0
              END DO
            END DO
            NF=0
            DO K1=1,4
              N1=NS(K1)
              IF(N1/=0)THEN
              NF=NF+1
              NS(NF)=N1
              END IF
            END DO
            IF (NF < 3)CYCLE
C
C           permute
            NMIN=NS(1)
            DO II=2,NF
              NMIN=MIN(NMIN,NS(II))
            END DO
            DO IPERM=1,NF
              IF(NMIN==NS(IPERM).AND.
     .           NS(MOD(IPERM,NF)+1)/=NS(IPERM))THEN
                DO II=1,NF
                  NI(II)=NS(MOD(II+IPERM-2,NF)+1)
                END DO
                EXIT
              END IF
            END DO
C
C           looks for an elt sharing the face.
            DO K=KNOD2ELS(NI(1))+1,KNOD2ELS(NI(1)+1)
              KS=NOD2ELS(K)
              IF(KS==JS .OR. KS > NUMELS8+NUMELS10)CYCLE
              IF (KEY(1:6)=='GRBRIC'.AND.TAGBUF(KS)==0.AND.IFRE==0)CYCLE ! if IFRE=0 on cherche la connectivite uniquement avec les elements du marques du groupe (cycle), sinon si IFRE=1 on cherche la connectivit� avec tout le monde
              IF (KEY(1:6)/='GRBRIC'.AND.TAGBUF(IPARTS(KS))==0)CYCLE     
              DO II=1,NF
                NODTAG(NI(II))=0
              END DO
              DO II=1,8
                NODTAG(IXS(II+1,KS))=1
              END DO
              NN=0
              DO II=1,NF
                NN=NN+NODTAG(NI(II))
              END DO
              IF(NN==NF)THEN
                DO KK=1,6
            DO II=1,4
              MS(II)=IXS(FACES(II,KK)+1,KS)
            END DO
C
C                 keep only 1 occurrence of each node (triangles, degenerated cases...)
            DO K1=1,3
              DO K2=K1+1,4
                IF(MS(K2)==MS(K1))MS(K2)=0
              END DO
            END DO
            MF=0
            DO K1=1,4
              N1=MS(K1)
              IF(N1/=0)THEN
                MF=MF+1
                MS(MF)=N1
              END IF
            END DO
            IF(MF /= NF)CYCLE
C
C                 permute
                MMIN=MS(1)
                DO II=2,MF
                  MMIN=MIN(MMIN,MS(II))
                END DO
            DO IPERM=1,MF
              IF(MMIN==MS(IPERM).AND.
     .                 MS(MOD(IPERM,MF)+1)/=MS(IPERM))THEN
                DO II=1,MF
                  MI(II)=MS(MOD(II+IPERM-2,MF)+1)
                END DO
                EXIT
              END IF
            END DO
                  IF(MI(1)==NI(1).AND.MI(NF)==NI(2))THEN
C                    FACTAG(JS) moins face jj
                     FASTAG(JS)=FASTAG(JS)+PWR(JJ)
                     GO TO 300
                  END IF
                END DO
              END IF
            END DO
 300        CONTINUE
          END DO
        END DO
      END IF
C-----------
      IF(KEY(1:6)/='GRBRIC') THEN
        FLAG_PART=.TRUE.
        NUM_PART = NINDX
      ELSE
        FLAG_PART=.FALSE.        
        NUM_PART = 1
        NUM_ELM = NINDX_SOL
      ENDIF
      DO JS_PART=1,NUM_PART
        IF(FLAG_PART) THEN
                ID_PART = INDX(JS_PART)
                NUM_ELM = SURF_ELM(ID_PART)%NSOL
        ENDIF
        DO JS_ELM=1,NUM_ELM
          IF(FLAG_PART) THEN
                JS = SURF_ELM(ID_PART)%SOL_PART( JS_ELM )
          ELSE
                JS = INDX_SOL( JS_ELM )
                
          ENDIF

!      DO JS=1,NUMELS8
!        IF ((KEY(1:6)/='GRBRIC'.AND.IABS(TAGBUF(IPARTS(JS))) == 1).OR.
!     .      (KEY(1:6)=='GRBRIC'.AND.IABS(TAGBUF(JS)) == 1) ) THEN
          LL=FASTAG(JS)
          DO JJ=1,6
            IF(MOD(LL,PWR(JJ+1))/PWR(JJ)/=0)CYCLE
C
C           still needs to filter degenerated faces
            DO K1=1,4
              I1      =FACES(K1,JJ)+1
              FACE(K1)=IXS(I1,JS)
            END DO
            DO K1=1,4
              N1=FACE(K1)
              DO K2=1,4
          IF(K2/=K1)THEN
            N2=FACE(K2)
            IF(N2==N1)FACE(K2)=0
          END IF
              END DO
            END DO
            NN=0
            DO K1=1,4
              N1=FACE(K1)
              IF(N1/=0)THEN
              NN=NN+1
              FACE(NN)=N1
              END IF
            END DO
C---   find shells SURF/PART/EXT
            IF(FLAG == 0 .and. NN == 3) THEN 
              KS = 0 
              ISHEL = 0
              DO K=KNOD2ELTG(FACE(1))+1,KNOD2ELTG(FACE(1)+1)
                KS=NOD2ELTG(K)
                ISHEL = 0
                DO I=1,3
                  DO J=1,3
                    IF(FACE(I) == IXTG(J+1,KS)) ISHEL = ISHEL + 1
                  ENDDO
                ENDDO
                IF (ISHEL == 3)EXIT
                KS = 0
              ENDDO
              IF(KS == 0)THEN
                NSEG = NSEG + 1
              ELSEIF (IABS(TAGBUF(IPARTTG(KS))) /= 1) THEN
                NSEG = NSEG + 1
              ENDIF
            ELSEIF(FLAG == 0 .and. NN == 4) THEN 
              KS = 0
              ISHEL = 0
              DO K=KNOD2ELC(FACE(1))+1,KNOD2ELC(FACE(1)+1)
                KS=NOD2ELC(K)
                ISHEL = 0
                DO I=1,4
                  DO J=1,4
                    IF(FACE(I) == IXC(J+1,KS)) ISHEL = ISHEL + 1
                  ENDDO
                ENDDO
                IF (ISHEL == 4)EXIT
                KS = 0
              ENDDO
              IF(KS == 0)THEN
                NSEG = NSEG + 1
              ELSEIF (IABS(TAGBUF(IPARTC(KS))) /= 1)THEN
                NSEG = NSEG + 1
              ENDIF
            ELSEIF(NN==3)THEN
              KS = 0
              ISHEL = 0
              DO K=KNOD2ELTG(FACE(1))+1,KNOD2ELTG(FACE(1)+1)
                KS=NOD2ELTG(K)
                ISHEL = 0
                DO I=1,3
                  DO J=1,3
                    IF(FACE(I) == IXTG(J+1,KS)) ISHEL = ISHEL + 1
                  ENDDO
                ENDDO
                IF (ISHEL == 3)EXIT
                KS = 0
              ENDDO
              IF(KS == 0)THEN
                NSEG = NSEG + 1
                ISEG = NSEG
                CALL SSURF10(FACE(1),FACE(2),FACE(3),FACE(3),JS,
     .                   NSEG0 ,ISEG ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM)
              ELSEIF (IABS(TAGBUF(IPARTTG(KS))) /= 1)THEN
                NSEG = NSEG + 1
                ISEG = NSEG
                CALL SSURF10(FACE(1),FACE(2),FACE(3),FACE(3),JS,
     .                   NSEG0 ,ISEG ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM)
              ENDIF               
            ELSEIF(NN==4)THEN
              KS = 0
              ISHEL = 0
              DO K=KNOD2ELC(FACE(1))+1,KNOD2ELC(FACE(1)+1)
                KS=NOD2ELC(K)
                ISHEL = 0
                DO I=1,4
                  DO J=1,4
                    IF(FACE(I) == IXC(J+1,KS)) ISHEL = ISHEL + 1
                  ENDDO
                ENDDO
                IF (ISHEL == 4)EXIT
                KS = 0
              ENDDO
              IF(KS == 0)THEN
               NSEG = NSEG + 1
               ISEG = NSEG
               CALL SSURF10(FACE(1),FACE(2),FACE(3),FACE(4),JS,
     .                   NSEG0 ,ISEG ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM)
              ELSEIF (IABS(TAGBUF(IPARTC(KS))) /= 1 ) THEN
               NSEG = NSEG + 1
               ISEG = NSEG
               CALL SSURF10(FACE(1),FACE(2),FACE(3),FACE(4),JS,
     .                   NSEG0 ,ISEG ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM)
              ENDIF                
            END IF
C---
          END DO
!        ENDIF
        ENDDO   !       end of JS_ELM=1,NUM_ELM
      ENDDO     !       end JS_PART=1,NUM_PART
!      
      IF(KEY(1:6)/='GRBRIC') THEN
        FLAG_PART=.TRUE.
        NUM_PART = NINDX
      ELSE
        FLAG_PART=.FALSE.        
        NUM_PART = 1
        NUM_ELM = NINDX_SOL10
      ENDIF

      DO JS_PART=1,NUM_PART
        IF(FLAG_PART) THEN
                ID_PART = INDX(JS_PART)
                NUM_ELM = SURF_ELM(ID_PART)%NSOL10
        ENDIF

        DO JS_ELM=1,NUM_ELM
          IF(FLAG_PART) THEN
                JS = SURF_ELM(ID_PART)%SOL10_PART( JS_ELM )
          ELSE
                JS = INDX_SOL10( JS_ELM )
          ENDIF
          J = JS - NUMELS8

!      DO J=1,NUMELS10
!        JS = J+NUMELS8
!        IF ((KEY(1:6)/='GRBRIC'.AND.IABS(TAGBUF(IPARTS(JS))) == 1).OR.
!     .      (KEY(1:6)=='GRBRIC'.AND.IABS(TAGBUF(JS)) == 1) ) THEN
          LL=FASTAG(JS)
          DO JJ=3,6
            IF(MOD(LL,PWR(JJ+1))/PWR(JJ) /= 0)CYCLE
C
C           still needs to filter degenerated faces
            DO K1=1,4
              FACE(K1)=IXS(FACES(K1,JJ)+1,JS)
            END DO
            DO K1=1,3
              DO K2=K1+1,4
                IF(FACE(K2) == FACE(K1)) FACE(K2)=0
              END DO
            END DO
            NN=0
            DO K1=1,4
              IF(FACE(K1) /= 0)THEN
                NN=NN+1
                FACE(NN)=FACE(K1)
              END IF
            END DO
C---
            IF(NN == 3)THEN
              NNS=1
              FC10(1)=IXS10(FACES10(1,JJ),J)  
              FC10(2)=IXS10(FACES10(2,JJ),J)  
              FC10(3)=IXS10(FACES10(3,JJ),J)  
              IF(FC10(1) /= 0)NNS=NNS+1  
              IF(FC10(2) /= 0)NNS=NNS+1  
              IF(FC10(3) /= 0)NNS=NNS+1  
              IF(NNS == 3)NNS=2  
              NSEG=NSEG+NNS
              IF (FLAG == 1 .and. NNS == 4) THEN
c               4 triangles
                  ISEG = NSEG-NNS+1
                  CALL SSURF10(FACE(1),FC10(1),FC10(3),FC10(3),JS,
     .                   NSEG0 ,ISEG ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM)
                  ISEG = NSEG-NNS+2
                  CALL SSURF10(FACE(2),FC10(2),FC10(1),FC10(1),JS,
     .                   NSEG0 ,ISEG ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM)
                  ISEG = NSEG-NNS+3
                  CALL SSURF10(FACE(3),FC10(3),FC10(2),FC10(2),JS,
     .                   NSEG0 ,ISEG ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM)
                  ISEG = NSEG-NNS+4
                  CALL SSURF10(FC10(1),FC10(2),FC10(3),FC10(3),JS,
     .                   NSEG0 ,ISEG ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM)
              ELSEIF (FLAG == 1 .and. NNS == 3) THEN
c               1 quadrangle, 1 triangle
                IF(FC10(1) == 0)THEN                    
                  ISEG = NSEG-NNS+1
                  CALL SSURF10(FACE(1),FACE(2),FC10(2),FC10(3),JS,
     .                   NSEG0 ,ISEG ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM)
                  ISEG = NSEG-NNS+2
                  CALL SSURF10(FACE(3),FC10(3),FC10(2),FC10(2),JS,
     .                   NSEG0 ,ISEG ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM)
                ELSEIF(FC10(2) == 0)THEN                    
                  ISEG = NSEG-NNS+1
                  CALL SSURF10(FACE(2),FACE(3),FC10(3),FC10(1),JS,
     .                   NSEG0 ,ISEG ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM)
                  ISEG = NSEG-NNS+2
                  CALL SSURF10(FACE(1),FC10(1),FC10(3),FC10(3),JS,
     .                   NSEG0 ,ISEG ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM)
                ELSEIF(FC10(3) == 0)THEN                    
                  ISEG = NSEG-NNS+1
                  CALL SSURF10(FACE(3),FACE(1),FC10(1),FC10(2),JS,
     .                   NSEG0 ,ISEG ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM)
                  ISEG = NSEG-NNS+2
                  CALL SSURF10(FACE(2),FC10(2),FC10(1),FC10(1),JS,
     .                   NSEG0 ,ISEG ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM)
                ENDIF                  
              ELSEIF (FLAG == 1 .and. NNS == 2) THEN
c               2 triangles
                IF(FC10(1) /= 0)THEN                    
                  ISEG = NSEG-NNS+1
                  CALL SSURF10(FACE(3),FACE(1),FC10(1),FC10(1),JS,
     .                   NSEG0 ,ISEG ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM)
                  ISEG = NSEG-NNS+2
                  CALL SSURF10(FACE(2),FACE(3),FC10(1),FC10(1),JS,
     .                   NSEG0 ,ISEG ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM)
                ELSEIF(FC10(2) /= 0)THEN                    
                  ISEG = NSEG-NNS+1
                  CALL SSURF10(FACE(1),FACE(2),FC10(2),FC10(2),JS,
     .                   NSEG0 ,ISEG ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM)
                  ISEG = NSEG-NNS+2
                  CALL SSURF10(FACE(3),FACE(1),FC10(2),FC10(2),JS,
     .                   NSEG0 ,ISEG ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM)
                ELSEIF(FC10(3) /= 0)THEN                    
                  ISEG = NSEG-NNS+1
                  CALL SSURF10(FACE(2),FACE(3),FC10(3),FC10(3),JS,
     .                   NSEG0 ,ISEG ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM)
                  ISEG = NSEG-NNS+2
                  CALL SSURF10(FACE(1),FACE(2),FC10(3),FC10(3),JS,
     .                   NSEG0 ,ISEG ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM)
                ENDIF                  
              ELSEIF (FLAG == 1 .and. NNS == 1) THEN
c                 1 triangle
                  ISEG = NSEG-NNS+1
                  CALL SSURF10(FACE(1),FACE(2),FACE(3),FACE(3),JS,
     .                   NSEG0 ,ISEG ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM)
              END IF
            END IF
C---
          END DO
!        ENDIF
        ENDDO   !       end of JS_ELM=1,NUM_ELM
      ENDDO     !       end JS_PART=1,NUM_PART
C-----------
      RETURN
      END

Chd|====================================================================
Chd|  SSURF10                       source/groups/ssurftag.F      
Chd|-- called by -----------
Chd|        SSURFTAG                      source/groups/ssurftag.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SSURF10(N1    ,N2   ,N3        ,N4        ,JS       ,
     .                   NSEG0 ,ISEG ,SURF_NODES,SURF_ELTYP,SURF_ELEM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N1,N2,N3,N4,JS,NSEG0,ISEG
      INTEGER SURF_NODES(NSEG0,4),SURF_ELTYP(NSEG0),SURF_ELEM(NSEG0)
C-----------------------------------------------
!---
      SURF_NODES(ISEG,1) = N1
      SURF_NODES(ISEG,2) = N2
      SURF_NODES(ISEG,3) = N3
      SURF_NODES(ISEG,4) = N4
!
      SURF_ELTYP(ISEG) = 1
      SURF_ELEM(ISEG)  = JS
!---
      RETURN
      END
Chd|====================================================================
Chd|  SURFEXT_TAGN                  source/groups/ssurftag.F      
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SURFEXT_TAGN(IXS    ,KNOD2ELS,NOD2ELS  ,IXS10   ,FASTAG,itab)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),KNOD2ELS(*),NOD2ELS(*),
     .        IXS10(6,*),FASTAG(NUMELS),itab(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,J,K,JS,KS,II,JJ,K1,K2,LL,FACE(4),FC10(3),NN,KK,I1,
     .        NI(4),NS(4),MI(4),MS(4),NMIN,MMIN,NF,MF,IPERM,N1,N2,I
      INTEGER NODTAG(NUMNOD),FACES(4,6),PWR(7),
     .        FACES10(3,6),NNS
      DATA FACES/4,3,2,1,
     .           5,6,7,8,
     .           1,2,6,5,
     .           3,4,8,7,
     .           2,3,7,6,
     .           1,5,8,4/
      DATA FACES10/0,0,0,
     .             0,0,0,
     .             3,6,4,
     .             5,6,2,
     .             1,2,3,
     .             4,5,1/
      DATA PWR/1,2,4,8,16,32,64/
C=======================================================================
      FASTAG=0
C       Tag nodes External surface (solid)
        DO JS=1,NUMELS
          DO JJ=1,6
            DO II=1,4
              NS(II)=IXS(FACES(II,JJ)+1,JS)
            END DO
C
C           keep only 1 occurrence of each node (triangles, degenerated cases...)
            DO K1=1,3
              DO K2=K1+1,4
                IF(NS(K2)==NS(K1))NS(K2)=0
              END DO
            END DO
            NF=0
            DO K1=1,4
              N1=NS(K1)
              IF(N1/=0)THEN
              NF=NF+1
              NS(NF)=N1
              END IF
            END DO
            IF (NF < 3)CYCLE
C
C           permute
            NMIN=NS(1)
            DO II=2,NF
              NMIN=MIN(NMIN,NS(II))
            END DO
            DO IPERM=1,NF
              IF(NMIN==NS(IPERM).AND.
     .           NS(MOD(IPERM,NF)+1)/=NS(IPERM))THEN
                DO II=1,NF
                  NI(II)=NS(MOD(II+IPERM-2,NF)+1)
                END DO
                EXIT
              END IF
            END DO
C
C           looks for an elt sharing the face.
            DO K=KNOD2ELS(NI(1))+1,KNOD2ELS(NI(1)+1)
              KS=NOD2ELS(K)
              IF(KS==JS .OR. KS > NUMELS8+NUMELS10)CYCLE
              DO II=1,NF
                NODTAG(NI(II))=0
              END DO
              DO II=1,8
                NODTAG(IXS(II+1,KS))=1
              END DO
              NN=0
              DO II=1,NF
                NN=NN+NODTAG(NI(II))
              END DO
              IF(NN==NF)THEN
                DO KK=1,6
            DO II=1,4
              MS(II)=IXS(FACES(II,KK)+1,KS)
            END DO
C
C                 keep only 1 occurrence of each node (triangles, degenerated cases...)
            DO K1=1,3
              DO K2=K1+1,4
                IF(MS(K2)==MS(K1))MS(K2)=0
              END DO
            END DO
            MF=0
            DO K1=1,4
              N1=MS(K1)
              IF(N1/=0)THEN
                MF=MF+1
                MS(MF)=N1
              END IF
            END DO
            IF(MF /= NF)CYCLE
C
C                 permute
                MMIN=MS(1)
                DO II=2,MF
                  MMIN=MIN(MMIN,MS(II))
                END DO
            DO IPERM=1,MF
              IF(MMIN==MS(IPERM).AND.
     .                 MS(MOD(IPERM,MF)+1)/=MS(IPERM))THEN
                DO II=1,MF
                  MI(II)=MS(MOD(II+IPERM-2,MF)+1)
                END DO
                EXIT
              END IF
            END DO
                  IF(MI(1)==NI(1).AND.MI(NF)==NI(2))THEN
C                    FACTAG(JS) moins face jj
                     FASTAG(JS)=FASTAG(JS)+PWR(JJ)
                     GO TO 300
                  END IF
                END DO
              END IF
            END DO
 300        CONTINUE
          END DO
        END DO
C-----------
      RETURN
      END

