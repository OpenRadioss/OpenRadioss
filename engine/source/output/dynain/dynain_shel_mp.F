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
Chd|  DYNAIN_SHEL_MP                source/output/dynain/dynain_shel_mp.F
Chd|-- called by -----------
Chd|        GENDYNAIN                     source/output/dynain/gendynain.F
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DYNAIN_SHEL_MP(ITAB   ,ITABG   ,LENG    ,IGEO     ,IXC    ,
     .                          IXTG   ,IPARTC  ,IPARTTG ,IPART_DYNAIN     ,
     .                          NODTAG ,DYNAIN_INDXC,DYNAIN_INDXTG,IPARG   ,
     .                          ELBUF_TAB,THKE  ,IPART     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "scr05_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "spmd_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(*), ITABG(*), LENG, IGEO(NPROPGI,*), 
     .        IXC(NIXC,*), IXTG(NIXTG,*),IPARTC(*), IPARTTG(*), 
     .        IPART_DYNAIN(*),NODTAG(*) , DYNAIN_INDXC(*) ,
     .        DYNAIN_INDXTG(*),IPARG(NPARG,*),IPART(LIPART1,*)

      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      my_real
     .   THKE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, JJ, IPRT0, IPRT, K, II
      INTEGER NG, NEL, NFT, LFT, LLT, ITY, MLW, ITHK,IOFF,
     .        IPROP,ID_PROP,IERR , N4SHELL, N3SHELL, IGTYP, IGTYP0
      INTEGER WORK(70000)
      INTEGER ,  DIMENSION(:),ALLOCATABLE :: NPC , NPTG
      INTEGER ,  DIMENSION(:,:),ALLOCATABLE :: CLEF
      double precision  THKN ,BETA
      double precision ,  DIMENSION(:),ALLOCATABLE :: THKC , THKTG ,BETAC , BETATG
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C--------------------------------------------------------

C-----------------------
C    Allocation Tabs
C-----------------------
      ALLOCATE(NPC(8*NUMELC),STAT=IERR)
      ALLOCATE(NPTG(7*NUMELTG),STAT=IERR)
      ALLOCATE(CLEF(2,MAX(NUMELC,NUMELTG)),STAT=IERR)
      ALLOCATE(THKC(NUMELC),STAT=IERR)
      ALLOCATE(THKTG(NUMELTG),STAT=IERR)
      ALLOCATE(BETAC(NUMELC),STAT=IERR)
      ALLOCATE(BETATG(NUMELTG),STAT=IERR)
C-----------------------------------------------
C     4-NODE SHELLS
C-----------------------------------------------
      JJ = 0
      II = 0

      IF(NUMELC.NE.0)THEN

       DO NG=1,NGROUP
        ITY   =IPARG(5,NG)
        IF(ITY.EQ.3) THEN
          NEL   =IPARG(2,NG)
          NFT	=IPARG(3,NG)
          GBUF => ELBUF_TAB(NG)%GBUF   
          MLW   =IPARG(1,NG)
          IPROP =IPARG(62,NG)
          ITHK  =IPARG(28,NG)
          ID_PROP=IGEO(1,IPROP)
          IGTYP= IPARG(38,NG)
          IF(IGTYP/= 1) IGTYP = 2
          LFT=1
          LLT=NEL

          DO I=LFT,LLT
           N  = I + NFT

           IPRT=IPARTC(N)
           IF(IPART_DYNAIN(IPRT)==0)CYCLE

           NPC(JJ+1) = IXC(NIXC,N)
           NPC(JJ+2) = ITAB(IXC(2,N))
           NPC(JJ+3) = ITAB(IXC(3,N))
           NPC(JJ+4) = ITAB(IXC(4,N))
           NPC(JJ+5) = ITAB(IXC(5,N))
           NPC(JJ+6) = IPART(4,IPRT)
           NPC(JJ+7) = NINT(GBUF%OFF(I))
           NPC(JJ+8) = IGTYP
           II = II + 1
           IF (MLW .NE. 0 .AND. MLW .NE. 13) THEN
             IF (ITHK >0 ) THEN
                 THKC(II) = GBUF%THK(I)
             ELSE
                 THKC(II) = THKE(N)
             END IF
           ELSE
             THKC(II) = ZERO
           ENDIF
           JJ = JJ + 8

           DYNAIN_NUMELC = DYNAIN_NUMELC+1

           CLEF(1,DYNAIN_NUMELC)=IGTYP
           CLEF(2,DYNAIN_NUMELC)=IXC(NIXC,N)

           NODTAG(IXC(2,N))=1
           NODTAG(IXC(3,N))=1
           NODTAG(IXC(4,N))=1
           NODTAG(IXC(5,N))=1
          
           IF(IGTYP /= 1) THEN
            BETAC(II) = (HUNDRED80*ACOS(GBUF%BETAORTH(I)))/PI
           ENDIF


         END DO
        END IF
       END DO
      END IF
C----
      DO N=1,DYNAIN_NUMELC
        DYNAIN_INDXC(N)=N
      END DO
      CALL MY_ORDERS(0,WORK,CLEF,DYNAIN_INDXC,DYNAIN_NUMELC,2)


C-----------------------------------------------
C     3-NODE SHELLS
C-----------------------------------------------
      JJ = 0
      II = 0
      IF(NUMELTG.NE.0)THEN
       DO NG=1,NGROUP
        ITY   =IPARG(5,NG)
        IF(ITY.EQ.7) THEN
          NEL	=IPARG(2,NG)
          NFT	=IPARG(3,NG)
          GBUF => ELBUF_TAB(NG)%GBUF   
          MLW   =IPARG(1,NG)
          ITHK  =IPARG(28,NG)
          IPROP =IPARG(62,NG)
          ID_PROP=IGEO(1,IPROP)
          IGTYP= IPARG(38,NG)
          IF(IGTYP/= 1) IGTYP = 2
          LFT=1
          LLT=NEL
C
          DO I=LFT,LLT
           N  = I + NFT

           IPRT=IPARTTG(N)
           IF(IPART_DYNAIN(IPRT)==0)CYCLE

           NPTG(JJ+1) = IXTG(NIXTG,N)
           NPTG(JJ+2) = ITAB(IXTG(2,N))
           NPTG(JJ+3) = ITAB(IXTG(3,N))
           NPTG(JJ+4) = ITAB(IXTG(4,N))
           NPTG(JJ+5) = IPART(4,IPRT)
           NPTG(JJ+6) = NINT(GBUF%OFF(I))
           NPTG(JJ+7) = IGTYP
           II = II + 1
           IF (MLW .NE. 0 .AND. MLW .NE. 13) THEN
             IF (ITHK >0 ) THEN
                 THKTG(II) = GBUF%THK(I)
             ELSE
                 THKTG(II) = THKE(N)
             END IF
           ELSE
             THKTG(II) = ZERO
           ENDIF
           JJ = JJ + 7

           DYNAIN_NUMELTG =DYNAIN_NUMELTG+1

           CLEF(1,DYNAIN_NUMELTG)=IGTYP
           CLEF(2,DYNAIN_NUMELTG)=IXTG(NIXTG,N)

           NODTAG(IXTG(2,N))=1
           NODTAG(IXTG(3,N))=1
           NODTAG(IXTG(4,N))=1

           IF(IGTYP /= 1) THEN
            BETATG(II) = (HUNDRED80*ACOS(GBUF%BETAORTH(I)))/PI
           ENDIF

         END DO
        END IF
       END DO
      END IF

C-----
      DO N=1,DYNAIN_NUMELTG
        DYNAIN_INDXTG(N)=N
      END DO
      CALL MY_ORDERS(0,WORK,CLEF,DYNAIN_INDXTG,DYNAIN_NUMELTG,2)
C-----------------------------------------------------------
C     Output 
C------------------------------------------------------------

C---------Non Orthotropic elements ------------

      IGTYP0 = 0
      DO N=1,DYNAIN_NUMELC
        K=DYNAIN_INDXC(N)
        JJ=8*(K-1)
        IOFF=NPC(JJ+7)
        IGTYP = NPC(JJ+8)
        THKN = THKC(K)
        IF(IOFF >= 1) THEN
           IF(IGTYP==1) THEN
              IF(IGTYP/=IGTYP0) THEN  
                 WRITE(IUDYNAIN,'(A)')'*ELEMENT_SHELL_THICKNESS'
                 WRITE(IUDYNAIN,'(A)')
     .            '$SHELLID PART_ID    NOD1    NOD2    NOD3    NOD4'
                 WRITE(IUDYNAIN,'(A)')
     .            '$          THIC1           THIC2           THIC3           THIC4' 
                 IGTYP0 = IGTYP         
              ENDIF

              WRITE(IUDYNAIN,'(6I8)')
     .           NPC(JJ+1),NPC(JJ+6),NPC(JJ+2),NPC(JJ+3),NPC(JJ+4),NPC(JJ+5)
              WRITE(IUDYNAIN,'(1P4G16.9)')
     .          THKN,THKN,THKN,THKN

           ELSE
             EXIT
           ENDIF
        ENDIF
        
      END DO

      N4SHELL = N 

      DO N=1,DYNAIN_NUMELTG
         K=DYNAIN_INDXTG(N)
         JJ=7*(K-1)
         IOFF=NPTG(JJ+6)
         IGTYP = NPTG(JJ+7)
         THKN = THKTG(K)
         IF(IOFF >= 1) THEN
           IF(IGTYP==1) THEN
             WRITE(IUDYNAIN,'(5I8)')
     .          NPTG(JJ+1),NPTG(JJ+5),NPTG(JJ+2),NPTG(JJ+3),NPTG(JJ+4)
             WRITE(IUDYNAIN,'(1P3G16.9)')
     .          THKN,THKN,THKN
            ELSE
              EXIT
            ENDIF
         ENDIF
      END DO
      N3SHELL = N 

C--------- Orthotropic elements ------------

      IGTYP0 = 1
      DO N=N4SHELL,DYNAIN_NUMELC
        K=DYNAIN_INDXC(N)
        JJ=8*(K-1)
        IOFF=NPC(JJ+7)
        IGTYP = NPC(JJ+8)
        THKN = THKC(K)
        BETA = BETAC(K)
        IF(IOFF >= 1) THEN
           IF(IGTYP/=IGTYP0) THEN    
                 IGTYP0 = IGTYP          
              WRITE(IUDYNAIN,'(A)')'*ELEMENT_SHELL_THICKNESS_BETA'
              WRITE(IUDYNAIN,'(A)')
     .          '$SHELLID PART_ID    NOD1    NOD2    NOD3    NOD4'
              WRITE(IUDYNAIN,'(A)')
     .          '$          THIC1           THIC2           THIC3           THIC4            BETA'          
            ENDIF

             WRITE(IUDYNAIN,'(6I8)')
     .           NPC(JJ+1),NPC(JJ+6),NPC(JJ+2),NPC(JJ+3),NPC(JJ+4),NPC(JJ+5)
             WRITE(IUDYNAIN,'(1P5G16.9)')
     .          THKN,THKN,THKN,THKN,BETA
        ENDIF
        
      END DO
     

      DO N=N3SHELL,DYNAIN_NUMELTG
         K=DYNAIN_INDXTG(N)
         JJ=7*(K-1)
         IOFF=NPTG(JJ+6)
         THKN = THKTG(K)
         BETA = BETATG(K)
         IF(IOFF >= 1) THEN
           WRITE(IUDYNAIN,'(5I8)')
     .          NPTG(JJ+1),NPTG(JJ+5),NPTG(JJ+2),NPTG(JJ+3),NPTG(JJ+4)
           WRITE(IUDYNAIN,'(1P3G16.9,16X,1PG16.9)')
     .          THKN,THKN,THKN,BETA
         ENDIF
      END DO


C-----------------------
C    DEAllocation Tabs
C-----------------------
       DEALLOCATE(NPC,NPTG,CLEF,THKC,THKTG,BETAC,BETATG)
C-----------------------------------------------

      RETURN
      END
