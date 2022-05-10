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
Chd|  STAT_TRUSS_SPMD               source/output/sta/stat_truss_spmd.F
Chd|-- called by -----------
Chd|        GENSTAT                       source/output/sta/genstat.F   
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        SPMD_IGET_PARTN_STA           source/mpi/output/spmd_stat.F 
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE STAT_TRUSS_SPMD(ITAB   ,IPART     ,IXT   ,IPARTT,IPART_STATE,
     .                           NODTAG ,STAT_INDXT,LENGT ,IPARG ,ELBUF_TAB  ,
     .                           SIZP0  ,IDEL      )
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
#include      "scr05_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "spmd_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(*),IPART(LIPART1,*),IXT(NIXT,*),IPARTT(*),
     .        IPART_STATE(*),NODTAG(*), STAT_INDXT(*),IPARG(NPARG,*)
      INTEGER LENGT,SIZP0,IDEL
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,JJ,IPRT,BUF,IPRT0,K,II
      INTEGER NG,NEL,NFT,LFT,LLT,ITY,IOFF
      INTEGER IADD(NPART+1),IADG(NSPMD,NPART)
      INTEGER NP(5*NUMELT),
     .        NPGLOB(7*LENGT),
     .        WORK(70000),CLEF(2,NUMELTRG)
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C-----------------------------------------------
C     TRUSS
C-----------------------------------------------
      IADD = 0
      NPGLOB(1:7*LENGT) = 0
C
C en spmd il faut envoyer l'info meme qd on a 0 elem local
      JJ = 0
      II = 0
      DO NG=1,NGROUP
        ITY = IPARG(5,NG)
        IF (ITY == 4) THEN
          NEL = IPARG(2,NG)
          NFT = IPARG(3,NG)
          GBUF => ELBUF_TAB(NG)%GBUF   
          LFT=1
          LLT=NEL
c
          DO I=LFT,LLT
            N = I + NFT
            IPRT=IPARTT(N)
            IF (IPART_STATE(IPRT) /= 0) THEN
c
            NP(JJ+1) = IXT(NIXT,N)
            NP(JJ+2) = ITAB(IXT(2,N))
            NP(JJ+3) = ITAB(IXT(3,N))
            NP(JJ+4) = IPRT
            NP(JJ+5) = IABS(NINT(GBUF%OFF(I)))
c
            II = II + 1
c
            JJ = JJ + 5
c
            STAT_NUMELT = STAT_NUMELT + 1
c
            NODTAG(IXT(2,N))=1
            NODTAG(IXT(3,N))=1
            ENDIF ! IF (IPART_STATE(IPRT) == 0)
          ENDDO ! DO I=LFT,LLT
        ENDIF ! IF (ITY == 4)
      ENDDO ! DO NG=1,NGROUP
C----
      STAT_NUMELT_G = 0
      CALL SPMD_IGET_PARTN_STA(5,STAT_NUMELT,STAT_NUMELT_G,LENGT,NP,
     .  		       IADG,NPGLOB,STAT_INDXT)
C----
      IF (ISPMD == 0) THEN
        DO N=1,STAT_NUMELT_G
          STAT_INDXT(N)=N
          CLEF(1,N)=NPGLOB(5*(N-1)+5)
          CLEF(2,N)=NPGLOB(5*(N-1)+1)
        ENDDO
        CALL MY_ORDERS(0,WORK,CLEF,STAT_INDXT,STAT_NUMELT_G,2)
C----
        IPRT0=0
        DO N=1,STAT_NUMELT_G
          K=STAT_INDXT(N)
          JJ=5*(K-1)
          IPRT=NPGLOB(JJ+4)
          IOFF=NPGLOB(JJ+5)
          IF (IDEL==0 .OR. (IDEL==1 .AND. IOFF >= 1)) THEN
            IF (IPRT /= IPRT0) THEN
              WRITE(IUGEO,'(A,I10)')'/TRUSS/',IPART(4,IPRT)
              WRITE(IUGEO,'(A)')
     .      '#TRUSS_ID       NOD1      NOD2'
              IPRT0=IPRT
            ENDIF
            WRITE(IUGEO,'(3I10)') NPGLOB(JJ+1),NPGLOB(JJ+2),NPGLOB(JJ+3)
          ENDIF !IF (IDEL)
        ENDDO ! DO N=1,STAT_NUMELT_G
      ENDIF ! IF (ISPMD == 0)
C-----------------------------------------------
      RETURN
      END
