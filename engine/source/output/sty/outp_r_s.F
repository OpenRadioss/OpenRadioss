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
Chd|  OUTP_R_S                      source/output/sty/outp_r_s.F  
Chd|-- called by -----------
Chd|        GENOUTP                       source/output/sty/genoutp.F   
Chd|-- calls ---------------
Chd|        SPMD_RGATHER9_1COMM           source/mpi/interfaces/spmd_outp.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE OUTP_R_S(NBX   ,KEY   ,TEXT ,ELBUF_TAB,IPARG,
     2                    DD_IAD,SIZLOC,SIZP0,SIZ_WR   )
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
#include      "scr01_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"     
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*10 KEY
      CHARACTER*40 TEXT
      INTEGER NBX
      INTEGER IPARG(NPARG,*), DD_IAD(NSPMD+1,*),SIZLOC,SIZP0,SIZ_WR
C
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J, JJ,K,L
      INTEGER NG, NEL, NFT, IAD, ITY, LFT, LLT,
     .        JJ_OLD, NGF, NGL, NN, LEN, NUVAR, NPTT, NPTS,
     .        LIAD
      INTEGER RESP0,WRTLEN,RES,COMPTEUR
      INTEGER, DIMENSION(NSPGROUP) :: JJ_LOC
      INTEGER, DIMENSION(NSPGROUP+1,NSPMD) :: ADRESS 
      my_real
     .   WA(SIZLOC),WAP0(SIZ_WR),WAP0_LOC(SIZP0)
      my_real
     .   FUNC(6)
C
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
C-----------------------------------------------      
C NEW SUBROUTINE FOR SPRINGS
C-----------------------------------------------
       IF(ISPMD==0) THEN 
        WRITE(IUGEO,'(2A)')'/SPRING     /SCALAR    /',KEY
        WRITE(IUGEO,'(A)')TEXT
        IF (OUTYY_FMT == 2) THEN
         WRITE(IUGEO,'(A)')'#FORMAT: (1P6E12.5) (VAR(I),I=1,NUMELR)'
        ELSE
         WRITE(IUGEO,'(A)')'#FORMAT: (1P6E20.13) (VAR(I),I=1,NUMELR)'
        END IF
       ENDIF
! -----------------------------                   
       JJ_OLD = 1
       NGF = 1
       NGL = 0
       RESP0=0
       JJ = 0
       COMPTEUR = 0
       DO NN=1,NSPGROUP
        NGL = NGL + DD_IAD(ISPMD+1,NN)
        DO NG = NGF, NGL
         ITY   =IPARG(5,NG)
         GBUF => ELBUF_TAB(NG)%GBUF
         IF(ITY==6) THEN
           NEL   =IPARG(2,NG)
           NFT   =IPARG(3,NG)
           IAD   =IPARG(4,NG)
           LFT=1
           LLT=NEL 
           DO I=LFT,LLT
             JJ = JJ + 1
             IF (NBX==1)WA(JJ) = GBUF%OFF(I)
cc             IF (NBX==1)WA(JJ) = BUFEL((IAD-1)+ I) 
           ENDDO 
         ENDIF  
        ENDDO 
        NGF = NGL + 1
        JJ_LOC(NN) = JJ - COMPTEUR            ! size of each group
        COMPTEUR = JJ                         
       ENDDO
!     ++++++++++
       IF( NSPMD>1 ) THEN
        CALL SPMD_RGATHER9_1COMM(WA,JJ,JJ_LOC,WAP0_LOC,SIZP0,ADRESS)
       ELSE
        WAP0_LOC(1:JJ) = WA(1:JJ)
        ADRESS(1,1) = 1
        DO NN = 2,NSPGROUP+1
         ADRESS(NN,1) = JJ_LOC(NN-1) + ADRESS(NN-1,1)
        ENDDO
       ENDIF
!     ++++++++++
       IF(ISPMD==0) THEN
         RESP0 = 0 
         DO NN=1,NSPGROUP
          COMPTEUR = 0
          DO K = 1,NSPMD
           IF((ADRESS(NN+1,K)-1-ADRESS(NN,K))>=0) THEN
            DO L = ADRESS(NN,K),ADRESS(NN+1,K)-1
             COMPTEUR = COMPTEUR + 1
             WAP0(COMPTEUR+RESP0) = WAP0_LOC(L)
            ENDDO  ! l=... , ...
           ENDIF   !if(size_loc>0)
          ENDDO    ! k=1,nspmd

          JJ_OLD = COMPTEUR+RESP0
          IF(JJ_OLD>0) THEN
           RES=MOD(JJ_OLD,6)
           WRTLEN=JJ_OLD-RES
           IF (WRTLEN>0) THEN
            IF (OUTYY_FMT==2) THEN
             WRITE(IUGEO,'(1P6E12.5)')(WAP0(J),J=1,WRTLEN)
            ELSE
             WRITE(IUGEO,'(1P6E20.13)')(WAP0(J),J=1,WRTLEN)
            ENDIF
           ENDIF
           DO I=1,RES
            WAP0(I)=WAP0(WRTLEN+I)
           ENDDO
           RESP0=RES
          ENDIF        ! if(jj_old>0)
         ENDDO         ! nn=1,nspgroup
         IF (RESP0>0) THEN
          IF (OUTYY_FMT==2) THEN
            WRITE(IUGEO,'(1P6E12.5)')(WAP0(J),J=1,RESP0)
          ELSE
            WRITE(IUGEO,'(1P6E20.13)')(WAP0(J),J=1,RESP0)
          ENDIF
         ENDIF
       ENDIF            ! ispmd=0
       RETURN
       END

Chd|====================================================================
Chd|  COUNT_ARSZ_RS                 source/output/sty/outp_r_s.F  
Chd|-- called by -----------
Chd|        GENOUTP                       source/output/sty/genoutp.F   
Chd|        OUTP_ARSZ_RS                  source/mpi/interfaces/spmd_outp.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE COUNT_ARSZ_RS(IPARG,DD_IAD,WASZ,SIZ_WRITE_LOC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),DD_IAD(NSPMD+1,*),
     .          WASZ,SIZ_WRITE_LOC(NSPGROUP+1)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGF,NGL,NN,ITY,NEL,NG,JJ
C-----------------------------------------------
      WASZ = 0
      IF (OUTP_RS(1) == 1) THEN
        NGF = 1
        NGL = 0
        DO NN=1,NSPGROUP
         JJ = 0
         NGL = NGL + DD_IAD(ISPMD+1,NN)
         DO NG = NGF, NGL
          ITY   =IPARG(5,NG)
          IF(ITY == 6) THEN
            NEL = IPARG(2,NG)
            JJ = JJ + NEL
          ENDIF
         ENDDO 
         NGF = NGL + 1
         WASZ = WASZ + JJ
         SIZ_WRITE_LOC(NN) = JJ
        ENDDO
      ENDIF
      SIZ_WRITE_LOC(NSPGROUP+1) = WASZ
      RETURN
      END
