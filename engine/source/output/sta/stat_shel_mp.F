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
Chd|  STAT_SHEL_MP                  source/output/sta/stat_shel_mp.F
Chd|-- called by -----------
Chd|        GENSTAT                       source/output/sta/genstat.F
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE STAT_SHEL_MP(ITAB,ITABG,LENG,IPART,IGEO,
     .            IXC,IXTG,IPARTC,IPARTTG,IPART_STATE,
     .            NODTAG,STAT_INDXC,STAT_INDXTG,SH4TREE,SH3TREE,
     .            IPARG ,SH4TRIM   ,SH3TRIM    ,ELBUF_TAB,THKE ,
     .            IDEL)
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
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ITAB(*), ITABG(*), LENG, IPART(LIPART1,*),
     .          IGEO(NPROPGI,*), IXC(NIXC,*), IXTG(NIXTG,*),
     .          IPARTC(*), IPARTTG(*), IPART_STATE(*),
     .          NODTAG(*), STAT_INDXC(*), STAT_INDXTG(*),
     .          SH4TREE(KSH4TREE,*), SH3TREE(KSH3TREE,*),
     .          IPARG(NPARG,*), SH4TRIM(*), SH3TRIM(*),
     .          IDEL
        TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
        my_real
     .     THKE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, N, JJ, IPRT0, IPRT, K, II
        INTEGER NG, NEL, NFT, LFT, LLT, ITY, MLW, ITHK,IOFF
        INTEGER NP(8*MAX(NUMELC,NUMELTG)),
     .          WORK(70000),CLEF(2,MAX(NUMELC,NUMELTG))
        double precision
     .     THK(MAX(NUMELC,NUMELTG))
        TYPE(G_BUFEL_)  ,POINTER :: GBUF
C-----------------------------------------------
C     4-NODE SHELLS
C-----------------------------------------------
        JJ = 0
        II = 0
        IF(NUMELC/=0)THEN

          DO NG=1,NGROUP
            ITY   =IPARG(5,NG)
            IF(ITY==3) THEN
              NEL   =IPARG(2,NG)
              NFT  =IPARG(3,NG)
              GBUF => ELBUF_TAB(NG)%GBUF
              MLW   =IPARG(1,NG)
              ITHK  =IPARG(28,NG)
              LFT=1
              LLT=NEL
              DO I=LFT,LLT
                N  = I + NFT

                IPRT=IPARTC(N)
                IF(IPART_STATE(IPRT)==0)CYCLE

                NP(JJ+1) = IXC(NIXC,N)
                NP(JJ+2) = ITAB(IXC(2,N))
                NP(JJ+3) = ITAB(IXC(3,N))
                NP(JJ+4) = ITAB(IXC(4,N))
                NP(JJ+5) = ITAB(IXC(5,N))
                NP(JJ+6) = IPRT
                NP(JJ+7) = IABS(NINT(GBUF%OFF(I)))
                II = II + 1
                IF (MLW /= 0 .AND. MLW /= 13) THEN
                  IF (ITHK >0 ) THEN
                    THK(II) = GBUF%THK(I)
                  ELSE
                    THK(II) = THKE(N)
                  END IF
                ELSE
                  THK(II) = ZERO
                ENDIF
                JJ = JJ + 7

                STAT_NUMELC =STAT_NUMELC+1
                CLEF(1,STAT_NUMELC)=IPRT
                CLEF(2,STAT_NUMELC)=IXC(NIXC,N)

                NODTAG(IXC(2,N))=1
                NODTAG(IXC(3,N))=1
                NODTAG(IXC(4,N))=1
                NODTAG(IXC(5,N))=1

              END DO
            END IF
          END DO
        END IF
C----
        DO N=1,STAT_NUMELC
          STAT_INDXC(N)=N
        END DO
        CALL MY_ORDERS(0,WORK,CLEF,STAT_INDXC,STAT_NUMELC,2)
C----
        IPRT0=0
        DO N=1,STAT_NUMELC
          K=STAT_INDXC(N)
          JJ=7*(K-1)
          IPRT=NP(JJ+6)
          IOFF=NP(JJ+7)
          IF(IDEL==0.OR.(IDEL==1.AND.IOFF >= 1)) THEN
            IF(IPRT /= IPRT0)THEN
              WRITE(IUGEO,'(A,I10)')'/SHELL/',IPART(4,IPRT)
              WRITE(IUGEO,'(A)')
     .        '#  SHELLID      NOD1      NOD2      NOD3      NOD4                                THK'
              IPRT0=IPRT
            END IF
            WRITE(IUGEO,'(5I10,30X,1PE20.13)')
     .      NP(JJ+1),NP(JJ+2),NP(JJ+3),NP(JJ+4),NP(JJ+5),THK(K)
          ENDIF
        END DO
C----
C     Specific adaptive meshing :
        IF(NADMESH /=0)THEN
          JJ = 0
          IF(NUMELC/=0)THEN
            DO NG=1,NGROUP
              ITY   =IPARG(5,NG)
              IF(ITY==3) THEN
                NEL   =IPARG(2,NG)
                NFT   =IPARG(3,NG)
                LFT=1
                LLT=NEL
                DO I=LFT,LLT
                  N  = I + NFT

                  IPRT=IPARTC(N)
                  IF(IPART_STATE(IPRT)==0)CYCLE

                  NP(JJ+1) = IXC(NIXC,N)
                  IF(SH4TREE(2,N) /= 0)THEN
                    NP(JJ+2) = IXC(NIXC,SH4TREE(2,N)  )
                    NP(JJ+3) = IXC(NIXC,SH4TREE(2,N)+1)
                    NP(JJ+4) = IXC(NIXC,SH4TREE(2,N)+2)
                    NP(JJ+5) = IXC(NIXC,SH4TREE(2,N)+3)
                  ELSE
                    NP(JJ+2) =0
                    NP(JJ+3) =0
                    NP(JJ+4) =0
                    NP(JJ+5) =0
                  END IF
                  NP(JJ+6) = SH4TREE(3,N)
                  NP(JJ+7) = IPRT
                  IF(LSH4TRIM /= 0)THEN
                    IF(SH4TRIM(N)==-1)THEN
                      NP(JJ+8) = -1
                    ELSE
                      NP(JJ+8) = 0
                    END IF
                  ELSE
                    NP(JJ+8) = 0
                  END IF
                  JJ = JJ + 8
                END DO
              END IF
            END DO
          END IF

          IPRT0=0
          DO N=1,STAT_NUMELC
            K=STAT_INDXC(N)
            JJ=8*(K-1)
            IPRT=NP(JJ+7)
            IF(IPRT /= IPRT0)THEN
              WRITE(IUGEO,'(A)')'/ADMESH/STATE/SHELL'
              WRITE(IUGEO,'(2A)')
     .        '#  SHELLID       ID1       ID2        ID3       ID4     LEVEL',
     .        '  IMAPPING'
              IPRT0=IPRT
            END IF
            WRITE(IUGEO,'(7I10)')
     .       NP(JJ+1),NP(JJ+2),NP(JJ+3),NP(JJ+4),NP(JJ+5),NP(JJ+6),NP(JJ+8)
          END DO

        END IF
C-----------------------------------------------
C     3-NODE SHELLS
C-----------------------------------------------
        JJ = 0
        II = 0
        IF(NUMELTG/=0)THEN
          DO NG=1,NGROUP
            ITY   =IPARG(5,NG)
            IF(ITY==7) THEN
              NEL  =IPARG(2,NG)
              NFT  =IPARG(3,NG)
              GBUF => ELBUF_TAB(NG)%GBUF
              MLW   =IPARG(1,NG)
              ITHK  =IPARG(28,NG)
              LFT=1
              LLT=NEL
C
              DO I=LFT,LLT
                N  = I + NFT

                IPRT=IPARTTG(N)
                IF(IPART_STATE(IPRT)==0)CYCLE

                NP(JJ+1) = IXTG(NIXTG,N)
                NP(JJ+2) = ITAB(IXTG(2,N))
                NP(JJ+3) = ITAB(IXTG(3,N))
                NP(JJ+4) = ITAB(IXTG(4,N))
                NP(JJ+5) = IPRT
                NP(JJ+6) = IABS(NINT(GBUF%OFF(I)))
                II = II + 1
                IF (MLW /= 0 .AND. MLW /= 13) THEN
                  IF (ITHK >0 ) THEN
                    THK(II) = GBUF%THK(I)
                  ELSE
                    THK(II) = THKE(N)
                  END IF
                ELSE
                  THK(II) = ZERO
                ENDIF
                JJ = JJ + 6

                STAT_NUMELTG =STAT_NUMELTG+1
                CLEF(1,STAT_NUMELTG)=IPRT
                CLEF(2,STAT_NUMELTG)=IXTG(NIXTG,N)

                NODTAG(IXTG(2,N))=1
                NODTAG(IXTG(3,N))=1
                NODTAG(IXTG(4,N))=1

              END DO
            END IF
          END DO
        END IF

C-----
        DO N=1,STAT_NUMELTG
          STAT_INDXTG(N)=N
        END DO
        CALL MY_ORDERS(0,WORK,CLEF,STAT_INDXTG,STAT_NUMELTG,2)
C-----
        IPRT0=0
        DO N=1,STAT_NUMELTG
          K=STAT_INDXTG(N)
          JJ=6*(K-1)
          IPRT=NP(JJ+5)
          IOFF=NP(JJ+6)
          IF(IDEL==0.OR.(IDEL==1.AND.IOFF >= 1)) THEN
            IF(IPRT /= IPRT0)THEN
              WRITE(IUGEO,'(A,I10)')'/SH3N/',IPART(4,IPRT)
              WRITE(IUGEO,'(A)')
     .        '#   SH3NID      NOD1      NOD2      NOD3                                      THK'
              IPRT0=IPRT
            END IF
            WRITE(IUGEO,'(4I10,40X,1PE20.13)')
     .      NP(JJ+1),NP(JJ+2),NP(JJ+3),NP(JJ+4),THK(K)
          ENDIF
        END DO
C-----
C     Specific adaptive meshing :
        IF(NADMESH /=0)THEN
          JJ = 0
          IF(NUMELTG/=0)THEN

            DO NG=1,NGROUP
              ITY   =IPARG(5,NG)
              IF(ITY==7) THEN
                NEL  =IPARG(2,NG)
                NFT  =IPARG(3,NG)
                LFT=1
                LLT=NEL
C
                DO I=LFT,LLT
                  N  = I + NFT

                  IPRT=IPARTTG(N)
                  IF(IPART_STATE(IPRT)==0)CYCLE

                  NP(JJ+1) = IXTG(NIXTG,N)
                  IF(SH3TREE(2,N) /= 0)THEN
                    NP(JJ+2) = IXTG(NIXTG,SH3TREE(2,N)  )
                    NP(JJ+3) = IXTG(NIXTG,SH3TREE(2,N)+1)
                    NP(JJ+4) = IXTG(NIXTG,SH3TREE(2,N)+2)
                    NP(JJ+5) = IXTG(NIXTG,SH3TREE(2,N)+3)
                  ELSE
                    NP(JJ+2) =0
                    NP(JJ+3) =0
                    NP(JJ+4) =0
                    NP(JJ+5) =0
                  END IF
                  NP(JJ+6) = SH3TREE(3,N)
                  NP(JJ+7) = IPRT
                  IF(LSH3TRIM /= 0)THEN
                    IF(SH3TRIM(N)==-1)THEN
                      NP(JJ+8) = -1
                    ELSE
                      NP(JJ+8) = 0
                    END IF
                  ELSE
                    NP(JJ+8) = 0
                  END IF
                  JJ = JJ + 8
                END DO
              END IF
            END DO
          END IF

          IPRT0=0
          DO N=1,STAT_NUMELTG
            K=STAT_INDXTG(N)
            JJ=8*(K-1)
            IPRT=NP(JJ+7)
            IF(IPRT /= IPRT0)THEN
              WRITE(IUGEO,'(A)')'/ADMESH/STATE/SH3N'
              WRITE(IUGEO,'(2A)')
     .        '#   SH3NID       ID1       ID2        ID3       ID4     LEVEL',
     .        '  IMAPPING'
              IPRT0=IPRT
            END IF
            WRITE(IUGEO,'(7I10)')
     .       NP(JJ+1),NP(JJ+2),NP(JJ+3),NP(JJ+4),NP(JJ+5),NP(JJ+6),NP(JJ+8)
          END DO

        END IF
C-----------------------------------------------
        RETURN
      END
