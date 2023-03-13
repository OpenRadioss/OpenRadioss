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
Chd|  THNST                         source/output/th/thnst.F      
Chd|-- called by -----------
Chd|        HIST2                         source/output/th/hist2.F      
Chd|-- calls ---------------
Chd|        XTH                           source/output/th/thnst.F      
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE THNST(ELBUF_TAB,IPARG,NTHGRP2, ITHGRP,ITHBUF,
     .                 GEO ,KXX, WA  )
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
#include      "task_c.inc"
#include      "param_c.inc"
#include      "scr23_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),ITHBUF(*),KXX(NIXX,*)
      INTEGER, INTENT(in) :: NTHGRP2
      INTEGER, DIMENSION(NITHGR,*), INTENT(in) :: ITHGRP
      my_real
     .   GEO(NPROPG,*),WA(*)
C
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II, I, J, N, IH, NG, ITY, MTE, NB0, NB1, NB2, NB3,
     .   NB4, NB5, NNB3, MB1, MB2, MB3, MB4, MB5, K, IST, IP, L,
     .   LWA, IMAT, IPROP, NX, IGTYP, NUVAR, NUVARN,NEL,NFT,
     .   KVAR,KVARN
      INTEGER :: NITER,IAD,NN,IADV,NVAR,ITYP,IJK
      my_real
     .   WWA(100)
C
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
C-------------------------
C           ELEMENTS NSTRANDS
C-------------------------

        IJK = 0
        DO NITER=1,NTHGRP2
            ITYP=ITHGRP(2,NITER)
            NN  =ITHGRP(4,NITER)
            IAD =ITHGRP(5,NITER)
            NVAR=ITHGRP(6,NITER)
            IADV=ITHGRP(7,NITER)
            II=0
            IF(ITYP==100)THEN
!   -------------------------------
                II=0
                IH=IAD

                DO WHILE (ITHBUF(IH+NN) /= ISPMD .AND. IH < IAD+NN)
                    IH = IH + 1
                ENDDO
                IF (IH >= IAD+NN) GOTO 666 

                DO NG=1,NGROUP
                    ITY=IPARG(5,NG)
                    IF (ITY == 100) THEN
C       multi-purpose elements are nstrands elements only (to be modified).
                        NEL=IPARG(2,NG)
                        NFT=IPARG(3,NG)
                        GBUF => ELBUF_TAB(NG)%GBUF

                        DO I=1,NEL
                            N  =I+NFT
                            K  =ITHBUF(IH)
                            IF (K == N) THEN
                                NX    =KXX(3,N)
                                IPROP =KXX(2,N)
                                NUVAR =NINT(GEO(25,IPROP))
                                NUVARN=NINT(GEO(35,IPROP))
    
                                KVAR  = NUVAR*(I-1)+1
                                KVARN = NUVARN*NX*(I-1)+1
C
C                               initial total length UVAR(NB1:NB1)
                                NB1=1
C                               previous elongation  UVAR(NB2:NB2)
                                NB2=NB1+1
C                               previous force average UVAR(NB3:NB3)
                                NB3=NB2+1
C                               initial nodes masses UVARN(MB1:MB1+NX-1)
                                MB1=1
C                               forces into strands UVARN(MB2:MB2+NX-1)
                                MB2=MB1+NX
C                               strands initial length UVARN(MB3:MB3+NX-1)
                                MB3=MB2+NX
C                               strands elongations UVARN(MB4:MB4+NX-1)
                                MB4=MB3+NX
C                               strands internal energy UVARN(MB5:MB5+NX-1)
                                MB5=MB4+NX
                                II = (IH - IAD)*NVAR
                                DO J=1,NN

C                                   loop over strands into the ONE NSTRAND TH group.
C                                   strand to save:
                                    IST=ITHBUF(IH+3*NN)               
                                    WWA(1) = GBUF%OFF(I)
C                                   force into strand :
                                    CALL XTH(NUVAR,GBUF%VAR(KVAR),NUVARN,GBUF%VARN(KVARN),WWA,
     .                                       NX   ,2             ,IST   )
                                    WWA(3) =ZERO
                                    WWA(4) =ZERO
                                    WWA(5) =ZERO
                                    WWA(6) =ZERO
                                    WWA(7) =ZERO
C                                   strand elongation :
                                    CALL XTH(NUVAR,GBUF%VAR(KVAR),NUVARN,GBUF%VARN(KVARN),WWA,
     .                                       NX   ,8             ,IST   )
    
                                    WWA(9) =ZERO
                                    WWA(10)=ZERO
                                    WWA(11)=ZERO
                                    WWA(12)=ZERO
                                    WWA(13)=ZERO       
C                                   strand internal energy :
                                    CALL XTH(NUVAR,GBUF%VAR(KVAR),NUVARN,GBUF%VARN(KVARN),WWA,
     .                                       NX   ,14            ,IST   )
                                    WWA(15)=ZERO
                                    WWA(16)=ZERO
                                    DO L=IADV,IADV+NVAR-1
                                        K=ITHBUF(L)
                                        IJK=IJK+1
                                        WA(IJK)=WWA(K)
                                    ENDDO
                                    IH=IH+1
                                ENDDO ! DO J=1,NN
                                IJK = IJK + 1
                                WA(IJK) = II
                            ENDIF ! IF (K == N)
                        ENDDO ! DO I=1,NEL
                    ENDIF ! IF (ITY == 100)
                ENDDO ! DO NG=1,NGROUP
 666    continue
!   -------------------------------
            ENDIF
        ENDDO
C
        RETURN
        END SUBROUTINE THNST





Chd|====================================================================
Chd|  XTH                           source/output/th/thnst.F      
Chd|-- called by -----------
Chd|        THNST                         source/output/th/thnst.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE XTH(NUVAR ,UVAR  ,NUVARN ,UVARN ,WWA,
     .               NX    ,II    ,IST    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUVAR,NUVARN,NX,II,IST
      my_real
     .   UVAR(NUVAR),UVARN(NUVARN*NX),WWA(100)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NB1,NB2,NB3,MB1,MB2,MB3,MB4,MB5
C-----------------------------------------------
C       initial total length UVAR(NB1:NB1)
      NB1=1
C       previous elongation  UVAR(NB2:NB2)
      NB2=NB1+1
C       previous force       UVAR(NB3:NB3)
      NB3=NB2+1
C       initial nodes masses UVARN(MB1:MB1+NX-1)
      MB1=1
C       forces into strands UVARN(MB2:MB2+NX-1)
C       using UVARN(MB2:MB2+NX-2) only.
      MB2=MB1+NX
C       strands initial length UVARN(MB3:MB3+NX-1)
C       using UVARN(MB3:MB3+NX-2) only.
      MB3=MB2+NX
C       strands elongations UVARN(MB4:MB4+NX-1)
C       using UVARN(MB4:MB4+NX-2) only.
      MB4=MB3+NX
C       strands internal energy UVARN(MB5:MB5+NX-1)
C       using UVARN(MB5:MB5+NX-2) only.
      MB5=MB4+NX
C-----------------------------------------------
      IF (II == 2) THEN ! force into strand
        WWA(II) = UVAR(NB3) + UVARN(MB2+IST-1)
      ELSEIF (II == 8) THEN ! strand elongation
        WWA(II) = UVARN(MB4+IST-1)
      ELSEIF (II == 14) THEN ! strand internal energy
        WWA(II) = UVARN(MB5+IST-1)
      ENDIF
C-----
      RETURN
      END
