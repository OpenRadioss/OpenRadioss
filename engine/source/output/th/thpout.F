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
Chd|  THPOUT                        source/output/th/thpout.F     
Chd|-- called by -----------
Chd|        HIST2                         source/output/th/hist2.F      
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE THPOUT(IPARG , NTHGRP2  , ITHGRP ,GEO,  IXP,  
     .                  ITHBUF, ELBUF_TAB, WA     )
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
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),ITHBUF(*)
      INTEGER, INTENT(in) :: NTHGRP2
      INTEGER, DIMENSION(NITHGR,*), INTENT(in) :: ITHGRP
      INTEGER, DIMENSION(NIXP,NUMELP) ,INTENT(IN):: IXP
      my_real
     .   WA(*)
      my_real
     .   GEO(NPROPG,NUMGEO)
C
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II,I,J,K,L,N,IP,IH,NG,IPT,NPT,ITY,MTE,LWA,JJ,IK,
     .        ILAYER,NEL,NFT,IGTYP,IPA,KK(3)
      INTEGER :: NITER,IAD,NN,IADV,NVAR,ITYP,IJK,PID
C
      my_real
     .        AREA,AREAPT,SX,SXY,SZX,IDX     
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
      TYPE(BUF_LAY_)  ,POINTER :: BUFLY
      TYPE(L_BUFEL_)  ,POINTER :: LBUF
C-----------------------------------------------

C-------------------------
C           ELEMENTS POUTRE
C-------------------------
        IJK = 0
        IPA  = 400        
        DO NITER=1,NTHGRP2
            ITYP=ITHGRP(2,NITER)
            NN  =ITHGRP(4,NITER)
            IAD =ITHGRP(5,NITER)
            NVAR=ITHGRP(6,NITER)
            IADV=ITHGRP(7,NITER)
            II=0
            IF(ITYP==5)THEN
!   -------------------------------
                II=0
                IH=IAD
                            
                DO WHILE (ITHBUF(IH+NN) /= ISPMD .AND. IH < IAD+NN)
                    IH = IH + 1                           
                ENDDO                                   
                IF (IH >= IAD+NN) GOTO 666                
C
                DO NG=1,NGROUP
                    ITY=IPARG(5,NG)
                    GBUF => ELBUF_TAB(NG)%GBUF
                    ILAYER = 1
                    BUFLY => ELBUF_TAB(NG)%BUFLY(ILAYER)

                    IF (ITY == 5) THEN
                        MTE=IPARG(1,NG)
                        NEL=IPARG(2,NG)
                        NFT=IPARG(3,NG)
                        NPT  = IPARG(6,NG)
                        IGTYP =IPARG(38,NG)

                        DO I=1,3
                            KK(I) = NEL*(I-1)
                        ENDDO

                        DO I=1,NEL
                            N=I+NFT
                            K=ITHBUF(IH)
                            IP=ITHBUF(IH+NN)
                            PID = IXP(5,NFT+I) 
                            IF (IGTYP == 3) AREA = GEO(1,PID)   
                            IF(IGTYP == 18 ) THEN
                              AREA = ZERO
                              DO IPT = 1, NPT
                                AREA = AREA + GEO(IPA+IPT,PID)
                              ENDDO
                            ENDIF
                                           
                           
                            IF (K == N) THEN
                                IH=IH+1
                                II = ((IH-1) - IAD)*NVAR
                                DO WHILE (ITHBUF(IH+NN) /= ISPMD .AND. IH < IAD+NN)
                                    IH = IH + 1
                                ENDDO

                                IF (IH > IAD+NN) GOTO 666

                                DO L=IADV,IADV+NVAR-1
                                    K=ITHBUF(L)
                                    IJK=IJK+1
                                    IF (ITHBUF(L) == 1) THEN
                                        WA(IJK)=GBUF%OFF(I)
                                    ELSEIF(ITHBUF(L) == 2)THEN
                                        WA(IJK)=GBUF%FOR(KK(1)+I)
                                    ELSEIF (ITHBUF(L) == 3) THEN
                                        WA(IJK)=GBUF%FOR(KK(2)+I)
                                    ELSEIF (ITHBUF(L) == 4) THEN
                                        WA(IJK)=GBUF%FOR(KK(3)+I)
                                    ELSEIF (ITHBUF(L) == 5) THEN
                                        WA(IJK)=GBUF%MOM(KK(1)+I)
                                    ELSEIF (ITHBUF(L) == 6) THEN
                                        WA(IJK)=GBUF%MOM(KK(2)+I)
                                    ELSEIF (ITHBUF(L) == 7) THEN
                                        WA(IJK)=GBUF%MOM(KK(3)+I)
                                    ELSEIF (ITHBUF(L) == 8) THEN
                                        WA(IJK)=GBUF%EINT(I) + GBUF%EINT(I+NEL)
                                    ELSEIF (ITHBUF(L) == 9) THEN
                                        WA(IJK)=ZERO 
                                        IF (IGTYP == 3) THEN
                                           ! stress = force/area for the 3 directions
                                           SX   = GBUF%FOR(KK(1)+I)/AREA
                                           WA(IJK)=SX 
                                        ELSEIF(IGTYP == 18 ) THEN
                                           IF (BUFLY%L_SIG > 0) THEN
                                            DO IPT = 1,NPT
                                             AREAPT  = GEO(IPA+IPT,PID) 
                                             LBUF  => BUFLY%LBUF(1,1,IPT)
                                             WA(IJK) = WA(IJK)+ LBUF%SIG(KK(1)+I) * AREAPT/AREA
                                            ENDDO 
                                           END IF !(BUFLY%L_SIG > 0) 
                                        END IF! (IGTYP) 
                                    ELSEIF (ITHBUF(L) == 10) THEN
                                        WA(IJK)=ZERO 
                                        IF (IGTYP == 3) THEN
                                           ! stress = force/area for the 3 directions
                                           SXY  = GBUF%FOR(KK(2)+I)/AREA
                                           WA(IJK)=SXY 
                                        ELSEIF(IGTYP == 18 ) THEN
                                           IF (BUFLY%L_SIG > 0) THEN
                                            DO IPT = 1,NPT
                                             AREAPT  = GEO(IPA+IPT,PID) 
                                             LBUF  => BUFLY%LBUF(1,1,IPT)
                                             WA(IJK) = WA(IJK)+ LBUF%SIG(KK(2)+I)*AREAPT/AREA
                                            ENDDO 
                                           END IF !(BUFLY%L_SIG > 0) 
                                        END IF! (IGTYP) 
                                    ELSEIF (ITHBUF(L) == 11) THEN
                                        WA(IJK)=ZERO 
                                        IF (IGTYP == 3) THEN
                                           ! stress = force/area for the 3 directions
                                           SZX  = GBUF%FOR(KK(3)+I)/AREA
                                           WA(IJK)=SZX 
                                        ELSEIF(IGTYP == 18 ) THEN
                                           IF (BUFLY%L_SIG > 0) THEN                                            
                                            DO IPT = 1,NPT
                                             AREAPT  = GEO(IPA+IPT,PID) 
                                             LBUF  => BUFLY%LBUF(1,1,IPT)
                                             WA(IJK) = WA(IJK)+ LBUF%SIG(KK(3)+I) * AREAPT/AREA
                                            ENDDO 
                                           END IF !(BUFLY%L_SIG > 0) 
                                        END IF! (IGTYP) 
                                    ELSEIF (ITHBUF(L) > 11 .AND.ITHBUF(L) <= 254 ) THEN
                                        IF(IGTYP == 18 ) THEN
                                            IDX = (ITHBUF(L) - 12)/ 3                                            
                                            JJ  = NINT(IDX)
                                            IPT = JJ + 1
                                            IK  = MOD((ITHBUF(L) - 12),3) + 1
                                            LBUF  => BUFLY%LBUF(1,1,IPT) 
                                            WA(IJK) = LBUF%SIG(KK(IK)+I)
                                        ENDIF
                                    ELSEIF (ITHBUF(L) == 255) THEN
                                        WA(IJK)=ZERO
                                        IF(IGTYP == 3 ) THEN 
                                          IF(GBUF%G_PLA>0)THEN
                                            WA(IJK)=GBUF%PLA(I) 
                                          ENDIF
                                        ELSEIF(IGTYP == 18 ) THEN
                                           IF (BUFLY%L_PLA > 0) THEN                                            
                                            DO IPT = 1,NPT
                                             AREAPT  = GEO(IPA+IPT,PID) 
                                             LBUF  => BUFLY%LBUF(1,1,IPT)
                                             WA(IJK) = WA(IJK)+ LBUF%PLA(I) * AREAPT/AREA
                                            ENDDO 
                                           END IF 
                                        END IF! (IGTYP) 
                                    ELSEIF (ITHBUF(L) > 255 .AND.ITHBUF(L) <= 336 ) THEN
                                        IF(IGTYP == 18 ) THEN
                                           IF (BUFLY%L_PLA > 0) THEN                                                                                     
                                            IPT = ITHBUF(L) - 255 
                                            LBUF  => BUFLY%LBUF(1,1,IPT) 
                                            WA(IJK) = LBUF%PLA(I)
                                           ENDIF
                                        ENDIF
                                    ELSEIF (ITHBUF(L) == 337 ) THEN
                                      IF(GBUF%G_EPSD>0)THEN
                                        WA(IJK)=GBUF%EPSD(I) 
                                      ENDIF
                                    ENDIF
                                ENDDO
                                IJK = IJK + 1
                                WA(IJK) = II
                            ENDIF
                        ENDDO
                     ENDIF
                ENDDO
 666    continue
!   -------------------------------
            ENDIF
        ENDDO
C---
C-----------
      RETURN
      END SUBROUTINE THPOUT