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
Chd|  MULTIFLUID_INIT3T             source/multifluid/multifluid_init3t.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        M5IN3                         source/initial_conditions/detonation/m5in3.F
Chd|        MATINI                        source/materials/mat_share/matini.F
Chd|        S4COOR3                       source/elements/solid/solide4/s4coor3.F
Chd|        S4DERI3                       source/elements/solid/solide4/s4deri3.F
Chd|        S4MASS3                       source/elements/solid/solide4/s4mass3.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE MULTIFLUID_INIT3T(ELBUF_STR, 
     .     NEL, NSIGS, NSIGI, IXS, IGEO, IPM, IPARG, ALE_CONNECTIVITY, IPARTS, PTSOL, NPF, IPART, ILOADP,
     .     XREFS, GEO, PM, FACLOAD, TF, SKEW, SIGI, BUFMAT, X, 
     .     WMA, PARTSAV, MAS, V, MSS, MSSF, MSSA, MSNF, MCPS, ERROR_THROWN, DETONATORS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE MESSAGE_MOD
      USE DETONATORS_MOD
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
!     NIXS, LVEUL
#include      "param_c.inc"
!     NFT
#include      "vect01_c.inc"
!     NUMSOL
#include      "scry_c.inc"
!     LIPART1
#include      "scr17_c.inc"
!     SIZLOADP
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(ELBUF_STRUCT_), INTENT(IN), TARGET :: ELBUF_STR
      INTEGER, INTENT(IN) :: NEL, NSIGS, NSIGI, IXS(NIXS, *), IGEO(NPROPGI, *), IPM(NPROPMI, *), 
     .     IPARG(*), 
     .     IPARTS(*), PTSOL(*), NPF(*), IPART(LIPART1, *), ILOADP(SIZLOADP, *)
      my_real, INTENT(IN) :: XREFS(8, 3, *), X(3, *), GEO(NPROPG, *),  
     .     FACLOAD(LFACLOAD, *), TF(*), SKEW(LSKEW, *), SIGI(NSIGI, *), BUFMAT(*)
      my_real,INTENT(INOUT) :: PM(NPROPM, *)
      my_real, INTENT(INOUT) :: WMA(*), PARTSAV(20, *), MAS(*), V(*), 
     .     MSNF(*), MCPS(8, *), MSSF(8, *), MSS(8, *), MSSA(*)
      LOGICAL :: ERROR_THROWN
      TYPE(DETONATOR_STRUCT_) DETONATORS
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(G_BUFEL_) ,POINTER :: GBUF  
      TYPE(BUF_MAT_) ,POINTER :: MBUF
      INTEGER :: ILAY, NLAY, PID(MVSIZ), NGL(MVSIZ), MAT(MVSIZ), 
     .     IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ)
      my_real :: X1(MVSIZ), Y1(MVSIZ), Z1(MVSIZ),
     .     X2(MVSIZ), Y2(MVSIZ), Z2(MVSIZ),
     .     X3(MVSIZ), Y3(MVSIZ), Z3(MVSIZ),
     .     X4(MVSIZ), Y4(MVSIZ), Z4(MVSIZ),
     .     RX(MVSIZ),RY(MVSIZ),RZ(MVSIZ),
     .     SX(MVSIZ),SY(MVSIZ),SZ(MVSIZ),
     .     TX(MVSIZ),TY(MVSIZ),TZ(MVSIZ),      
     .     PX1(MVSIZ),PX2(MVSIZ),PX3(MVSIZ),PX4(MVSIZ),
     .     PY1(MVSIZ),PY2(MVSIZ),PY3(MVSIZ),PY4(MVSIZ),
     .     PZ1(MVSIZ),PZ2(MVSIZ),PZ3(MVSIZ),PZ4(MVSIZ),
     .     VOLU(MVSIZ), BID(MVSIZ), DUMMY, PRES,VFRAC
      INTEGER :: II, IP, IBID, MATLAW
      DOUBLE PRECISION 
     .   VOLDP(MVSIZ)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      GBUF => ELBUF_STR%GBUF
C     Number of layers ( = number of material in law 151)
      NLAY = ELBUF_STR%NLAY
C     Gather coordinates
      CALL S4COOR3(X, XREFS(1, 1, NFT + 1), IXS(1, NFT + 1), NGL, 
     .     MAT, PID, IX1, IX2, IX3, IX4, 
     .     X1, X2, X3, X4, 
     .     Y1, Y2, Y3, Y4,
     .     Z1, Z2, Z3, Z4)
C     Volume
      CALL S4DERI3(GBUF%VOL, DUMMY, GEO, IGEO, 
     .     RX, RY, RZ, SX, SY, SZ, TX, TY, TZ, 
     .     X1, X2, X3, X4, 
     .     Y1, Y2, Y3, Y4, 
     .     Z1, Z2, Z3, Z4, 
     .     PX1, PX2, PX3, PX4, 
     .     PY1, PY2, PY3, PY4, 
     .     PZ1, PZ2, PZ3, PZ4, GBUF%JAC_I, 
     .     GBUF%DELTAX, VOLU, NGL, PID, MAT,
     .     PM ,VOLDP)

      PM(104,IXS(1, 1 + NFT)) = ZERO  !global pressure
      
C     Loop over the materials
      DO ILAY = 1, NLAY
C     Layer buffer
         LBUF => ELBUF_STR%BUFLY(ILAY)%LBUF(1,1,1)
         MBUF => ELBUF_STR%BUFLY(ILAY)%MAT(1,1,1)
         DO II = LFT, LLT
C     Material
            MAT(II) = IPM(20 + ILAY, IXS(1, II + NFT))
C     Partial volumes
            LBUF%VOL(II) = PM(20 + ILAY, IXS(1, II + NFT)) * GBUF%VOL(II)
            LBUF%VOL0DP(II) = PM(20 + ILAY, IXS(1, II + NFT)) * VOLDP(II)
         ENDDO
C     Material initialization
         IP = 1
         IBID = 0
         CALL MATINI(PM,  IXS, NIXS, X,
     .        GEO, ALE_CONNECTIVITY, DETONATORS, IPARG, 
     .        SIGI, NEL, SKEW, IGEO,
     .        IPART,IPARTS,
     .        MAT, IPM, NSIGS, NUMSOL, PTSOL,
     .        IP, NGL,NPF, TF, BUFMAT,
     .        GBUF, LBUF, MBUF, ELBUF_STR, ILOADP,
     .        FACLOAD, GBUF%DELTAX)
     
         VFRAC = PM(20+ILAY,IXS(1, 1 + NFT))
         PRES  = PM(104, IPM(20+ILAY,IXS(1, 1 + NFT)))
         PM(104,IXS(1, 1 + NFT)) = PM(104,IXS(1, 1 + NFT)) + VFRAC * PRES !global pressure
     
         MATLAW = IPM(2, MAT(1))
         IF (MATLAW == 5) THEN
! JWL MAT
            IF (.NOT. ERROR_THROWN) THEN
               IF (PM(44, MAT(1)) == ZERO) THEN
                  CALL ANCMSG(MSGID = 1623, MSGTYPE = MSGERROR, ANMODE = ANINFO, 
     .                 I1 = IPM(1, IXS(1, 1 + NFT)), I2 = IPM(1, MAT(1)))
               ENDIF
               ERROR_THROWN = .TRUE.
            ENDIF
            CALL M5IN3(PM, MAT, IPM(1, IXS(1,LFT+NFT)), DETONATORS, LBUF%TB, NGL, IPARG, X, IXS, NIXS)
         ENDIF
      ENDDO

      IF (NLAY > 1) THEN     

C     Mass globalization
         
         DO II = LFT, LLT
            GBUF%RHO(II) = ZERO
         ENDDO
         DO ILAY = 1, NLAY
            LBUF  => ELBUF_STR%BUFLY(ILAY)%LBUF(1,1,1)
            DO II = LFT, LLT
               GBUF%RHO(II) = GBUF%RHO(II) + LBUF%RHO(II) * PM(20 + ILAY, IXS(1, II + NFT))
            ENDDO
         ENDDO

C      Temperature globalization. We must solve later T such as e+p/rho=integral(Cp_global(T),dT)
         GBUF%TEMP(LFT:LLT)=ZERO
         DO ILAY = 1, NLAY
            LBUF  => ELBUF_STR%BUFLY(ILAY)%LBUF(1,1,1)
            DO II = LFT, LLT
               GBUF%TEMP(II) = GBUF%TEMP(II) + LBUF%TEMP(II) * PM(20 + ILAY, IXS(1, II + NFT))*LBUF%RHO(II)/GBUF%RHO(II)   !volfrac*densfrac=massfrac
            ENDDO
         ENDDO        
                  
      ENDIF
      CALL S4MASS3(
     1     GBUF%RHO   ,MAS       ,PARTSAV,X   ,V,
     2     IPARTS(NFT + 1),MSS(1,NFT + 1),MSNF   ,MSSF(1,NFT + 1),WMA,
     3     BID      ,BID       ,MCPS(1,NFT + 1),BID,BID ,
     4     MSSA       ,IX1     ,IX2     ,IX3     ,IX4    ,
     5     GBUF%FILL, GBUF%VOL)
      END SUBROUTINE MULTIFLUID_INIT3T
