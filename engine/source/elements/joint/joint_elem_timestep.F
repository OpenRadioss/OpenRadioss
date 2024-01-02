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
Chd|  JOINT_ELEM_TIMESTEP           source/elements/joint/joint_elem_timestep.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE JOINT_ELEM_TIMESTEP(MS,IN,STIFN,STIFR,IXR,IPART,
     1                               IPARTR,IGEO,GEO,NPBY,IPARG,ELBUF_TAB,
     2                               DT2T,NELTST,ITYPTST,NRBODY,ITAB)
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
#include      "param_c.inc"
#include      "scr02_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXR(NIXR,*),IPART(LIPART1,*),IPARTR(*),
     .   IGEO(NPROPGI,*),NPBY(NNPBY,*),
     .   IPARG(NPARG,*),NELTST,ITYPTST,NRBODY,ITAB(*)
      my_real STIFN(*), STIFR(*),MS(*) ,IN(*),GEO(NPROPG,*),DT2T      
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,M1,M2,IG,IGTYP,KAD,ITYP,NG,JFT,JLT,NEL,
     .        NB8,NFT,NUVAR,IRB,NV
      my_real DTN,DTR,DTRB1,DTRB2,DT
      TYPE(G_BUFEL_),POINTER :: GBUF
C----------------------------------------------------------      
      
      IF ((NODADT==0).AND.(IDTMINS/=2)) THEN
C      
      DO NG=1,NGROUP
        ITYP = IPARG(5,NG)
          NEL = IPARG(2,NG)
          NFT = IPARG(3,NG) 
          JFT = 1
          JLT = MIN(NVSIZ,NEL)  
        GBUF => ELBUF_TAB(NG)%GBUF
          IF (ITYP == 6) THEN
C
C--------> Boucle sur les elements ressort -------
          DO I=JFT,JLT
              J = I + NFT    
            IG = IPART(2,IPARTR(J))
            IGTYP =  IGEO(11,IG)
              NUVAR =  NINT(GEO(25,IG))
            NV = NUVAR*(I-1) + 1
            IF (IGTYP==45) THEN    
C           
C--------> Calcul pas de temps nodal RB1 -------         
                IRB = NINT(GBUF%VAR(NV + 37))
              IF (IRB > 0) THEN
                  M1 = NPBY(1,IRB)
              ELSE
                M1 = IXR(2,J)
              ENDIF 
                DTRB1 = DTFAC1(11)*SQRT(TWO*MS(M1)/MAX(EM20,STIFN(M1)))
              IF (IN(M1) > 0) THEN
                  DTRB1 = MIN(DTRB1,DTFAC1(11)*SQRT(TWO*IN(M1)/MAX(EM20,STIFR(M1))))
              ENDIF                               
C
C--------> Calcul pas de temps nodal RB2 ------- 
                IRB = NINT(GBUF%VAR(NV + 38))
              IF (IRB > 0) THEN
                  M2 = NPBY(1,IRB)
              ELSE
                M2 = IXR(3,J)
              ENDIF            
                DTRB2 = DTFAC1(11)*SQRT(TWO*MS(M2)/MAX(EM20,STIFN(M2)))
              IF (IN(M2) > 0) THEN
                  DTRB2 = MIN(DTRB2,DTFAC1(11)*SQRT(TWO*IN(M2)/MAX(EM20,STIFR(M2))))
              ENDIF
C
C--------> Calcul pas de temps du Joint ------- 
                DT = MIN(DTRB1,DTRB2)                
              IF(DT<DT2T) THEN
                DT2T=DT
                ITYPTST=11
                IF (DTRB1 < DTRB2) THEN
                  NELTST = ITAB(M1)
                ELSE
                  NELTST = ITAB(M2)
                ENDIF
              ENDIF
                
            ENDIF                                                                                
          ENDDO
C    
        ENDIF   
      ENDDO             
C      
      ENDIF
C
      RETURN
                
      END
