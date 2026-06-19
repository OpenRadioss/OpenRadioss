Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||====================================================================
!||    deallocate_one_element_group   ../starter/source/elements/elbuf_init/deallocate_one_element_group.F
!||--- called by ------------------------------------------------------
!||    deallocate_elbuf               ../starter/source/elements/elbuf_init/deallocate_buffer.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||====================================================================
      SUBROUTINE DEALLOCATE_ONE_ELEMENT_GROUP(
     .                         ELBUF_TAB,NLAY,NPTR,NPTS  ,NPTT, NINTLAY,
     .                         NEL      ,NPT ,NG  ,NGROUP,ITY,  IGTYP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      use my_dealloc_mod, only : my_dealloc
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NLAY,NINTLAY,NPTR,NPTS,NPTT,NEL,NPT,NG,NGROUP,ITY,IGTYP
      TYPE(ELBUF_STRUCT_),TARGET ,DIMENSION(NGROUP)     :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IR,IS,IT,NPG,NPTTOT
      TYPE(BUF_LAY_)     , POINTER :: BUFLY
      TYPE(G_BUFEL_)     , POINTER :: GBUF
      TYPE(L_BUFEL_)     , POINTER :: LBUF
      TYPE(BUF_INTLOC_)  , POINTER :: ILBUF

C=======================================================================
      IF (ITY == 1 .OR. ITY == 2 .OR. ITY == 51 .OR. ITY == 101) THEN
        NPG = NPTR*NPTS*NPTT
        NPTTOT = NPT
      ELSEIF (ITY == 3 .OR. ITY == 7) THEN
        NPG = NPTR*NPTS
        NPTTOT  = 0
        DO I=1,NLAY
          NPTTOT = NPTTOT + NPG*ELBUF_TAB(NG)%BUFLY(I)%NPTT
        ENDDO
        IF (NPT == 0) NPTTOT = NPT
      ELSEIF (ITY == 4 .OR. ITY == 5 .OR. ITY == 6  .OR. ITY == 100)THEN
        NPG = 0
        NPTTOT = 0
      ENDIF

      GBUF => ELBUF_TAB(NG)%GBUF

C Care Deallocation is inverted against allocation routines.
C Lowest arrays needs to be deallocated first.

c---
c 1-D elements (TRUSS, BEAM, SPRING, NSTRAND)
c---
      IF (ASSOCIATED(GBUF%AREA)) CALL MY_DEALLOC(GBUF%AREA)  
      IF (ASSOCIATED(GBUF%SKEW)) CALL MY_DEALLOC(GBUF%SKEW)  
      IF (ASSOCIATED(GBUF%LENGTH)) CALL MY_DEALLOC(GBUF%LENGTH)  
      IF (ASSOCIATED(GBUF%TOTDEPL)) CALL MY_DEALLOC(GBUF%TOTDEPL)  
      IF (ASSOCIATED(GBUF%TOTROT)) CALL MY_DEALLOC(GBUF%TOTROT)  
      IF (ASSOCIATED(GBUF%FOREP)) CALL MY_DEALLOC(GBUF%FOREP)  
      IF (ASSOCIATED(GBUF%MOMEP)) CALL MY_DEALLOC(GBUF%MOMEP)  
      IF (ASSOCIATED(GBUF%DEP_IN_TENS)) CALL MY_DEALLOC(GBUF%DEP_IN_TENS)  
      IF (ASSOCIATED(GBUF%DEP_IN_COMP)) CALL MY_DEALLOC(GBUF%DEP_IN_COMP)  
      IF (ASSOCIATED(GBUF%ROT_IN_TENS)) CALL MY_DEALLOC(GBUF%ROT_IN_TENS)  
      IF (ASSOCIATED(GBUF%ROT_IN_COMP)) CALL MY_DEALLOC(GBUF%ROT_IN_COMP)  
      IF (ASSOCIATED(GBUF%POSX)) CALL MY_DEALLOC(GBUF%POSX)  
      IF (ASSOCIATED(GBUF%POSY)) CALL MY_DEALLOC(GBUF%POSY)  
      IF (ASSOCIATED(GBUF%POSZ)) CALL MY_DEALLOC(GBUF%POSZ)
      IF (ASSOCIATED(GBUF%POSXX)) CALL MY_DEALLOC(GBUF%POSXX)  
      IF (ASSOCIATED(GBUF%POSYY)) CALL MY_DEALLOC(GBUF%POSYY)  
      IF (ASSOCIATED(GBUF%POSZZ)) CALL MY_DEALLOC(GBUF%POSZZ)
      IF (ASSOCIATED(GBUF%YIELD)) CALL MY_DEALLOC(GBUF%YIELD)
      IF (ASSOCIATED(GBUF%LENGTH_ERR)) CALL MY_DEALLOC(GBUF%LENGTH_ERR)
      IF (ASSOCIATED(GBUF%DV)) CALL MY_DEALLOC(GBUF%DV)
      IF (ASSOCIATED(GBUF%DFS)) CALL MY_DEALLOC(GBUF%DFS)
      IF (ASSOCIATED(GBUF%SKEW_ERR)) CALL MY_DEALLOC(GBUF%SKEW_ERR)
      IF (ASSOCIATED(GBUF%E6)) CALL MY_DEALLOC(GBUF%E6)
      IF (ASSOCIATED(GBUF%RUPTCRIT)) CALL MY_DEALLOC(GBUF%RUPTCRIT)
      IF (ASSOCIATED(GBUF%MASS)) CALL MY_DEALLOC(GBUF%MASS)
      IF (ASSOCIATED(GBUF%V_REPCVT)) CALL MY_DEALLOC(GBUF%V_REPCVT)
      IF (ASSOCIATED(GBUF%VR_REPCVT)) CALL MY_DEALLOC(GBUF%VR_REPCVT)
      IF (ASSOCIATED(GBUF%VAR)) CALL MY_DEALLOC(GBUF%VAR)
      IF (ASSOCIATED(GBUF%VARN)) CALL MY_DEALLOC(GBUF%VARN)
      IF (ASSOCIATED(GBUF%DEFINI)) CALL MY_DEALLOC(GBUF%DEFINI)
      IF (ASSOCIATED(GBUF%FORINI)) CALL MY_DEALLOC(GBUF%FORINI)
      IF (ASSOCIATED(GBUF%INIFRIC)) CALL MY_DEALLOC(GBUF%INIFRIC)
      IF (ASSOCIATED(GBUF%DT_PITER)) CALL MY_DEALLOC(GBUF%DT_PITER)

      CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%FILL)

 
      IF (NPTTOT == 1) THEN    
C Only Pointer Associations.
C No allocation in this case
      ELSE  ! NPT > 1                                                                
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%BFRAC)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%BFRAC)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%OFF)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%OFF)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%EINT)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%EINT)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%EINS)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%EINS)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%RHO)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%RHO)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%QVIS)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%QVIS)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%DELTAX)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%DELTAX)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%VOL)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%VOL)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%EPSD)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%EPSD)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%EPSQ)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%EPSQ)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%PLA )) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%PLA)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%TEMP)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%TEMP)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%TB)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%TB)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%RK)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%RK)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%RE)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%RE)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%SIG)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%SIG)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%FORTH)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%FORTH)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%EINTTH)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%EINTTH)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%SEQ)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%SEQ)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%ABURN  )) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%ABURN)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%MU  )) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%MU)

      ENDIF  ! NPT

      IF (NPG == 1) THEN
C Only Pointer Associations.
C No allocation in this case
      ELSEIF (NPG > 1) THEN
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%FORPG)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%FORPG)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%MOMPG)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%MOMPG)
C
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%FORPGPINCH)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%FORPGPINCH)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%MOMPGPINCH)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%MOMPGPINCH)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%EPGPINCHXZ)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%EPGPINCHXZ)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%EPGPINCHYZ)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%EPGPINCHYZ)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%EPGPINCHZZ)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%EPGPINCHZZ)
C
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%STRPG)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%STRPG)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%STRWPG))CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%STRWPG)
      ENDIF

c-------------------------------------------------
C       global buffer allocation
c-------------------------------------------------
      GBUF => ELBUF_TAB(NG)%GBUF

      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%NOFF))  CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%NOFF)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%IERR))  CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%IERR)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%GAMA))  CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%GAMA)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%SMSTR)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%SMSTR)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%HOURG)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%HOURG)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%TAG22)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%TAG22)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%STRA))  CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%STRA)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%STRW))  CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%STRW)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%SIGI))  CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%SIGI)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%DMG))   CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%DMG)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%UELR))  CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%UELR)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%UELR1)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%UELR1)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%DAMDL))  CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%DAMDL)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%FOR))  CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%FOR)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%MOM))  CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%MOM)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%THK))  CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%THK)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%THK_I))  CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%THK_I)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%JAC_I))  CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%JAC_I)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%DT))     CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%DT)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%ISMS))   CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%ISMS)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%STRHG))  CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%STRHG)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%BPRELD)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%BPRELD)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%COR_NF)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%COR_NF)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%COR_FR)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%COR_FR)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%COR_XR)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%COR_XR)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%MAXFRAC)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%MAXFRAC)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%MAXEPS)) CALL MY_DEALLOC(ELBUF_TAB(NG)%GBUF%MAXEPS)
C-------------------------------------------------
c     Inter layers (ply xfem)
c-------------------------------------------------
      DO I = 1, NINTLAY
        IF (ASSOCIATED(ELBUF_TAB(NG)%INTLAY(I)%EINT)) CALL MY_DEALLOC(ELBUF_TAB(NG)%INTLAY(I)%EINT)
        IF (ASSOCIATED(ELBUF_TAB(NG)%INTLAY(I)%COUNT)) CALL MY_DEALLOC(ELBUF_TAB(NG)%INTLAY(I)%COUNT)       

        DO IR = 1,NPTR                                                   
          DO IS = 1,NPTS                                                 
            ILBUF => ELBUF_TAB(NG)%INTLAY(I)%ILBUF(IR,IS)              
            IF (ASSOCIATED(ILBUF%EPS)) CALL MY_DEALLOC(ILBUF%EPS)
            IF (ASSOCIATED(ILBUF%SIG)) CALL MY_DEALLOC(ILBUF%SIG)
          ENDDO                                                            
        ENDDO                                                            
      ENDDO                                                             

c-------------------------------------------------
C layer mean variable (NPG = 1)
c-------------------------------------------------
C Nothing to do - only pointer settings

c-------------------------------------------------
c     LAYER Local variables per integration point
c-------------------------------------------------
      DO I = 1, NLAY
         BUFLY => ELBUF_TAB(NG)%BUFLY(I)
         IF (IGTYP == 51 .OR. IGTYP == 52) THEN
            NPTT = BUFLY%NPTT
         ELSE
           NPTT = ELBUF_TAB(NG)%NPTT
         ENDIF

         DO IR = 1,NPTR
           DO IS = 1,NPTS
             DO IT = 1,NPTT
                LBUF => ELBUF_TAB(NG)%BUFLY(I)%LBUF(IR,IS,IT) 
c
                IF (ASSOCIATED(LBUF%OFF)) CALL MY_DEALLOC(LBUF%OFF)
                IF (ASSOCIATED(LBUF%GAMA)) CALL MY_DEALLOC(LBUF%GAMA)
                IF (ASSOCIATED(LBUF%STRA)) CALL MY_DEALLOC(LBUF%STRA)
                IF (ASSOCIATED(LBUF%FRAC)) CALL MY_DEALLOC(LBUF%FRAC)
                IF (ASSOCIATED(LBUF%BFRAC)) CALL MY_DEALLOC(LBUF%BFRAC)
                IF (ASSOCIATED(LBUF%EINT))  CALL MY_DEALLOC(LBUF%EINT)
                IF (ASSOCIATED(LBUF%EINS)) CALL MY_DEALLOC(LBUF%EINS)
                IF (ASSOCIATED(LBUF%RHO)) CALL MY_DEALLOC(LBUF%RHO)
                IF (ASSOCIATED(LBUF%DP_DRHO)) CALL MY_DEALLOC(LBUF%DP_DRHO)
                IF (ASSOCIATED(LBUF%QVIS)) CALL MY_DEALLOC(LBUF%QVIS)
                IF (ASSOCIATED(LBUF%DELTAX)) CALL MY_DEALLOC(LBUF%DELTAX)
                IF (ASSOCIATED(LBUF%VOL)) CALL MY_DEALLOC(LBUF%VOL)
                IF (ASSOCIATED(LBUF%EPSA)) CALL MY_DEALLOC(LBUF%EPSA)
                IF (ASSOCIATED(LBUF%EPSD)) CALL MY_DEALLOC(LBUF%EPSD)
                IF (ASSOCIATED(LBUF%EPSQ)) CALL MY_DEALLOC(LBUF%EPSQ)
                IF (ASSOCIATED(LBUF%EPSF)) CALL MY_DEALLOC(LBUF%EPSF)
                IF (ASSOCIATED(LBUF%PLA))  CALL MY_DEALLOC(LBUF%PLA)
                IF (ASSOCIATED(LBUF%TEMP))  CALL MY_DEALLOC(LBUF%TEMP)
                IF (ASSOCIATED(LBUF%TB))  CALL MY_DEALLOC(LBUF%TB)
                IF (ASSOCIATED(LBUF%RK))  CALL MY_DEALLOC(LBUF%RK)
                IF (ASSOCIATED(LBUF%RE))  CALL MY_DEALLOC(LBUF%RE)
                IF (ASSOCIATED(LBUF%VK))  CALL MY_DEALLOC(LBUF%VK)
                IF (ASSOCIATED(LBUF%SF))  CALL MY_DEALLOC(LBUF%SF)
                IF (ASSOCIATED(LBUF%ROB))  CALL MY_DEALLOC(LBUF%ROB)
                IF (ASSOCIATED(LBUF%DAM))  CALL MY_DEALLOC(LBUF%DAM)
                IF (ASSOCIATED(LBUF%DSUM))  CALL MY_DEALLOC(LBUF%DSUM)
                IF (ASSOCIATED(LBUF%DGLO))  CALL MY_DEALLOC(LBUF%DGLO)
                IF (ASSOCIATED(LBUF%CRAK))  CALL MY_DEALLOC(LBUF%CRAK)
                IF (ASSOCIATED(LBUF%EPE))  CALL MY_DEALLOC(LBUF%EPE)
                IF (ASSOCIATED(LBUF%ANG))  CALL MY_DEALLOC(LBUF%ANG)
                IF (ASSOCIATED(LBUF%EPC))  CALL MY_DEALLOC(LBUF%EPC)
                IF (ASSOCIATED(LBUF%XST))  CALL MY_DEALLOC(LBUF%XST)
                IF (ASSOCIATED(LBUF%SSP))  CALL MY_DEALLOC(LBUF%SSP)
                IF (ASSOCIATED(LBUF%Z))    CALL MY_DEALLOC(LBUF%Z)
                IF (ASSOCIATED(LBUF%VISC))  CALL MY_DEALLOC(LBUF%VISC)
                IF (ASSOCIATED(LBUF%SIGL))  CALL MY_DEALLOC(LBUF%SIGL)
                IF (ASSOCIATED(LBUF%SIGV))  CALL MY_DEALLOC( LBUF%SIGV)
                IF (ASSOCIATED(LBUF%SIGA))  CALL MY_DEALLOC(LBUF%SIGA)
                IF (ASSOCIATED(LBUF%SIGB))  CALL MY_DEALLOC(LBUF%SIGB)
                IF (ASSOCIATED(LBUF%SIGC))  CALL MY_DEALLOC(LBUF%SIGC)
                IF (ASSOCIATED(LBUF%SIGD))  CALL MY_DEALLOC(LBUF%SIGD)
                IF (ASSOCIATED(LBUF%SIGF))  CALL MY_DEALLOC(LBUF%SIGF)
                IF (ASSOCIATED(LBUF%SIG))  CALL MY_DEALLOC(LBUF%SIG)
                IF (ASSOCIATED(LBUF%SIGPLY))  CALL MY_DEALLOC(LBUF%SIGPLY)
                IF (ASSOCIATED(LBUF%FOR))  CALL MY_DEALLOC(LBUF%FOR)
                IF (ASSOCIATED(LBUF%MOM))  CALL MY_DEALLOC(LBUF%MOM)
                IF (ASSOCIATED(LBUF%THK))  CALL MY_DEALLOC(LBUF%THK)
                IF (ASSOCIATED(LBUF%SMSTR))  CALL MY_DEALLOC(LBUF%SMSTR)
                IF (ASSOCIATED(LBUF%DMG)) CALL MY_DEALLOC(LBUF%DMG)
                IF (ASSOCIATED(LBUF%FORTH)) CALL MY_DEALLOC(LBUF%FORTH)
                IF (ASSOCIATED(LBUF%EINTTH)) CALL MY_DEALLOC(LBUF%EINTTH)
                IF (ASSOCIATED(LBUF%SEQ)) CALL MY_DEALLOC(LBUF%SEQ)
                IF (ASSOCIATED(LBUF%JAC_I)) CALL MY_DEALLOC(LBUF%JAC_I)
                IF (ASSOCIATED(LBUF%FAC_YLD)) CALL MY_DEALLOC(LBUF%FAC_YLD)
                IF (ASSOCIATED(LBUF%ABURN)) CALL MY_DEALLOC(LBUF%ABURN)
                IF (ASSOCIATED(LBUF%MU)) CALL MY_DEALLOC(LBUF%MU)
              ENDDO                                                           
            ENDDO                                                          
           ENDDO 
          ENDDO

c-------------------------------------------------
c     layer buffer allocation
c-------------------------------------------------
        DO I = 1, NLAY                                                        
          BUFLY => ELBUF_TAB(NG)%BUFLY(I)                                     
c
          IF (ASSOCIATED(BUFLY%DMG))   CALL MY_DEALLOC( BUFLY%DMG)                  
          IF (ASSOCIATED(BUFLY%GAMA))  CALL MY_DEALLOC( BUFLY%GAMA)       
          IF (ASSOCIATED(BUFLY%DIRA))  CALL MY_DEALLOC( BUFLY%DIRA)       
          IF (ASSOCIATED(BUFLY%DIRB))  CALL MY_DEALLOC( BUFLY%DIRB)       
          IF (ASSOCIATED(BUFLY%CRKDIR))CALL MY_DEALLOC( BUFLY%CRKDIR)       
          IF (ASSOCIATED(BUFLY%OFFPG)) CALL MY_DEALLOC( BUFLY%OFFPG)       
          IF (ASSOCIATED(BUFLY%OFF))   CALL MY_DEALLOC( BUFLY%OFF)       
c
          IF (NPG > 1) THEN
            IF (ASSOCIATED(BUFLY%PLAPT)) CALL MY_DEALLOC( BUFLY%PLAPT)                       
            IF (ASSOCIATED(BUFLY%SIGPT)) CALL MY_DEALLOC( BUFLY%SIGPT)
 
          ELSEIF (NPG == 1) THEN
C             Nothing to deallocate
          ENDIF
C
          IF (ASSOCIATED(BUFLY%HOURG)) CALL MY_DEALLOC(BUFLY%HOURG)           
          IF (ASSOCIATED(BUFLY%UELR)) CALL MY_DEALLOC(BUFLY%UELR)                     
          IF (ASSOCIATED(BUFLY%UELR1)) CALL MY_DEALLOC(BUFLY%UELR1)                    
      ENDDO                                                                 
C-----------------------------------------------
      RETURN
      END
