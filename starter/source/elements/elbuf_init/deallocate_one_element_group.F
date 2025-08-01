Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||--- uses       -----------------------------------------------------
!||    message_mod                    ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE DEALLOCATE_ONE_ELEMENT_GROUP(
     .                         ELBUF_TAB,NLAY,NPTR,NPTS  ,NPTT, NINTLAY,
     .                         NEL      ,NPT ,NG  ,NGROUP,ITY,  IGTYP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE MESSAGE_MOD
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
      INTEGER I,J,IL,IR,IS,IT,Stat,NPG,ILAYER,IPT,
     .        LEN1,LEN2,LEN_PLAPT,LEN_SIGPT,NPTTOT
      TYPE(BUF_MAT_)     , POINTER :: MATBUF
      TYPE(BUF_LAY_)     , POINTER :: BUFLY
      TYPE(BUF_INTLAY_)  , POINTER :: INTLAY
      TYPE(BUF_VISC_)    , POINTER :: VISCBUF
      TYPE(G_BUFEL_)     , POINTER :: GBUF
      TYPE(L_BUFEL_)     , POINTER :: LBUF
      TYPE(BUF_INTLOC_)  , POINTER :: ILBUF
      TYPE(FAIL_LOC_)    , POINTER :: FLOC

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
      IF (ASSOCIATED(GBUF%AREA)) DEALLOCATE(GBUF%AREA)  
      IF (ASSOCIATED(GBUF%SKEW)) DEALLOCATE(GBUF%SKEW)  
      IF (ASSOCIATED(GBUF%LENGTH)) DEALLOCATE(GBUF%LENGTH)  
      IF (ASSOCIATED(GBUF%TOTDEPL)) DEALLOCATE(GBUF%TOTDEPL)  
      IF (ASSOCIATED(GBUF%TOTROT)) DEALLOCATE(GBUF%TOTROT)  
      IF (ASSOCIATED(GBUF%FOREP)) DEALLOCATE(GBUF%FOREP)  
      IF (ASSOCIATED(GBUF%MOMEP)) DEALLOCATE(GBUF%MOMEP)  
      IF (ASSOCIATED(GBUF%DEP_IN_TENS)) DEALLOCATE(GBUF%DEP_IN_TENS)  
      IF (ASSOCIATED(GBUF%DEP_IN_COMP)) DEALLOCATE(GBUF%DEP_IN_COMP)  
      IF (ASSOCIATED(GBUF%ROT_IN_TENS)) DEALLOCATE(GBUF%ROT_IN_TENS)  
      IF (ASSOCIATED(GBUF%ROT_IN_COMP)) DEALLOCATE(GBUF%ROT_IN_COMP)  
      IF (ASSOCIATED(GBUF%POSX)) DEALLOCATE(GBUF%POSX)  
      IF (ASSOCIATED(GBUF%POSY)) DEALLOCATE(GBUF%POSY)  
      IF (ASSOCIATED(GBUF%POSZ)) DEALLOCATE(GBUF%POSZ)
      IF (ASSOCIATED(GBUF%POSXX)) DEALLOCATE(GBUF%POSXX)  
      IF (ASSOCIATED(GBUF%POSYY)) DEALLOCATE(GBUF%POSYY)  
      IF (ASSOCIATED(GBUF%POSZZ)) DEALLOCATE(GBUF%POSZZ)
      IF (ASSOCIATED(GBUF%YIELD)) DEALLOCATE(GBUF%YIELD)
      IF (ASSOCIATED(GBUF%LENGTH_ERR)) DEALLOCATE(GBUF%LENGTH_ERR)
      IF (ASSOCIATED(GBUF%DV)) DEALLOCATE(GBUF%DV)
      IF (ASSOCIATED(GBUF%DFS)) DEALLOCATE(GBUF%DFS)
      IF (ASSOCIATED(GBUF%SKEW_ERR)) DEALLOCATE(GBUF%SKEW_ERR)
      IF (ASSOCIATED(GBUF%E6)) DEALLOCATE(GBUF%E6)
      IF (ASSOCIATED(GBUF%RUPTCRIT)) DEALLOCATE(GBUF%RUPTCRIT)
      IF (ASSOCIATED(GBUF%MASS)) DEALLOCATE(GBUF%MASS)
      IF (ASSOCIATED(GBUF%V_REPCVT)) DEALLOCATE(GBUF%V_REPCVT)
      IF (ASSOCIATED(GBUF%VR_REPCVT)) DEALLOCATE(GBUF%VR_REPCVT)
      IF (ASSOCIATED(GBUF%VAR)) DEALLOCATE(GBUF%VAR)
      IF (ASSOCIATED(GBUF%VARN)) DEALLOCATE(GBUF%VARN)
      IF (ASSOCIATED(GBUF%DEFINI)) DEALLOCATE(GBUF%DEFINI)
      IF (ASSOCIATED(GBUF%FORINI)) DEALLOCATE(GBUF%FORINI)
      IF (ASSOCIATED(GBUF%INIFRIC)) DEALLOCATE(GBUF%INIFRIC)
      IF (ASSOCIATED(GBUF%DT_PITER)) DEALLOCATE(GBUF%DT_PITER)

      DEALLOCATE(ELBUF_TAB(NG)%GBUF%FILL)

 
      IF (NPTTOT == 1) THEN    
C Only Pointer Associations.
C No allocation in this case
      ELSE  ! NPT > 1                                                                
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%BFRAC)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%BFRAC)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%OFF)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%OFF)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%EINT)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%EINT)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%EINS)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%EINS)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%RHO)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%RHO)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%QVIS)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%QVIS)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%DELTAX)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%DELTAX)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%VOL)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%VOL)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%EPSD)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%EPSD)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%EPSQ)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%EPSQ)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%PLA )) DEALLOCATE(ELBUF_TAB(NG)%GBUF%PLA)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%TEMP)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%TEMP)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%TB)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%TB)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%RK)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%RK)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%RE)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%RE)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%SIG)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%SIG)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%FORTH)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%FORTH)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%EINTTH)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%EINTTH)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%SEQ)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%SEQ)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%ABURN  )) DEALLOCATE(ELBUF_TAB(NG)%GBUF%ABURN)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%MU  )) DEALLOCATE(ELBUF_TAB(NG)%GBUF%MU)

      ENDIF  ! NPT

      IF (NPG == 1) THEN
C Only Pointer Associations.
C No allocation in this case
      ELSEIF (NPG > 1) THEN
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%FORPG)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%FORPG)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%MOMPG)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%MOMPG)
C
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%FORPGPINCH)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%FORPGPINCH)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%MOMPGPINCH)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%MOMPGPINCH)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%EPGPINCHXZ)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%EPGPINCHXZ)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%EPGPINCHYZ)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%EPGPINCHYZ)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%EPGPINCHZZ)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%EPGPINCHZZ)
C
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%STRPG)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%STRPG)
        IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%STRWPG))DEALLOCATE(ELBUF_TAB(NG)%GBUF%STRWPG)
      ENDIF

c-------------------------------------------------
C       global buffer allocation
c-------------------------------------------------
      GBUF => ELBUF_TAB(NG)%GBUF

      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%NOFF))  DEALLOCATE(ELBUF_TAB(NG)%GBUF%NOFF)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%IERR))  DEALLOCATE(ELBUF_TAB(NG)%GBUF%IERR)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%GAMA))  DEALLOCATE(ELBUF_TAB(NG)%GBUF%GAMA)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%SMSTR)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%SMSTR)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%HOURG)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%HOURG)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%TAG22)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%TAG22)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%STRA))  DEALLOCATE(ELBUF_TAB(NG)%GBUF%STRA)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%STRW))  DEALLOCATE(ELBUF_TAB(NG)%GBUF%STRW)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%SIGI))  DEALLOCATE(ELBUF_TAB(NG)%GBUF%SIGI)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%DMG))   DEALLOCATE(ELBUF_TAB(NG)%GBUF%DMG)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%UELR))  DEALLOCATE(ELBUF_TAB(NG)%GBUF%UELR)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%UELR1)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%UELR1)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%DAMDL))  DEALLOCATE(ELBUF_TAB(NG)%GBUF%DAMDL)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%FOR))  DEALLOCATE(ELBUF_TAB(NG)%GBUF%FOR)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%MOM))  DEALLOCATE(ELBUF_TAB(NG)%GBUF%MOM)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%THK))  DEALLOCATE(ELBUF_TAB(NG)%GBUF%THK)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%THK_I))  DEALLOCATE(ELBUF_TAB(NG)%GBUF%THK_I)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%JAC_I))  DEALLOCATE(ELBUF_TAB(NG)%GBUF%JAC_I)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%DT))     DEALLOCATE(ELBUF_TAB(NG)%GBUF%DT)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%ISMS))   DEALLOCATE(ELBUF_TAB(NG)%GBUF%ISMS)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%STRHG))  DEALLOCATE(ELBUF_TAB(NG)%GBUF%STRHG)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%BPRELD)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%BPRELD)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%COR_NF)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%COR_NF)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%COR_FR)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%COR_FR)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%COR_XR)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%COR_XR)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%MAXFRAC)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%MAXFRAC)
      IF (ASSOCIATED(ELBUF_TAB(NG)%GBUF%MAXEPS)) DEALLOCATE(ELBUF_TAB(NG)%GBUF%MAXEPS)
C-------------------------------------------------
c     Inter layers (ply xfem)
c-------------------------------------------------
      DO I = 1, NINTLAY
        IF (ASSOCIATED(ELBUF_TAB(NG)%INTLAY(I)%EINT)) DEALLOCATE(ELBUF_TAB(NG)%INTLAY(I)%EINT)
        IF (ASSOCIATED(ELBUF_TAB(NG)%INTLAY(I)%COUNT)) DEALLOCATE(ELBUF_TAB(NG)%INTLAY(I)%COUNT)       

        DO IR = 1,NPTR                                                   
          DO IS = 1,NPTS                                                 
            ILBUF => ELBUF_TAB(NG)%INTLAY(I)%ILBUF(IR,IS)              
            IF (ASSOCIATED(ILBUF%EPS)) DEALLOCATE(ILBUF%EPS)
            IF (ASSOCIATED(ILBUF%SIG)) DEALLOCATE(ILBUF%SIG)
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
                IF (ASSOCIATED(LBUF%OFF)) DEALLOCATE(LBUF%OFF)
                IF (ASSOCIATED(LBUF%GAMA)) DEALLOCATE(LBUF%GAMA)
                IF (ASSOCIATED(LBUF%STRA)) DEALLOCATE(LBUF%STRA)
                IF (ASSOCIATED(LBUF%FRAC)) DEALLOCATE(LBUF%FRAC)
                IF (ASSOCIATED(LBUF%BFRAC)) DEALLOCATE(LBUF%BFRAC)
                IF (ASSOCIATED(LBUF%EINT))  DEALLOCATE(LBUF%EINT)
                IF (ASSOCIATED(LBUF%EINS)) DEALLOCATE(LBUF%EINS)
                IF (ASSOCIATED(LBUF%RHO)) DEALLOCATE(LBUF%RHO)
                IF (ASSOCIATED(LBUF%DP_DRHO)) DEALLOCATE(LBUF%DP_DRHO)
                IF (ASSOCIATED(LBUF%QVIS)) DEALLOCATE(LBUF%QVIS)
                IF (ASSOCIATED(LBUF%DELTAX)) DEALLOCATE(LBUF%DELTAX)
                IF (ASSOCIATED(LBUF%VOL)) DEALLOCATE(LBUF%VOL)
                IF (ASSOCIATED(LBUF%EPSA)) DEALLOCATE(LBUF%EPSA)
                IF (ASSOCIATED(LBUF%EPSD)) DEALLOCATE(LBUF%EPSD)
                IF (ASSOCIATED(LBUF%EPSQ)) DEALLOCATE(LBUF%EPSQ)
                IF (ASSOCIATED(LBUF%EPSF)) DEALLOCATE(LBUF%EPSF)
                IF (ASSOCIATED(LBUF%PLA))  DEALLOCATE(LBUF%PLA)
                IF (ASSOCIATED(LBUF%TEMP))  DEALLOCATE(LBUF%TEMP)
                IF (ASSOCIATED(LBUF%TB))  DEALLOCATE(LBUF%TB)
                IF (ASSOCIATED(LBUF%RK))  DEALLOCATE(LBUF%RK)
                IF (ASSOCIATED(LBUF%RE))  DEALLOCATE(LBUF%RE)
                IF (ASSOCIATED(LBUF%VK))  DEALLOCATE(LBUF%VK)
                IF (ASSOCIATED(LBUF%SF))  DEALLOCATE(LBUF%SF)
                IF (ASSOCIATED(LBUF%ROB))  DEALLOCATE(LBUF%ROB)
                IF (ASSOCIATED(LBUF%DAM))  DEALLOCATE(LBUF%DAM)
                IF (ASSOCIATED(LBUF%DSUM))  DEALLOCATE(LBUF%DSUM)
                IF (ASSOCIATED(LBUF%DGLO))  DEALLOCATE(LBUF%DGLO)
                IF (ASSOCIATED(LBUF%CRAK))  DEALLOCATE(LBUF%CRAK)
                IF (ASSOCIATED(LBUF%EPE))  DEALLOCATE(LBUF%EPE)
                IF (ASSOCIATED(LBUF%ANG))  DEALLOCATE(LBUF%ANG)
                IF (ASSOCIATED(LBUF%EPC))  DEALLOCATE(LBUF%EPC)
                IF (ASSOCIATED(LBUF%XST))  DEALLOCATE(LBUF%XST)
                IF (ASSOCIATED(LBUF%SSP))  DEALLOCATE(LBUF%SSP)
                IF (ASSOCIATED(LBUF%Z))    DEALLOCATE(LBUF%Z)
                IF (ASSOCIATED(LBUF%VISC))  DEALLOCATE(LBUF%VISC)
                IF (ASSOCIATED(LBUF%SIGL))  DEALLOCATE(LBUF%SIGL)
                IF (ASSOCIATED(LBUF%SIGV))  DEALLOCATE( LBUF%SIGV)
                IF (ASSOCIATED(LBUF%SIGA))  DEALLOCATE(LBUF%SIGA)
                IF (ASSOCIATED(LBUF%SIGB))  DEALLOCATE(LBUF%SIGB)
                IF (ASSOCIATED(LBUF%SIGC))  DEALLOCATE(LBUF%SIGC)
                IF (ASSOCIATED(LBUF%SIGD))  DEALLOCATE(LBUF%SIGD)
                IF (ASSOCIATED(LBUF%SIGF))  DEALLOCATE(LBUF%SIGF)
                IF (ASSOCIATED(LBUF%SIG))  DEALLOCATE(LBUF%SIG)
                IF (ASSOCIATED(LBUF%SIGPLY))  DEALLOCATE(LBUF%SIGPLY)
                IF (ASSOCIATED(LBUF%FOR))  DEALLOCATE(LBUF%FOR)
                IF (ASSOCIATED(LBUF%MOM))  DEALLOCATE(LBUF%MOM)
                IF (ASSOCIATED(LBUF%THK))  DEALLOCATE(LBUF%THK)
                IF (ASSOCIATED(LBUF%SMSTR))  DEALLOCATE(LBUF%SMSTR)
                IF (ASSOCIATED(LBUF%DMG)) DEALLOCATE(LBUF%DMG)
                IF (ASSOCIATED(LBUF%FORTH)) DEALLOCATE(LBUF%FORTH)
                IF (ASSOCIATED(LBUF%EINTTH)) DEALLOCATE(LBUF%EINTTH)
                IF (ASSOCIATED(LBUF%SEQ)) DEALLOCATE(LBUF%SEQ)
                IF (ASSOCIATED(LBUF%JAC_I)) DEALLOCATE(LBUF%JAC_I)
                IF (ASSOCIATED(LBUF%FAC_YLD)) DEALLOCATE(LBUF%FAC_YLD)
                IF (ASSOCIATED(LBUF%ABURN)) DEALLOCATE(LBUF%ABURN)
                IF (ASSOCIATED(LBUF%MU)) DEALLOCATE(LBUF%MU)
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
          IF (ASSOCIATED(BUFLY%DMG))   DEALLOCATE( BUFLY%DMG)                  
          IF (ASSOCIATED(BUFLY%GAMA))  DEALLOCATE( BUFLY%GAMA)       
          IF (ASSOCIATED(BUFLY%DIRA))  DEALLOCATE( BUFLY%DIRA)       
          IF (ASSOCIATED(BUFLY%DIRB))  DEALLOCATE( BUFLY%DIRB)       
          IF (ASSOCIATED(BUFLY%CRKDIR))DEALLOCATE( BUFLY%CRKDIR)       
          IF (ASSOCIATED(BUFLY%OFFPG)) DEALLOCATE( BUFLY%OFFPG)       
          IF (ASSOCIATED(BUFLY%OFF))   DEALLOCATE( BUFLY%OFF)       
c
          IF (NPG > 1) THEN
            IF (ASSOCIATED(BUFLY%PLAPT)) DEALLOCATE( BUFLY%PLAPT)                       
            IF (ASSOCIATED(BUFLY%SIGPT)) DEALLOCATE( BUFLY%SIGPT)
 
          ELSEIF (NPG == 1) THEN
C             Nothing to deallocate
          ENDIF
C
          IF (ASSOCIATED(BUFLY%HOURG)) DEALLOCATE(BUFLY%HOURG)           
          IF (ASSOCIATED(BUFLY%UELR)) DEALLOCATE(BUFLY%UELR)                     
          IF (ASSOCIATED(BUFLY%UELR1)) DEALLOCATE(BUFLY%UELR1)                    
      ENDDO                                                                 
C-----------------------------------------------
      RETURN
      END
