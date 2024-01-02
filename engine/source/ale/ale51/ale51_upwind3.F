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
Chd|  ALE51_UPWIND3                 source/ale/ale51/ale51_upwind3.F
Chd|-- called by -----------
Chd|        AFLUXT                        source/ale/ale51/afluxt.F     
Chd|        ALE51_FINISH                  source/ale/ale51/ale51_finish.F
Chd|        ALE51_INIT                    source/ale/ale51/ale51_init.F 
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE ALE51_UPWIND3(PM     ,IXS  ,FLUX ,FLU1 ,ALE_CONNECT,
     .                         ITRIMAT,DDVOL,QMV  ,IFLG ,
     .                         NV46)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C  This subroutines computes from direct fluxes :
C  -1- FLUX(1:6,*), FLU1(*) : Decomposition Factors from Biased Upwind Method
C  -2- QMV(7:12)            :
C  -3- DDVOL                : D/DV . D/DT . VOL = DV/DT
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE I22TRI_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,NUMELS), ITRIMAT,IFLG,NV46
      my_real PM(NPROPM,NUMMAT), FLUX(NV46,*), FLU1(*),DDVOL(*)
      my_real,TARGET :: QMV(12,*)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II,MAT(MVSIZ),IAD2
      my_real REDUC, UPWL(NV46,MVSIZ),FLUX1(MVSIZ), FLUX2(MVSIZ), FLUX3(MVSIZ), FLUX4(MVSIZ),FLUX5(MVSIZ), FLUX6(MVSIZ)
      my_real, TARGET :: QMVL(7:12,MVSIZ)
      my_real, DIMENSION(:) ,POINTER :: pQMV7,pQMV8,pQMV9,pQMV10,pQMV11,pQMV12
      LOGICAL :: debug_outp
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------
      DO I=LFT,LLT
        FLUX1(I) = FLUX(1,I)
        FLUX2(I) = FLUX(2,I)
        FLUX3(I) = FLUX(3,I)
        FLUX4(I) = FLUX(4,I)
      ENDDO
      IF(NV46==6)THEN
      DO I=LFT,LLT
        FLUX5(I) = FLUX(5,I)
        FLUX6(I) = FLUX(6,I)
      ENDDO      
      ENDIF
C-----------------------------------------------
      DO I=LFT,LLT
        II=I+NFT
        MAT(I)=IXS(1,II)
      ENDDO
C-----------------------------------------------
C     UPWIND
C-----------------------------------------------
      DO J=1,NV46
        DO I=LFT,LLT
          UPWL(J,I)=PM(16,MAT(I))
        ENDDO
      ENDDO
      !======================================================!
      !  BOUNDARY FACE : no volume flux by default           !
      !    slip wall bc                                      !
      !======================================================!
      DO I=LFT,LLT
       IAD2 = ALE_CONNECT%ee_connect%iad_connect(I + NFT)
       REDUC=PM(92,MAT(I))
       !---face1---!       
       II=ALE_CONNECT%ee_connect%connected(IAD2 + 1 - 1)
       IF(II == 0)THEN
        FLUX1(I)=FLUX1(I)*REDUC
       ENDIF
       !---face2---!
       II=ALE_CONNECT%ee_connect%connected(IAD2 + 2 - 1)
       IF(II == 0)THEN
        FLUX2(I)=FLUX2(I)*REDUC
       ENDIF
       !---face3---!
       II=ALE_CONNECT%ee_connect%connected(IAD2 + 3 - 1)
       IF(II == 0)THEN
        FLUX3(I)=FLUX3(I)*REDUC
       ENDIF
       !---face4---!
       II=ALE_CONNECT%ee_connect%connected(IAD2 + 4 - 1)
       IF(II == 0)THEN
        FLUX4(I)=FLUX4(I)*REDUC
       ENDIF
       IF(NV46==6)THEN
         !---face5---!
         II=ALE_CONNECT%ee_connect%connected(IAD2 + 5 - 1)
         IF(II == 0)THEN
          FLUX5(I)=FLUX5(I)*REDUC
         ENDIF
         !---face6---!
         II=ALE_CONNECT%ee_connect%connected(IAD2 + 6 - 1)
         IF(II == 0)THEN
          FLUX6(I)=FLUX6(I)*REDUC
         ENDIF
       ENDIF
       ! IF(NV46==6)THEN
      ENDDO
      
      DO I=LFT,LLT
        FLUX(1,I) = FLUX1(I)-UPWL(1,I)*ABS(FLUX1(I))
        FLUX(2,I) = FLUX2(I)-UPWL(2,I)*ABS(FLUX2(I))
        FLUX(3,I) = FLUX3(I)-UPWL(3,I)*ABS(FLUX3(I))
        FLUX(4,I) = FLUX4(I)-UPWL(4,I)*ABS(FLUX4(I))
      ENDDO  
      IF(NV46==6)THEN
      DO I=LFT,LLT
        FLUX(5,I) = FLUX5(I)-UPWL(5,I)*ABS(FLUX5(I))
        FLUX(6,I) = FLUX6(I)-UPWL(6,I)*ABS(FLUX6(I))
      ENDDO            
      ENDIF    
      
      IF(IFLG == 1)THEN
        pQMV7  => QMV(07,LFT:LLT)
        pQMV8  => QMV(08,LFT:LLT)
        pQMV9  => QMV(09,LFT:LLT)
        pQMV10 => QMV(10,LFT:LLT)
        pQMV11 => QMV(11,LFT:LLT)
        pQMV12 => QMV(12,LFT:LLT)                                        
      ELSE
        pQMV7  => QMVL(07,LFT:LLT)
        pQMV8  => QMVL(08,LFT:LLT)
        pQMV9  => QMVL(09,LFT:LLT)
        pQMV10 => QMVL(10,LFT:LLT)
        pQMV11 => QMVL(11,LFT:LLT)
        pQMV12 => QMVL(12,LFT:LLT)      
      ENDIF           
      !MOMENTUM ADVECTION NEEDED FOR AMOMT3.F

      IF(NV46==6)THEN
        DO I=LFT,LLT
          pQMV7(I)  = FLUX1(I)+UPWL(1,I)*ABS(FLUX1(I))
          pQMV8(I)  = FLUX2(I)+UPWL(2,I)*ABS(FLUX2(I))
          pQMV9(I)  = FLUX3(I)+UPWL(3,I)*ABS(FLUX3(I))
          pQMV10(I) = FLUX4(I)+UPWL(4,I)*ABS(FLUX4(I))
          pQMV11(I) = FLUX5(I)+UPWL(5,I)*ABS(FLUX5(I))
          pQMV12(I) = FLUX6(I)+UPWL(6,I)*ABS(FLUX6(I))                    
          FLU1(I) = pQMV7(I) + pQMV8(I) + pQMV9(I) + pQMV10(I) +pQMV11(I) + pQMV12(I)
        ENDDO           
        IF(ITRIMAT  > 0)THEN
          DO I=LFT,LLT
            DDVOL(I)=HALF*( FLU1(I)+ FLUX(1,I)+FLUX(2,I)+FLUX(3,I)+FLUX(4,I)+FLUX(5,I)+FLUX(6,I) )
          ENDDO
        ENDIF                
      ELSE
        DO I=LFT,LLT
          pQMV7(I)  = FLUX1(I)+UPWL(1,I)*ABS(FLUX1(I))
          pQMV8(I)  = FLUX2(I)+UPWL(2,I)*ABS(FLUX2(I))
          pQMV9(I)  = FLUX3(I)+UPWL(3,I)*ABS(FLUX3(I))
          pQMV10(I) = FLUX4(I)+UPWL(4,I)*ABS(FLUX4(I))                
          FLU1(I) = pQMV7(I) + pQMV8(I) + pQMV9(I) + pQMV10(I)
        ENDDO        
        IF(ITRIMAT  > 0)THEN
          DO I=LFT,LLT
            DDVOL(I)=HALF*( FLU1(I)+ FLUX(1,I)+FLUX(2,I)+FLUX(3,I)+FLUX(4,I) )
          ENDDO
        ENDIF        
      ENDIF
 
      !INTERFACE 22 ONLY - OUTPUT---------------! OBSOLETE
      debug_outp = .false.      
      if(ibug22_upwind /= 0)then
        if(ibug22_upwind  > 0)then
          do i=lft,llt
            if(ixs(11,I+nft) == ibug22_upwind)debug_outp=.true.
          enddo
        elseif(ibug22_upwind==-1)then
          debug_outp = .true.
        endif
        if(((itrimat /= ibug22_itrimat).and.(ibug22_itrimat /= -1)))debug_outp=.false.
      endif
!#!include "lockon.inc"      
      if(debug_outp)then
        print *, "    |----ale51_upwind3.F-----|"
        print *, "    |   THREAD INFORMATION   |"
        print *, "    |------------------------|" 
        print *, "    NCYCLE=", NCYCLE
        do i=lft,llt
          if(ibug22_upwind/=ixs(11,I+NFT).and.ibug22_upwind/=-1)cycle
          !if (tag22(i)==zero)print *,"       UNCUT"!cycle
          print *,                    "      brique    =", ixs(11,nft+i)
          print *,                    "      itrimat   =", itrimat
          write (*,FMT='(A,6E26.14)')"       Flux(1:6) =", FLUX(1:6,I)
          write (*,FMT='(A,1E26.14)')"       Flu1      =", FLU1(I)
          print *, "      ------------------------"          
        enddo
!#!include "lockoff.inc"       
      endif
      !-----------------------------------------!

C-----------------------------------------------
      RETURN
      END
C
