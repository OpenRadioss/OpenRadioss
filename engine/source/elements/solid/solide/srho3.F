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
!||    srho3            ../engine/source/elements/solid/solide/srho3.F
!||--- called by ------------------------------------------------------
!||    ig3duforc3       ../engine/source/elements/ige3d/ig3duforc3.F
!||    s10forc3         ../engine/source/elements/solid/solide10/s10forc3.F
!||    s16forc3         ../engine/source/elements/thickshell/solide16/s16forc3.F
!||    s20forc3         ../engine/source/elements/solid/solide20/s20forc3.F
!||    s4forc3          ../engine/source/elements/solid/solide4/s4forc3.F
!||    s6cforc3         ../engine/source/elements/thickshell/solide6c/s6cforc3.F
!||    s8cforc3         ../engine/source/elements/thickshell/solide8c/s8cforc3.F
!||    s8eforc3         ../engine/source/elements/solid/solide8e/s8eforc3.F
!||    s8sforc3         ../engine/source/elements/solid/solide8s/s8sforc3.F
!||    s8zforc3         ../engine/source/elements/solid/solide8z/s8zforc3.F
!||    scforc3          ../engine/source/elements/thickshell/solidec/scforc3.F
!||    sforc3           ../engine/source/elements/solid/solide/sforc3.F
!||    szforc3          ../engine/source/elements/solid/solidez/szforc3.F
!||--- calls      -----------------------------------------------------
!||    ancmsg           ../engine/source/output/message/message.F
!||    arret            ../engine/source/system/arret.F
!||--- uses       -----------------------------------------------------
!||    ale_mod          ../common_source/modules/ale/ale_mod.F
!||    i22bufbric_mod   ../common_source/modules/interfaces/cut-cell-search_mod.F
!||    message_mod      ../engine/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE SRHO3(
     1   PM,        VOLO,      RHON,      EINT,
     2   DIVDE,     FLUX,      FLU1,      VOLN,
     3   DVOL,      NGL,       MAT,       OFF,
     4   IS_MAT_BCS,TAG22,     VOLDP,     VOL0DP,
     5   AMU,       OFFG,      NEL,       MTN,
     6   JALE,      ISMSTR,    JEUL,      JLAG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE I22BUFBRIC_MOD
      USE ALE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "scr06_c.inc"
#include      "impl1_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "inter22.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: MTN
      INTEGER, INTENT(IN) :: JALE
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER, INTENT(IN) :: JEUL
      INTEGER, INTENT(IN) :: JLAG
      INTEGER NGL(*), MAT(*), IS_MAT_BCS, IB,NIN,MCELL
      
      my_real PM(NPROPM,NUMMAT),VOLO(*), RHON(*),EINT(*),FLUX(6,*), FLU1(*),
     .        VOLN(*), DVOL(*),DIVDE(*),OFF(*),TAG22(*),AMU(*) ,OFFG(*)
      DOUBLE PRECISION VOLDP(*),VOL0DP(*),DVDP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real :: RHO0,DVV, E0,VAVG,RV,RVP,RHON_OLD(MVSIZ),DDVOL,RHOREF
      INTEGER :: I, J,COUNT,LIST(MVSIZ),II, MX
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      RHON_OLD(1:NEL)=RHON(1:NEL)
      RHO0 = ZERO
      IF(ALE%GLOBAL%INCOMP==1 .AND. JEUL+JALE==1)THEN
        MX        = MAT(1)
        RHO0      = PM(1,MX)
        E0        = PM(23,MX)
        DO I=1,NEL
          DVV     = DIVDE(I)
          RHON(I) = RHON(I)-DVV*RHO0
          VAVG    = HALF*(VOLN(I)+VOLO(I))
          DVOL(I) = VAVG*DVV
          EINT(I) = EINT(I)*VOLO(I)-E0*DVV*VAVG
        ENDDO
      ELSE
        IF(JLAG/=0)THEN
          MX      = MAT(1)
          RHO0    = PM(1,MX)
C--- due to /INIBRI/EREF        
          IF (TT==ZERO) THEN
           IF (ISMSTR==11) THEN
            VOLO(1:NEL)=VOLN(1:NEL)
           ELSEIF(ISMSTR==1) THEN
            DO I=1,NEL
              IF(OFFG(I)>ONE) VOLO(I)=VOLN(I)
            ENDDO
           END IF
          END IF
          IF (IMPL_S>0.AND.ILINE>0) THEN
            DO I=1,NEL
              RHON(I) = RHO0
              EINT(I) = EINT(I)*VOLN(I)
            ENDDO
          ELSE
            IF (MTN /= 115) THEN 
              DO I=1,NEL
                IF(OFFG(I)==ZERO.AND.VOLN(I)==ONE) VOLN(I)=VOLO(I)
                DVOL(I) = VOLN(I)-(RHO0/RHON(I))*VOLO(I)
                RHON(I) = RHO0*(VOLO(I)/VOLN(I))
                EINT(I) = EINT(I)*VOLO(I)
              ENDDO
            ELSE
              DO I=1,NEL
                IF(OFFG(I)==ZERO.AND.VOLN(I)==ONE) VOLN(I)=VOLO(I)
                DVOL(I) = VOLN(I)-(RHON(I+NEL)/RHON(I))*VOLO(I)
                RHON(I) = RHON(I+NEL)*(VOLO(I)/VOLN(I))
                EINT(I) = EINT(I)*VOLO(I)
              ENDDO            
            ENDIF
          ENDIF
        ELSEIF(JALE/=0)THEN                                                                                              
          DO I=1,NEL                                                                                                     
           RHON(I)    = RHON(I)/VOLN(I)                                                                                    
           DVOL(I)    = VOLN(I)-VOLO(I)+HALF*DT1*(FLU1(I)+FLUX(1,I)+FLUX(2,I)+FLUX(3,I)+FLUX(4,I)+FLUX(5,I)+FLUX(6,I))   
           VOLO(I)    = VOLN(I)                                                                                            
         ENDDO                                                                                                             
        ELSEIF(JEUL/=0)THEN                                                                                              
         DO I=1,NEL                                                                                                      
          RHON(I)     = RHON(I)/VOLN(I)                                                                                    
          DVOL(I)     = HALF*DT1*(FLU1(I)+FLUX(1,I)+FLUX(2,I)+FLUX(3,I)+FLUX(4,I)+FLUX(5,I)+FLUX(6,I))                   
         ENDDO                                                                                                             
        ENDIF!(JLAG/=0) 
        
        !---interface22---!
        IF(INT22/=0)THEN
          IF(JEUL+JALE/=0)THEN      
           NIN        = 1
           DO I=1,NEL         
             IB       = NINT(TAG22(I))
             IF(IB==0)CYCLE
             MCELL    = BRICK_LIST(NIN,IB)%mainID
             DDVOL    = BRICK_LIST(NIN,IB)%POLY(MCELL)%DDVOL
             DVOL(I)  = DT1 * DDVOL
             IF(JEUL/=0)THEN
               RHON(I) = RHON(I) * VOLN(I) / brick_list(nin,ib)%vnew_scell
             ENDIF
             VOLN(I)  = brick_list(nin,ib)%vnew_scell   
             VOLO(I)  = brick_list(nin,ib)%vold_scell  
             DVOL(I)  = DVOL(I) + VOLN(I)-VOLO(I)           !USE ALE FORMULATION FOR POLYHEDRA EVEN  WITH EULERIAN MATERIAL             
             VOLO(I)  = VOLN(I) 
                           
             brick_list(nin,ib)%vold_scell  = VOLN(I) !for convection during next cycle : aconve() 
C             write(*,FMT='(A,I10,A,F30.16)') "brick id =", NGL(I), "   mass= ", RHON(I)*VOLN(I)
           ENDDO!next I
          ENDIF!(JEUL+JALE/=0)        
        ENDIF!INT22 
        !-----------------!        
                                                                                                                     
      ENDIF!IF(ALE%GLOBAL%INCOMP==1 .AND. JEUL+JALE==1)THEN

      IF(JALE+JEUL/=0)THEN
       COUNT=0
       DO I=1,NEL
         IF(IS_MAT_BCS== 1)CYCLE
         IF(RHON(I)> ZERO)CYCLE
         IF(OFF(I)== ZERO )CYCLE
           COUNT       = COUNT + 1
           LIST(COUNT) = I
       ENDDO
       
       DO II = 1,COUNT
         I = LIST(II)
         CALL ANCMSG(MSGID=167,ANMODE=ANINFO,I1=NGL(I),R1=RHON(I))
         CALL ARRET(2)
       ENDDO
      ENDIF
C      
      IF (ISMDISP>0.OR.ISMSTR==11) THEN
C      change DXX,DYY,DZZ by DIVDE(I)=dt1*(DXX(I)+DYY(I)+DZZ(I)) as input
C---------RHON(I) = RHO0*(VOLO(I)/VOLN(I)) just calculated above
C---------VOLN(I)=VOLO(I) for small strain excepting initial stat case
       DO I=1,NEL
         DVDP   = DIVDE(I)
         RHON(I) = RHON_OLD(I) - RHON(I)*DVDP
         RHON(I) = MAX(RHON(I),EM30)
         DVOL(I)=VOLN(I)*DVDP
       ENDDO
      ELSEIF ((ISMSTR<=4.OR.ISMSTR==12).AND.JLAG>0) THEN
       DO I=1,NEL
        IF(OFFG(I)>ONE) THEN
         DVDP  = DIVDE(I)
         RHOREF = RHON(I)
         RHON(I) = RHON_OLD(I) - RHOREF*DVDP
         RHON(I) = MAX(RHON(I),EM30)
         DVOL(I)=VOLN(I)*DVDP
         IF (ISMSTR==12) AMU(I) =RHON(I)/RHOREF - ONE
        END IF
       ENDDO
      ENDIF
C      
      IF((ALE%GLOBAL%INCOMP/=1 .OR. (JEUL+JALE)/=1).AND.JLAG/=0.AND.N2D==0
     .     .AND.IMPL_S==0.AND.ISMSTR/=1.AND.ISMSTR/=3.AND.ISMSTR/=11)THEN
       IF(IRESP==1)THEN
C------VOLDP doesn't change after switching to small strain,modifying VOL0DP to      
        DO I=1,NEL
         IF(OFFG(I)>ONE) THEN
          DVDP = DIVDE(I)*(VOLO(I)/VOLN(I))
          VOL0DP(I)=VOL0DP(I)-DVDP*VOLDP(I)
         ELSEIF(OFFG(I)==ZERO) THEN
          VOLDP(I)=VOLO(I)
         ELSE
          DVDP = VOLDP(I)-(RHO0/RHON_OLD(I))*VOL0DP(I)
          DVOL(I) = DVDP
          RHON(I) = RHO0*(VOL0DP(I)/VOLDP(I))
         END IF
        ENDDO
        AMU(1:NEL) = VOL0DP(1:NEL)/VOLDP(1:NEL) - ONE
       END IF
      ENDIF
C-----AMU compute for DP in MMAIN: AMU() = RHON()/RHO0 - 1, because RHON*VOLDP=RHO0*VOL0DP     
      
C-----------------------------------------------
      RETURN
      END SUBROUTINE SRHO3
