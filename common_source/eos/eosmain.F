Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  EOSMAIN                       eos/eosmain.F                 
Chd|-- called by -----------
Chd|        INIGRAV_EOS                   starter/source/initial_conditions/inigrav/inigrav_eos.F
Chd|        SOLVE_EINT                    starter/source/initial_conditions/inimap/ini_inimap1d.F
Chd|        M12LAW                        engine/source/materials/mat/mat012/m12law.F
Chd|        MMAIN                         engine/source/materials/mat_share/mmain.F
Chd|        MULTI_SUBMATLAW               engine/source/multifluid/multi_submatlaw.F
Chd|        SIGEPS75                      engine/source/materials/mat/mat075/sigeps75.F
Chd|-- calls ---------------
Chd|        COMPACTION                    eos/compaction.F              
Chd|        EOSLINEAR                     eos/eoslinear.F               
Chd|        EOSPOLYNO                     eos/eospolyno.F               
Chd|        GRUNEISEN                     eos/gruneisen.F               
Chd|        IDEALGAS                      eos/idealgas.F                
Chd|        IDEALGAS_VT                   eos/idealgas_vt.F             
Chd|        JWL                           eos/jwl.F                     
Chd|        LSZK                          eos/lszk.F                    
Chd|        MURNAGHAN                     eos/murnaghan.F               
Chd|        NASG                          eos/nasg.F                    
Chd|        NOBLE_ABEL                    eos/noble_abel.F              
Chd|        OSBORNE                       eos/osborne.F                 
Chd|        POWDER_BURN                   eos/powder_burn.F             
Chd|        PUFF                          eos/puff.F                    
Chd|        SESAME                        eos/sesame.F                  
Chd|        STIFFGAS                      eos/stiffgas.F                
Chd|        TABULATED                     eos/tabulated.F               
Chd|        TILLOTSON                     eos/tillotson.F               
Chd|====================================================================
      SUBROUTINE EOSMAIN(IFLAG ,NEL   ,EOSTYP ,PM    ,OFF   ,EINT  ,
     2                   RHO   ,RHO0  ,MU     ,MU2   ,ESPE  ,
     3                   DVOL  ,DF    ,VNEW   ,MAT   ,PSH   ,
     4                   PNEW  ,DPDM  ,DPDE   ,THETA ,ECOLD ,
     5                   BUFMAT,SIG   ,MU_OLD ,MLW   ,POLD  ,
     6                   NPF   ,TF)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutines call the expected EOS solver
C depending on EOSTYP(INTEGER) value.
      !------------------------------------!                
      !  IEOS     !    EOS                 !             
      !-----------!------------------------!  
      !   0       !     LINEAR             !                 
      !   1       !     POLYNOMIAL         !             
      !   2       !     GRUNEISEN          !             
      !   3       !     TILLOTSON          !             
      !   4       !     PUFF               !             
      !   5       !     SESAME             !             
      !   6       !     NOBLE-ABEL         ! 2017.0            
      !   7       !     IDEAL GAS          ! 2018.0            
      !   8       !     MUNAGHAN           ! 2018.0      
      !   9       !     OSBORNE            ! 2018.0      
      !  10       !     STIFFENED GAS      ! 2018.0      
      !  11       !     LSZK               ! 2018.0  
      !  12       !     POWDER-BURN        ! 2019.1
      !  13       !     COMPACTION         ! 2019.1 
      !  14       !     NASG               ! 2020.0   
      !  15       !     JWL                ! internal use : INIMAP   
      !  16       !     IDEALGAS_VT        ! 2022.0
      !  17       !     TABULATED          ! 2022.2
      !------------------------------------!  
!
!  input   EINT(:) :  rho.e   (law151    , unit J/m**3)
!                     rho.e.V (other laws, unit J)
!
! output   EPSE(:) :  rho0.e (=E)   (all laws, used such as P_eos:=P(mu,E) )
!     
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc" 
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "scr06_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAT(NEL),IFLAG, EOSTYP, NEL, MLW
      my_real
     .   PM(NPROPM,NUMMAT),BUFMAT(NEL)   ,OFF(NEL)  , 
     .   EINT(NEL) ,RHO(NEL)  ,RHO0(NEL) , 
     .   MU(NEL)   ,MU2(NEL)  ,ESPE(NEL) ,DVOL(NEL), 
     .   DF(NEL)   ,VNEW(NEL) ,PSH(NEL)  ,PNEW(NEL),
     .   DPDM(NEL) ,DPDE(NEL) ,THETA(NEL),ECOLD(NEL),
     .   SIG(NEL,6), MU_OLD(NEL),POLD(NEL)
      INTEGER,INTENT(IN)::NPF(SNPC)
      my_real,INTENT(IN)::TF(STF)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX

      IF(IFLAG == 0) THEN
       DO I=1,NEL
        MU2(I)=MAX(ZERO,MU(I))**2
        ESPE(I)=DF(I)*EINT(I)/ MAX(EM15,VNEW(I))         !ESPE=rho0.e   since EINT=rho.e.V  and DF=rho0/rho   =>  DF*EINT/V = E
       ENDDO
       IF(EOSTYP==13 .OR. EOSTYP==16)THEN
         DO I = 1, NEL
           POLD(I)=-THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
         ENDDO
       ENDIF                
      ELSEIF (IFLAG == 2) THEN
      !LAW151 ONLY
         DO I = 1, NEL
            IF (VNEW(I) > ZERO) THEN
               MU2(I)=MAX(ZERO,MU(I))**2
            ENDIF
            ESPE(I) =  DF(I) * EINT(I)                   !law151 : EINT=rho.e
         ENDDO
         IF(EOSTYP==13 .OR. EOSTYP==16)THEN
           DO I = 1, NEL
             POLD(I)=-THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
           ENDDO
         ENDIF         
      ENDIF    
      
      ! inutile de conserver cela, si PNEW a une expression cumulative avec IEOSTYP=1 mais dans tous les autres cas il est ecrase.
      ! a quoi bon mettre a 0.0 quelque chose qui va etre ecrase dans le bloc SELECT CASE ce dessous.
      !IF (IFLAG==1)THEN
      !  IF (MLW /= 6 .AND. MLW /= 17) THEN
      !      IF(EOSTYP/=13)PNEW(1:NEL) = ZERO
      !  ENDIF
      !ENDIF
           
      SELECT CASE (EOSTYP)

        CASE (0)
         !--------------------------------------------------!
         !         LINEAR EOS                               !
         !--------------------------------------------------! 
         CALL EOSLINEAR(
     1                  IFLAG  ,NEL    ,PM     ,OFF    ,EINT  ,MU  ,MU2, 
     2                  DVOL   ,DF     ,VNEW   ,MAT    ,PNEW  ,DPDM,
     3                  DPDE   ,THETA  ,ECOLD  )

       CASE (1)
         !--------------------------------------------------!
         !         POLYNOMIAL EOS                           !
         !--------------------------------------------------! 
         CALL EOSPOLYNO(
     1                  IFLAG  ,NEL    ,PM     ,OFF    ,EINT  ,MU  ,MU2, 
     2                  ESPE   ,DVOL   ,DF     ,VNEW   ,MAT   ,PSH ,
     3                  PNEW   ,DPDM   ,DPDE   ,THETA  ,ECOLD )

       CASE (2)
         !--------------------------------------------------!
         !         GRUNEISEN EOS                            !
         !--------------------------------------------------! 
         CALL GRUNEISEN(
     1                  IFLAG  ,NEL    ,PM     ,OFF    ,EINT  ,MU  ,MU2, 
     2                  ESPE   ,DVOL   ,DF     ,VNEW   ,MAT   ,RHO0,
     3                  PNEW   ,DPDM   ,DPDE   ,THETA  ,ECOLD )

       CASE (3)
         !--------------------------------------------------!
         !         TILLOTSON EOS                            !
         !--------------------------------------------------! 
         CALL TILLOTSON(
     1                  IFLAG  ,NEL    ,PM     ,OFF    ,EINT  ,MU  ,MU2,      
     2                  ESPE   ,DVOL   ,DF     ,VNEW   ,MAT   ,          
     3                  PNEW   ,DPDM   ,DPDE   ,THETA  ,ECOLD )  

       CASE (4) 
         !--------------------------------------------------!
         !         PUFF EOS                                 !
         !--------------------------------------------------! 
         CALL PUFF(
     1                  IFLAG  ,NEL    ,PM     ,OFF    ,EINT  ,MU  ,MU2, 
     2                  ESPE   ,DVOL   ,DF     ,VNEW   ,MAT   ,
     3                  PNEW   ,DPDM   ,DPDE   ,THETA  ,ECOLD )

       CASE (5)
         !--------------------------------------------------!
         !         SESAME EOS                               !
         !--------------------------------------------------! 
         CALL SESAME(
     1                  IFLAG  ,NEL    ,PM     ,OFF    ,EINT  ,RHO ,RHO0 ,
     2                  ESPE   ,DVOL   ,MAT    ,PNEW   ,DPDM  ,DPDE,  
     3                  THETA  ,BUFMAT )

        CASE(6)
         !--------------------------------------------------!
         !         NOBLE-ABEL EOS (COVOLUME EOS)            !
         !--------------------------------------------------!          
          CALL NOBLE_ABEL(
     1                  IFLAG ,NEL  ,PM   ,OFF  ,EINT ,MU  ,MU2, 
     2                  ESPE  ,DVOL ,DF   ,VNEW ,MAT  ,PSH ,
     3                  PNEW  ,DPDM ,DPDE ,THETA,ECOLD)

        CASE(7)
         !--------------------------------------------------!
         !         IDEAL GAS EOS                            !
         !--------------------------------------------------!          
          CALL IDEALGAS(
     1                  IFLAG ,NEL  ,PM   ,OFF  ,EINT ,MU  ,MU2, 
     2                  ESPE  ,DVOL ,DF   ,VNEW ,MAT  ,PSH ,
     3                  PNEW  ,DPDM ,DPDE ,THETA,ECOLD)

        CASE(8)
         !--------------------------------------------------!
         !         OSBORNE EOS                              !
         !--------------------------------------------------!          
          CALL MURNAGHAN(
     1                  IFLAG ,NEL  ,PM   ,OFF  ,EINT ,MU  ,MU2, 
     2                  ESPE  ,DVOL ,DF   ,VNEW ,MAT  ,PSH ,
     3                  PNEW  ,DPDM ,DPDE ,THETA,ECOLD)  
             
        CASE(9)
         !--------------------------------------------------!
         !         OSBORNE EOS                              !
         !--------------------------------------------------!          
          CALL OSBORNE(
     1                  IFLAG ,NEL  ,PM   ,OFF  ,EINT ,MU  ,MU2, 
     2                  ESPE  ,DVOL ,DF   ,VNEW ,MAT  ,PSH ,
     3                  PNEW  ,DPDM ,DPDE ,THETA,ECOLD)     

        CASE(10)
         !--------------------------------------------------!
         !         STIFFENED GAS EOS                        !
         !--------------------------------------------------!          
          CALL STIFFGAS(
     1                  IFLAG ,NEL  ,PM   ,OFF  ,EINT ,MU  ,MU2, 
     2                  ESPE  ,DVOL ,DF   ,VNEW ,MAT  ,PSH ,
     3                  PNEW  ,DPDM ,DPDE ,THETA,ECOLD)     

        CASE(11)
         !--------------------------------------------------!
         !         STIFFENED GAS EOS                        !
         !--------------------------------------------------!          
          CALL LSZK(
     1                  IFLAG ,NEL  ,PM   ,OFF  ,EINT ,MU  ,MU2, 
     2                  ESPE  ,DVOL ,DF   ,VNEW ,MAT  ,PSH ,
     3                  PNEW  ,DPDM ,DPDE ,THETA,ECOLD) 

        CASE(12)
         !--------------------------------------------------!
         !         POWDER BURN EOS                          !
         !--------------------------------------------------!          
          CALL POWDER_BURN(
     1                  IFLAG ,NEL  ,PM   ,OFF  ,EINT ,MU  ,MU2, 
     2                  ESPE  ,DVOL ,DF   ,VNEW ,MAT  ,PSH ,
     3                  PNEW  ,DPDM ,DPDE ,THETA,ECOLD) 

        CASE(13)
         !--------------------------------------------------!
         !         COMPACTION EOS                           !
         !--------------------------------------------------!          
          CALL COMPACTION(
     1                  IFLAG ,NEL  ,PM   ,OFF  ,EINT  ,MU  ,MU2, 
     2                  ESPE  ,DVOL ,DF   ,VNEW ,MAT  ,PSH  ,POLD,
     3                  PNEW  ,DPDM ,DPDE ,THETA,ECOLD,SIG  ,MU_OLD) 

        CASE(14)
         !--------------------------------------------------!
         !         NASG EOS                                 !
         !--------------------------------------------------!          
          CALL NASG(
     1                  IFLAG ,NEL  ,PM   ,OFF  ,EINT ,MU  ,MU2, 
     2                  ESPE  ,DVOL ,DF   ,VNEW ,MAT  ,PSH ,
     3                  PNEW  ,DPDM ,DPDE ,THETA,ECOLD)    
        CASE(15)
         !--------------------------------------------------!
         !         JWL EOS (inernal use, no /EOS/JWL so far)!
         !--------------------------------------------------!          
          CALL JWL(
     1                  IFLAG ,NEL  ,PM   ,OFF  , EINT ,MU  ,MU2, 
     2                  ESPE  ,DVOL ,DF   ,VNEW , MAT  ,PSH ,
     3                  PNEW  ,DPDM ,DPDE ,THETA, ECOLD)  
     
        CASE(16)
         !--------------------------------------------------!
         !         IDEAL GAS EOS                            !
         !--------------------------------------------------!          
          CALL IDEALGAS_VT(
     1                  IFLAG ,NEL  ,PM   ,OFF  ,EINT ,MU  ,MU2, 
     2                  ESPE  ,DVOL ,DF   ,VNEW ,MAT  ,PSH ,
     3                  PNEW  ,DPDM ,DPDE ,THETA,ECOLD,POLD) 
        CASE(17)
         !--------------------------------------------------!
         !         TABULATED EOS                            !
         !--------------------------------------------------!          
          CALL TABULATED(
     1                  IFLAG ,NEL  ,PM   ,OFF  ,EINT ,MU  ,MU2, 
     2                  ESPE  ,DVOL ,DF   ,VNEW ,MAT  ,PSH ,
     3                  PNEW  ,DPDM ,DPDE ,THETA,ECOLD, 
     4                  NPF   ,TF)                     
       END SELECT
      
      
      RETURN
      END
