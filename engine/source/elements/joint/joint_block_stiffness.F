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
Chd|  JOINT_BLOCK_STIFFNESS         source/elements/joint/joint_block_stiffness.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        GET_U_GEO                     source/user_interface/upidmid.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE JOINT_BLOCK_STIFFNESS(ITAB,MS,IN,STIFN,STIFR,
     1                    WEIGHT ,IXR,IPART,X,
     2                    IPARTR,IGEO,GEO,NPBY,IPARG,ELBUF_TAB,DMAS,DINER)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "scr02_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "cong2_c.inc"
#include      "units_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(*),WEIGHT(*),IXR(NIXR,*),
     .   IPART(LIPART1,*),IPARTR(*),IGEO(NPROPGI,*),NPBY(NNPBY,*),
     .   IPARG(NPARG,*)
C     REAL
      my_real STIFN(*), STIFR(*),MS(*) ,IN(*),X(3,*),
     .   DMAS,DINER,GEO(NPROPG,*)      
C
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,M1,M2,IG,IGTYP,N1,N2,KAD,ITYP,NG,JFT,JLT,NEL,
     .        NB8,FLAG,NFT,NUVAR,JNTYP,IRB1,IRB2,FLAG_S,FLAG_PR,NV
      my_real MASS,INER1,INER2,KM1,KRM1,KM2,KRM2,XX,KX1,KX2,KR1,DTC,ALPHA,
     .        XL,KXMAX,KRMAX,KXMIN1,KXMIN2,KX,KR,DTCG,SCF,GET_U_GEO,
     .        XX1,XX2,DTA,MASS1,MASS2     
C
      TYPE(G_BUFEL_),POINTER :: GBUF
C-----------------------------------------------
      EXTERNAL GET_U_GEO             
C----------------------------------------------------------

      FLAG_PR = 0
      
      DTC = DT2      
      DTA = DTMIN1(11)

      IF (TT==0) THEN
C----------------------------------------------------------            
      IF (NODADT>0) THEN
        DTC = MAX(DTC,DTMIN1(11)/DTFAC1(11))
        IF (DTMX>EM20) DTC = MIN(DTC,DTMX)
      ENDIF
      
      IF (DTC>90000) THEN      
        IF (DTA==0) THEN
          print *,"ERROR NO TARGET TIME STEP DT=",DTC
          print *,"STIFFNESS CAN NOT BE COMPUTED"    
            CALL ARRET(2)
        ELSE
          DTC = DTA    
        ENDIF
      ENDIF
     
      ALPHA = 4.1

      DO NG=1,NGROUP
        ITYP = IPARG(5,NG)
          NEL = IPARG(2,NG)
          NFT = IPARG(3,NG) 
          JFT = 1
          JLT = MIN(NVSIZ,NEL)  
        GBUF => ELBUF_TAB(NG)%GBUF
          IF (ITYP /= 6) GOTO 250
C--------> Boucle sur les elements ressort -------
        DO I=JFT,JLT
            J = I + NFT    
          IG = IPART(2,IPARTR(J))
          IGTYP =  IGEO(11,IG)
            NUVAR =  NINT(GEO(25,IG))
          NV = NUVAR*(I-1) + 1
          IF (IGTYP==45) THEN
              SCF  = GET_U_GEO(11,IG)      
              JNTYP = NINT(GET_U_GEO(1,IG))
            FLAG = NINT(GET_U_GEO(10,IG))
            FLAG_S = 0                  
              IF (FLAG==0) THEN
C
            IF (FLAG_PR==0) THEN
              WRITE(IOUT,100)
              WRITE(IOUT,200)
                FLAG_PR = 1      
              ENDIF
C                                                
            N1 = IXR(2,J)
            N2 = IXR(3,J)
C
              IRB1 = NINT(GBUF%VAR(NV + 37))
            IF (IRB1 > 0) THEN
                M1 = NPBY(1,IRB1)
            ELSE
              M1 = N1
            ENDIF
C
              IRB2 = NINT(GBUF%VAR(NV + 38))
            IF (IRB2 > 0) THEN
                M2 = NPBY(1,IRB2)
            ELSE
              M2 = N2
            ENDIF  
C             
              XL = ((X(1,N1)-X(1,N2))**2)+((X(2,N1)-X(2,N2))**2)
     .             +((X(3,N1)-X(3,N2))**2)    
              XX1 = ((X(1,N1)-X(1,M1))**2)+((X(2,N1)-X(2,M1))**2)
     .             +((X(3,N1)-X(3,M1))**2)    
              XX2 = ((X(1,N2)-X(1,M2))**2)+((X(2,N2)-X(2,M2))**2)
     .             +((X(3,N2)-X(3,M2))**2)
              XX = MAX(XX1,XX2)
C           
C--------> Calcul raideur cote noeud 1 -------         
              MASS1 = MS(M1)
              INER1 = IN(M1)
              KM1 = STIFN(M1)
              KRM1 = STIFR(M1)      
C
            KX1 = (2*MASS1/(ALPHA*DTC*DTC)) - KM1
            IF (INER1 > ZERO) THEN
              KX2 = 0.8*(INER1/(ALPHA*DTC*DTC)- KRM1)/(MAX(EM20,(XX+XL)))
              KR = INER1/(ALPHA*DTC*DTC)- KRM1
            ELSE
              KX2 = EP30
              KR = ZERO
            ENDIF          
              KXMAX = MIN(KX1,KX2)              
C                                   
C--------> Calcul raideur cote noeud 2 -------          
              MASS2 = MS(M2)
              INER2 = IN(M2)
              KM2 = STIFN(M2)
              KRM2 = STIFR(M2) 
C
            KX1 = (2*MASS2/(ALPHA*DTC*DTC)) - KM2
            IF (INER2 > ZERO) THEN
              KX2 = 0.8*(INER2/(ALPHA*DTC*DTC)- KRM2)/(MAX(EM20,(XX+XL)))
              KR1 = INER2/(ALPHA*DTC*DTC)- KRM2
            ELSE
              KX2 = EP30
              KR1 = ZERO
            ENDIF
C        
              KXMAX = MIN(KX1,KX2,KXMAX)    
              KR = MIN(KR,KR1)
                       
C--------> Calcul raideur finale et affectation------- 
            KX = MAX(KXMAX,2*KM1,2*KM2)
              IF ((KX - KXMAX)>1e-8) FLAG_S = 1
            IF ((INER1 == ZERO).OR.(INER2 == ZERO)) THEN
C--         Raideur en rotation mise    zero si kjoint attach      un noeud de solid --
              KR = ZERO
                KRMAX = KR
              FLAG_S = 0
            ELSE
                KRMAX = KR
                KR = MAX(KR,2*KRM1,2*KRM2)
            ENDIF
C
            IF ((IRB1 > 0) .AND.(IRB2 > 0)) THEN 
                IF ((KX - KXMAX)>1e-8) WRITE(IOUT,300)
                IF ((KR - KRMAX)>1e-8) WRITE(IOUT,400)
            ELSE
                IF ((KX - KXMAX)>1e-8) WRITE(IOUT,1300)
                IF ((KR - KRMAX)>1e-8) WRITE(IOUT,1400)
            ENDIF            
C
            IF (FLAG_S==1) THEN
                KR = MAX(KR,1.3*KX*(XX+XL))
              ENDIF
C      
              KX = SCF*KX      
              KR = SCF*KR        
C
            WRITE(IOUT,'(4X,I10,5X,I2,8X,1PE11.4,8X,1PE11.4)') 
     .            IXR(NIXR,J),JNTYP,KX,KR                 
C
            GBUF%VAR(NV + 16) = KX
            GBUF%VAR(NV + 17) = KR
              
C--------> Spherical Joint-------
            IF (JNTYP==1) THEN       
              GBUF%VAR(NV + 18) = KX
              GBUF%VAR(NV + 19) = KX
              GBUF%VAR(NV + 20) = KX
C--------> Revolutional Joint-------
            ELSEIF (JNTYP==2) THEN       
              GBUF%VAR(NV + 18) = KX
              GBUF%VAR(NV + 19) = KX
              GBUF%VAR(NV + 20) = KX
              GBUF%VAR(NV + 31) = KR 
              GBUF%VAR(NV + 32) = KR
C--------> Cylindrical Joint-------
            ELSEIF (JNTYP==3) THEN       
              GBUF%VAR(NV + 19) = KX
              GBUF%VAR(NV + 20) = KX
              GBUF%VAR(NV + 31) = KR
              GBUF%VAR(NV + 32) = KR
C--------> Planar Joint-------
            ELSEIF (JNTYP==4) THEN       
              GBUF%VAR(NV + 18) = KX
              GBUF%VAR(NV + 31) = KR
              GBUF%VAR(NV + 32) = KR
C--------> Universal Joint-------
            ELSEIF (JNTYP==5) THEN       
              GBUF%VAR(NV + 18) = KX
              GBUF%VAR(NV + 19) = KX
              GBUF%VAR(NV + 20) = KX
              GBUF%VAR(NV + 30) = KR
C--------> Translational Joint-------
            ELSEIF (JNTYP==6) THEN        
              GBUF%VAR(NV + 19) = KX
              GBUF%VAR(NV + 20) = KX
              GBUF%VAR(NV + 30) = KR
              GBUF%VAR(NV + 31) = KR
              GBUF%VAR(NV + 32) = KR
C--------> Oldham Joint-------
            ELSEIF (JNTYP==7) THEN       
              GBUF%VAR(NV + 18) = KX
              GBUF%VAR(NV + 30) = KR
              GBUF%VAR(NV + 31) = KR
              GBUF%VAR(NV + 32) = KR
C--------> Rigid Joint-------
            ELSEIF (JNTYP==8) THEN
              GBUF%VAR(NV + 18) = KX
              GBUF%VAR(NV + 19) = KX
              GBUF%VAR(NV + 20) = KX
              GBUF%VAR(NV + 30) = KR
              GBUF%VAR(NV + 31) = KR
              GBUF%VAR(NV + 32) = KR
              ENDIF ! IF (JNTYP)
C
            ENDIF ! IF (FLAG==0)
          ENDIF ! IF (IGTYP==45)
C
        ENDDO
250     CONTINUE   
      ENDDO

      WRITE(IOUT,'( )')
C----------------------------------------------------------       
            
      ENDIF      
C
      RETURN
100   FORMAT(/,
     & 1X,'AUTOMATIC STIFFNESS COMPUTATION FOR JOINTS'/)
200   FORMAT(1X,'JOINT ID',11X,'TYPE',6X,'KNN',16X,'KNR')
C
300   FORMAT(1X,'WARNING TRANS. STIFF. IMPOSED BY STIFF. ON RBODY')
400   FORMAT(1X,'WARNING ROT. STIFF. IMPOSED BY STIFF. ON RBODY')
C
1300   FORMAT(1X,'WARNING TRANS. STIFF. IMPOSED BY STIFF. ON CONNECTED STRUCTURES')
1400   FORMAT(1X,'WARNING ROT. STIFF. IMPOSED BY STIFF. ON CONNECTED STRUCTURES')                    
      END    
