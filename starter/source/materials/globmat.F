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
Chd|  GLOBMAT                       source/materials/globmat.F    
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE GLOBMAT( IGEO       ,GEO      ,PM, PM_STACK,GEO_STACK,
     .                    IGEO_STACK)
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SUBMODEL_MOD
!!      USE STACK_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "sphcom.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IGEO(NPROPGI,*),IGEO_STACK(3* NPT_STACK+2,*)
      my_real
     .       GEO(NPROPG,*),PM(NPROPM,*),GEO_STACK(6*NPT_STACK+1,*),
     .       PM_STACK(20,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IGMAT,IPOS,IGTYP,IPMAT ,IPTHK ,IPPOS ,IPGMAT,NPT,N,
     .        I1,I2,I3,MATLY,ICRYPT,NLAY,ILAY,IPANG,IPPID,IS,PIDS
      my_real 
     .   A11,A11R,C1,IZ,G,NU,A12,E,RHOG,B1T2, THICKT,SSP, THKLY,POSLY,
     .   RHO,C1THK,A12THK,A1THK ,GTHK,NUTHK ,ETHK,RHOG0,RHOCPG,RHO0,RHOCP
C      
!!      TYPE (STACK_PLY) :: STACK          
C-----------------------------------------------
C-----------------------------------------------    
C=======================================================================
C    For Shell
C----------------------------------------------- 
C
C stockage de Geo
C 
       ICRYPT = 0
       DO I=1,NUMGEO
         IGTYP=IGEO(11,I)
         IGMAT = IGEO(98,I)
         IPOS = IGEO(99,I)
         NPT = INT(GEO(6,I))
         IF(IGTYP == 11 .AND. IGMAT > 0) THEN
           A11   = ZERO
           A11R  = ZERO
           C1    = ZERO
           IZ    = ZERO
           G     = ZERO
           NU    = ZERO
           A12   = ZERO
           E     = ZERO
           RHOG  = ZERO
           B1T2  = ZERO
           RHOG0  = ZERO
           RHOCPG  = ZERO
C           
           IPMAT  = 100
           IPTHK  = 300
           IPPOS  = 400
           IPGMAT = 700
           
           NPT = INT(GEO(6,I))
           THICKT = ZERO
           DO N=1,NPT
             I1=IPTHK+N
             I3=IPPOS+N
             THICKT= GEO(200,I)
             THKLY = GEO(I1,I)*THICKT
             POSLY = GEO(I3,I)*THICKT
             I2=IPMAT+N
             MATLY = IGEO(I2,I)
             ETHK  = PM(20,MATLY)*THKLY
             NUTHK = PM(21,MATLY)*THKLY
             GTHK  =  PM(22,MATLY)*THKLY
             A1THK = PM(24,MATLY)*THKLY
             A12THK = PM(25,MATLY)*THKLY
             C1THK = PM(32,MATLY)*THKLY
             RHOG = RHOG + PM(1,MATLY)*THKLY 
             RHOG0 = RHOG0 + PM(89,MATLY)*THKLY 
             RHOCPG = RHOCPG + PM(69,MATLY)*THKLY 
             A11   =  A11 + A1THK
             B1T2 = B1T2 + A1THK*POSLY
             A11R   = A11R + A1THK*(THKLY*THKLY*ONE_OVER_12 + POSLY*POSLY)
             IZ = IZ + THKLY*(THKLY*THKLY*ONE_OVER_12 + POSLY*POSLY) 
             C1 = C1 + C1THK
             G = G + GTHK
             NU = NU + NUTHK
             A12 = A12 + A12THK
             E = E + ETHK
          ENDDO
          RHO = RHOG/MAX(EM20,THICKT)
          RHO0 = RHOG0/MAX(EM20,THICKT)
          RHOCP = RHOCPG/MAX(EM20,THICKT)
          E = E/MAX(EM20,THICKT)
          A11 = A11/MAX(EM20,THICKT) 
          IF(IPOS > 0)A11 = TWO*A11
          A12 = A12/MAX(EM20,THICKT)
          IZ = ONE_OVER_12*THICKT**3
          A11R =A11R/MAX(EM20, IZ)
          C1 = C1 /MAX(EM20,THICKT)
          G = G /MAX(EM20,THICKT)
          NU = NU /MAX(EM20,THICKT)
          SSP = A11/MAX(EM20,RHO)   
          SSP = SQRT(SSP) 
          GEO(IPGMAT +1 ,I) = RHO
          GEO(IPGMAT +2 ,I) = E
          GEO(IPGMAT +3 ,I) = NU
          GEO(IPGMAT +4 ,I) = G
          GEO(IPGMAT +5 ,I) = A11
          GEO(IPGMAT +6 ,I) = A12
          GEO(IPGMAT +7 ,I) = A11R
          GEO(IPGMAT +8 ,I) = C1  
          GEO(IPGMAT +9 ,I) = SSP  
C used for QEPH          
          GEO(IPGMAT +10,I) = SQRT(G)
          GEO(IPGMAT +11,I) = SQRT(A11)
          GEO(IPGMAT +12,I) = SQRT(A12)
          GEO(IPGMAT +13,I) = SQRT(NU)
          GEO(IPGMAT +14,I) = RHO0
          GEO(IPGMAT +15,I) = RHOCP
C          
          IF(ICRYPT.NE.0)THEN
             WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
          ELSE
             WRITE(IOUT,100)IGEO(1,I),RHO,E,NU,G
          ENDIF 
        ELSEIF(IGTYP == 52 .OR. 
     .         ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0)) THEN
           DO IS = 1,NS_STACK 
C initialisation of parameters
            PIDS = IGEO_STACK(2,IS)
            IF(PIDS == I) THEN
             A11   = ZERO
             A11R  = ZERO
             C1    = ZERO
             IZ    = ZERO
             G     = ZERO
             NU    = ZERO
             A12   = ZERO
             E     = ZERO
             RHOG  = ZERO
             B1T2  = ZERO
             RHOG0  = ZERO
             RHOCPG  = ZERO
             IPANG = 1
             IPPID = 2
C                       
             NLAY   =  IGEO_STACK(1,IS) 
             
             IPMAT  =  IPPID + NLAY ! layer material address  ( NLAY = NPT )
             IPTHK  =  IPANG + NLAY ! layer thickness address ( NLAY = NPT )
             IPPOS  =  IPTHK + NLAY ! layer position address  ( NLAY = NPT )
             THICKT = ZERO
             DO ILAY=1,NLAY
               THICKT =  GEO_STACK(1,IS)
               THKLY  = GEO_STACK(IPTHK  + ILAY,IS)*THICKT
               POSLY  = GEO_STACK(IPPOS  + ILAY,IS)*THICKT
               MATLY  = IGEO_STACK(IPMAT + ILAY,IS)
               ETHK   = PM(20,MATLY)*THKLY
               NUTHK  = PM(21,MATLY)*THKLY
               GTHK   = PM(22,MATLY)*THKLY
               A1THK  = PM(24,MATLY)*THKLY
               A12THK = PM(25,MATLY)*THKLY
               C1THK  = PM(32,MATLY)*THKLY
               RHOG = RHOG + PM(1,MATLY)*THKLY 
               RHOG0 = RHOG0 + PM(89,MATLY)*THKLY 
               RHOCPG = RHOCPG + PM(69,MATLY)*THKLY 
               A11   =  A11 + A1THK
               B1T2 = B1T2 + A1THK*POSLY
               A11R   = A11R + A1THK*(THKLY*THKLY*ONE_OVER_12 + POSLY*POSLY)
               IZ = IZ + THKLY*(THKLY*THKLY*ONE_OVER_12 + POSLY*POSLY) 
               C1 = C1 + C1THK
               G = G + GTHK
               NU = NU + NUTHK
               A12 = A12 + A12THK
               E = E + ETHK 
             ENDDO ! NLAY
              RHO = RHOG/MAX(EM20,THICKT)
              RHO0 = RHOG0/MAX(EM20,THICKT)
              RHOCP = RHOCPG/MAX(EM20,THICKT)
              E = E/MAX(EM20,THICKT)
              A11 = A11/MAX(EM20,THICKT) 
              IF(IPOS > 0)A11 = TWO*A11
              A12 = A12/MAX(EM20,THICKT)
              IZ = ONE_OVER_12*THICKT**3
              A11R =A11R/MAX(EM20, IZ)
              C1 = C1 /MAX(EM20,THICKT)
              G = G /MAX(EM20,THICKT)
              NU = NU /MAX(EM20,THICKT)
              SSP = A11/MAX(EM20,RHO)   
              SSP = SQRT(SSP) 
              PM_STACK (1 ,IS) = RHO
              PM_STACK (2 ,IS) = E
              PM_STACK (3 ,IS) = NU
              PM_STACK (4 ,IS) = G
              PM_STACK(5 ,IS) = A11
              PM_STACK (6 ,IS) = A12
              PM_STACK (7 ,IS) = A11R
              PM_STACK(8 ,IS) = C1  
              PM_STACK (9 ,IS) = SSP
C used fo    r QEPH     
              PM_STACK(10,IS) = SQRT(G)
              PM_STACK(11,IS) = SQRT(A11)
              PM_STACK(12,IS) = SQRT(A12)
              PM_STACK(13,IS) = SQRT(NU)
              PM_STACK(14,IS)  = RHO0
              PM_STACK(15,IS)  = RHOCP
              IF(ICRYPT.NE.0)THEN
                 WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
              ELSE
                 WRITE(IOUT,100)IGEO(1,I),RHO,E,NU,G
              ENDIF
            ENDIF  
           ENDDO ! NS_STACK   
         ENDIF 
        ENDDO  !  NUMGEO
C-------- 
 100  FORMAT(//,
     &  5X,'CHARACTERISTICS OF GLOBAL MATERIAL FOR COMPOSITE LAYERED', 
     &     ' SHELL PROPERTY SET ',/
     & ,5X,'     HAVE BEEN RECOMPUTED IN ORDER TO ENSURE STABILITY',/
     & ,5X,'PROPERTY SET NUMBER . . . . . . . . . . . .=',I10/   
     & ,5X,'INITIAL DENSITY. . . . . . . . . . . . . . =',1PG20.13/
     & ,5X,'YOUNG MODULUS . . . . . . . . . . . . . . .=',1PG20.13/
     & ,5X,'POISSON RATIO . . . . . . . . . . . . . . .=',1PG20.13/
     & ,5X,'SHEAR MODULUS . . . . . . . . . . . . . . .=',1PG20.13//)
      RETURN
      END
