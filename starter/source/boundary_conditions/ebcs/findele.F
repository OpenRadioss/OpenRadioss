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
Chd|  FINDELE                       source/boundary_conditions/ebcs/findele.F
Chd|-- called by -----------
Chd|        INIEBCS                       source/boundary_conditions/ebcs/iniebcs.F
Chd|-- calls ---------------
Chd|        NORMA1                        source/interfaces/inter3d1/norma1.F
Chd|        IFACE                         source/ale/ale3d/iface.F      
Chd|        IFACE2                        source/ale/ale3d/iface.F      
Chd|        IFACE2T                       source/ale/ale3d/iface.F      
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FINDELE(ALE_CONNECTIVITY, NNODE, NIX, NVOIS, IDSU,ID,NSEG,IX,
     .                   ISEG,IELE,ITYPE,IFAC,SURF_NODES,IADD,INVC,PM,X,TYPE,IGEO)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
      INTEGER NNODE, NIX, NVOIS, IDSU,ID,NSEG,IX(NIX,*),ISEG(*),IELE(*),ITYPE(*),
     .        IADD(*),INVC(*),IFAC(*),TYPE,SURF_NODES(NSEG,4)
      INTEGER,INTENT(IN) :: IGEO(NPROPGI,NUMGEO)
      my_real PM(NPROPM,NUMMAT),X(3,SX/3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II,JJ,J,K,L,M,PP,NN,KK,NEL,IRECT(4),IAD,N,ALE,NF,IP(NNODE),TURBU,NEIGH,CON(8),IS
      INTEGER IFACE, IFACE2, IFACE2T, JALE_FROM_MAT, JALE_FROM_PROP,MINUS
      my_real :: N1, N2, N3, DDS,AREA
      my_real :: XX1(4), XX2(4),XX3(4),XS1,YS1,ZS1,XC,YC,ZC
      EXTERNAL  IFACE, IFACE2, IFACE2T
      DATA CON/1,2,3,4,5,6,7,8/
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      TURBU=0
      NEIGH=0
      IF(IPRI>=1)WRITE(IOUT,1000)ID,IDSU

      DO  J=1,NSEG
        DO K=1,4
          IRECT(K)=SURF_NODES(J,K)
        ENDDO
        IF (IRECT(3) == 0) IRECT(3) = IRECT(2)
        IF(IRECT(4)==0) IRECT(4)=IRECT(3)

        NEL=0
        DO 230 IAD=IADD(IRECT(1)),IADD(IRECT(1)+1)-1
          DO K=1,NNODE
            IP(K)=0
          ENDDO
          N = INVC(IAD)
          DO 220 JJ=1,4
            II=IRECT(JJ)
            DO K=1,NNODE
              IF(IX(K+1,N)==II)THEN
                IP(K)=1
                GOTO 220
              ENDIF
            ENDDO
            GOTO 230
  220     CONTINUE

          IF (N2D == 0) THEN
             ! 3D case (8 nodes)
             NF=IFACE(IP,CON)
             IF (IP(1) * IP(3) * IP(6) /= 0) THEN
                NF = 5
             ELSEIF (IP(1) * IP(3) * IP(5) /= 0) THEN
                NF = 6
             ELSEIF (IP(3) * IP(6) * IP(5) /= 0) THEN
                NF = 2
             ELSEIF (IP(6) * IP(5) * IP(1) /= 0) THEN
                NF = 4
             ENDIF
          ELSEIF (NNODE == 4) THEN
             ! 2D case (4 nodes : QUADS)
             NF = IFACE2(IP, CON)
          ELSEIF (NNODE == 3) THEN
             ! 2D case (3 nodes : TRIANGLES)
             NF = IFACE2T(IP, CON)
          ENDIF
          NEL = N

  230   CONTINUE

        IF (NEL==0) THEN
          IERR=IERR+1
          NEIGH=NEIGH+1
          WRITE(IOUT,*)' ** ERROR EBCS ',ID,' CANNOT FIND NEIGHBORING BRICK FOR SEGMENT',J,' OF SURFACE',IDSU
          GOTO 500
        ENDIF

        XS1=ZERO
        YS1=ZERO
        ZS1=ZERO
        DO JJ=1,4
          NN=IRECT(JJ)
          XX1(JJ)=X(1,NN)
          XX2(JJ)=X(2,NN)
          XX3(JJ)=X(3,NN)
          XS1=XS1+FOURTH*X(1,NN)
          YS1=YS1+FOURTH*X(2,NN)
          ZS1=ZS1+FOURTH*X(3,NN)
        ENDDO

        IF (N2D == 0) THEN               
           CALL NORMA1(N1,N2,N3,AREA,XX1,XX2,XX3)      
        ELSE                               
           N1 = ZERO                       
           N2 = XX3(2) - XX3(1)            
           N3 = -(XX2(2) - XX2(1))         
           AREA = SQRT(N2 * N2 + N3 * N3)  
           N2 = N2 / AREA                  
           N3 = N3 / AREA                  
        ENDIF                              

        XC=ZERO
        YC=ZERO
        ZC=ZERO
        DO  K=1,NNODE
          KK=IX(K+1,NEL)
          XC=XC+X(1,KK)
          YC=YC+X(2,KK)
          ZC=ZC+X(3,KK) 
        ENDDO
        XC=XC/NNODE
        YC=YC/NNODE
        ZC=ZC/NNODE

        DDS=N1*(XC-XS1)+N2*(YC-YS1)+N3*(ZC-ZS1)
        IF(DDS>0)THEN
          IS=-1
        ELSE
          IS=1
        ENDIF
 
        IELE(J)=NEL
        ITYPE(J)=NNODE
        IF (TYPE == 8 .OR. TYPE == 9 .OR. TYPE == 10) THEN
           IFAC(J) = NF
        ENDIF
        M=IX(1,NEL)
        PP=IX(NIX-1,NEL)
        JALE_FROM_MAT = INT(PM(72,M))
        JALE_FROM_PROP = IGEO(62,PP)
        ALE = JALE_FROM_MAT + JALE_FROM_PROP 
        IF(ALE/=0)THEN
          SEGINDX = SEGINDX+1
          ISEG(J) = IS*SEGINDX
          IAD = ALE_CONNECTIVITY%ee_connect%iad_connect(NEL)
          MINUS = -1
!          IF(TYPE==10) MINUS = 1
          ALE_CONNECTIVITY%ee_connect%connected(iad + NF - 1) = -SEGINDX   !NEGATIVE VALUE => STORAGE OF SEGMENT ID
          IF(IPRI>=1)WRITE(IOUT,FMT=FMW_10I)J,IX(NIX,NEL),NF,ISEG(J)
        ELSE
          IF(IPRI>=1)WRITE(IOUT,FMT=FMW_10I)J,IX(NIX,NEL),0,0
        ENDIF
        TURBU=MAX(TURBU,INT(PM(70,M)))
 500    CONTINUE
      ENDDO
      
      IF(TURBU/=0)THEN
        IERR=IERR+1
        WRITE(ISTDO,*)' ** ERROR EBCS ',ID,' TURBULENCE NOT YET SUPPORTED'
        WRITE(IOUT,*)' ** ERROR EBCS ',ID,' TURBULENCE NOT YET SUPPORTED'
      ENDIF
      IF(NEIGH/=0)THEN
        WRITE(ISTDO,*)' ** ERROR EBCS ',ID,NEIGH,' SEGMENTS NOT FACING A BRICK '
      ENDIF

      RETURN
C-----------------------------------------------
 1000 FORMAT(//,'ELEMENTARY BCS',I10,' SURFACE ',I10,/,
     .          '-----------------------------------------',/, 
     .          ' SEGMENT     ELT      FACE      SEGINDX  ')
      END
