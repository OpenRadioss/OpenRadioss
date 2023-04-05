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
Chd|  WRITE_BUF_LAW51               source/materials/mat/mat051/write_buf_law51.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        I22BUFBRIC_MOD                ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE WRITE_BUF_LAW51 (
     1     IXS   , NFT     , NUVAR  , NEL    ,  UVAR   ,
     2     I     , UNEPHASE, DD     , dbVOLD , dbVOLD_f,
     3     VOLUME, VOLD    , EPSPXX , EPSPYY ,   EPSPZZ,
     4     TAG22 , BFRAC   ,RHO01   ,RHO02   ,   RHO03 ,
     5     RHO04)  
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD   
      USE I22BUFBRIC_MOD       
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "mvsiz_p.inc"
#include      "inter22.inc"
#include      "mmale51_c.inc"
C
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: NEL, NUPARAM, NUVAR,IXS(NIXS,*)
      INTEGER :: NFT, UNEPHASE
      my_real :: UVAR(NEL,NUVAR), DD, dbVOLD(4),dbVOLD_f(4),TAG22(MVSIZ),VOLUME(MVSIZ),VOLD, VAR
      my_real :: EPSPXX(MVSIZ), EPSPYY(MVSIZ), EPSPZZ(MVSIZ),BFRAC
      my_real :: RHO01, RHO02, RHO03, RHO04, RHO0(4)

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,II, ITRIMAT, IPRESENT(4), Q, KK
      my_real :: P
C-----------------------------------------------
C   S o u r c e L i n e s
C-----------------------------------------------

      RHO0 = ( / RHO01, RHO02, RHO03, RHO04 / )
                                                              
      II = I+ NFT                                                          
      write (*,FMT='(A16,I10)'   ) "LAW51- brick ID=",IXS(NIXS,I+NFT)
      
      VAR=ZERO
      DO ITRIMAT = 1,TRIMAT                                                
        KK = N0PHAS + (ITRIMAT-1)*NVPHAS        
        VAR = VAR + UVAR(I,11+KK)
      ENDDO                 
    
      IF(INT22>0)THEN
        IF(TAG22(I)>ZERO)THEN
        write (*,FMT='(A)')
     ."                 +---------SuperCell-----------+---------ELEMENT-----------+-------SUM(submat)-----------+-------SUM(submat
     .)+flux-----+"
        write (*,FMT='(A16,4F30.16)') "           Vold=",BRICK_LIST(1,NINT(TAG22(I)))%Vold_SCell,VOLD    ,SUM(dbVOLD(1:4)),
     .                                                                                                    SUM(dbVOLD_f(1:4))
        write (*,FMT='(A16,3F30.16)') "           Vnew=",BRICK_LIST(1,NINT(TAG22(I)))%Vnew_SCell,VOLUME(I) ,VAR
        ELSE
        write (*,FMT='(A)')
     ."                 +-----------ELEMENT-----------+"
        write (*,FMT='(A16,1F30.16)') "           Vold=",VOLD 
        write (*,FMT='(A16,1F30.16)') "           Vnew=",VOLUME(I)             
        ENDIF
      ELSE
        write (*,FMT='(A)')
     ."                 +-----------ELEMENT-----------+"      
        write (*,FMT='(A16,1F30.16)') "           Vold=",VOLD      
        write (*,FMT='(A16,1F30.16)') "           Vnew=",VOLUME(I)
      ENDIF
        write (*,FMT='(A16,3F30.16)') "          EPSii=",EPSPXX(I),EPSPYY(I), EPSPZZ(I)
        write (*,FMT='(A16,1F30.16)') "             DD=", DD        
        write (*,FMT='(A16,1F30.16)') "          BFRAC=", BFRAC        
      
      !submat4
      Q           = UNEPHASE / 8
      IPRESENT(4) = Q
      UNEPHASE    = UNEPHASE - Q*8
      !submat3
      Q           = UNEPHASE / 4
      IPRESENT(3) = Q
      UNEPHASE    = UNEPHASE - Q*4
      !submat2
      Q           = UNEPHASE / 2
      IPRESENT(2) = Q
      UNEPHASE    = UNEPHASE - Q*2
      !submat1
      IPRESENT(1) = UNEPHASE
 
      VAR = ZERO
 
      DO ITRIMAT = 1,TRIMAT                                                
        !=====================!                                            
        ! submat_id = ITRIMAT !                                            
        !=====================!                                            
        KK = N0PHAS + (ITRIMAT-1)*NVPHAS 
        write (*,FMT='(A16,I1)')     "   +-----submat=",ITRIMAT  
        IF(IPRESENT(ITRIMAT)==0)THEN
        write (*,FMT='(A16   )')     "           empty"         
          CYCLE        
        ENDIF
        write (*,FMT='(A16,E30.16)') "   1.     Vfrac=", UVAR(I,1+KK)                
        write (*,FMT='(A16,E30.16)') "   2.     Sd_xx=", UVAR(I,2+KK)                                                       
        write (*,FMT='(A16,E30.16)') "   3.     Sd_yy=", UVAR(I,3+KK)                                                       
        write (*,FMT='(A16,E30.16)') "   4.     Sd_zz=", UVAR(I,4+KK)                                                       
        write (*,FMT='(A16,E30.16)') "   5.     Sd_xy=", UVAR(I,5+KK)                                                       
        write (*,FMT='(A16,E30.16)') "   6.     Sd_yz=", UVAR(I,6+KK)                                                       
        write (*,FMT='(A16,E30.16)') "   7.     Sd_zx=", UVAR(I,7+KK)                                                       
        write (*,FMT='(A16,E30.16)') "   8.         E=", UVAR(I,8+KK)                                                                                                                    
        write (*,FMT='(A16,E30.16)') "   9.       RHO=", UVAR(I,9+KK)                                                       
        write (*,FMT='(A16,E30.16)') "   8*11    EINT=", UVAR(I,8+KK)*UVAR(I,11+KK)
        write (*,FMT='(A16,E30.16)') "   9*11    MASS=", UVAR(I,9+KK)*UVAR(I,11+KK)         
        write (*,FMT='(A16,E30.16)') "   10.        Q=", UVAR(I,10+KK)                                                      
        write (*,FMT='(A16,E30.16)') "   11.     Vnew=", UVAR(I,11+KK)                                                      
        write (*,FMT='(A16,E30.16)') "           Vold=", dbVOLD(ITRIMAT)        
        write (*,FMT='(A16,E30.16)') "   12.      RHO=", UVAR(I,12+KK)                                                      
        write (*,FMT='(A16,E30.16)') "   13.    DDVOL=", UVAR(I,13+KK)                                                      
        write (*,FMT='(A16,E30.16)') "   14.      SSP=", UVAR(I,14+KK)                                                      
        write (*,FMT='(A16,E30.16)') "   15.     PLAS=", UVAR(I,15+KK)                                                      
        write (*,FMT='(A16,E30.16)') "   16.        T=", UVAR(I,16+KK)                                                      
        write (*,FMT='(A16,E30.16)') "   17.   Edif/V=", UVAR(I,17+KK)                                                      
        write (*,FMT='(A16,E30.16)') "   18.        P=", UVAR(I,18+KK)                                                      
        write (*,FMT='(A16,E30.16)') "   19.      EPX=", UVAR(I,19+KK)
        write (*,FMT='(A16,E30.16)') "     .       MU=", UVAR(I,9+KK)/RHO0(ITRIMAT) - ONE        
        VAR = VAR + UVAR(I,11+KK)
      ENDDO                                                                

      RETURN
      END


