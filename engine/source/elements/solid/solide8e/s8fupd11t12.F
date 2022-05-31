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
Chd|  S8FUPD11T12                   source/elements/solid/solide8e/s8fupd11t12.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|-- calls ---------------
Chd|        S8_IS17JAC_I                  source/elements/solid/solide8e/s8ederi3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE S8FUPD11T12(ELBUF_TAB, OFFG   ,OFFG0  ,X  ,XDP  ,
     .     NC1   ,NC2   ,NC3   ,NC4   ,NC5   ,NC6   ,NC7   ,NC8  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
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
#include      "vect01_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL ou REAL*8
      INTEGER 
     .      NC1(*),NC2(*),NC3(*),NC4(*),NC5(*),NC6(*),NC7(*),NC8(*)
      my_real
     .   OFFG(*),OFFG0(*),X(3,*)
      DOUBLE PRECISION 
     .   XDP(3,*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,NCH,INDEX(MVSIZ),J    
      DOUBLE PRECISION 
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .   XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ),
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),
     .   ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ)
C=======================================================================
C-----Always in global system and fixed then (not like Ismstr=11
        NCH=0
        DO I=LFT,LLT
         IF(OFFG(I)/=OFFG0(I).AND.ABS(OFFG(I)) > ONE ) THEN
           NCH = NCH +1
           INDEX(NCH) = I
         END IF
	END DO
	IF (NCH==0) RETURN
C	
      IF(IRESP==1)THEN 
#include "vectorize.inc"
        DO J=1,NCH
          I = INDEX(J)
          XD1(J)=XDP(1,NC1(I))  
          YD1(J)=XDP(2,NC1(I))  
          ZD1(J)=XDP(3,NC1(I))  
          XD2(J)=XDP(1,NC2(I))  
          YD2(J)=XDP(2,NC2(I))  
          ZD2(J)=XDP(3,NC2(I))  
          XD3(J)=XDP(1,NC3(I))  
          YD3(J)=XDP(2,NC3(I))  
          ZD3(J)=XDP(3,NC3(I))  
          XD4(J)=XDP(1,NC4(I))  
          YD4(J)=XDP(2,NC4(I))  
          ZD4(J)=XDP(3,NC4(I))  
          XD5(J)=XDP(1,NC5(I))  
          YD5(J)=XDP(2,NC5(I))  
          ZD5(J)=XDP(3,NC5(I))  
          XD6(J)=XDP(1,NC6(I))  
          YD6(J)=XDP(2,NC6(I))  
          ZD6(J)=XDP(3,NC6(I))  
          XD7(J)=XDP(1,NC7(I))  
          YD7(J)=XDP(2,NC7(I))  
          ZD7(J)=XDP(3,NC7(I))  
          XD8(J)=XDP(1,NC8(I))  
          YD8(J)=XDP(2,NC8(I))  
          ZD8(J)=XDP(3,NC8(I))  	  	  	  
        ENDDO
      ELSE	
#include "vectorize.inc"
        DO J=1,NCH
          I = INDEX(J)
          XD1(J)=X(1,NC1(I))  
          YD1(J)=X(2,NC1(I))  
          ZD1(J)=X(3,NC1(I))  
          XD2(J)=X(1,NC2(I))  
          YD2(J)=X(2,NC2(I))  
          ZD2(J)=X(3,NC2(I))  
          XD3(J)=X(1,NC3(I))  
          YD3(J)=X(2,NC3(I))  
          ZD3(J)=X(3,NC3(I))  
          XD4(J)=X(1,NC4(I))  
          YD4(J)=X(2,NC4(I))  
          ZD4(J)=X(3,NC4(I))  
          XD5(J)=X(1,NC5(I))  
          YD5(J)=X(2,NC5(I))  
          ZD5(J)=X(3,NC5(I))  
          XD6(J)=X(1,NC6(I))  
          YD6(J)=X(2,NC6(I))  
          ZD6(J)=X(3,NC6(I))  
          XD7(J)=X(1,NC7(I))  
          YD7(J)=X(2,NC7(I))  
          ZD7(J)=X(3,NC7(I))  
          XD8(J)=X(1,NC8(I))  
          YD8(J)=X(2,NC8(I))  
          ZD8(J)=X(3,NC8(I))  
        ENDDO      
      ENDIF
      CALL S8_IS17JAC_I(ELBUF_TAB,
     .              XD1   ,XD2   ,XD3   ,XD4   ,XD5   ,XD6   ,XD7   ,XD8   ,
     .              YD1   ,YD2   ,YD3   ,YD4   ,YD5   ,YD6   ,YD7   ,YD8   ,
     .              ZD1   ,ZD2   ,ZD3   ,ZD4   ,ZD5   ,ZD6   ,ZD7   ,ZD8   ,
     .              INDEX ,NCH   ,LLT)
C-----------
      RETURN
      END

