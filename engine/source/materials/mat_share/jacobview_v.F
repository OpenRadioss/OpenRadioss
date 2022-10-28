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
        SUBROUTINE  JACOBIEW_V(DIM1,DIM2,A,EW,EV,NROT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
        INTEGER, INTENT(in) :: DIM1
        INTEGER, INTENT(in) :: DIM2
        INTEGER, DIMENSION(DIM1) :: NROT
        my_real, DIMENSION(DIM1,DIM2), intent(inout) :: EW 
        my_real, DIMENSION(DIM1,DIM2,DIM2), intent(inout) :: A,EV

        my_real, DIMENSION(:,:), ALLOCATABLE :: B,Z
        INTEGER IZ,IS,ITER,J,IJK,I
        INTEGER :: NINDX
        INTEGER, DIMENSION(DIM1) :: INDX
        my_real, DIMENSION(DIM1) :: SUMRS,EPS
        my_real :: G,H,T,C,S,TAU,THETA      
C----------------------------------------------------------------

        ! ---------------------
        ALLOCATE( B(DIM1,DIM2) )
        ALLOCATE( Z(DIM1,DIM2) )
        DO IZ=1,DIM2
            DO IS=1,DIM2
                IF(IZ>IS) THEN 
                    DO I=1,DIM1
                        A(I,IS,IZ) = A(I,IZ,IS) 
                    ENDDO
                ENDIF
                EV(1:DIM1,IZ,IS)=ZERO
            ENDDO
            B(1:DIM1,IZ)=A(1:DIM1,IZ,IZ)
            EW(1:DIM1,IZ)=B(1:DIM1,IZ)
            Z(1:DIM1,IZ)=0.
            EV(1:DIM1,IZ,IZ)=ONE
        ENDDO
        NROT(1:DIM1)=0
        ! ---------------------
 

        ! ---------------------
        DO ITER = 1,50
            SUMRS(1:DIM1) = ZERO
            ! ---------------------  
            ! diagonal sum          
            DO IZ=1,DIM2-1
                DO IS=IZ+1,DIM2
                    SUMRS(1:DIM1)=SUMRS(1:DIM1)+ABS(A(1:DIM1,IZ,IS))
                ENDDO
            ENDDO
            ! ---------------------
            ! check if the computation is mandatory : sumrs(i) = 0 --> skip the element
            NINDX = 0
            DO I=1,DIM1
                IF (SUMRS(I)/=ZERO) THEN
                    NINDX = NINDX + 1
                    INDX(NINDX) = I
                ENDIF
            ENDDO
            ! ---------------------
            IF (ITER > 4)   THEN
                EPS(1:DIM1) = ZERO
            ELSE
                EPS(1:DIM1) = ONE_FIFTH*SUMRS(1:DIM1)/DIM2**2
            ENDIF
            ! ---------------------
            DO IZ=1,DIM2-1
                DO IS=IZ+1,DIM2
#include "vectorize.inc"                       
                    DO IJK=1,NINDX
                        I = INDX(IJK)
                        G = 100. * ABS(A(I,IZ,IS))
                        IF ( ITER>4 .AND. ABS(EW(I,IZ))+G==ABS(EW(I,IZ))
     &                          .AND. ABS(EW(I,IS))+G==ABS(EW(I,IS))) THEN
                            A(I,IZ,IS)=ZERO
                        ELSEIF (ABS(A(I,IZ,IS)) > EPS(I)) THEN
                            H = EW(I,IS)-EW(I,IZ)
                            IF (ABS(H)+G==ABS(H)) THEN
                                T = A(I,IZ,IS)/H
                            ELSE
                                THETA = HALF*H/A(I,IZ,IS)
                                T=ONE/(ABS(THETA)+SQRT(ONE+THETA**2))
                                IF (THETA < ZERO) T=-T
                            ENDIF
                            C=ONE/SQRT(ONE+T**2)
                            S=T*C
                            TAU=S/(ONE+C)
                            H=T*A(I,IZ,IS)
                            Z(I,IZ)=Z(I,IZ)-H
                            Z(I,IS)=Z(I,IS)+H
                            EW(I,IZ)=EW(I,IZ)-H
                            EW(I,IS)=EW(I,IS)+H
                            A(I,IZ,IS)=ZERO
                            DO J=1,IZ-1
                                G=A(I,J,IZ)
                                H=A(I,J,IS)
                                A(I,J,IZ)=G-S*(H+G*TAU)
                                A(I,J,IS)=H+S*(G-H*TAU)
                            ENDDO
                            DO J=IZ+1,IS-1
                                G=A(I,IZ,J)
                                H=A(I,J,IS)
                                A(I,IZ,J)=G-S*(H+G*TAU)
                                A(I,J,IS)=H+S*(G-H*TAU)
                            ENDDO
                            DO J=IS+1,DIM2
                                G=A(I,IZ,J)
                                H=A(I,IS,J)
                                A(I,IZ,J)=G-S*(H+G*TAU)
                                A(I,IS,J)=H+S*(G-H*TAU)
                            ENDDO
                            DO J=1,DIM2
                                G=EV(I,J,IZ)
                                H=EV(I,J,IS)
                                EV(I,J,IZ)=G-S*(H+G*TAU)
                                EV(I,J,IS)=H+S*(G-H*TAU)
                            ENDDO
                            NROT(I)=NROT(I)+1                        
                        ENDIF
                    ENDDO
                ENDDO
            ENDDO
            ! ---------------------
            ! update b/ew/z
            DO IZ=1,DIM2
#include "vectorize.inc"                    
                DO IJK=1,NINDX
                    I = INDX(IJK)
                    B(I,IZ)=B(I,IZ)+Z(I,IZ)
                    EW(I,IZ)=B(I,IZ)
                    Z(I,IZ)=ZERO
                ENDDO
            ENDDO
            ! ---------------------
        ENDDO

        DEALLOCATE( B )
        DEALLOCATE( Z )

        RETURN
        END SUBROUTINE JACOBIEW_V  