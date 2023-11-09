Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
Chd|====================================================================
Chd|  TEST_SPLINE                   source/materials/tools/test_splines.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        SPLINE_INTERPOL_2D            source/materials/tools/spline_interpol_2d.F
Chd|====================================================================
       SUBROUTINE TEST_SPLINE(NPT0,NSUB,XF,YF,XX,YY)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
       !compute SPLINE length with third order method (SIMPSON)
       !  INPUT  - LOCAL_PT : 4 control points P0,P1,P2,P3
       !  INPUT  - ALPHA : CCR SPLINE PARAMETER
       !  INPUT  - T : position [0,1] on SPLINE [T1,T2].  1.0 means full spline length
       !  OUTPUT - LEN : length of the curve parametrised with t in [0,T]  T<=1.0
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  ,INTENT(IN)    :: NPT0   ! number of points of input function
      INTEGER  ,INTENT(IN)    :: NSUB   ! 
      my_real  ,DIMENSION(NPT0)   ,INTENT(IN)  :: XF,YF   ! initial curve coordinates
      my_real  ,DIMENSION((NPT0-1)*NSUB+1) ,INTENT(OUT) :: XX,YY   ! curve coordinates build with splines
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,K,NPTS,NSEG
      my_real :: TT,DX,DY,NX,NY
      my_real ,DIMENSION(:,:)  ,ALLOCATABLE :: SPLINE_KNOTS
      my_real ,DIMENSION(:)    ,ALLOCATABLE :: CTRL_PTX,CTRL_PTY
      my_real ,DIMENSION(4)  :: PTX,PTY,KNOTS
      my_real ,PARAMETER  :: ALPHA = 0.5
C-----------------------------------------------
C   S o u r c e   L i n e s
c=======================================================================
      NPTS = NPT0 + 2
      NSEG = NPT0 - 1

      ALLOCATE (CTRL_PTX(NPTS))
      ALLOCATE (CTRL_PTY(NPTS))
      ALLOCATE (SPLINE_KNOTS(NSEG,4))

c     calculate spline control points
      CTRL_PTX(2:NPTS-1) = XF(1:NPT0)
      CTRL_PTY(2:NPTS-1) = YF(1:NPT0)
      ! Add start point - minimum of bending energy            
      CTRL_PTX(1) = (HALF*CTRL_PTX(2) - FOUR*CTRL_PTX(3) + CTRL_PTX(4)) * HALF                  
      CTRL_PTY(1) = (HALF*CTRL_PTY(2) - FOUR*CTRL_PTY(3) + CTRL_PTY(4)) * HALF                  
      ! Add end point - minimum of bending energy               
      CTRL_PTX(NPTS) = (CTRL_PTX(NPTS-3) - FOUR*CTRL_PTX(NPTS-2) + FIVE*CTRL_PTX(NPTS-1)) * HALF                  
      CTRL_PTY(NPTS) = (CTRL_PTY(NPTS-3) - FOUR*CTRL_PTY(NPTS-2) + FIVE*CTRL_PTY(NPTS-1)) * HALF                  
c
      K = 0
      DO I = 1,NSEG                         
        PTX(1) = CTRL_PTX(I)                  
        PTX(2) = CTRL_PTX(I+1)                  
        PTX(3) = CTRL_PTX(I+2)                  
        PTX(4) = CTRL_PTX(I+3)
        PTY(1) = CTRL_PTY(I)                  
        PTY(2) = CTRL_PTY(I+1)                  
        PTY(3) = CTRL_PTY(I+2)                  
        PTY(4) = CTRL_PTY(I+3)
c
        KNOTS(1) = ZERO
        DX = PTX(2) - PTX(1)
        DY = PTY(2) - PTY(1)
        KNOTS(2) = SPLINE_KNOTS(I,1) + EXP(ALPHA*LOG(SQRT(DX**2 + DY**2)))
        DX = PTX(3) - PTX(2)
        DY = PTY(3) - PTY(2)
        KNOTS(3) = KNOTS(2) + EXP(ALPHA*LOG(SQRT(DX**2 + DY**2)))
        DX = PTX(4) - PTX(3)
        DY = PTY(4) - PTY(3)
        KNOTS(4) = KNOTS(3) + EXP(ALPHA*LOG(SQRT(DX**2 + DY**2)))
        SPLINE_KNOTS(I,1:4) = KNOTS(1:4) 
c
        DO J = 1,NSUB                                      
          TT = (J-ONE) / NSUB
          K = K + 1
          CALL SPLINE_INTERPOL_2D(PTX, PTY ,KNOTS, TT,  NX  ,NY  )
          XX(K) = NX
          YY(K) = NY
        ENDDO                                            
        
      ENDDO 
      ! last point                                           
      TT = ONE
      K = K + 1
      XX(K) = XF(NPT0)
      YY(K) = YF(NPT0)

c      CALL SPLINE_INTERPOL_2D(PTX, PTY ,KNOTS, TT,  NX  ,NY  )
c      XX(K) = NX
c      YY(K) = NY
c
      DEALLOCATE (SPLINE_KNOTS)
      DEALLOCATE (CTRL_PTY)
      DEALLOCATE (CTRL_PTX)
c-----------
      RETURN
      END SUBROUTINE
