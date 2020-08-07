PROGRAM median
  INTEGER , PARAMETER :: LU =8, MAXDI=4
  
  INTEGER :: status,nval
  
  REAL :: data1,temp,sum=0 
  REAL :: mediam
  
  CHARACTER (LEN=80) :: filename
  REAL , DIMENSION(MAXDI) :: a

  
  
  WRITE (*,*) "Ingresa el nombre del archivo"
  READ (*,*) filename

  OPEN (UNIT=LU, FILE=filename, STATUS="OLD",IOSTAT=status,ACTION="READ")

  principal: IF (status == 0) THEN
     
     DO
        READ(LU,*,IOSTAT=status) data1
        IF ( status /= 0) EXIT
        nval = nval + 1
        a(nval) = data1
        
     ENDDO
     WRITE(*,*) nval
     outer: DO i=1, nval-1



        inner: DO j=i+1, nval
           IF (a(i) > a(j)) THEN
              
              temp=a(i)
           a(i)=a(j)
           a(j)=temp
        ENDIF

        

           
     ENDDO inner

     
     
  ENDDO outer
  
  
  WRITE(*,*) a



ELSE

   WRITE (*,*) "Problemas en el archivo"

ENDIF principal
     
DO i=1, nval
   
   sum = sum + a(i)
   
   
   
ENDDO

average=sum/nval


IF (mod(nval , 2 ) == 0) THEN
        mediam = ( a(nval/2) + a(nval/2+1) )/2
     ELSE
        mediam = a(nval/2+1)
        
     ENDIF
     
     WRITE(*,*) "La mediana es " , mediam
     WRITE(*,*) "El promedio es ", average
     

  
  
     
     
     
     
   END PROGRAM median
