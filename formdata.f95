program formdata
   implicit none
   ! variable definitions
   integer, parameter :: NR = 3, NL = 25, NS = 6, NT = 20
   integer :: i, j, k, idx
   integer, dimension(NS+1,NL) :: d_sixes
   integer, dimension(NT+1,NL,NR) :: d_twenties
   integer, dimension(NS,NL) :: e_sixes, d6
   integer, dimension(NT,NL,NR) :: e_twenties, d20
   ! processing
   ! read deciphering level mappings
   open (7,file='purple.txt',status='old',action='read')
   read (7,*) d_sixes
   read (7,*) d_twenties
   close (7)
   ! test successful reads
   write (*,1000) ((d_sixes(i,j),i=1,NS+1),j=1,NL)
   1000 format (7i2)
   write (*,2000) (((d_twenties(i,j,k),i=1,NT+1),j=1,NL),k=1,NR)
   2000 format (25(21i3,/))
   ! determine enciphering levels from deciphering levels
   ! sixes
   do j = 1, NL
      do i = 1, NS
         idx = 1
         do while (i /= d_sixes(idx+1,j))
            idx = idx + 1
         end do
         e_sixes(i,j) = idx
      end do
   end do
   ! twenties
   do k = 1, 3
      do j = 1, NL
         do i = 1, NT
            idx = 1
            do while (i /= d_twenties(idx+1,j,k))
               idx = idx + 1
            end do
          e_twenties(i,j,k) = idx
         end do
      end do
   end do
   ! test successful conversion to enciphering mappings
   write (*,3000) ((e_sixes(i,j),i=1,NS),j=1,NL)
   3000 format (6i2)
   write (*,4000) (((e_twenties(i,j,k),i=1,NT),j=1,NL),k=1,NR)
   4000 format (25(20i3,/))  
   ! write records to direct access data file
   open (8,file='relays.dat',access='direct',form='formatted',recl=6600)
   write (8,5000,rec=1) e_sixes, e_twenties,((d_sixes(i,j),i=2,NS+1),j=1,NL), &
      (((d_twenties(i,j,k),i=2,NT+1),j=1,NL),k=1,NR)
   5000 format (3300i2)
   ! 5000 format (3300z0.2) hex with leading zeros
   close (8)
   ! test reading from direct access file
   open (8,file='relays.dat',access='direct',form='formatted',recl=6600)
   read (8,5000,rec=1) e_sixes, e_twenties, d6, d20
   close (8)
   ! test successful read from direct access file
   write (*,6000) e_sixes
   6000 format (6i2)
   write (*,7000) e_twenties
   7000 format (25(20i3,/))
   write (*,6000) d6
   write (*,7000) d20
end program formdata
