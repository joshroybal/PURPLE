program cgipurple
   use cgi_module
   implicit none
   ! interface declaration
   interface
      ! subroutine emits top of html page
      subroutine print_header()
         ! void
      end subroutine print_header
      ! subroutine emits bottom of html page
      subroutine print_footer()
         ! void
      end subroutine print_footer
      ! level incrementing subroutine
      subroutine step(levcnt, fst, mdm, slw)
         integer, intent(inout), dimension(0:3) :: levcnt
         integer, intent(in) :: fst, mdm, slw
      end subroutine step
      ! function simulates pegboard mapping of sixes and twenties
      function pegboard(ptch, sixes, twenties, VOWELS, CONSONANTS) result(ctch)
         character, intent(in) :: ptch
         character (len = *), intent(in) :: sixes, twenties, VOWELS, CONSONANTS
         character :: ctch
      end function pegboard
      ! function simulates sixes relay
      function relay_six(NS, NL, NR, relay, lev, stp)
         integer :: relay_six
         integer, intent(in) :: NS, NL, NR, lev, stp
         integer, intent(in), dimension(NS,NL) :: relay
      end function relay_six
      ! function simulates twenties relay
      function relay_twenty(NT, NL, NR, relay, lev, stp)
         integer :: relay_twenty
         integer, intent(in) :: NT, NL, NR
         integer, intent(in), dimension(NT,NL,NR) :: relay
         integer, intent(in), dimension(0:NR) :: lev
         integer, intent(inout) :: stp
      end function relay_twenty     
   end interface
   ! variable definitions
   integer, parameter :: NR = 3, NL = 25, NS = 6, NT = 20, NC = 26, & 
   LIM = 262144 
   character (len = NS), parameter :: VOWELS = 'AEIOUY'
   character (len = NT), parameter :: CONSONANTS = 'BCDFGHJKLMNPQRSTVXWZ'
   character (len = NC), parameter :: ALPHABET = 'ABCDEFGHIJKLMNOPQESTUVWXYZ'
   integer :: i, j, k, n, idx, pos, fst, mdm, slw, recno, eof
   real :: t1, t2, t3, t4
   logical :: valid
   character :: ch, flag
   character (len = 12) :: nstr
   character (len = NS) :: sixes
   character (len = NT) :: twenties
   character (len = NC) :: ciphertext
   character (len = 80) :: field, record
   character (len = LIM) :: buf, msg
   integer, dimension(NS,NL) :: r6
   integer, dimension(NT,NL,NR) :: r20
   integer, dimension(0:NR) :: lev
   character, dimension(LIM) :: outbuf, inbuf
   ! processing
   print 1000, 'Content-Type: text/html; charset=us-ascii'
   print 2000, '<!DOCTYPE html>'
   call print_header()
   ! before emitting anything lets get all the data
   call getenv('CONTENT_LENGTH', nstr)
   read (nstr,*) n
   if (n .gt. LIM) then 
      print 2000, '<p>I''m sorry Dave, I''m afraid I can''t do that.</p>'
      print 2000, '<p>8192 word buffer size exceeded.</p>'
      do ! read the rest of the data
         read (*,*,iostat=eof) ch
         if (eof .ne. 0) exit
      end do
      call print_footer()
      stop
   end if
   ! read in all the data
   read 2000, buf
   ! grab the fields from the data buffer
   field = get_form_field("sixes", buf, LIM)
   sixes = field(1:6)
   field = get_form_field("twenties", buf, LIM)
   twenties = field(1:20)
   ! check if sixes and twenties are well formed
   ciphertext = sixes // twenties 
   valid = .true.
   do i = 1, NC
      idx = index(ciphertext, alphabet(i:i))
      if (idx .eq. 0) then ! not in ciphertext, so not valid
         valid = .false.
         exit
      else
         idx = index(ciphertext(idx+1:), alphabet(i:i))
         if (idx .ne. 0) then ! duplicate in ciphertext, so not valid
            valid = .false.
            exit
         end if
      end if
   end do
   ! fall back to default if ciphertext alphabet not valid
   if (valid .eqv. .false.) then
      sixes = VOWELS
      twenties = CONSONANTS
   end if
   flag = get_form_field("option", buf, LIM)
   if (flag == 'e') then
      recno = 1
      fst = 1
      mdm = 2
      slw = 3
   else
      recno = 2
      fst = 3
      mdm = 2
      slw = 1
   end if
   msg = get_form_field("message", buf, LIM)
   call cpu_time(t1)
   call asciify(msg)
   call cpu_time(t2)
   n = len(trim(msg))
   j = 0
   do i = 1, n
      ch = msg(i:i)
      if (ch .ge. 'a' .and. ch .le. 'z') ch = char(ichar(ch) - 32)
      if (ch .ge. 'A' .and. ch .le. 'Z') then
         j = j + 1
         inbuf(j) = ch
      end if      
   end do
   !call cpu_time(t2)
   ! get the relays from the formatted direct access file
   open (7,file='relays.dat',access='direct',form='formatted',recl=3300)
   read (7,3000,rec=recno) r6, r20
   close (7)
   lev = 1  ! initialize level counters to default
   ! now process the message buffer
   call cpu_time(t3)
   do i = 1, j
      ch = inbuf(i)
      ch = pegboard(ch, sixes, twenties, VOWELS, CONSONANTS)
      idx = index(VOWELS, ch)
      if (idx /= 0) then
         idx = relay_six(NS, NL, NR, r6, lev(0), idx)
         outbuf(i) = sixes(idx:idx)
         call step(lev, fst, mdm, slw)
         cycle ! we are done here
      end if
      idx = index(CONSONANTS, ch)
      if (idx /= 0) then
         idx = relay_twenty(NT, NL, NR, r20, lev, idx)
         outbuf(i) = twenties(idx:idx)
         call step(lev, fst, mdm, slw)
      end if
   end do
   call cpu_time(t4)
   print *, '<form action = "purple.cgi" method = "POST" accept-charset="us-ascii">'
   print *, '<label>sixes</label><div><input type="text" name="sixes" value="', sixes, '"></div>'
   print *, '<label>twenties</label><div><input type="text" name="twenties" value="', twenties, '"></div>'
   print *, '<label>levels</label><div><input type="text" name="levels" value="1 1 1"></div>'
   ! toggle encrypt/decrypt radio buttons
   if (flag == 'e') then
      print *, '<div><label>encrypt</label><input type="radio" name="option" value="e">'
      print *, '<label>decrypt</label><input type="radio" name="option" value="d" checked></div>'
   else
      print *, '<div><label>encrypt</label><input type="radio" name="option" value="e" checked>'
      print *, '<label>decrypt</label><input type="radio" name="option" value="d"></div>'
   end if
   print *, '<label>message</label><div><textarea name="message" rows="12" cols="80">'
   print 5000, (outbuf(i),i=1,j)
   print *, '</textarea></div>'
   print *, '<input type="submit">'
   print *, '</form>'
   print *, '<p></p>'
   write (nstr,6000) t2-t1
   print *, '<div>cgi-bin url decoding elapsed time = ', trim(adjustl(nstr)), ' seconds</div>'
   write (nstr,6000) t4-t3
   print *, '<div>purple machine processing elapsed time = ', trim(adjustl(nstr)), ' seconds</div>'   
   print *, '<p></p>'
   call print_footer()
   1000 format (a,/)
   2000 format (a)
   3000 format (1650i2)
   4000 format (a,((4(5a1,x),5a1)),a)
   5000 format (4(5a1,x),5a1)
   6000 format (f12.3)
end program cgipurple

! subroutine emits top of html page
subroutine print_header()
   ! processing
   print 1000, '<html>'
   print 1000, '<head>'
   print 1000, '<meta charset="us-ascii">'
   print 1000, '<title>sputnik</title>'
   print 1000, '<meta name="viewport" content="width=device-width, initial-scale=1.0">'
   print 1000, '<link rel="stylesheet" media="all" type="text/css" href="/includes/style.css"/>'
   print 1000, '</head>'
   print 1000, '<body>'
   print 1000, '<header><p>PURPLE</p></header>'
   1000 format (a)
end subroutine print_header

! subroutine emits bottom of html page
subroutine print_footer()
   ! processing
   print 1000, '<footer><p>CopyLeft 2018 Josh Roybal - All Wrongs Reserved</p></footer>'
   print 1000, '</body>'
   print 1000, '</html>'
   1000 format (a)
end subroutine print_footer

! function emulates pegboard mapping of sixes and twenties
function pegboard(ptch, sixes, twenties, VOWELS, CONSONANTS) result(ctch)
   implicit none
   ! dummy arguments
   character, intent(in) :: ptch
   character (len = *), intent(in) :: sixes, twenties, VOWELS, CONSONANTS
   ! local variables
   character :: ctch
   integer :: idx
   ! processing
   idx = index(sixes, ptch)
   if (idx /= 0) then
      ctch = VOWELS(idx:idx)
      return
   end if
   idx = index(twenties, ptch)
   if (idx /= 0) then
      ctch = CONSONANTS(idx:idx)
      return
   end if
   ctch = ptch ! fail safe
end function pegboard

! subroutine increments levels
subroutine step(levcnt, fst, mdm, slw)
   implicit none
   ! dummy arguments
   integer, intent(inout), dimension(0:3) :: levcnt
   integer, intent(in) :: fst, mdm, slw
   ! local variables
   integer i
   ! processing
   ! check which twenties relay steps
   if (levcnt(0) == 24 .and. levcnt(mdm) == 25) then 
      levcnt(slw) = levcnt(slw) + 1
   else if (levcnt(0) == 25) then
      levcnt(mdm) = levcnt(mdm) + 1
   else
      levcnt(fst) = levcnt(fst) + 1
   end if
      
   levcnt(0) = levcnt(0) + 1  ! always advances

   ! reset when advanced past 25th level
   do i = 0, 3
      if (levcnt(i) > 25) levcnt(i) = 1
   end do 
end subroutine step

! function simulates sixes relay
function relay_six(NS, NL, NR, relay, lev, stp)
   implicit none
   ! dummy arguments
   integer :: relay_six
   integer, intent(in) :: NS, NL, NR, lev, stp
   integer, intent(in), dimension(NS,NL) :: relay
   ! processing
   relay_six = relay(stp,lev)
end function relay_six

! function simulates twenties relay
function relay_twenty(NT, NL, NR, relay, lev, stp)
   implicit none
   ! dummy arguments
   integer :: relay_twenty
   integer, intent(in) :: NT, NL, NR
   integer, intent(in), dimension(NT,NL,NR) :: relay
   integer, intent(in), dimension(0:NR) :: lev
   integer, intent(inout) :: stp
   ! local variables
   integer :: i
   ! processing
   do i = 1, NR
      stp = relay(stp,lev(i),i)
   end do
   relay_twenty = stp
end function relay_twenty
