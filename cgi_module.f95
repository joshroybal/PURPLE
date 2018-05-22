! Fortran cgi-bin data parsing module

module cgi_module
   contains
   ! function grabs indicated form field
   function get_form_field(fieldname, form_data, LIM) result(form_value)
      implicit none
      ! dummy arguments
      character (len = *), intent(in) :: fieldname, form_data
      integer, intent(in) :: LIM
      ! local variables
      integer idx1, idx2, n
      character (len = LIM) :: form_value, tmp
      ! processing
      n = len(trim(form_data))
      idx1 = INDEX(form_data, fieldname)
      tmp = form_data(idx1:n)
      idx1 = INDEX(tmp, '=') + 1
      idx2 = INDEX(tmp, '&') - 1
      if (idx2 .eq. -1) idx2 = n
      form_value = tmp(idx1:idx2)
   end function get_form_field

   ! function replaces substring text with substring rep in string s
   ! by David Frank  dave_frank@hotmail.com
   ! http://home.earthlink.net/~dave_gemini/strings.f90
   ! modified by Josh Roybal to replace with blanks if that's what user wants
   ! https://github.com/joshroybal
   ! developer@joshroybal.com
   function replace_text (s, text, rep)  result(outs)
      implicit none
      ! dummy arguments
      character (len = *), intent(in) :: s, text, rep
      ! local variables
      character (len = len(trim(s))+100) :: outs
      integer :: i, nt, nr, ns
      ! processing
      ns = len(trim(s))
      outs = s(:ns)
      ! if (index(outs(:ns), text) .eq. 0) return
      nt = len(trim(text))
      ! if (rep .ne. ' ') then
      !    nr = len(trim(rep))
      ! else
      !    nr = 1
      ! end if
      nr = len(trim(rep))
      do
         i = index(outs,text(:nt))
         if (i .eq. 0) exit
         outs = outs(:i-1) // rep(:nr) // outs(i+nt:)
      end do
   end function replace_text

   ! function runs through many of the html codes and replaces them with their
   ! ascii equivalents
   ! this can be improved in future with a loop running through hex and ascii
   function parse_url_encoding(istr) result(ostr)
      implicit none
      ! dummy arguments
      character (len = *), intent(in) :: istr
      ! local variables
      integer :: i
      character (len = len(istr)+100) :: ostr     
      !processing
      ! ostr = istr
      ! call replace_character(ostr, '+', ' ')
      ostr = replace_text(istr, '%09', ' ')
      ! ostr = replace_text(ostr, '%0A', ' ')   ! already done
      ! ostr = replace_text(ostr, '%0D', ' ')   ! already done      
      ! ostr = replace_text(ostr, '%20', ' ')   ! no spaces
      ostr = replace_text(ostr, '%21', '!')
      ostr = replace_text(ostr, '%22', '"')
      ostr = replace_text(ostr, '%23', '#')
      ostr = replace_text(ostr, '%24', '$')
      ostr = replace_text(ostr, '%25', '%')
      ostr = replace_text(ostr, '%26', '&')
      ostr = replace_text(ostr, '%27', "'")
      ostr = replace_text(ostr, '%28', '(')
      ostr = replace_text(ostr, '%29', ')')
      ostr = replace_text(ostr, '%2A', '*')
      ostr = replace_text(ostr, '%2B', '+')
      ostr = replace_text(ostr, '%2C', ',')
      ostr = replace_text(ostr, '%2D', '-')
      ostr = replace_text(ostr, '%2E', '.')
      ostr = replace_text(ostr, '%2F', '/')
      ostr = replace_text(ostr, '%3A', ':')
      ostr = replace_text(ostr, '%3B', ';')
      ostr = replace_text(ostr, '%3C', '<')
      ostr = replace_text(ostr, '%3D', '=')
      ostr = replace_text(ostr, '%3E', '>')
      ostr = replace_text(ostr, '%3F', '?')
      ostr = replace_text(ostr, '%40', '@')
      ostr = replace_text(ostr, '%5B', '[')
      ostr = replace_text(ostr, '%5C', '\')
      ostr = replace_text(ostr, '%5D', ']')
      ostr = replace_text(ostr, '%5E', '^')
      ostr = replace_text(ostr, '%5F', '_')
      ostr = replace_text(ostr, '%60', '`')
      ostr = replace_text(ostr, '%7B', '{')
      ostr = replace_text(ostr, '%7C', '|')
      ostr = replace_text(ostr, '%7D', '}')
      ostr = replace_text(ostr, '%7E', '~')
      call replace_character(ostr, '+', ' ')
   end function parse_url_encoding

   ! subroutine performs simple in place character replacement
   subroutine replace_character(str, ch, rep)
      implicit none
      ! dummy arguments
      character (len = *), intent(inout) :: str
      character, intent(in) :: ch, rep
      ! local variables
      integer :: i, n
      ! processing
      if (index(str, ch) .eq. 0) return   ! nothing to do
      n = len(trim(str))
      do i = 1, n
         if (str(i:i) .eq. ch) str(i:i) = rep
      end do
   end subroutine replace_character
   
   ! subroutine performs simple in place character replacement
   ! after some experimentation I have gotten the best run time with this
   subroutine replace_url_encoding(str, url_code, ch)
      implicit none
      ! dummy arguments
      character (len = *), intent(inout) :: str
      character (len = *), intent(in) :: url_code
      character, intent(in) :: ch
      ! local variables
      integer :: i, idx, ns, nss
      ! processing
      ns = len(trim(str))
      nss = len(trim(url_code))
      idx = 1
      do
         idx = index(str, url_code)
         if (idx .eq. 0) exit
         str(idx:idx) = ch
         str = str(1:idx) // str(idx + nss:ns)
      end do
   end subroutine replace_url_encoding

   ! asciify string buffer littered with url encodings
   subroutine asciify(str)
      ! dummy arguments
      character (len = *), intent(inout) :: str
      ! processing
      call replace_url_encoding(str, '%09', char(9))
      call replace_url_encoding(str, '%0D%0A', char(10))
      call replace_url_encoding(str, '%20', ' ')
      call replace_url_encoding(str, '%21', '!')
      call replace_url_encoding(str, '%22', '"')
      call replace_url_encoding(str, '%23', '#')
      call replace_url_encoding(str, '%24', '$')
      call replace_url_encoding(str, '%25', '%')
      call replace_url_encoding(str, '%26', '&')
      call replace_url_encoding(str, '%27', "'")
      call replace_url_encoding(str, '%28', '(')
      call replace_url_encoding(str, '%29', ')')
      call replace_url_encoding(str, '%2A', '*')
      call replace_url_encoding(str, '%2B', '+')
      call replace_url_encoding(str, '%2C', ',')
      call replace_url_encoding(str, '%2D', '-')
      call replace_url_encoding(str, '%2E', '.')
      call replace_url_encoding(str, '%2F', '/')
      call replace_url_encoding(str, '%3A', ':')
      call replace_url_encoding(str, '%3B', ';')
      call replace_url_encoding(str, '%3C', '<')
      call replace_url_encoding(str, '%3D', '=')
      call replace_url_encoding(str, '%3E', '>')
      call replace_url_encoding(str, '%3F', '?')
      call replace_url_encoding(str, '%40', '@')
      call replace_url_encoding(str, '%5B', '[')
      call replace_url_encoding(str, '%5C', '\')
      call replace_url_encoding(str, '%5D', ']')
      call replace_url_encoding(str, '%5E', '^')
      call replace_url_encoding(str, '%5F', '_')
      call replace_url_encoding(str, '%60', '`')
      call replace_url_encoding(str, '%7B', '{')
      call replace_url_encoding(str, '%7C', '|')
      call replace_url_encoding(str, '%7D', '}')
      call replace_url_encoding(str, '%7E', '~')
      call replace_character(str, '+', ' ')
   end subroutine asciify
end module
