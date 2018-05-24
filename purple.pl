#!/usr/bin/perl

use strict;
use warnings;
use CGI;
use Time::HiRes;
# input
my $start = [ Time::HiRes::gettimeofday( ) ];
my $cgi = CGI->new();
my $sixes = $cgi->param("sixes");
my $twenties = $cgi->param("twenties");
my $switch_settings = $cgi->param("levels");
my $flag = $cgi->param("option");
my $message_input = $cgi->param("message");
# processing
$message_input =~ s/[^A-Za-z]+//g;
$message_input = uc $message_input;
my $message_length = length $message_input;
my $message_output = `echo $message_input | ./purple $flag $message_length`;
my $elapsed = Time::HiRes::tv_interval( $start );
# output
print "Content-type:text/html\n\n";
print "<!DOCTYPE html>\n";
print "<head>\n";
print "<meta charset='utf-8'>\n";
print "<title>PURPLE</title>\n";
print "<meta name='viewport' content='width=device-width, initial-scale=1.0'>\n";
print "<link rel='stylesheet' media='all' type='text/css' href='/includes/style.css'/>\n";
print "</head>\n";
print "<body>\n";
print "<header><p>PURPLE</p></header>\n";
print "<p></p>\n";
print "<div><a href='/index.php'>Home</a> | <a href='/purple.html'>Back</a></div>\n";
print "<p></p>\n";
print "<form action ='purple.pl' method='POST' accept-charset='utf-8'>\n";
print "<label>sixes</label>\n";
print "<div><input type='text' size='8' name='sixes' value='$sixes'></div>\n";
print "<label>twenties</label>\n";
print "<div><input type='text' size='24' name='twenties' value='$twenties'></div>\n";
print "<label>switch settings</label>\n";
print "<div><input type='text' size='8' name='levels' value='$switch_settings'></div>\n";
print "<label>message</label>\n";
# toggle encrypt/decrypt based on what this script got
if ($flag eq "e") {
   # decrypt is checked
   print "<div><label>encrypt</label><input type='radio' name='option' value='e'>\n";
   print "<label>decrypt</label><input type='radio' name='option' value='d' checked></div>\n";
} else {
   # encrypt is checked
   print "<div><label>encrypt</label><input type='radio' name='option' value='e' checked>\n";
   print "<label>decrypt</label><input type='radio' name='option' value='d'></div>\n";
}
print "<div><textarea name='message' rows='12' cols='80'>$message_output</textarea></div>\n";
print "<input type='submit'>\n";
print "</form>\n";
print "<p>Elapsed time: $elapsed seconds!</p>\n";
# give the user the home & back links
print "<p></p>\n";
print "<div><a href='/index.php'>Home</a> | <a href='/purple.html'>Back</a></div>\n";
print "<p></p>\n";
print "<footer><p>CopyLeft 2018 Josh Roybal - All Wrongs Reserved</p></footer>\n";
print "</body>\n";
print "</html>\n";
