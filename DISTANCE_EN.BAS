'* ROAD DISTANCES (KM) BETWEEN CITIES OF MAINLAND GREECE
'* ver 1.0 **** 1991-12-03 + Original version for PC-MASTER (as Agrinio Club)
'* ver 1.1 **** 1996-11-23 + Changes in colors
'                          | More readable source
'                          | Added message on exit
'                          | Added version number on title
'                          + Set to compile for 8086 without 8087
'* ver 1.2 **** 2019-03-04 + English version for archiving on Github
'* (C)opyright 1991-2019 George Birbilis - Zoomicon

$CPU 8086
$float EMULATE
$option CNTLBREAK OFF

ON KEY(1)GOSUB Telos:KEY(1)ON

START:
SCREEN 0,0,0:WIDTH 80:COLOR 7,0:CLS

COLOR 7,7:LOCATE 25,1:?SPC(79)

color 7,4:locate 25,2:?"F1";
LOCATE 25,21:?" "CHR$(24)SPC(7)CHR$(25)SPC(7)CHR$(27)SPC(11)CHR$(26);
LOCATE 25,66:?"ENTER";

COLOR 0,7:LOCATE 25,4:?"-Exit ";:locate 25,71:?"-Selection ";
locate 25,23:?"-Up ";:locate 25,31:?"-Down ";:locate 25,39:?"-Left ";:LOCATE 25,51:?"-Right";

LOCATE 17,2:color 7,0
?"To find the distance between two cities place the bright bar"
?"on the first city and press ENTER. Do the same with the second city.
?"The program will give you the road distance between the two cities (km) and will"
?"ask for the average speed with which you'll cover the distance. If you enter an"
?"average speed then the program will give you the average time in which"
?"you'll cover the road distance between the two cities."

color 0,1:locate 24,1:?spc(79):locate 24,13:color 7,1:?"Copyright 1991-2019  George Birbilis / Zoomicon";

locate 1,1:color 0,7:?SPC(7)"ROAD DISTANCES (KM) BETWEEN CITIES OF MAINLAND GREECE (# 1.2)"spc(7);

'-------------------------

color 7,0

OPEN "I",#2,"CITIES.DAT"

FOR D%=1 TO 13
INPUT #2,CIT$
LOCATE D%+2,2:IF D%<10 THEN ?D%"  "CIT$ ELSE ?D%" "CIT$
NEXT D%

FOR D%=1 TO 13
INPUT #2,CIT$
LOCATE D%+2,29:?D%+13" "CIT$
NEXT D%

FOR D%=1 TO 13
INPUT #2,CIT$
LOCATE D%+2,56:?D%+26" "CIT$
NEXT D%

CLOSE #2

'-------------------------

call menu(3,3,19,1,15,7,ch1$)
a%=val(left$(ch1$,2))

10 call menu(3,3,19,1,15,7,ch2$)
b%=val(left$(ch2$,2))
if a%=b% then 10

'-------------------------

open "i",1,"DISTANCE.DAT"
if b%<a% then swap a%,b%

for t%=2 to b%-1
 line input #1,a$
next t%

for t%=1 to a%
 input #1,b%
next t%

close #1

'-------------------------

s1%=instr(5,ch1$," "):s2%=instr(5,ch2$," ")
call wind(1,1,218,191,23,79,217,192,196,179, 1,7)

color 1,7:LOCATE 3,12
?"The distance between "mid$(ch1$,5,s1%-5)" and "mid$(ch2$,5,s2%-5)" is";

color 20,7:?b%;:color 1,7:?"km"
3 color 15,9:locate 5,3:?"Enter the average speed with which you'll cover the distance "spc(5)"km/h"
color 31,1:locate 5,69:call getname(tax$,4):tax=val(tax$):if tax=0 then 3

color 1,15:hrs=b%/tax:locate 8,15:?"You will cover the distance in ";:color 8,15:?hrs;:color 1,15
if hrs<>1 then ?" hours" else ?" hour "

LOCATE 21,23:?"Do you want to try again (Y/n) ;";:ans$=INPUT$(1)
LOCATE 21,55:if instr("Yy",ans$) THEN ?"YES":GOTO START
?"NO"

Telos:
 COLOR 7,0:CLS
 ?"/-------------------------------\"
 ?"|ROAD DISTANCES (KM) BETWEEN    |"
 ?"|CITIES OF MAINLAND GREECE #1.2 |"
 ?"|-------------------------------|"
 ?"|     G. Birbilis               |"
 ?"\-------------------------------/"
 END

'-------------------------------------------------------------------

sub menu(ilx%,ily%,ox%,fgr%,bgr%,space%,choice$)
 o$=chr$(0)
 BILX=ILX:BILY=ILY
 cv=0
 goto fcr

 '**** controls ****
 red:
 k$=inkey$

 select case k$
  case "8",o$+"H"
   bily%=ily%:bilx%=ilx%:ily%=ily%-1:goto fcr
  case "2",o$+"P"
   bily%=ily%:bilx%=ilx%:ily%=ily%+1:goto fcr
  case "4",o$+"K"
   bily%=ily%:bilx%=ilx%:ilx%=ilx%-ox%-space%-1:goto fcr
  case "6",o$+"M"
   bily%=ily%:bilx%=ilx%:ilx%=ilx%+ox%+space%+1:goto fcr
  case chr$(13)
   locate ily%,ilx%:color 7,0:?choice$;
   color 3,0:?chr$(27)chr$(205)         'note choice
   exit sub
  case else
   goto red
 end select

 fcr:
 if ily%<1 then ily%=bily%:goto red
 if ilx%<1 then ilx%=bilx%:goto red
 if ilx%>79-ox% then ilx%=bilx%:goto red
 ccr=screen(ily%,ilx%)
 if ccr=32 then ily%=bily%:ilx%=bilx%:goto red
 if cv=0 then cat
 locate bily%,bilx%:color 7,0:?choice$

 cat:
 cv=1
 choice$=""
 k%=0
 for ix%=ilx% to ilx%+ox%-1
  c%=screen(ily%,ix%)
  choice$=choice$+chr$(c%)
 next ix%
 locate ily%,ilx%:color fgr%,bgr%:?choice$
 goto red
end sub

'----------------------------------------------------------------------

'This subroutine creates a window on the screen
sub wind(x1%,y1%,a,a1,x2,y2%,b,b1,c,d,fgr%,bgr%)
 color fgr%,bgr%
 locate x1%,y1%:?chr$(a):locate x2,y2%:?chr$(b)
 locate x1%,y2%:?chr$(a1):locate x2,y1%:?chr$(b1)
 k$=string$(y2%-y1%-1,c):locate x1%,y1%+1:?k$:locate x2,y1%+1:?k$
 for s%=x1%+1 to x2-1
  locate s%,y1%:?chr$(d);spc(y2%-y1%-1);chr$(d)
 next s%
end sub

'This subroutine accepts a name from keyboard
sub getname(nam$,lim)
 nam$="":y=pos(0):x=csrlin:?string$(lim,95):locate x,y:f=1
 while f<lim+1
2 d$=input$(1)
 select case d$
  case chr$(13)
   delay .5:exit sub
  case chr$(9)
   goto 2
  case chr$(8)
   f=f-1:if f=0 then f=f+1:goto 2
   nam$=left$(nam$,f-1):y=pos(0):x=csrlin:locate x,y-f:?STRING$(lim,95);:locate x,y-f:?nam$;:goto 2
  case else
   IF ASC(D$)>47 AND ASC(D$)<58 THEN ?d$; ELSE 2
   nam$=nam$+d$:f=f+1
 end select
 wend
 delay .5
end sub
