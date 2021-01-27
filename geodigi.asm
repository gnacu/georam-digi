;----[ geodigi.a ]----------------------

         .include "//os/h/:modules.h"

         #inc_s "pointer"

         #inc_k "file"
         #inc_k "io"
         #inc_k "key"

         #inc_s "basic"

         jmp start

delay    .byte $00 ;Cycles Per Sample

playidx  = $61     ;In-Page Index
         ; $62,$63 ;GeoRAM Page
         ; $64     ;Should stay zero.

lobyte   = $65 ;part of BASIC FAC

pageflag = $66

geopglo  = $67
geopghi  = $68

;---------------------------------------

filename .text "sample.wav"

maxmem   .byte $00,$00,$00,$00
mempgs   .byte $00,$08,$10,$20,$40

memtablo .byte $00
         .byte <str5mem
         .byte <str1mem
         .byte <str2mem
         .byte <str4mem

memtabhi .byte $00
         .byte >str5mem
         .byte >str1mem
         .byte >str2mem
         .byte >str4mem

str5mem  .null "GeoRAM 512K Detected "
str1mem  .null "GeoRAM 1MB Detected "
str2mem  .null "GeoRAM 2MB Detected "
str4mem  .null "GeoRAM 4MB Detected "

;----[ Messages ]-----------------------

         #inc_s "string"

         ;--- Error Messages --- $00-$06

         ;$00: Not a RIFF or not a WAVE
         .null "Unrecognized File Type"

         ;$01: Not uncompressed PCM data
         .null "Unsupported Data Format"

         ;$02: Not mono or stereo
         .text "Unsupported # of "
         .null "Channels"

         ;$03: More than 22050Hz
         .null "Unsupported Sample Rate"

         ;$04: Not 8- or 16-bit Samples
         .null "Unsupported Sample Size"

         ;$05: GeoRAM Not Detected
         .null "GeoRAM Not Detected"

         ;$06: Data Chunk Size >2MB
         .text "Memory Limit, "
         .null "Sample Data Truncated"


         ;--- Other Messages --- $07-$XX

         ;$07: Welcome Message
         .null "GeoRAM Digi"

         ;$08: Copyright Message
         .text "Copyright (c)2021 "
         .null "Greg Nacu"

         ;$09: Checking Message
         .null "Checking File Metadata"

         ;$0a: Loading Message
         .null "Loading File Data"

         ;$0b: Loading/Downsampling Msg
         .text "Loading and Downsampl"
         .null "ing File Data"

         ;$0c: Data Ready Message
         .null "Data Loaded"

;----[ Strings ]------------------------

str1chn  .null "Mono "
str2chn  .null "Stereo "

str8bit  .null "8-Bit "
str16bit .null "16-Bit (to 8-Bit) "

str22rat .null "22050Hz "
str16rat .null "16000Hz "
str11rat .null "11025Hz "
str80rat .null "8000Hz "

;---------------------------------------

filetype .text "riff"    ;ASCII "RIFF"
datatype .text "wave"    ;ASCII "WAVE"
fmt_chnk .byte $66,$6d,$74,$20 ;"fmt "
datachnk .byte $64,$61,$74,$61 ;"data"

rifffile .text "    "  ;RIFF
         .byte 0,0,0,0 ;RIFF size
rifftype .text "    "  ;WAVE

subchnk  .text "    " ;"fmt " or "data"
subchsz  .byte 0,0,0,0

         ;-- "fmt " info ---------------

audfmt   .word 0   ;1=PCM,!1=unsupported
channels .word 0   ;1=mono, 2=stereo
samprate .word 0,0 ;ie: 22050,11025,etc
byterate .word 0,0 ;bytes per second
         .word 0   ;block align
sampbits .word 0   ;8 or 16 bits

         ;-- "data" info ---------------

datasz   .byte 0,0,0,0

;---------------------------------------

start
         .block
ptr      = $fb ;$fc

         lda #$0e ;Lowercase/Uppercase
         jsr chrout

         lda #13  ;CR
         jsr chrout

         lda #13  ;CR
         jsr chrout

         lda #$07 ;Welcome
         jsr msgout

         lda #$08 ;Copyright
         jsr msgout

         lda #13  ;CR
         jsr chrout

         ;--- Detect GeoRAM ------------

         ;TODO: Restore overwritten data
         ;to prevent corruption of any-
         ;thing already in RAM.

         lda #$00
         sta $dffe

         lda #%11111111
         sta $dfff
         lda #$04   ;4MB Marker
         sta $de00

         lda #%01111111
         sta $dfff
         lda #$03   ;2MB Marker
         sta $de00

         lda #%00111111
         sta $dfff
         lda #$02   ;1MB Marker
         sta $de00

         lda #%00011111
         sta $dfff
         lda #$01   ;512KB Marker
         sta $de00

         lda #%00000000
         sta $dfff

         lda #$01  ;4 Byte Magic Marker
         sta $de00
         lda #$02
         sta $de01
         lda #$03
         sta $defe
         lda #$04
         sta $deff

         lda $de00 ;Confirm 4 Byte Magic
         cmp #$01
         bne nomem
         lda $de01
         cmp #$02
         bne nomem
         lda $defe
         cmp #$03
         bne nomem
         lda $deff
         cmp #$04
         beq *+7

nomem    lda #$05   ;GeoRAM Not Detected
         jmp errout


         ;Config maxmem from Marker Code

         lda #%11111111
         sta $dfff
         ldx $de00

         lda mempgs,x
         sta maxmem+2

         ;Output Detected Memory String

         lda memtablo,x
         pha
         lda memtabhi,x
         tay
         pla
         jsr strout

         lda #13  ;CR
         jsr chrout

         ;----------------------------

         lda #$09 ;Checking Metadata
         jsr msgout

         ;Open RIFF File for Read

         jsr openfile

         ;------------------------------
         ;Load RIFF Header

         lda #12
         #ldxy rifffile
         jsr loadntoa

         ;Compare File Type
         ldx #3
         lda filetype,x
         cmp rifffile,x
         bne *+7 ;Error
         dex
         bpl *-9
         bmi *+7 ;Success

         lda #$00     ;Not RIFF File
         jmp errout

         ;Compare Data Type
         ldx #3
         lda datatype,x
         cmp rifftype,x
         bne *+7 ;Error
         dex
         bpl *-9
         bmi *+7 ;Success

         lda #$00     ;Not WAVE Data
         jmp errout


         ;------------------------------
         ;Load Chunk Header

         ;NOTE: I believe the standard
         ;requires the "fmt " chunk to
         ;preceed the "data" chunk.

next     lda #8
         #ldxy subchnk
         jsr loadntoa

         ;Check "fmt " Chunk
         ldx #3
         lda fmt_chnk,x
         cmp subchnk,x
         beq *+5
         jmp notfmt
         dex
         bpl *-12

         ;Load fmt chunk data
         lda subchsz+0     ;Should be 16
         #ldxy audfmt
         jsr loadntoa

         ;Validate Uncompressed PCM Data

         lda audfmt+1
         beq *+7
         lda #$01     ;Not PCM
         jmp errout

         lda audfmt+0
         cmp #$01
         beq *+7
         lda #$01     ;Not PCM
         jmp errout

         ;Validate either mono or stereo

         lda channels+1
         beq *+7
         lda #$02     ;Too Many Channels
         jmp errout

         lda channels+0
         cmp #$01
         beq *+11
         cmp #$02
         beq *+13
         lda #$02     ;Too Many Channels
         jmp errout

         ;Output number of channels msg
         lda #<str1chn
         ldy #>str1chn
         bne *+6
         lda #<str2chn
         ldy #>str2chn

         jsr $ab1e        ;Print Str Y/A


         ;Validate supported sample rate

         #ldxy srtab
         #stxy ptr

         ldx #4       ;4 Supported Rates

nextrate ldy #3

         lda (ptr),y
         cmp samprate,y
         bne notrate
         dey
         bpl *-8

         ;Found Matching Rate

         ldy #4        ;NTSC Cycle Delay

         lda $02a6     ;0=NTSC,1=PAL
         beq *+3

         iny            ;PAL Cycle Delay

         lda (ptr),y
         sta delay

         jmp foundrate

notrate  dex
         bne *+7
         lda #$03     ;Unsupported Rate
         jmp errout

         clc
         lda ptr+0
         adc #8
         sta ptr+0
         lda ptr+1
         adc #0
         sta ptr+1

         jmp nextrate

foundrate ;Output Sample Rate Message
         ldy #6
         lda (ptr),y
         pha
         iny
         lda (ptr),y
         tay
         pla

         jsr $ab1e        ;Print Str Y/A


         ;Validate supported sample bits

         lda sampbits+1
         beq *+7
         lda #$04     ;Not 8-Bit/16-Bit
         jmp errout

         lda sampbits+0
         cmp #16
         beq *+11
         cmp #8
         beq *+13
         lda #$04     ;Not 8-Bit/16-Bit
         jmp errout

         lda #<str16bit
         ldy #>str16bit
         bne *+6
         lda #<str8bit
         ldy #>str8bit

         jsr strout

         ;Valid, Supported "fmt " Chunk

         jmp next
notfmt

         ;Check "data" Chunk
         ldx #3
         lda datachnk,x
         cmp subchnk,x
         beq *+5
         jmp notdata
         dex
         bpl *-12

         lda sampbits
         cmp #8
         bne *+8

         jsr load8bit
         jmp datadone

         jsr load16bit
         jmp datadone

notdata

         ;Skip Other Chunks

         lda subchsz+1
         ora subchsz+2
         beq skippart

skipfull ;Skip Full Pages

         lda #$00     ;256 Bytes
         jsr loadnton

         dec subchsz+1
         bne skipfull

         lda subchsz+2
         beq skippart

         dec subchsz+2
         jmp skipfull

skippart ;Skip Partial Page

         lda subchsz+0
         jsr loadnton

         jmp next
;---------------------------------------

datadone

         lda #13
         jsr chrout

         lda #$0c ;Data Ready Msg
         jsr msgout

         ;Prevent Timer A NMI

         jsr pause


         ;Configure CIA2 for Digimax Out

         lda #%11111111
         sta $dd02 ;Port A: All Outputs
         sta $dd03 ;Port B: All Outputs


         ;Configure CIA2 (NMI) Timer A

         lda #$00
         sta $dd05 ;Timer A, Count Hi

         lda delay
         sta $dd04 ;Timer A, Count Lo

         ;Force load
         ;Continous Fire Mode
         ;PortB Ouput Off
         ;Start the Timer

         lda #%00010001
         sta $dd0e         ;CIA2 Timer A

         ;Configure NMI Handler

         lda channels
         cmp #$02
         beq *+8
         ldx #<play1chn
         ldy #>play1chn
         bne *+6
         ldx #<play2chn
         ldy #>play2chn

         #stxy $0318  ;KERNAL NMI Vector


         ;Rewind and Resume!

         jsr rewind
         jsr resume

;---------------------------------------
;Keyboard Scanning Loop

iloop
         ;--- TEST: Keep testing this---

         ldy playidx+2

         ldx playidx+1
         inx
         stx pageflag

         bne *+3
         iny

         tya

         asl pageflag
         rol a
         asl pageflag
         rol a

         sta geopghi
         stx geopglo

         ;---- End -----


         lda playidx+2
         cmp datasz+2
         bcc continue
         bne doend

         lda playidx+1
         cmp datasz+1
         bcc continue
         bne doend

         lda playidx+0
         cmp datasz+0
         bcc continue
         beq continue

doend    ;Playback hit end, wrap around
         lda repeat
         beq *+8

         jsr rewind
         clc
         bcc *+5

         jsr dostop

         ;Keyboard Controls:
         ; Space -> Pause/Resume
         ; F1    -> Back     5 Seconds
         ; F7    -> Forward 10 Seconds
         ; _     -> Rewind
         ; s     -> Stop
         ; +     -> Screen On
         ; -     -> Screen Off
         ; r     -> Repeat On/Off
         ; ^Q    -> Quit

continue jsr getin
         beq iloop

         cmp #" "
         bne notspace

         lda playing
         beq *+8
         jsr pause
         jmp iloop
         jsr resume
         jmp iloop
notspace

         cmp #"s"
         bne *+8

         jsr dostop
         jmp iloop

         cmp #"{arrow left}"
         bne *+8

         jsr rewind
         jmp iloop

         cmp #$85 ;F1 Key
         bne *+8

         jsr back5
         jmp iloop

         cmp #$88 ;F7 Key
         bne *+8

         jsr fwrd10
         jmp iloop

         cmp #"+"
         bne *+10

         lda #%00011011  ;Screen On
         sta $d011
         jmp iloop

         cmp #"-"
         bne *+10

         lda #%00001011  ;Screen Off
         sta $d011
         jmp iloop

         cmp #"r"
         bne *+13

         lda repeat
         eor #1
         sta repeat
         jmp iloop

         cmp #$11 ;CTRL+Q
         bne *+5
         jmp ($fffc)


         jmp iloop

         rts
         .bend

msgout   ;Output Message to Screen
         ;A -> Message Index
         .block
         jsr getstrxy
         txa

         ;Fallthrough...
         .bend

strout   ;Output String to Screen
         ;Y -> String Ptr Hi Byte
         ;A -> String Ptr Lo Byte

         jsr $ab1e        ;Print Str Y/A

         lda #13 ;Carriage Return
         jmp chrout

;----[ Data Loading ]-------------------

openfile ;Open File for Read
         .block
         jsr clrchn

         lda #5  ;Logical File Number
         ldx $ba ;Current Device
         ldy #5  ;Data Channel 5
         jsr setlfs

         lda #10 ;10 Character Name
         #ldxy filename
         jsr setnam

         jsr open

         ldx #5 ;Logical File Number
         jmp chkin
         .bend

errout   ;Output Error Message
         ;A -> Message Index
         jsr msgout

         ;Fallthrough...

closfile ;Close File and Clear Channels
         .block
         lda #5 ;Logical File Number
         jsr close

         jmp clrchn
         .bend

loadntoa ;Load # bytes to an address
         ;A      -> # Bytes to load
         ;RegPtr -> Target address

         ;Note: $00 = Load 256 Bytes
         .block
         sta limit+1
         #stxy next+4

         ldy #0

next     jsr chrin
         sta $ffff,y           ;Self-Mod
         iny
limit    cpy #$ff              ;Self-Mod
         bne next

         rts
         .bend

loadnton ;Load # bytes to /dev/null
         ;A -> # Bytes to load

         ;Note: $00 = Load 256 Bytes
         .block
         sta limit+1

         ldy #0

next     jsr chrin    ;Load it, toss it.
         iny
limit    cpy #$ff              ;Self-Mod
         bne next

         rts
         .bend

load8bit ;Load 8-Bit Data Chunk
         .block
         ;If chunk size > maxmem
         ;  Reduce chunk size to maxmem
         ;  Output truncated message

         lda subchsz+3
         cmp maxmem+3
         bcc valid
         bne over

         lda subchsz+2
         cmp maxmem+2
         bcc valid
         bne over

         lda subchsz+1
         cmp maxmem+1
         bcc valid
         bne over

         lda subchsz+0
         cmp maxmem+0
         bcc valid
         beq valid

over     ;Data Over Maximum Memory Size
         lda maxmem+0
         sta subchsz+0

         lda maxmem+1
         sta subchsz+1

         lda maxmem+2
         sta subchsz+2

         lda maxmem+3
         sta subchsz+3

         ;Output Truncated Message

         lda #$06   ;Insufficient Memory
         jsr msgout

valid
         ;Copy chunk size to datasz

         lda subchsz+0
         sta datasz+0

         lda subchsz+1
         sta datasz+1

         lda subchsz+2
         sta datasz+2

         lda subchsz+3
         sta datasz+3


         lda #$0a ;Loading Data Msg
         jsr msgout


         ;Configure RAM for sequential
         ;page incrementing.

         ldx #0
         lda #0
         jsr cnframpg

         ;Check for bizarre case where
         ;total data size <= 256 bytes

         lda subchsz+1
         ora subchsz+2
         beq loadpart

loadfull ;Load Full Pages

         ldy #0

         jsr chrin
         sta $de00,y  ;GeoRAM Window
         iny
         bne *-7      ;256 Bytes

         lda subchsz+1
         and #%00000011
         bne *+7

         lda #$a6 ;Hash Block, each 1KB
         jsr chrout

         jsr incrampg

         dec subchsz+1
         bne loadfull

         lda subchsz+2
         beq loadpart

         dec subchsz+2
         jmp loadfull

loadpart ;Load Final Partial Page

         lda subchsz+0
         #ldxy $de00  ;GeoRAM Window
         jsr loadntoa

         ;Close File
         ;Ignoring extra chunks

         jmp closfile
         .bend

load16bit ;Load 16-Bit Data Chunk
          ;Downsample to 8-Bit and
          ;adjust datasize to half
         .block

         ;Enable SID Noise for Random
         lda #$ff
         sta $d40e ;Max Freq Lo Voice3
         sta $d40f ;Max Freq Hi Voice3
         lda #%10000000
         sta $d412 ;Noise Form, Silent

         ;Downsample 16-bit to 8-bit.
         ;Chunk size is 2x datasz.
         ;Copy chunksize/2 to datasz.
         ;Divide ByteRate by 2.

         ;If datasz > maxmem
         ;  Reduce datasize to maxmem
         ;  Output truncated message
         ;  Copy datasz to chunksize


         ;Copy chunksize/2 to datasz

         lda subchsz+3
         lsr a
         sta subchsz+3
         sta datasz+3

         lda subchsz+2
         ror a
         sta subchsz+2
         sta datasz+2

         lda subchsz+1
         ror a
         sta subchsz+1
         sta datasz+1

         lda subchsz+0
         ror a
         sta subchsz+0
         sta datasz+0

         ;Check if datasz > maxmem

         lda datasz+3
         cmp maxmem+3
         bcc valid
         bne over

         lda datasz+2
         cmp maxmem+2
         bcc valid
         bne over

         lda datasz+1
         cmp maxmem+1
         bcc valid
         bne over

         lda datasz+0
         cmp maxmem+0
         bcc valid
         beq valid

over     ;Data Over Maximum Memory Size
         lda maxmem+0
         sta subchsz+0
         sta datasz+0

         lda maxmem+1
         sta subchsz+1
         sta datasz+1

         lda maxmem+2
         sta subchsz+2
         sta datasz+2

         lda maxmem+3
         sta subchsz+3
         sta datasz+3

         ;Output Truncated Message

         lda #$06   ;Insufficient Memory
         jsr msgout

valid

         lda #$0b ;Loading/DownSamp. Msg
         jsr msgout

         ;Divide ByteRate by 2.

         lsr byterate+3
         ror byterate+2
         ror byterate+1
         ror byterate+0

         ;Configure RAM for sequential
         ;page incrementing.

         ldx #0
         lda #0
         jsr cnframpg

         ;Check for bizarre case where
         ;total data size <= 256 bytes

         lda subchsz+1
         ora subchsz+2
         beq loadpart

         ;Samples are Signed 16-Bit
         ;Little Endian. Downsampled
         ;to Unsigned 8-Bit with audio
         ;dithering from SID noise.

loadfull ;Load Full Pages

         ldy #0

         jsr chrin    ;Fetch Lo-Byte
         clc
         adc $d41b    ;C <- Dither Bit
         php

         jsr chrin    ;Fetch Hi-Byte
         eor #$80     ;Signed->Unsigned

         plp
         adc #0       ;Add Dither Bit

         sta $de00,y  ;GeoRAM Window
         iny
         bne *-18      ;256 Bytes

         lda subchsz+1
         and #%00000011
         bne *+7

         lda #$a6 ;Hash Block, each 1KB
         jsr chrout

         jsr incrampg

         dec subchsz+1
         bne loadfull

         lda subchsz+2
         beq loadpart

         dec subchsz+2
         jmp loadfull

loadpart ;Load Final Partial Page

         lda subchsz+0
         sta partsz+1

         ldy #0

         jsr chrin    ;Fetch Lo-Byte
         clc
         adc $d41b    ;C <- Dither Bit
         php
         
         jsr chrin    ;Fetch Hi-Byte
         eor #$80     ;Signed->Unsigned

         plp
         adc #0       ;Add Dither Bit

         sta $de00,y  ;GeoRAM Window
         iny
partsz   cpy #$ff           ;Self-Mod
         bne *-20

         ;Close File
         ;Ignoring extra chunks

         jmp closfile
         .bend

;----[ Playback Control ]---------------

;Routines:
;
; resume - Resume Playback
; pause  - Pause Playback
; dostop - Pause and Rewind
; rewind - Reset Play Index
; back5  - Skip Back     5 Seconds
; fwrd10 - Skip Forward 10 Seconds
;

playing  .byte 1          ;Playing State
repeat   .byte 0          ;Repeat  State

resume   ;Resume Playback
         .block
         lda playing
         beq *+3
         rts            ;Already Playing

         lda #1
         sta playing    ;Mark as Playing

         ;Turn off the screen.
         lda #%00001011
         sta $d011

         ;Set current GeoRAM Page
         ldx playidx+1
         lda playidx+2
         jsr setrampg

         ;Leave playidx unchanged

         ;Enable CIA2 Timer A NMI

         lda #%10000001 ;Enable Timer A
         sta $dd0d

         rts
         .bend

pause    ;Pause Playback
         .block
         lda playing
         bne *+3
         rts             ;Already Paused

         lda #0
         sta playing     ;Mark as Paused

         ;Leave playidx unchanged

         ;Prevent CIA2 Timer A NMI

         lda #%00011111 ;Clear All
         sta $dd0d

         rts
         .bend

dostop   ;Pause and Rewind
         .block

         ;Turn on the screen.
         lda #%00011011
         sta $d011

         jsr pause
         jmp rewind
         .bend

rewind   ;Reset Playback Index
         .block
         lda playing
         pha
         jsr pause

         lda #$00
         sta playidx+0
         sta playidx+1
         sta playidx+2
         sta playidx+3

         pla
         beq *+5
         jsr resume

         rts
         .bend

back5    ;Skip Back 5 Seconds
         .block
         lda playing
         pha
         jsr pause

         ;playidx -= byterate*5

         ldx #4

next     ;playidx -= byterate
         sec

         lda playidx+0
         sbc byterate+0
         sta playidx+0

         lda playidx+1
         sbc byterate+1
         sta playidx+1

         lda playidx+2
         sbc byterate+2
         sta playidx+2

         lda playidx+3
         sbc byterate+3
         sta playidx+3

         dex
         bpl next

         ;if playidx > datasz
         ;  playidx <- 0,0,0,0

         lda playidx+3
         cmp datasz+3
         bcc valid
         bne under

         lda playidx+2
         cmp datasz+2
         bcc valid
         bne under

         lda playidx+1
         cmp datasz+1
         bcc valid
         bne under

         lda playidx+0
         cmp datasz+0
         bcc valid
         beq valid

under    ;Underflew, reset to zero
         lda #0
         sta playidx+0
         sta playidx+1
         sta playidx+2
         sta playidx+3

valid    pla
         beq *+5
         jsr resume

         rts
         .bend

fwrd10   ;Skip Forward 10 Seconds
         .block
         lda playing
         pha
         jsr pause

         ;playidx += byterate*10

         ldx #9

next     ;playidx += byterate
         clc

         lda playidx+0
         adc byterate+0
         sta playidx+0

         lda playidx+1
         adc byterate+1
         sta playidx+1

         lda playidx+2
         adc byterate+2
         sta playidx+2

         lda playidx+3
         adc byterate+3
         sta playidx+3

         dex
         bpl next

         ;if playidx > datasz
         ;  playidx <- datasz
         ;  pause
         ;else
         ;  resume

         lda playidx+3
         cmp datasz+3
         bcc valid
         bne over

         lda playidx+2
         cmp datasz+2
         bcc valid
         bne over

         lda playidx+1
         cmp datasz+1
         bcc valid
         bne over

         lda playidx+0
         cmp datasz+0
         bcc valid
         beq valid

over     ;Overflew. Set to datasz.
         lda datasz+0
         sta playidx+0

         lda datasz+1
         sta playidx+1

         lda datasz+2
         sta playidx+2

         lda datasz+3
         sta playidx+3

         pla
         rts ;Don't resume

valid    pla
         beq *+5
         jsr resume

         rts
         .bend


;----[ NMI Playback ]-------------------

;Sample Rate Table

;NTSC Clock Rate: 1022727Hz
; PAL Clock Rate:  985248Hz

;Rate      NTSC     PAL
;22050Hz   46.38c   44.68c
;16000Hz   63.92c   61.58c
;11025Hz   92.76c   89.36c
; 8000Hz  127.84c  123.16c

         ;     lo  lmi hmi hi ntsc pal
srtab    .byte $22,$56,$00,$00,$2e,$2d
         .byte <str22rat,>str22rat

         .byte $80,$3e,$00,$00,$40,$3e
         .byte <str16rat,>str16rat

         .byte $11,$2b,$00,$00,$5d,$59
         .byte <str11rat,>str11rat

         .byte $40,$1f,$00,$00,$80,$7b
         .byte <str80rat,>str80rat

;NMI Timing

;Initial NMI Sequence: 7
;KERNAL NMI Vector:    7

play1chn ;Output a Mono Sample
         .block

         ;Read CIA2 Interupt Register
         bit $dd0d   ;Ready for next NMI

                     ;18

         ;Backup CPU Registers
         pha         ;3
         txa         ;2
         pha         ;3

         ldx playidx ;3

         ;Update playidx

         inc playidx  ;5
         beq skipsamp ;2 not taken

         lda $de00,x ;4 ;A <- Sample

         ldx #%00000011 ;Left Channel
         stx $dd00
         sta $dd01   ;10

         ldx #%00000111 ;Right Channel
         stx $dd00
         sta $dd01   ;10

         ;Restore CPU Registers
         pla         ;4
         tax         ;2
         pla         ;4

         rti         ;6

                  ;---------------------
                  ;  No RAM Page Change
                  ;
                  ;  76 Cycles

skipsamp
         ;Page Complete, Update RAM Page
                      ;37 up to here

         inc playidx+1 ;5
         bne *+4       ;2
         inc playidx+2 ;5

         lda geopghi   ;3
         ldx geopglo   ;3

         sta $dfff     ;4
         stx $dffe     ;4

         ;Restore CPU Registers
         pla           ;4
         tax           ;2
         pla           ;4

         rti           ;6
                  ;---------------------
                  ;    79 Cycles
                  ;
                  ;If RAM Page Changes
         .bend


play2chn ;Output a Stereo Sample
         .block

         ;Read CIA2 Interupt Register
         bit $dd0d   ;Ready for next NMI

                     ;18

         ;Backup CPU Registers
         pha         ;3
         txa         ;2
         pha         ;3

         ldx playidx ;3

         ;Update playidx

         inx          ;2
         inx          ;2

         stx playidx  ;3
         beq skipsamp ;2 not taken

         lda #%00000011 ;Left Channel
         sta $dd00
         lda $de00-2,x  ;A <- Sample L
         sta $dd01    ;15 (pg boundary)

         lda #%00000111 ;Right Channel
         sta $dd00
         lda $de00-1,x ;A <- Sample R
         sta $dd01    ;15 (pg boundary)

         ;Restore CPU Registers
         pla         ;4
         tax         ;2
         pla         ;4

         rti         ;6

                  ;---------------------
                  ;  No RAM Page Change
                  ;
                  ;  84 Cycles

skipsamp
         ;Page Complete, Update RAM Page
                      ;39 up to here

         inc playidx+1 ;5
         bne *+4       ;2
         inc playidx+2 ;5

         lda geopghi   ;3
         ldx geopglo   ;3

         sta $dfff     ;4
         stx $dffe     ;4

         ;Restore CPU Registers
         pla           ;4
         tax           ;2
         pla           ;4

         rti           ;6
                  ;---------------------
                  ;    81 Cycles
                  ;
                  ;If RAM Page Changes
         .bend

;----[ Memory Manage ]------------------

incrampg ;Increment GeoRAM Page
         inc cnframpg-2
         bne *+5
         inc cnframpg-1

         ldx cnframpg-2
         lda cnframpg-1

         jmp setrampg

         .byte 0,0

cnframpg ;Config GeoRAM Page Sequence
         ;X -> Lo Byte
         ;A -> Hi Byte
         stx cnframpg-2
         sta cnframpg-1

         ;Fallthrough...

setrampg ;Set GeoRAM Page
         ;X -> Lo Byte
         ;A -> Hi Byte
         .block

         stx lobyte

         asl lobyte
         rol a
         asl lobyte
         rol a

         sta $dfff
         stx $dffe

         rts
         .bend


;----[ Digimax Tests ]------------------

sawtooth ;Play Infinite Sawtooth Wave
         .block

         ldx #$00

next     inx
         inx
         inx

         lda #%00000011 ;Left Channel
         sta $dd00

         stx $dd01

         lda #%00000111 ;Right Channel
         sta $dd00

         stx $dd01

         ldy #0

delay    iny
         cpy #$01
         bne delay

         jmp next
         .bend
