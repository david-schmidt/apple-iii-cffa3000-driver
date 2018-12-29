; CFFA3000

;          .TITLE "Apple /// Compact Flash For Apple 3000 (CFFA3000) Driver"
           .PROC  CFFA3000

                .feature labels_without_colons
                .setcpu "6502"
                .reloc

DriverVersion   = $1000           ; Version number
DriverMfgr      = $4453           ; Driver Manufacturer - DS

;
; SOS Equates
;
ExtPG     = $1401                 ; Driver extended bank address offset
AllocSIR  = $1913                 ; Allocate system internal resource
SELC800   = $1922                 ; Enable Expansion ROM Space
SysErr    = $1928                 ; Report error to system
EReg      = $FFDF                 ; Environment register
ReqCode   = $C0                   ; Request code
SOS_Unit  = $C1                   ; Unit number
SosBuf    = $C2                   ; SOS buffer pointer (2 bytes)
ReqCnt    = $C4                   ; Requested byte count
CtlStat   = $C2                   ; Control/status code
CSList    = $C3                   ; Control/status list pointer
SosBlk    = $C6                   ; Starting block number
QtyRead   = $C8                   ; Pointer to bytes read return by D_READ
;
; Our temps in zero page
;
Count     = $CD                   ; 2 bytes
C800Ptr   = $CF                   ; 2 bytes
;
; Parameter block specific to current SOS request
;
CFFAUnit  = $D1
Num_Blks  = $D4                   ; 2 bytes lsb, msb
DataBuf   = $D6                   ; 2 bytes
;
; SOS Error Codes
;
XDNFERR   = $10                   ; Device not found
XBADDNUM  = $11                   ; Invalid device number
XREQCODE  = $20                   ; Invalid request code
XCTLCODE  = $21                   ; Invalid control/status code
XCTLPARAM = $22                   ; Invalid control/status parameter
XNORESRC  = $25                   ; Resources not available
XBADOP    = $26                   ; Invalid operation
XIOERROR  = $27                   ; I/O error
XNODRIVE  = $28                   ; Drive not connected
XBYTECNT  = $2C                   ; Byte count not a multiple of 512
XBLKNUM   = $2D                   ; Block number to large
XDISKSW   = $2E                   ; Disk switched
XNORESET  = $33                   ; Device reset failed
;
; CFFA3000 Constants
;
kCmd3K_SetEnvironment = $01       ; Set X = 0xa3 for Apple ///
kCmd3K_Status         = $10
kCmd3K_Read           = $11
kCmd3K_Write          = $12
CFFA3K_API            = $CFED     ; CFFA3000 API entry point
shUnitNumber          = $CFDF
shStatusByte          = $CFE0
shBlockNumber         = $CFE1
;
; Switch Macro
;
                 .MACRO  SWITCH index,bounds,adrs_table,noexec      ;See SOS Reference
                 .IFNBLANK index        ;If PARM1 is present,
                 LDA     index          ; load A with switch index
                 .ENDIF
                 .IFNBLANK bounds       ;If PARM2 is present,
                 CMP     #bounds+1      ; perform bounds checking
                 BCS     @110           ; on switch index
                 .ENDIF
                 ASL     A              ;Multiply by 2 for table index
                 TAY
                 LDA     adrs_table+1,Y ;Get switch address from table
                 PHA                    ; and push onto Stack
                 LDA     adrs_table,Y
                 PHA
                 .IFBLANK noexec
                 ; .IF noexec <> '*'     ;If PARM4 is omitted,
                   RTS                    ; exit to code
                 ; .ENDIF
                .ENDIF
@110
                 .ENDMACRO

          .SEGMENT "TEXT"
;
; Comment Field of driver
;
          .WORD  $FFFF ; Signal that we have a comment
          .WORD  COMMENT_END - COMMENT
COMMENT:  .BYTE  "Apple /// CFFA3000 (Compact Flash For Apple 3000) Driver by David Schmidt 2018"
COMMENT_END:

          .SEGMENT "DATA"
;------------------------------------
;
; Device identification Block (DIB) - Volume #0
;
;------------------------------------

DIB_0     .WORD     DIB_1            ; Link pointer
          .WORD     Entry            ; Entry pointer
          .BYTE     $0B              ; Name length byte
          .BYTE     ".CFFA3000D1    "; Device name
          .BYTE     $80              ; Active, no page alignment
DIB0_Slot .BYTE     $01              ; Slot number
          .BYTE     $00              ; Unit number
          .BYTE     $F1              ; Type
          .BYTE     $10              ; Subtype
          .BYTE     $00              ; Filler
DIB0_Blks .WORD     $0000            ; # Blocks in device
          .WORD     DriverMfgr       ; Manufacturer
          .WORD     DriverVersion    ; Driver version
          .WORD     $0000            ; DCB length followed by DCB
;
; Device identification Block (DIB) - Volume #1
; Page alignment begins here
;
DIB_1     .WORD     DIB_2            ; Link pointer
          .WORD     Entry            ; Entry pointer
          .BYTE     $0B              ; Name length byte
          .BYTE     ".CFFA3000D2    "; Device name
          .BYTE     $80              ; Active
DIB1_Slot .BYTE     $01              ; Slot number
          .BYTE     $01              ; Unit number
          .BYTE     $F1              ; Type
          .BYTE     $10              ; Subtype
          .BYTE     $00              ; Filler
DIB1_Blks .WORD     $0000            ; # Blocks in device
          .WORD     DriverMfgr       ; Driver manufacturer
          .WORD     DriverVersion    ; Driver version
          .WORD     $0000            ; DCB length followed by DCB
;
; Device identification Block (DIB) - Volume #2
;
DIB_2     .WORD     DIB_3            ; Link pointer
          .WORD     Entry            ; Entry pointer
          .BYTE     $0B              ; Name length byte
          .BYTE     ".CFFA3000D3    "; Device name
          .BYTE     $80              ; Active
DIB2_Slot .BYTE     $01              ; Slot number
          .BYTE     $02              ; Unit number
          .BYTE     $F1              ; Type
          .BYTE     $10              ; Subtype
          .BYTE     $00              ; Filler
DIB2_Blks .WORD     $0000            ; # Blocks in device
          .WORD     DriverMfgr       ; Driver manufacturer
          .WORD     DriverVersion    ; Driver version
          .WORD     $0000            ; DCB length followed by DCB
;
; Device identification Block (DIB) - Volume #3
;
DIB_3     .WORD     DIB_4            ; Link pointer
          .WORD     Entry            ; Entry pointer
          .BYTE     $0B              ; Name length byte
          .BYTE     ".CFFA3000D4    "; Device name
          .BYTE     $80              ; Active
DIB3_Slot .BYTE     $01              ; Slot number
          .BYTE     $03              ; Unit number
          .BYTE     $F1              ; Type
          .BYTE     $10              ; Subtype
          .BYTE     $00              ; Filler
DIB3_Blks .WORD     $0000            ; # Blocks in device
          .WORD     DriverMfgr       ; Driver manufacturer
          .WORD     DriverVersion    ; Driver version
          .WORD     $0000            ; DCB length followed by DCB
;
; Device identification Block (DIB) - Volume #4
;
DIB_4     .WORD     DIB_5            ; Link pointer
          .WORD     Entry            ; Entry pointer
          .BYTE     $0B              ; Name length byte
          .BYTE     ".CFFA3000D5    "; Device name
          .BYTE     $80              ; Active
DIB4_Slot .BYTE     $01              ; Slot number
          .BYTE     $04              ; Unit number
          .BYTE     $F1              ; Type
          .BYTE     $10              ; Subtype
          .BYTE     $00              ; Filler
DIB4_Blks .WORD     $0000            ; # Blocks in device
          .WORD     DriverMfgr       ; Driver manufacturer
          .WORD     DriverVersion    ; Driver version
          .WORD     $0000            ; DCB length followed by DCB
;
; Device identification Block (DIB) - Volume #5
;
DIB_5     .WORD     DIB_6            ; Link pointer
          .WORD     Entry            ; Entry pointer
          .BYTE     $0B              ; Name length byte
          .BYTE     ".CFFA3000D6    "; Device name
          .BYTE     $80              ; Active
DIB5_Slot .BYTE     $01              ; Slot number
          .BYTE     $05              ; Unit number
          .BYTE     $F1              ; Type
          .BYTE     $10              ; Subtype
          .BYTE     $00              ; Filler
DIB5_Blks .WORD     $0000            ; # Blocks in device
          .WORD     DriverMfgr       ; Driver manufacturer
          .WORD     DriverVersion    ; Driver version
          .WORD     $0000            ; DCB length followed by DCB
;
; Device identification Block (DIB) - Volume #6
;
DIB_6     .WORD     DIB_7            ; Link pointer
          .WORD     Entry            ; Entry pointer
          .BYTE     $0B              ; Name length byte
          .BYTE     ".CFFA3000D7    "; Device name
          .BYTE     $80              ; Active
DIB6_Slot .BYTE     $01              ; Slot number
          .BYTE     $06              ; Unit number
          .BYTE     $F1              ; Type
          .BYTE     $10              ; Subtype
          .BYTE     $00              ; Filler
DIB6_Blks .WORD     $0000            ; # Blocks in device
          .WORD     DriverMfgr       ; Driver manufacturer
          .WORD     DriverVersion    ; Driver version
          .WORD     $0000            ; DCB length followed by DCB
;
; Device identification Block (DIB) - Volume #7
;
DIB_7     .WORD     $0000            ; Link pointer
          .WORD     Entry            ; Entry pointer
          .BYTE     $0B              ; Name length byte
          .BYTE     ".CFFA3000D8    "; Device name
          .BYTE     $80              ; Active
DIB7_Slot .BYTE     $01              ; Slot number
          .BYTE     $07              ; Unit number
          .BYTE     $F1              ; Type
          .BYTE     $10              ; Subtype
          .BYTE     $00              ; Filler
DIB7_Blks .WORD     $0000            ; # Blocks in device
          .WORD     DriverMfgr       ; Driver manufacturer
          .WORD     DriverVersion    ; Driver version
          .WORD     $0000            ; DCB length followed by DCB

;------------------------------------
;
; Local storage locations
;
;------------------------------------

LastOP     .RES      $08, $FF             ; Last operation for D_REPEAT calls
SIR_Addr   .WORD     SIR_Tbl
SIR_Tbl    .RES      $05, $00
SIR_Len     =      *-SIR_Tbl
RdBlk_Proc .WORD     $0000
WrBlk_Proc .WORD     $0000
MaxUnits   .BYTE     $00                  ; The maximum number of units
DCB_Idx    .BYTE     $00                  ; DCB 0's blocks
           .BYTE     DIB1_Blks-DIB0_Blks  ; DCB 1's blocks
           .BYTE     DIB2_Blks-DIB0_Blks  ; DCB 2's blocks
           .BYTE     DIB3_Blks-DIB0_Blks  ; DCB 3's blocks
           .BYTE     DIB4_Blks-DIB0_Blks  ; DCB 4's blocks
           .BYTE     DIB5_Blks-DIB0_Blks  ; DCB 5's blocks
           .BYTE     DIB6_Blks-DIB0_Blks  ; DCB 6's blocks
           .BYTE     DIB7_Blks-DIB0_Blks  ; DCB 7's blocks
SigCF3K    .BYTE     "CF3K"               ; CFFA3000 signature in memory
CardIsOK   .BYTE     $00                  ; Have we found the CFFA3000 yet?
LastError  .BYTE     $00                  ; Recent error RC from CFFA3000

;------------------------------------
;
; Driver request handlers
;
;------------------------------------

Entry     LDA DIB0_Slot
          JSR SELC800                ; Turn on C800 ROM space from our slot
          LDA SOS_Unit
          STA CFFAUnit
          INC CFFAUnit               ; CFFA3000 unit is 1-based vs. SOS 0-based
          JSR Dispatch               ; Call the dispatcher
          LDX SOS_Unit               ; Get drive number for this unit
          LDA ReqCode                ; Keep request around for D_REPEAT
          STA LastOP,X               ; Keep track of last operation
          LDA #$00
          JSR SELC800                ; Unselect C800 ROM space
          RTS
;
; The Dispatcher.  Note that if we came in on a D_INIT call,
; we do a branch to Dispatch normally.  
; Dispatch is called as a subroutine!
;
DoTable   .WORD     DRead-1          ; 0 Read request
          .WORD     DWrite-1         ; 1 Write request
          .WORD     DStatus-1        ; 2 Status request
          .WORD     DControl-1       ; 3 Control request
          .WORD     BadReq-1         ; 4 Unused
          .WORD     BadReq-1         ; 5 Unused
          .WORD     BadOp-1          ; 6 Open - valid for character devices
          .WORD     BadOp-1          ; 7 Close - valid for character devices
          .WORD     DInit-1          ; 8 Init request
          .WORD     DRepeat-1        ; 9 Repeat last read or write request
Dispatch  SWITCH    ReqCode,9,DoTable ; Serve the request
;
; Dispatch errors
;
BadReq    LDA #XREQCODE              ; Bad request code!
          JSR SysErr                 ; Return to SOS with error in A
BadOp     LDA #XBADOP                ; Invalid operation!
          JSR SysErr                 ; Return to SOS with error in A

;
; D_REPEAT - repeat the last D_READ or D_WRITE call
;
DRepeat   LDX SOS_Unit
          LDA LastOP,X               ; Recall the last thing we did
          CMP #$02                   ; Looking for operation < 2
          BCS BadOp                  ; Can only repeat a read or write
          STA ReqCode
          JMP Dispatch

NoDevice  LDA #XDNFERR               ; Device not found
          JSR SysErr                 ; Return to SOS with error in A

;
; D_INIT call processing - called once each for all volumes.
;
DInit     LDA SOS_Unit               ; Check if we're initting the zeroeth unit
          BNE UnitInit               ; No - then skip the signature check

CheckSig  LDA #$C0                   ; Form a $CsF6 address, where s = slot #
          ORA DIB0_Slot              ; Add in slot number
          STA Count+1
          LDA #$F6
          STA Count
          LDY #$03
@1        LDA (Count),Y
          CMP SigCF3K,Y              ; Check for 'CF3K' signature in our slot
          BNE NoDevice               ; No device if all four bytes don't match
          DEY
          BPL @1

          LDA DIB0_Slot              ; Found a CFFA3000!
          ORA #$10                   ; SIR = 16+slot#
          STA SIR_Tbl
          STA CardIsOK               ; Remember that we found the card
          LDA #SIR_Len
          LDX SIR_Addr
          LDY SIR_Addr+1
          JSR AllocSIR               ; This one's mine!
          BCS NoDevice

          LDX #$A3
          LDA #kCmd3K_SetEnvironment
          JSR CFFA3K_API             ; Get CFFA3000 set up for us
          BCS NoDevice
          LDA #$00
          STA shUnitNumber
          LDA #kCmd3K_Status
          JSR CFFA3K_API             ; How many units can we expect at maximum?
          BCS NoDevice
          LDA shUnitNumber
          STA MaxUnits

UnitInit
          LDA CardIsOK               ; Did we previously find a card?
          BEQ NoDevice               ; If not... then bail
          LDA CFFAUnit               ; Which unit did we get called with?
          STA shUnitNumber
          LDA #kCmd3K_Status         ; Ask for the status of this unit
          JSR CFFA3K_API
          LDA #$00
          STA shBlockNumber
          STA shBlockNumber+1
          STA shBlockNumber+2
          STA shBlockNumber+3
          LDA #kCmd3K_Read           ; Do a dummy read of block zero
          JSR CFFA3K_API             ;   to clear out a device switched error
          LDA #kCmd3K_Status
          JSR CFFA3K_API
          BCS NoDevice

SaveCapacity
          LDX SOS_Unit               ; Get the stats on this unit
          LDY DCB_Idx,X
          LDA shBlockNumber
          STA DIB0_Blks,Y
          LDA shBlockNumber+1
          STA DIB0_Blks+1,Y
          LDA shBlockNumber+2
          CMP #$01                   ; Do we have 65536 (or more) blocks?
          BNE UIDone
          LDA #$FF                   ; Then we really only have 65535 blocks.
          STA DIB0_Blks,Y
          STA DIB0_Blks+1,Y

UIDone    CLC
          RTS

;
; D_READ call processing
;
DRead
          LDA CardIsOK               ; Did we previously find a card?
          BNE DReadGo
          JMP NoDevice               ; If not... then bail
DReadGo          
          JSR CkCnt                  ; Checks for validity, aborts if not
          JSR CkUnit                 ; Checks for unit below unit max
          LDA #$00                   ; Zero # bytes read
          STA Count                  ; Local count of bytes read
          STA Count+1
          TAY
          STA (QtyRead),Y            ; Userland count of bytes read
          INY
          STA (QtyRead),Y            ; Msb of userland bytes read
          LDA Num_Blks               ; Check for Num_Blks greater than zero
          ORA Num_Blks+1
          BEQ ReadExit
          JSR FixUp                  ; Correct for addressing anomalies
          JSR Read_Block             ; Transfer a block to/from the disk
          LDY #$00
          LDA Count                  ; Local count of bytes read
          STA (QtyRead),y            ; Update # of bytes actually read
          INY
          LDA Count+1
          STA (QtyRead),y
          BCS IO_Error               ; An error occurred
ReadExit  RTS                        ; Exit read routines
IO_Error  LDA #XIOERROR              ; I/O error
          JSR SysErr                 ; Return to SOS with error in A

;
; D_WRITE call processing
;
DWrite
          LDA CardIsOK               ; Did we previously find a card?
          BNE DWriteGo
          JMP NoDevice               ; If not... then bail
          
DWriteGo
          JSR CkCnt                  ; Checks for validity
          JSR CkUnit                 ; Checks for unit below unit max
CWrite    LDA Num_Blks               ; Check for Num_Blks greater than zero
          ORA Num_Blks+1
          BEQ WriteExit              ; Quantity to write is zero - so done
          JSR FixUp
          JSR Write_Block
          BCS IO_Error
WriteExit RTS

;
; D_STATUS call processing
;  $00 = Driver Status
;  $FE = Return preferrred bitmap location ($FFFF)
;
DStatus
          LDA CardIsOK               ; Did we previously find a card?
          BNE DStatusGo
          JMP NoDevice               ; If not... then bail

DStatusGo
          LDA CFFAUnit               ; Get the unit number we're talking about
          STA shUnitNumber
          LDA CtlStat                ; Which status code to run?
          BNE DS0
          LDA #kCmd3K_Status         ; Status code 0 - return the status byte
          JSR CFFA3K_API
          BCS DS1
          LDY #$00
          LDA shStatusByte
          STA (CSList),Y
          JSR SaveCapacity
          CLC
          RTS
DS1       CMP #$2F                   ; Did we get a fancy new $2f error?
          BNE DS2
          LDA #XDNFERR               ; Then change it to XDNFERR instead.
DS2       JSR SysErr                 ; Return to SOS with error in A
DS0       CMP #$FE
          BNE DSWhat

          LDY #$00                   ; Return preferred bit map locations.
          LDA #$FF                   ; We return FFFF, don't care
          STA (CSList),Y
          INY
          STA (CSList),Y       
          CLC
          RTS

DSWhat    LDA #XCTLCODE              ; Control/status code no good
          JSR SysErr                 ; Return to SOS with error in A


;
; D_CONTROL call processing
;  $00 = Reset device
;  $FE = Perform media formatting
;
DControl
          LDA CardIsOK               ; Did we previously find a card?
          BNE DContGo
          JMP NoDevice               ; If not... then bail
          
DContGo   LDA CtlStat                ; Control command
          BEQ CReset
          CMP #$FE                   ; Format?
          BEQ DCFormat
          JMP DCWhat                 ; Control code no good!
CReset    JSR UnitInit               ; Reset this unit
          BCS DCNoReset
DCDone    RTS          
DCNoReset LDA #XNORESET              ; Things went bad after reset
          JSR SysErr                 ; Return to SOS with error in A
DCWhat    LDA #XCTLCODE              ; Control/status code no good
          JSR SysErr                 ; Return to SOS with error in A

DCFormat
;
; Write Block0, Block1 to disk
;
          LDX SOS_Unit               ; Get the stats on this unit
          LDY DCB_Idx,X
          LDA DIB0_Blks,Y
          STA VolBlks                ; Fill VolBlks with capacity
          LDA DIB0_Blks+1,Y
          STA VolBlks+1
          LDA #$00
          STA VolBlks+2
          STA shBlockNumber
          STA shBlockNumber+1
          STA shBlockNumber+2
          STA shBlockNumber+3

          JSR ZeroFillC800
          LDA CFFAUnit               ; Get the unit number we're talking about
          STA shUnitNumber
          LDA #kCmd3K_Write
          JSR CFFA3K_API
          BCS Error
          INC shBlockNumber
          LDA #kCmd3K_Write
          JSR CFFA3K_API
          BCS Error
          JSR FormatFill
          JSR Catalog                ; Write Directory information to the disk
          RTS
Error     SEC
          JSR SysErr                 ; Return to SOS with error in A

;------------------------------------
;
; Utility routines
;
;------------------------------------

;
; Read_Block - Read requested blocks from device into memory
;
Read_Block
          LDA SosBuf                 ; Copy out buffer pointers
          STA DataBuf
          LDA SosBuf+1
          STA DataBuf+1
          LDA SosBuf+ExtPG
          STA DataBuf+ExtPG

          LDA CFFAUnit
          STA shUnitNumber           ; Now we have the CFFA3000 unit

          LDA SosBlk
          STA shBlockNumber
          LDA SosBlk+1
          STA shBlockNumber+1
          LDA #$00
          STA shBlockNumber+2
          STA shBlockNumber+3
Read3k    LDA #kCmd3K_Read
          JSR CFFA3K_API
          BCC @1                     ; Branch past error
          STA LastError
          CMP #XDISKSW
          BNE @0
          JSR ZeroUnit
          JSR UnitInit               ; Re-initialize this unit
          JMP Dispatch               ; Go around again!
@0        LDA LastError
          JSR SysErr                 ; Return to SOS with error in A
@1        LDA #$00
          STA C800Ptr
          LDA #$C8
          STA C800Ptr+1              ; Establish a pointer to C800
          LDY #$00
@2        LDA (C800Ptr),Y
          STA (DataBuf),Y
          INY
          BNE @2
          JSR IncrAdr
@3        LDA (C800Ptr),Y
          STA (DataBuf),Y
          INY
          BNE @3
          JSR IncrAdr

          DEC Num_Blks               ; Did we get what was asked for?
          BNE RdBlk2
          DEC Num_Blks+1
          BPL RdBlk2
          CLC
          RTS

RdBlk2    INC shBlockNumber          ; 16-bit increment of block number
          BNE Read3k
          INC shBlockNumber+1
          JMP Read3k

;
; Write_Block - write memory out to requested blocks
;
Write_Block
          LDA SosBuf                 ; Copy out buffer pointers
          STA DataBuf
          LDA SosBuf+1
          STA DataBuf+1
          LDA SosBuf+ExtPG
          STA DataBuf+ExtPG

          LDA SOS_Unit
          STA shUnitNumber
          CLC
          INC shUnitNumber           ; Now we have the CFFA3000 unit

          LDA SosBlk
          STA shBlockNumber
          LDA SosBlk+1
          STA shBlockNumber+1
          LDA #$00
          STA shBlockNumber+2
          STA shBlockNumber+3

Write3k   LDA #$00
          TAY
          STA C800Ptr
          LDA #$C8
          STA C800Ptr+1              ; Establish a pointer to C800
@2        LDA (DataBuf),Y
          STA (C800Ptr),Y
          INY
          BNE @2
          JSR IncrAdr
@3        LDA (DataBuf),Y
          STA (C800Ptr),Y
          INY
          BNE @3
          
          LDA #kCmd3K_Write
          JSR CFFA3K_API
          BCC @1                     ; Branch past error
          STA LastError
          CMP #XDISKSW
          BNE @0
          JSR ZeroUnit
          JSR UnitInit               ; Re-initialize this unit
          JMP Dispatch               ; Go around again!
@0        LDA LastError
          JSR SysErr                 ; Return to SOS with error in A
 
@1        JSR IncrAdr
          DEC Num_Blks               ; Did we put what was asked for?
          BNE WrBlk2                 ; Not done yet... go around again
          DEC Num_Blks+1             ; (16 bit decrement)
          BPL WrBlk2                 ; Not done yet... go around again
          CLC
          RTS                        ; We're done

WrBlk2    INC shBlockNumber          ; 16-bit increment of block number
          BNE Write3k
          INC shBlockNumber+1
          JMP Write3k

;
; ZeroUnit - clear out the capacity bytes of this unit
;
ZeroUnit  LDX SOS_Unit
          LDY DCB_Idx,X
          LDA #$00
          STA DIB0_Blks,Y
          STA DIB0_Blks+1,Y
          RTS
;
; Check ReqCnt to ensure it's a multiple of 512.
;
CkCnt     LDA ReqCnt                 ; Look at the lsb of bytes requested
          BNE @1                     ; No good!  lsb should be 00
          STA Num_Blks+1             ; Zero out the high byte of blocks
          LDA ReqCnt+1               ; Look at the msb
          LSR A                      ; Put bottom bit into carry, 0 into top
          STA Num_Blks               ; Convert bytes to number of blks to xfer
          BCC CvtBlk                 ; Carry is set from LSR to mark error.
@1        LDA #XBYTECNT
          JSR SysErr                 ; Return to SOS with error in A

;
; Test for valid block number; abort on error
;
CvtBlk    LDX SOS_Unit
          LDY DCB_Idx,X
          SEC
          LDA DIB0_Blks+1,Y          ; Blocks on unit msb
          SBC SosBlk+1               ; User requested block number msb
          BVS BlkErr                 ; Not enough blocks on device for request
          BEQ @1                     ; Equal msb; check lsb.
          RTS                        ; Greater msb; we're ok.
@1        LDA DIB0_Blks,Y            ; Blocks on unit lsb
          SBC SosBlk                 ; User requested block number lsb
          BVS BlkErr                 ; Not enough blocks on device for request
          RTS                        ; Equal or greater msb; we're ok.
BlkErr    LDA #XBLKNUM
          JSR SysErr                 ; Return to SOS with error in A


IncrAdr   INC C800Ptr+1              ; Increment buffer MSB from CFFA3000
          INC Count+1                ; Increment byte count MSB
BumpAdr   INC DataBuf+1              ; Increment DataBuf MSB in userland

;
; Fix up the buffer pointer to correct for addressing
; anomalies.  We just need to do the initial checking
; for two cases:
; 00xx bank N -> 80xx bank N-1
; 20xx bank 8F if N was 0
; FDxx bank N -> 7Dxx bank N+1
; If pointer is adjusted, return with carry set
;
FixUp     LDA DataBuf+1              ; Look at msb
          BEQ @1                     ; That's one!
          CMP #$FD                   ; Is it the other one?
          BCS @2                     ; Yep. fix it!
          RTS                        ; Pointer unchanged, return carry clear.
@1        LDA #$80                   ; 00xx -> 80xx
          STA DataBuf+1
          DEC DataBuf+ExtPG          ; Bank N -> band N-1
          LDA DataBuf+ExtPG          ; See if it was bank 0
          CMP #$7F                   ; (80) before the DEC.
          BNE @3                     ; Nope! all fixed.
          LDA #$20                   ; If it was, change both
          STA DataBuf+1              ; Msb of address and
          LDA #$8F
          STA DataBuf+ExtPG          ; Bank number for bank 8F
          RTS                        ; Return carry set
@2        AND #$7F                   ; Strip off high bit
          STA DataBuf+1              ; FDxx ->7Dxx
          INC DataBuf+ExtPG          ; Bank N -> bank N+1
@3        RTS                        ; Return carry set

CkUnit    LDA SOS_Unit               ; Checks for unit below unit max
          CMP MaxUnits
          BMI UnitOk
NoUnit    LDA #$11                   ; Report no unit to SOS
          JSR SysErr
UnitOk    CLC
          RTS

;
; Prepare BitMap and Link blocks for writing to disk
; Part of formatting support
;
FormatFill
          LDA #$05                   ; Block 5 on Disk
          STA shBlockNumber
          STA Storage                ; Length of DirTbl
          JSR ZeroFillC800
LLink     LDX Storage
          LDA DirTbl,X               ; Move Directory Link values into Buffer
          STA $C802                  ; Store next Directory block #
          DEX
          LDA DirTbl,X               ; Fetch another # from DirTbl
          STA $C800                  ; Store previous Directory block #
          DEX
          STX Storage
          LDA #kCmd3K_Write
          JSR CFFA3K_API             ; Write Directory Link values to disk
LDec      DEC shBlockNumber          ; Decrement MLI block number
          LDA shBlockNumber          ; See if MLIBlk = 2
          CMP #$02
          BNE LLink                  ; Process another Link block

;
; Calculate BitMap Size and cndo
; Part of formatting support
;
BlkCount                             ; Fill full pages first, then remainder
          LDA #$06                   ; First block to deal with: $06
          STA shBlockNumber
          CLC
          LDA VolBlks+1
          STA FullPages
          LDA VolBlks+2
          BEQ @1
          SEC 
@1        ROR FullPages              ; VolBlks is now divided by 512
          LSR FullPages              ; ... by 1024
          LSR FullPages              ; ... by 2048
          LSR FullPages              ; ... by 4096

          BEQ LastBlock              ; No full blocks?  Skip to remainder part.

          JSR FFFillC800             ; Set up to fill pages with $FFs
          LDA #kCmd3K_Write
          JSR CFFA3K_API
          LDA #$00
          STA BlkCnt
          STA $C800                  ; Mark first blocks as used
          STA $C801
          LDA #$03
          STA $C802

@2
          LDA #kCmd3K_Write
          JSR CFFA3K_API             ; Write Buffer BitMap to block on the disk
          LDA #$FF                   ; Mark first blocks as unused again
          STA $C800
          STA $C801
          STA $C802
          INC shBlockNumber
          INC BlkCnt
          LDA BlkCnt
          CMP FullPages
          BNE @2

LastBlock
          JSR BlkRemainder
          LDA #kCmd3K_Write
          JSR CFFA3K_API
          RTS

BlkRemainder
          JSR ZeroFillC800
          LDA VolBlks+1              ; Where # of blocks are stored
          LDX VolBlks
          LDY #$00
          STX Storage+1              ; Divide the # of blocks by 8 for bitmap
          LSR A                      ;   calculation
          ROR Storage+1
          LSR A
          ROR Storage+1
          LSR A
          ROR Storage+1
          STA Storage+2
BitMapCode
          LDA FullPages              ; Only tick off 7 blocks if
          BNE BitMapNotFirst         ;     this is the only page in the BAM
          LDA #$01                   ; Clear first 7 blocks (i.e. %00000001)
          STA (C800Ptr),Y
          JMP BitMapGo
BitMapNotFirst
          LDA #$FF
          STA (C800Ptr),Y
BitMapGo
          LDY Storage+1              ; Original low block count value
          BNE Jump11                 ; If it is 0 then make FF
          DEY                        ; Make FF
          DEC Storage+2              ; Make 256 blocks less one
          STY Storage+1              ; Make FF new low block value
Jump11    LDX Storage+2              ; High Block Value
          BNE Jump15                 ; If it isn't equal to 0 then branch
          LDY Storage+1
          JMP Jump19

Jump15    LDA #$C9                   ; Set the address of the upper part of
          STA C800Ptr+1              ;  Block in bitmap being created
          LDA #$FF
          LDY Storage+1              ; Using the low byte count
Jump20    DEY
          STA (C800Ptr),Y            ; Store them
          BEQ Jump17
          JMP Jump20
Jump17    DEY                        ; Fill in first part of block
          LDA #$C8
          STA C800Ptr+1
Jump19
          LDA #$FF
          DEY
          STA (C800Ptr),Y
          CPY #$01                   ; Except the first byte.
          BEQ Jump18
          JMP Jump19
Jump18    RTS

BlkCnt    .BYTE $00

;
; Catalog - Build a Directory Track
; Part of formatting support
;
Catalog   CLC
          LDA #$06
          ADC FullPages
          STA shBlockNumber
          LDA #kCmd3K_Write
          JSR CFFA3K_API             ; Write Buffer (BitMap) to block #6
          JSR ZeroFillC800
          LDY #$2A                   ; Move Block2 information to $C800
CLoop     LDA Block2,Y
          STA (C800Ptr),Y
          DEY
          BPL CLoop
          LDA #$02                   ; Write block #2 to the disk
          STA shBlockNumber
          LDA #kCmd3K_Write
          JSR CFFA3K_API
          RTS

;
; FillC800: clear buffer at $C800
;
FFFillC800
          LDA #$FF
          PHA
          JMP FillC800Go
ZeroFillC800
          LDA #$00
          PHA
FillC800Go
          LDA #$C8
          STA C800Ptr+1
          LDA #$00
          STA C800Ptr
          TAY
          LDX #$01                   ; Loop twice... 512 bytes
          PLA
FillLoop
          STA (C800Ptr),Y
          INY
          BNE FillLoop
          INC C800Ptr+1
          DEX
          BPL FillLoop

          LDA #$C8
          STA C800Ptr+1
          LDA #$00
          STA C800Ptr
          RTS

;
; Formatter Variable Storage Area
;
VolBlks  .BYTE $00, $00, $00         ; Number of blocks available
DirTbl   .BYTE $02, $04, $03         ; Linked list for directory blocks
         .BYTE $05, $04, $00
BitTbl   .BYTE $7f ; '01111111'      ; BitMap mask for bad blocks
         .BYTE $bf ; '10111111'
         .BYTE $df ; '11011111'
         .BYTE $ef ; '11101111'
         .BYTE $f7 ; '11110111'
         .BYTE $fb ; '11111011'
         .BYTE $fd ; '11111101'
         .BYTE $fe ; '11111110'
Storage  .BYTE $00, $00, $00         ; General purpose counter/storage byte
Pointer  .BYTE $00, $00              ; Storage for track count (8 blocks/track)
Track    .BYTE $00, $00              ; Track number being FORMATted
Sector   .BYTE $00, $00              ; Current sector number (max=16)
SlotF    .BYTE $00, $00              ; Slot/Drive of device to FORMAT
TRKcur   .BYTE $00, $00              ; Current track position
TRKdes   .BYTE $00, $00              ; Destination track position
TRKbeg   .BYTE $00                   ; Starting track number
TRKend   .BYTE $35                   ; Ending track number
FullPages
         .BYTE $00                   ; Number of BAM pages to fill
DevIndex .BYTE $00                   ; Space for index into DEVICES table
Util     .BYTE $00


Block2   .BYTE $00, $00, $03, $00    ; Image of block 2 - for $42 bytes
VolLen   .BYTE $F5                   ; $F0 + length of Volume Name
Volnam   .BYTE "BLANK          "     ; Volume Name
Reserved .BYTE $00, $00, $00, $00, $00, $00
UpLowCase
         .BYTE $00, $00
Datime   .BYTE $00, $00, $00, $00
Version  .BYTE $01
         .BYTE $00, $C3, $27, $0D
         .BYTE $00, $00, $06, $00

         .ENDPROC
         .END