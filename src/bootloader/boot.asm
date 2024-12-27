org 0x7c00
bits 16

%define ENDL 0x0D, 0x0A

;
; FAT12 header
;
jmp short start
; The line jmp short start does indeed jump to the start label in the bootloader code, but this jump does not affect the BPB and EBR. Here’s why:
; BPB and EBR are data: The BPB and EBR are just data structures placed at the beginning of the bootloader. They are not executable code. These data structures are part of the boot sector and are loaded into memory when the BIOS reads the first 512 bytes. The BIOS does not execute the BPB or EBR; it simply reads them to understand the structure of the filesystem and the boot process.
; The jump (jmp short start): This instruction just causes the CPU to start executing the bootloader code starting from the start label. The jmp does not skip over the BPB and EBR data; it simply tells the CPU to start executing code at the start label, which is after the BPB and EBR.
; So, the BIOS reads the first 512 bytes, and that includes the BPB and EBR (which are just data). Then, the BIOS loads the bootloader code (after the BPB and EBR) into memory and starts executing from the start label.
nop
bdp_oem:                    db 'MSWIN4.1'           ; 8B
bdp_bytes_per_sector:       dw 512
bdb_sectors_per_cluster:    db 1
bdb_reserved_sectors:       dw 1
bdb_fat_count:              db 2
bdb_dir_entries_count:      dw 0E0h
bdb_total_sectors:          dw 2880                 ; 2880 * 512 = 1.44MB
bdb_media_descriptor_type:  db 0F0h                 ; F0 = 3.5" Floppy
bdb_sectors_per_fat:        dw 9                    ; 9 sectors/fat
bdb_sectors_per_track:      dw 18
bdb_heads:                  dw 2
bdb_hidden_sectors:         dd 0
bdb_large_sector_count:     dd 0

; extended boot record
ebr_drive_number:           db 0                    ; 0x00 floppy, 0x80 hdd
                            db 0                    ; reserved
ebr_signature:              db 29h
ebr_volume_id:              db 12h, 34h, 56h, 78h   ; serial number
ebr_volume_label:           db 'OSTRYX     '        ; 11B padded with space
ebr_system_id:              db 'FAT12   '           ; 8B

;
; code goes here
;

start:
    jmp main
;
; Prints a string to the screen
; Params:
;   - ds:si points to string
;
puts:
    ; backup registers before modification
    push si
    push ax
    push bx

.loop:
    lodsb              ; loads next char. in al 
    or al, al           ; verify if next char. is null
    jz .done

    mov ah, 0x0e        ; call bios interrupt(video)
    mov bh, 0
    int 0x10

    jmp .loop

.done:
    pop bx
    pop ax
    pop si
    ret

main:

    ; setup data segments
    mov ax, 0               ; can't write to ds/es directly
    mov ds, ax
    mov es, ax

    ; setup stack
    mov ss, ax
    mov sp, 0x7C00
    
    ; read something from floppy 
    ; BIOS should set DL to drive number
    mov [ebr_drive_number], dl
    
    mov ax, 1               ; LBA=1, second sector from disk
    mov cl, 1               ; 1 sector to read
    mov bx, 0x7E00          ; data should be after the bootloader
    call disk_read

    ; print message
    mov si, msg_hello
    call puts

    cli                 ; disable interrupts, so CPU can't escape "halt" state
    hlt

floppy_error:
    mov si, msg_read_failed
    call puts
    jmp wait_key_and_reboot

wait_key_and_reboot:
    mov ah, 0
    int 16h             ; wait for keypress
    jmp 0FFFFh:0        ; jump to beginning of BIOS, should reboot

.halt:
    cli                 ; disable interrupts, so CPU can't escape "halt" state
    hlt

;
; Disk routines
;

;
; Converts an LBA address to a CHS address
; Parameters:
;   - ax: LBA address
; Returns:
;   - cx[bits 0-5]: sector number
;   - cx[bits 6-15]: cylinder
;   - dh: head
;
lba_to_chs:

    push ax
    push dx

    xor dx,dx                           ; dx = 0
    div word [bdb_sectors_per_track]    ; ax = LBA / SectorsPerTrack
                                        ; dx = LBA % SectorsPerTrack
    inc dx                              ; dx = (LBA % SectorsPerTrack + 1) = sector
    mov cx, dx                          ; cx = sector

    xor dx, dx
    div word [bdb_heads]                ; ax = (LBA / SectorsPerTrack) / heads = cylinder
                                        ; dx = (LBA / SectorsPerTrack) % heads = head
    mov dh, dl                          ; dh = head                                        
    mov ch, al                          ; ch = cylinder (lower 8 bits)
    shl ah, 6
    or cl, ah                           ; put upper 2bits of cylinder in cl
    
    pop ax
    mov dl, al                          ; restore DL
    pop ax
    ret

;
; Reads sectors from a disk
; Parameters:
;   - ax: LBA address
;   - cl: number of sectors to read (up to 128)
;   - dl: drive number
;   - es:bx: memory location where to store read data
;
disk_read:

    push ax                             ; save registers we will modify
    push bx
    push cx
    push dx
    push di

    push cx                             ; temp. save cl (number of sectors to read)
    call lba_to_chs                     ; compute CHS
    pop ax                              ; al = number of sectors to read
    
    mov ah, 02h
    mov di, 3                           ; retry count

.retry:
    pusha                               ; save all registers, as we don't know what bios might modify 
    stc                                 ; set carry flag, as some BIOS don't set it
    int 13h                             ; carry flag cleared = success
    jnc .done                           ; jump if carry not set
    
    ; read failed
    popa
    call disk_reset
    dec di
    test di, di
    jnz .retry

.fail:
    ; all attempts over
    jmp floppy_error

.done:
    popa
    
    pop di                             ; restore registers modified
    pop dx
    pop cx
    pop bx
    pop ax
    ret

;
; Resets disk controller
; Parameteres:
;   dl: drive number
;
disk_reset:
    pusha
    mov ah, 0
    stc
    int 13h
    jc floppy_error
    popa
    ret


msg_hello: db 'hey world!!!!', ENDL, 0
msg_read_failed: db 'Read from disk failed.', ENDL, 0

times 510-($-$$) db 0
dw 0AA55h