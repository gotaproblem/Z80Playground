# Z80 Playground
The "Z80 Playground" is a Z80 development board made by https://8bitstack.co.uk/

The original software uses the FAT file system to run CP/M. This has several disadvantages, including not being able to run submit files.
This repository offers an alternative ROM for those that are interested. It includes an enhanced Monitor and "Real" BIOS' for CP/M 2.2 and CP/M 3.1 (Plus).
Several CP/M tools are available (see below) to aid your experience.

### Initial Release:
**June 2021**
  * **"Real" CP/M 2.2 CBIOS**
  * **Four Disk Images**
  * **XModem Send and Receive**
  * **Disk Image formatter**
**July 2021**
  * **FAT file transfer tool**
**November 2021**
  * **"Real" CP/M 3.1 CBIOS**
  
**CP/M 2.2**
MON> cpm2
A CBIOS has been developed to run CP/M 2.2. It is configured to have four drives.  
The A: and B: drives have a capacity of 244KB and use a skew of 6, whilst the C: and D: drives are 8MB hard disks.
The A: drive contains files for a Z-System known as NZCOM. Just execute A>NZCOM to invoke, or leave alone, to experiance standard CP/M 2.2.
The C: drive contains CP/M 2.2 utilities.

**CP/M 3.1 (Plus)**
MON> cpm3
CP/M 3 is also configured to use the same four drives. However, the A: drive uses a different disk image - CPM3.DSK. This is transparent to the User; the User will always see A: drive.
The A: drive contains a few CP/M 3.1 utilities to get you going
The D: drive has the CP/M 3.1 source, allowing for customisation

**Disk Images**
  * **/CPM3.DSK** CP/M 3.1 boot disk
  * **/A.DSK** CP/M 2.2 boot disk
  * **/B.DSK** FDD empty
  * **/C.DSK** CP/M 2.2 tools and games etc.
  * **/D.DSK** CP/M 3.1 source files and tools to build
  
**XModem Send and Receive**
  * **8PXR.COM** XModem Receive. Works up to 115200 baud without handshaking
  Syntax: A>8pxr <ufn:>filename
  * **8PXS.COM** XModem Send.
  Syntax: A>8pxs <ufn:>filename
 
**Disk Image Formatter**
  * **8PFORMAT.COM**. Format any of the four disk images
  Syntax: A>8pformat <drive>
  
**File TransFER between USB FAT and CP/M**
  * **8PTFER.COM**. Transfer files (read and write) between USB Drive and CP/M
  Syntax: A>8ptfer </r | /w> <ufn:>filename

# Build ROM
Assemble cpm22.asm and cbios.asm
Assemble main.asm

Flash EEPROM with main.bin
