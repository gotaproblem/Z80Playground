# Z80Playground
The "Z80 Playground" is a Z80 development board made by https://8bitstack.co.uk/

This repository offers an alternative ROM for those that are interested. It includes an enhanced Monitor and a "Real" CP/M 2.2 Custom BIOS (CBIOS). 
CP/M tools will be published once I have developed them.

### Initial Release:
**June 2021**
  * **"Real" CP/M 2.2 CBIOS**
  * **Four Disk Images**
  * **XModem Send and Receive**
  * **Disk Image formatter**
  
**CBIOS**  
CP/M is configured to have four drives.  
The A: and B: drives have a capacity of 244KB and use a skew of 6, whilst the C: and D: drives are 8MB hard disks.

**Disk Images**
  * **/A.DSK**
  * **/B.DSK**
  * **/C.DSK**
  * **/D.DSK**
  
**XModem Send and Receive**
  * **8PXR.COM** XModem Receive. Works up to 115200 baud without handshaking
  * **8PXS.COM** XModem Send.
 
**Disk Image Formatter**
  * **8PFORMAT.COM**. Format any of the four disk images
  
# Build
Assemble cpm22.asm and cbios.asm

Assemble main.asm

Flash EEPROM with main.bin
