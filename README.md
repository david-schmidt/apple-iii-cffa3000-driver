## Introduction

The CFFA3000 is a USB/Compact Flash interface card for the Apple II and Apple /// computers. This project is the driver required for the Apple /// to recognize disk images hosted by the CFFA3000 card.

## Driver highlights:

 *  Supports eight SmartPort devices configured on the CFFA3000
 *  Has hot-swapping capabilities - drives are "removable" the same way a floppy is
 *  Has an integrated formatter that lays down a SOS/ProDOS filesystem structure
 *  Runs at full speed... 2 blazing MHz. No downshifting required, unlike the original CFFA hardware.

## Building

Building requires a python interpreter in order to do the System Configuration Program (SCP) duties as delivered from Rob Justice's [a3driverutil project](https://github.com/robjustice/a3driverutil) and [ca65](https://github.com/cc65/cc65) in order to do the assembling and linking.
