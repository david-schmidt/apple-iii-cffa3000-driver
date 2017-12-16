## Introduction

The CFFA3000 is a USB/Compact Flash interface card for the Apple II and Apple /// computers. This project is the driver required for the Apple /// to recognize disk images hosted by the CFFA3000 card.
Details

## Driver highlights:

 *  Supports eight SmartPort devices configured on the CFFA3000
 *  Has hot-swapping capabilities - drives are "removable" the same way a floppy is
 *  Has an integrated formatter that lays down a SOS/ProDOS filesystem structure
 *  Runs at full speed... 2 blazing MHz. No downshifting required, unlike the original CFFA hardware.

## Building

Building the driver is a little complicated since it needs to be in Apple Pascal object module (PCD) format for the Apple /// System Configuration Program to accept it. The general steps are: 

  1. Copy the source program to a (virtual) floppy disk using the likes of CiderPress or AppleCommander 
  1. Modify the file attributes so that the file ends in the suffix .TEXT, and that the file has a TXT attribute (filetype $04) 
  1. Use the Apple II ProDOS utilities to copy the file from the ProDOS disk to a Pascal-formatted disk (this will convert the file into the goofy Pascal Text (PTX) format) 
  1. Start up a (real or virtual) Pascal development environment 
  1. Assemble the TEXT file
  1. Copy the resulting PCD file to an Apple /// System Utilities disk 
  1. Run the System Configuration Utility (SCP) program 
  1. Load the resident SOS.DRIVER driver file 
  1. Load the new CFFA3000 PCD driver file you produced 
  1. Generate a new system, save the new SOS.DRIVER with the CFFA3000 driver in it to your boot disk
