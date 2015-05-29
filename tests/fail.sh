#!/bin/sh

# test that we fail right

$@ -o non/existent/directory/output Makefile 2>/dev/null
test $? -eq 1 || { echo bad first failure; exit 1; }

## output doesn't yet know whether it goes to files, and failure must be tolerated with TCP
#$@ -o /dev/full readme.txt 2>/dev/null 
#test $? -eq 1 || { echo bad second failure; exit 1; }

$@ -o ok nonreadablesam.ple 2>/dev/null 
test $? -eq 2 || { echo bad third failure; exit 1; }

