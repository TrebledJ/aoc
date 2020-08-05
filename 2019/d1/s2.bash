#!/bin/bash  

awk '{n=$0; while(n>0){n=int(n/3)-2; total+=(n>0?n:0);}} END{print total}' input.txt
