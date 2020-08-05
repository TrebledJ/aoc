#!/bin/bash

awk '{sum += int($0/3)-2} END{print sum}' input.txt
