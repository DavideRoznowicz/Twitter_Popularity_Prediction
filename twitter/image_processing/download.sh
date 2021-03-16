#!/bin/bash
echo "creo lista url"
python url.py > list_url.txt
echo "scarico immagini"
aria2c --input-file list_url.txt -j15
