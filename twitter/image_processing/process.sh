#!/bin/bash

#echo "creo lista url"
#python url.py > list_url.txt
#echo "scarico immagini"
#aria2c --input-file list_url.txt -j10

for file in ./*.png
do
filename=$(basename -- "$file")
extension="${filename##*.}"
filename="${filename%.*}"
convert $filename.png $filename.jpg
rm $filename.png 
done

ls | grep '.jpg' >list.txt

echo "id,c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16,c17,c18,c19,c20,c21,c22,c23,c24,c25,c26,c27,c28,c29,c30,c31,c32,c33,c34,c35,c36,c37,c38,c39,c40,c41,c42,c43,c44,c45,c46,c47,c48,c49" >>out.csv

while read line
do 
echo "elaboro ${line}" 
./read_O1 $line >> out.csv
mv ./$line ./processed/
done < list.txt 
