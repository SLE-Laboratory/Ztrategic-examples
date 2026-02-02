#csv_headers($file, $sequence)
csv_headers() {
    local filename=$1
    shift
    echo "--," | head -c -1 >> $filename
    for v in "$@"
    do 
        echo "$v," | head -c -1 >> $filename
    done 
    echo "" >> $filename
}


#sequence of values to iterate
sequenceRepMin=$(seq 5000 5000 40000)
sequenceLet=$(seq 8 2 16)
sequenceSmells=$(seq 25 25 500)
sequenceLetPrint=$(seq 7)

timeRepMin='timeRepMin.log'
memoryRepMin='memoryRepMin.log'
timeLet='timeLet.log'
memoryLet='memoryLet.log'
timeSmells='timeSmells.log'
memorySmells='memorySmells.log'
timeLetPrint='timeLetPrint.log'
memoryLetPrint='memoryLetPrint.log'


#(we could) compile with 
#ghc Examples.hs -O2 -main-is Examples -o Examples -no-keep-hi-files -no-keep-o-files
#instead we delegate the whole build job to stack
stack install

#cleanup; -f for silent
rm -f $timeRepMin $memoryRepMin $timeLet $memoryLet $timeSmells $memorySmells $timeLetPrint $memoryLetPrint

#----------
#----
#--- Time benchmarks  
#----
#----------

#time
#RepMin
echo "Beginning time benchmarks for RepMin in file $timeRepMin..."
csv_headers $timeRepMin $sequenceRepMin
for program in repminNone repminTUTP repminSTUTP repminAG repminMemo repminZtrategic repminZtrategicS
do 
    echo "$program," | head -c -1 >> $timeRepMin
    for input_size in $sequenceRepMin 
    do 
        { /usr/bin/time -p ./Examples $program $input_size > /dev/null; } 2>&1 | grep real | grep -Eo '[0-9]+([.][0-9]+)?' | head -c -1 >> $timeRepMin
        echo "," | head -c -1 >> $timeRepMin
    done
    echo "" >> $timeRepMin
done
echo "Done!"

#time
#Let
echo "Beginning time benchmarks for Let in file $timeLet..."
csv_headers $timeLet $sequenceLet
for program in letOptNone letOptZtrategic letOptMemoZtrategic letOptMemoZtrategicS letOptAG letOptMemoZtrategicClearing
do 
    echo "$program," | head -c -1 >> $timeLet
    for input_size in $sequenceLet
    do 
        { /usr/bin/time -p ./Examples $program $input_size > /dev/null; } 2>&1 | grep real | grep -Eo '[0-9]+([.][0-9]+)?' | head -c -1 >> $timeLet
        echo "," | head -c -1 >> $timeLet
    done
    echo "" >> $timeLet
done
echo "Done!"

#time
#Smells
echo "Beginning time benchmarks for Smells in file $timeSmells..."
csv_headers $timeSmells $sequenceSmells
for program in smellsNone smells smellsTerminals smellsStrafunski
do 
    echo "$program," | head -c -1 >> $timeSmells
    for input_size in $sequenceSmells 
    do 
        { /usr/bin/time -p ./Examples $program $input_size > /dev/null; } 2>&1 | grep real | grep -Eo '[0-9]+([.][0-9]+)?' | head -c -1 >> $timeSmells
        echo "," | head -c -1 >> $timeSmells
    done
    echo "" >> $timeSmells
done
echo "Done!"

#time
#LetPrint
echo "Beginning time benchmarks for LetPrint in file $timeLetPrint..."
csv_headers $timeLetPrint $sequenceLetPrint
for program in letPrint letPrintMemo
do 
    echo "$program," | head -c -1 >> $timeLetPrint
    for input_size in $sequenceLetPrint
    do 
        { /usr/bin/time -p ./Examples $program $input_size > /dev/null; } 2>&1 | grep real | grep -Eo '[0-9]+([.][0-9]+)?' | head -c -1 >> $timeLetPrint
        echo "," | head -c -1 >> $timeLetPrint
    done
    echo "" >> $timeLetPrint
done
echo "Done!"

#----------
#----
#--- Memory benchmarks  
#----
#----------

#memory
#RepMin
echo "Beginning memory benchmarks for RepMin in file $memoryRepMin..."
csv_headers $memoryRepMin $sequenceRepMin
for program in repminNone repminTUTP repminSTUTP repminAG repminMemo repminZtrategic repminZtrategicS
do 
    echo "$program," | head -c -1 >> $memoryRepMin
    for input_size in $sequenceRepMin 
    do 
        { /usr/bin/time -v ./Examples $program $input_size > /dev/null; } 2>&1 | grep "Maximum resident" | sed "s/[^0-9]\+\([0-9]\+\).*/\1/"  | head -c -1 >> $memoryRepMin
        echo "," | head -c -1 >> $memoryRepMin
    done
    echo "" >> $memoryRepMin
done
echo "Done!"

#memory
#Let
echo "Beginning memory benchmarks for Let in file $memoryLet..."
csv_headers $memoryLet $sequenceLet
for program in letOptNone letOptZtrategic letOptMemoZtrategic letOptMemoZtrategicS letOptAG letOptMemoZtrategicClearing
do 
    echo "$program," | head -c -1 >> $memoryLet
    for input_size in $sequenceLet
    do 
        { /usr/bin/time -v ./Examples $program $input_size > /dev/null; } 2>&1 | grep "Maximum resident" | sed "s/[^0-9]\+\([0-9]\+\).*/\1/"  | head -c -1 >> $memoryLet
        echo "," | head -c -1 >> $memoryLet
    done
    echo "" >> $memoryLet
done
echo "Done!"

#memory
#Smells
echo "Beginning memory benchmarks for Smells in file $memorySmells..."
csv_headers $memorySmells $sequenceSmells
for program in smellsNone smells smellsTerminals smellsStrafunski
do 
    echo "$program," | head -c -1 >> $memorySmells
    for input_size in $sequenceSmells 
    do 
        { /usr/bin/time -v ./Examples $program $input_size > /dev/null; } 2>&1 | grep "Maximum resident" | sed "s/[^0-9]\+\([0-9]\+\).*/\1/"  | head -c -1 >> $memorySmells
        echo "," | head -c -1 >> $memorySmells
    done
    echo "" >> $memorySmells
done
echo "Done!"

#memory
#LetPrint
echo "Beginning memory benchmarks for LetPrint in file $memoryLetPrint..."
csv_headers $memoryLetPrint $sequenceLetPrint
for program in letPrint letPrintMemo
do 
    echo "$program," | head -c -1 >> $memoryLetPrint
    for input_size in $sequenceLetPrint 
    do 
        { /usr/bin/time -v ./Examples $program $input_size > /dev/null; } 2>&1 | grep "Maximum resident" | sed "s/[^0-9]\+\([0-9]\+\).*/\1/"  | head -c -1 >> $memoryLetPrint
        echo "," | head -c -1 >> $memoryLetPrint
    done
    echo "" >> $memoryLetPrint
done
echo "Done!"
