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
#Let
echo "Beginning time benchmarks for Let in file $timeLet..."
csv_headers $timeLet $sequenceLet
for program in letOptZtrategic letOptZtrategicLocal letOptZtrategicAdhoc letOptZtrategicAdhocLocal
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

#----------
#----
#--- Memory benchmarks  
#----
#----------

#memory
#Let
echo "Beginning memory benchmarks for Let in file $memoryLet..."
csv_headers $memoryLet $sequenceLet
for program in letOptZtrategic letOptZtrategicLocal letOptZtrategicAdhoc letOptZtrategicAdhocLocal
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