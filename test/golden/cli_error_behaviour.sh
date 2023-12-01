# expression that yields only zero
## no files requested 
currycarbon "uncalC14(3200,30) * rangeBP(3000,2800)"
## hdr file requested
currycarbon "uncalC14(3200,30) * rangeBP(3000,2800)" --hdrFile /dev/null
## samples file requested
currycarbon "uncalC14(3200,30) * rangeBP(3000,2800)" --samplesFile /dev/null -n 3
## hdr & samples file requested
currycarbon "uncalC14(3200,30) * rangeBP(3000,2800)" --hdrFile /dev/null --samplesFile /dev/null -n 3