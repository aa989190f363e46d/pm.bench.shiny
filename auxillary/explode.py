from re import compile

flSrc = file('pollutantmean.variants.R','r')
lstStrings = flSrc.read().split('\n')
flSrc.close()

reHeader = compile('# https://.+((post|comment)-\d+)')

flOutput = []

for strCurrent in lstStrings:
  matchRes = reHeader.match(strCurrent)
  if matchRes:

    print matchRes.groups(1)

    if isinstance(flOutput,file):
      flOutput.flush()
      flOutput.close()

    flOutput = file('%s.R' % (matchRes.group(1),),'w')
  else:
    flOutput.write('%s\n' % (strCurrent,))

flOutput.flush()
flOutput.close()