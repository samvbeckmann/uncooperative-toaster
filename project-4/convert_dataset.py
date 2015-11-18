import sys

f = open('cars.data', 'w')
f.write('(\n')
with open(sys.argv[1], 'r') as infile:
    for line in infile:
        parts = line.split(',')
        str = '\t('
        if parts[6] == 'unacc\n':
            str += '-'
        else:
            str += '+'
        str += ' (buying %s)' % parts[0]
        str += ' (maint %s)' % parts[1]
        str += ' (doors %s)' % parts[2]
        str += ' (persons %s)' % parts[3]
        str += ' (lug_boot %s)' % parts[4]
        str += ' (safety %s))\n' % parts[5]
        f.write(str)
f.write(')\n')
f.close()
