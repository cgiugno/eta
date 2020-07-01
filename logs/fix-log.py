import sys
import re
import argparse

regex_locrecords = r'\(\$[\s]*LOC[\s]*([-+]?[0-9]*\.?[0-9]+)[\s]*([-+]?[0-9]*\.?[0-9]+)[\s]*([-+]?[0-9]*\.?[0-9]+)\)'

def main():
  """
  Given a filename of a log in the Blocks World output format (relative path from root eta directory), apply
  a fix to some syntax of the logs (potentially spanning multiple lines). Currently only fixes loc records format.
  """
  parser = argparse.ArgumentParser(description='Process BW log')
  parser.add_argument("filename")
  parser.add_argument("--fixlocrecords", action="store_true")
  parser.add_argument("--fixall", action="store_true")
  args = parser.parse_args()
  filename = args.filename
  fixlocrecords = args.fixlocrecords or args.fixall

  filename_in = filename
  filename_out = 'logs_fixed/' + filename.split('/')[-1]

  with open(filename_in,'r') as f_in:

    fixed_str = ''

    for line in f_in.readlines():
      fixed_str = fixed_str + line

    if fixlocrecords:
      fixed_str = re.sub(regex_locrecords, r'($ loc :x \1 :y \2 :z \3)', fixed_str)

    with open(filename_out, 'w') as f_out:
      f_out.write(fixed_str)


if __name__ == '__main__':
	main()