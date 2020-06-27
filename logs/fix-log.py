import sys
import re
import argparse

regex_locrecords = r'\(\$ loc|LOC ([-+]?[0-9]*\.?[0-9]+) .* ([-+]?[0-9]*\.?[0-9]+) .* ([-+]?[0-9]*\.?[0-9]+)\)'

def main():
  """
  Given a filename of a log in the Blocks World output format (relative path from root eta directory), apply
  a fix to some syntax of the logs (potentially spanning multiple lines). Currently only fixes loc records format.
  """
  parser = argparse.ArgumentParser(description='Process BW log')
  parser.add_argument("filename")
  parser.add_argument("--fixlocrecords", action="store_true")
  args = parser.parse_args()
  filename = args.filename
  fixlocrecords = args.fixlocrecords

  filename_in = filename
  filename_out = 'logs_fixed/' + filename.split('/')[-1]

  with open(filename_in,'r') as f_in:

    fixed_str = ''

    for line in f_in.readlines():
      fixed_str = fixed_str + line

    if fixlocrecords:
      fixed_str = re.sub(regex_locrecords, r'($ loc :x \1 :y \2 :z \3)', fixed_str, flags=re.DOTALL)

    with open(filename_out, 'w') as f_out:
      f_out.write(fixed_str)

    # log_num = 0
    # turn_tuple = dict()
    # f_out = None

    # for line in f_in.readlines():
    #   parts = line.split(': ')

    #   if len(parts) < 2:
    #     continue

    #   else:
    #     prefix, content = parts

    #     # If prefix is SESSION, close current log file, increment log_num, and start writing to new file
    #     if prefix.upper() == 'SESSION':
    #       # print('make new file: %s' % log_num)
    #       if f_out:
    #         f_out.close()
    #       f_out = open(filename_out + '_' + str(log_num),'w')
    #       log_num += 1
    #       continue

    #     _, action = prefix.split(') ')
    #     content = content.strip()

    #     # Add block move(s) to turn tuple, fixing move format if flag enabled
    #     if action == 'BLOCK MOVE':
    #       if fixmoves:
    #         content = re.sub(regex_moves, r'(from.p-arg \7) (to.p-arg \2)', content)
    #       if fixnames:
    #         content = re.sub(regex_names, r'(the.d (\1 block.n))', content)
    #       if fixlocrecords:
    #         content = re.sub(regex_locrecords, r'($ loc :x \1 :y \2 :z \3)', content)
    #       turn_tuple[action] = turn_tuple[action] + [content] if action in turn_tuple else [content]
    #     # Add any other action to turn tuple
    #     else:
    #       turn_tuple[action] = content

    #     # Each user turn is completed on USER_FEEDBACK
    #     if action == 'USER_FEEDBACK':

    #       # If user accidentally gives wrong input for USER_FEEDBACK, just treat as error
    #       if turn_tuple['USER_FEEDBACK'] not in ['c', 'i', 'p', 'e']:
    #         turn_tuple['USER_FEEDBACK'] = 'e'

    #       # If non-historical question, change USER_FEEDBACK to 'xc' or 'xi'
    #       # TODO: currently this removes questions like 'what is the fifth block I moved', which we don't want
    #       if 'is' in turn_tuple['USER'].lower() or 'are' in turn_tuple['USER'].lower():
    #         turn_tuple['USER_FEEDBACK'] = 'x' + turn_tuple['USER_FEEDBACK']

    #       # Create and write output string on new line
    #       str_user = '"' + turn_tuple['USER'].lower() + '"' if 'USER' in turn_tuple else '"None"'
    #       str_moves = '(' + ' '.join(turn_tuple['BLOCK MOVE']) + ')' if 'BLOCK MOVE' in turn_tuple else 'None'
    #       str_david = '"' + turn_tuple['DAVID'].upper() + '"' if 'DAVID' in turn_tuple else '"None"'
    #       str_feedback = turn_tuple['USER_FEEDBACK'].lower() if 'USER_FEEDBACK' in turn_tuple else 'None'
    #       str_out = '(%s %s %s %s)' % (str_user, str_moves, str_david, str_feedback)
    #       f_out.write(str_out + '\n')

    #       turn_tuple.clear()


if __name__ == '__main__':
	main()