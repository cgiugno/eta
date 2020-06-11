import sys
import re
from os import listdir
from os.path import isfile, join


def prepend_path(pdir, logs):
  """
  Appends directory to paths in logs.
  """
  return [pdir + log for log in logs]


def group_files(logs):
  """
  Groups files in logs by source.
  """
  logs_split = [log.split('_') for log in logs]
  logs_grouped = dict()

  for log_split in logs_split:
    source = '_'.join(log_split[0:3])
    log = '_'.join(log_split)
    logs_grouped[source] = logs_grouped[source] + [log] if source in logs_grouped else [log]
  
  return logs_grouped


def compute_accuracy(logs):
  """
  Computes the combined accuracy of the files in logs. Returns accuracy for only historical questions,
  and accuracy overall.
  Accuracy for only historical questions is calculated as: #c / (#c + #i + #p + #e), and for overall
  the values prefixed with 'x' are included analogously.
  In principle partially correct (#p) could be included in the numerator, but it generally
  makes little difference.
  """
  c, i, p, e = 0, 0, 0, 0
  xc, xi, xp, xe = 0, 0, 0, 0

  for log in logs:
    with open(log,'r') as f:
      for line in f.readlines():
        if ' C)' in line.upper():
          c += 1
        elif ' I)' in line.upper():
          i += 1
        elif ' P)' in line.upper():
          p += 1
        elif ' E)' in line.upper():
          e += 1
        elif ' XC)' in line.upper():
          xc += 1
        elif ' XI)' in line.upper():
          xi += 1
        elif ' XP)' in line.upper():
          xp += 1
        elif ' XE)' in line.upper():
          xe += 1
  
  acc_hist = c / (c+i+p+e)
  acc_all = (c+xc) / (c+xc+i+xi+p+xp+e+xe)

  return acc_hist, acc_all



def main():
  """
  Calculate the accuracy of the files in logs_out, as well as the accuracy of the files in logs.
  """

  path_logs = 'logs/logs/'
  path_logs_out = 'logs/logs_out/'
  logs = [f for f in listdir(path_logs) if isfile(join(path_logs, f))]
  logs_out = [f for f in listdir(path_logs_out) if isfile(join(path_logs_out, f))]

  # Group logs based on source
  logs_grouped = group_files(logs)
  logs_out_grouped = group_files(logs_out)

  # Prepend directories to paths
  logs = prepend_path(path_logs, logs)
  logs_out = prepend_path(path_logs_out, logs_out)
  for source in logs_grouped:
    logs_grouped[source] = prepend_path(path_logs, logs_grouped[source])
  for source in logs_out_grouped:
    logs_out_grouped[source] = prepend_path(path_logs_out, logs_out_grouped[source])

  # Get accuracies of each source
  for source in logs_grouped:
    acc_logs_source_hist, acc_logs_source_all = compute_accuracy(logs_grouped[source])
    print('Accuracy of initial logs (%s): %.2f (hist), %.2f (all)' % (source, acc_logs_source_hist, acc_logs_source_all))
  
  for source in logs_out_grouped:
    acc_logs_out_source_hist, acc_logs_out_source_all = compute_accuracy(logs_out_grouped[source])
    print('Accuracy of final logs (%s): %.2f (hist), %.2f (all)' % (source, acc_logs_out_source_hist, acc_logs_out_source_all))

  print('----------------------------------------------------------------')

  # Get overall accuracies
  acc_logs_hist, acc_logs_all = compute_accuracy(logs)
  acc_logs_out_hist, acc_logs_out_all = compute_accuracy(logs_out)
  print('Accuracy of initial logs: %.2f (hist), %.2f (all)' % (acc_logs_hist, acc_logs_all))
  print('Accuracy of final logs: %.2f (hist), %.2f (all)' % (acc_logs_out_hist, acc_logs_out_all))



if __name__ == '__main__':
	main()