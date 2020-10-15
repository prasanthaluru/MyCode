import re

original_sentence = ""
matrix = []
words = []

# Read Inputs
first_multiple_input = input().rstrip().split()

n = int(first_multiple_input[0])
m = int(first_multiple_input[1])

for _ in range(n):
    matrix_item = input()
    matrix.append(matrix_item)

# Transpose the matrix
transpose_matrix = [*zip(*matrix)]

for j in range(m):
    word = ''.join(transpose_matrix[j])
    words.append(word)

# Form the original sentence
original_sentence = ''.join(words)

# Identify last occurrence of alphanumeric code
index_of_last_alphanumeric = len(re.sub("[^a-zA-Z0-9]*$", "", original_sentence))

# Format the alphanumeric_part
#  - first replace non-alphanumerics with space
#  - replace multiple spaces
alphanumeric_part = original_sentence[0:index_of_last_alphanumeric]
formatted_sentence = re.sub('[^a-zA-Z\\d\\s]', " ", alphanumeric_part)
formatted_sentence = re.sub('\\s+', " ", formatted_sentence)

# finally rejoin the non-alphanumeric tail
result = formatted_sentence + original_sentence[index_of_last_alphanumeric:]
print(result)
