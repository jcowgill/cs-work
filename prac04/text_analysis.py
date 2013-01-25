# Problem 1 - Text Analysis
#


def analyze_file(file_data):
    '''Analyzes the text in the given file'''
    word_frequency = {}
    word_count = 0
    word_len_total = 0
    word_len_frequency = {}

    # Process each line separately (more efficient)
    for line in file_data:
        # Split line and analyze each word
        words = line.split()
        word_count += len(words)

        for word in words:
            # Record word in each structure
            word_len_total += len(word)

            if word in word_frequency:
                word_frequency[word] += 1
            else:
                word_frequency[word] = 1

            if len(word) in word_len_frequency:
                word_len_frequency[len(word)] += 1
            else:
                word_len_frequency[len(word)] = 1

    # Return turple of data
    return (word_frequency, word_count, float(word_len_total) / word_count, \
            word_len_frequency)
