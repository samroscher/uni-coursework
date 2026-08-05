# -*- coding: utf-8 -*-

### Practice with regex metacharacters ###

import re

# Define Regex Function to Find Matches
def find_regex_matches(regex, text_str):
    matches = re.finditer(regex, text_str, re.MULTILINE)

    for matchNum, match in enumerate(matches, start=1):
        print("Match {matchNum} was found at {start}-{end}: {match}".format(
            matchNum=matchNum, start=match.start(), end=match.end(), match=match.group()))

        for groupNum in range(0, len(match.groups())):
            groupNum = groupNum + 1

            print("Group {groupNum} found at {start}-{end}: {group}".format(
                groupNum=groupNum, start=match.start(groupNum), end=match.end(groupNum),
                group=match.group(groupNum)))

# Complete list of metacharacters:
# . ^ $ * + ? { } [ ] \ | ( )

# Example 1: . is the wildcard (matches any character)
regex = r"number ."
test_str = "number , number a, number 123, anumber, anumber a"
find_regex_matches(regex, test_str)

# Example 2: ^ matches the start of a string
regex = r"^hello"
test_str = "helloab, ahello, abchello, hello"
find_regex_matches(regex, test_str)

# Example 3: $ matches the end of a string
regex = r"goodbye$"
test_str = "hello and 123goodbye"
find_regex_matches(regex, test_str)

# Example 4: * matches zero or more repetitions of a regex
regex = r"The solution is 42*"
test_str = "The solution is 42222222 and The solution is 423 and the solution is 424"
find_regex_matches(regex, test_str)

# Example 5: + matches one or more repetitions of a regex
regex = r"he+llo"
test_str = "heeeeeeeellooooo, hellllllo, hello"
find_regex_matches(regex, test_str)

# Example 6: ? matches zero or one repetitions of a regex
regex = r"The solution is 42?"
test_str = "The solution is 42 and The solution is 43 and The solution is 42222."
find_regex_matches(regex, test_str)

# Example 7: {x} matches exactly x repetitions of a regex
regex = r"he{5}llo"
test_str = "hello, heello, heeeeello, heeeeeeeeeello, heeeeellllllllo"
find_regex_matches(regex, test_str)

# Example 8: {x,y} matches from x to y repetitions of a regex
regex = r"he{5,10}llo"
test_str = "hello, heeello, heeeeello, heeeeeeeello, heeeeeeeeeeeeeeeeeeeeeeeello"
find_regex_matches(regex, test_str)

# Example 9: [] indicates a set of characters
regex = r"[hello]"
test_str = "hello olleh hoell h e l l o 123ll"
find_regex_matches(regex, test_str)

# Example 10: () define a group
regex = r"(abc)"
test_str = "a b c bca acb abc abcdefghijk"
find_regex_matches(regex, test_str)

# Example 11: \ escapes any metacharacter
regex = r"\("
test_str = "(hello) (123 hello everyone"
find_regex_matches(regex, test_str)

# Example 12: | between to regex means either or
regex = r"hello|goodbye"
test_str = "hello123, 345goodbye hellogoodbye hi"
find_regex_matches(regex, test_str)

# Example 13: Matching practice I
test_str = """In this course we're going to learn basic
concepts of natural language processing. On day 1 we'll
cover basic concepts, while day 2 will deal with more
complex topics."""
regex = r"[b-df-hj-np-tv-z]+[a-z]+"
find_regex_matches(regex, test_str)
# Alternative
output = re.findall(regex, test_str)
print(f'Output of length {len(output)}:\n{output}')

# Example 14: Matching practice II
regex = r"[a-z]+l "
find_regex_matches(regex, test_str)
# Alternative
output = re.findall(regex, test_str)
print(f'Output of length {len(output)}:\n{output}')

# Example 15: Substitution practice
regex= r"[0-9]"
from pprint import pprint
pprint(re.sub(regex, "digit", test_str), width = 65, compact = True)

# Example 16: Splitting practice
regex = r"day [0-9]"
pprint(re.split(regex, test_str), width = 65, compact = True)

# Example 17: Matching practice III
re.match(r"on", test_str)
re.match(r"at", test_str)
re.match(r"in", test_str)
re.match(r"In", test_str)

# Example 18: Removing stopwords
stop_list = [" the", "my", "you", "a", "of", "in", "on", "to "]
pprint(re.sub(r" |".join(stop_list), " ", test_str), width = 65, compact = True)
# Try with test_str.lower() instead of test_str. What is the result?

# Example 19: Expand contractions
pprint(re.sub(r"we're", "we are", test_str), width = 65, compact = True)

# Pretty print comparison

from pprint import pprint

text = "This is an example of a long sentence that needs to be printed in a compact format."

# Non-compact format, width is ignored
pprint(text.split(" "), width=30, compact=False)

# Compact format, visualization of lines until n = width characters are printed
pprint(text.split(" "), width=30, compact=True)
