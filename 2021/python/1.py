#!/usr/bin/python3

def process_content(content):
    previous = content[0]
    count = 0
    for value in content[1:]:
        if value > previous:
            count += 1
        previous = value

    return count

def process_content_part2(content):
    previous_sum = sum(content[0:3])
    count = 0
    for i in range(1, len(content) - 2):
        current_sum = sum(content[i:i+3])
        if current_sum > previous_sum:
            count += 1
        previous_sum = current_sum

    return count

def main():
    with open("1.txt", "r") as f:
        content = [ int(s.replace("\n", "")) for s in f.readlines() ]
        print (process_content(content))
        print (process_content_part2(content))

if __name__ == "__main__":
    main()
