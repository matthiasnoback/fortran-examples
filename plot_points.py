import sys
import matplotlib.pyplot as plt
import numpy as np


def read_data(source):
    lines = source.strip().split("\n")
    data = []
    for line in lines:
        x, y = map(float, line.split())
        data.append([x, y])
    return np.array(data)


if len(sys.argv) > 1:
    # Read from file
    filename = sys.argv[1]
    with open(filename, "r") as file:
        content = file.read()
else:
    # Read from stdin
    content = sys.stdin.read()

# Create the numpy array
array = read_data(content)

# Extract x and y values
x = array[:, 0]
y = array[:, 1]

# Create the plot
plt.plot(x, y, linestyle="-")
plt.xlabel("x")
plt.ylabel("y")
plt.title("Function")
plt.rcParams.update({"font.size": 16})
plt.show()
