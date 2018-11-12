import os

with open(os.path.expanduser("~/.Xresources")) as xresources_file:
    for line in xresources_file:
        if line.startswith("URxvt.font"):
            font = line.split(":", 1)[1].strip()

class FontSize:
    def __init__(self):
        self.font_size = 20

    def __iadd__(self, x):
        self.font_size += x
        print(f"\33]710;xft:LiberationMono:size={self.font_size},{font}\007")
        return self

    def __isub__(self, x):
        self.font_size -= x
        print(f"\33]710;xft:LiberationMono:size={self.font_size},{font}\007")
        return self


font_size = FontSize()
