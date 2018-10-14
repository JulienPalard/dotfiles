class FontSize:
    def __init__(self):
        self.font_size = 20

    def __iadd__(self, x):
        self.font_size += x
        print(f"\33]50;xft:LiberationMono:size={self.font_size}\007")
        return self

    def __isub__(self, x):
        self.font_size -= x
        print(f"\33]50;xft:LiberationMono:size={self.font_size}\007")
        return self


font_size = FontSize()
