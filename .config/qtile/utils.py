# special dictionary that allows in-place redefinition
class SettingsDict(dict):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def extend(self, **kwargs):
        new_dict = self.copy()
        for key in kwargs:
            new_dict[key] = kwargs[key]
        return new_dict
