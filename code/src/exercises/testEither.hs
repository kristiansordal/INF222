extractArgs [] _ = []
extractArgs (arg : args) f = f arg : extractArgs args f
