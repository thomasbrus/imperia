# Imperia

A functional programming language that allows imperative like statements.

Everything evaluates to a value:

    length |list| = 1 + length |tail |list||

    winner = if won then "me" # => "me"
    congratulate |winners| = while length |winners| > 0 do
      "Congrats " ++ winner

    congratulate |[1, 2, 3]| # => ["Congrats 1", "Congrats 2", "..."]

# If statements

  


# List decomposition
    
    [x, y] = [1, 2]

    [x, xs..] = [1, 2, 3] # => x = 1, xs = [2, 3]

    length |[x, xs..]| = if x = [] 0 else 1 + length xs

    length |[1, 2, 3]|
      # x -> 1
      # xs -> [2, 3]