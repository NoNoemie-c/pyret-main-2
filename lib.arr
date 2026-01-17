fun _each<a, b>(f :: (a -> b), l :: List<a>) -> Nothing:
  cases (List<a>) l block:
    | empty => nothing
    | link(x, ll) => 
      f(x)
      _each(f, ll)
  end
end

fun _fold<a, b>(f :: (a, b -> a), x :: a, l :: List<b>) -> a:
  cases (List<a>) l block:
    | empty => x
    | link(y, ll) => 
      f(y, _fold(f, x, ll))
  end
end

fun print_list<a>(l :: List<a>) -> List<a> block:
  fun aux(t :: List<a>) -> Nothing:
    cases (List<a>) t block:
      | empty => nothing
      | link(x, tt) => 
        print(x)
        cases (List<a>) tt block:
          | empty => nothing
          | link(y, ttt) =>
            print(", ")
            aux(tt)
        end
    end
  end
  print("[list: ")
  aux(l)
  print("]")
  l
end