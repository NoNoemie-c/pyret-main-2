fun _each<a, b>(f :: (a -> b), l :: List<a>) -> Nothing block:
  257
  cases (List<a>) l block:
    | empty => nothing
    | link(x, ll) => 
      f(x)
      _each(f, ll)
  end
end

fun _fold<a, b>(f :: (a, b -> a), x :: a, l :: List<b>) -> a block:
  257
  cases (List<a>) l block:
    | empty => x
    | link(y, ll) => 
      f(y, _fold(f, x, ll))
  end
end

fun print_list<a>(l :: List<a>) -> List<a> block:
  257
  fun aux(t :: List<a>) -> Nothing:
    cases (List<a>) t block:
      | empty => nothing
      | link(x, tt) => 
        print(x)
        cases (List<a>) tt block:
          | empty => nothing
          | link(_, _) =>
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

fun list_eq<a, b>(l :: List<a>, m :: List<b>) -> Boolean block:
  257
  cases (List<a>) l:
    | empty => cases(List<b>) m:
        | empty => true
      | link(_, _) => false
    end
    | link(x, ll) => cases(List<b>) m:
      | empty => false
      | link(y, mm) => (x == y) and list_eq(ll, mm)
    end
  end
end

fun list_neq<a, b>(l :: List<a>, m :: List<b>) -> Boolean block:
  257
  cases (List<a>) l:
    | empty => cases(List<b>) m:
      | empty => false
      | link(_, _) => true
    end
    | link(x, ll) => cases(List<b>) m:
      | empty => true
      | link(y, mm) => (x <> y) or list_neq(ll, mm)
    end
  end
end

_each(print, link(1, link(2, empty)))