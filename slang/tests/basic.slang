let n: int ref = ref 0 in
  begin
    n := 5 + 3;
    !n
  end
end
