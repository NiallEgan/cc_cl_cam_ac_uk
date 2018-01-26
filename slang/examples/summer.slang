let n: int ref = ref ? in
  let sum: int ref = ref 0 in
    while 0 < !n do
      begin sum := (!sum + !n); n := (!n - 1) end
    end  (* this is a (* nested *)comment *)
  end
end
