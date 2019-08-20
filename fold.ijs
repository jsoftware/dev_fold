NB. Fold
Fold_j_ =: 2 : 0
'init mult fwd rev' =. 2 2 2 2 #: Foldtype_j_
4!:55 <'Foldtype_j_'
fzv =. FoldZv_j_
FoldZv_j_ = 0
if. dir =. fwd-rev do.  NB. if forward or reverse...
  nitems =. init + #y  NB. total # items, including init if any
  if. nitems=0 do.  NB. empty arg, treat as u/
    res =. v. u./ y
  else.
    cellx =. -rev  NB. first cell index, either 0 (fwd) or _1 (rev)
    if. init do. cellres =. x else. cellres =. cellx { y end.  NB. cellres=first cell
    if. nitems=1 do.  NB. if only one cell, run v on it
      try. res =. v. cellres  NB. try v
      catch. 13!:8 (3 3&, {~ 43 44&i.) 13!:11 '' [ FoldZv_j_ =. fzv  NB. if error, pass it through; if no results, error
      end.
    else.  NB. at least 2 cells.  We will run the loop
      cellx =. -init  NB. 1 less than index of first cell for left side
      res =. 0$a:   NB. init list of boxed results - to empty
      while. (FoldZv_j_<:0) *. (cellx=.>:cellx) < #y do.
        try. vres =. v. cellres =. ((24 b.^:rev cellx) { y) u. cellres
        catch.
          if. 43 = 13!:11'' do. break. end.  NB. abort iteration and end
          if. 44 = 13!:11'' do. continue. end.  NB. abort iteration and continue
          13!:8 ] 13!:11 '' [ FoldZv_j_ =. fzv  NB. not an iteration control, fail with that error code
        end.
        NB. continuing iteration.  cellres is the u result, vres is the boxed v result
        res =. res ,^:(mult *. FoldZv_j_>:0) <vres
      end.
      if. mult *. 0=# res do. 13!:8 (3) [ FoldZv_j_ =. fzv end.  NB. If nothing produced result, error
      res =. > res
    end.
  end.
else.  NB. repeated iteration on y, not related to items
  res =. '' [ cellres =. y   NB. no results, and 
  while. (FoldZv_j_<:0) do.
    try.
      if. init do. cellres =. x u. cellres else. cellres =. u. cellres end.
      vres =. v. cellres
    catch.
      if. 43 = 13!:11'' do. break. end.  NB. abort iteration and end
      if. 44 = 13!:11'' do. continue. end.  NB. abort iteration and continue
      13!:8 ] 13!:11 '' [ FoldZv_j_ =. fzv  NB. not an iteration control, fail with that error code
    end.
    NB. continuing iteration.  cellres is the u result, vres is the boxed v result
    res =. res ,^:(mult *. FoldZv_j_>:0) <vres
  end.
  if. mult *. 0=# res do. 13!:8 (3) [ FoldZv_j_ =. fzv end.
  res =. >res
end. 
)

   
FoldZ_j_ =: 4 : 0
assert. 0=#@$ y
assert. 0=#@$ x
if. y do.
  select. x do.
  case. _2 do.
    13!:8 ] 43
  case. _1 do.
    13!:8 ] 44
  case. 0 do.
  FoldZv_j_ =: _1
  case. 1 do.
  FoldZv_j_ =: 1
  end.
end.
$0
)

      
