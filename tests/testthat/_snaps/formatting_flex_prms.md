# summary.flex_prms and corresponding print works as expected

    Code
      print(sum_obj)
    Output
      Parameter Values:
          a  b  c
      foo 2  3  4
      bar 2 -8 10
      ho  2 -2  4
      
      Parameter Settings:
          a b c
      foo 1 4 0
      bar 2 d 5
      ho  3 d 5
      
      Special Dependencies:
      b ~ bar == ((a ~ foo) + -(c ~ bar))
      b ~ ho == -(a ~ foo)
      
      Custom Parameters:
          lorem
      foo   3.0
      bar  -3.2
      ho   -2.0
      

---

    Code
      print(a_flex_prms)
    Output
      Parameter Values:
          a  b  c
      foo 2  3  4
      bar 2 -8 10
      ho  2 -2  4
      
      Parameter Settings:
          a b c
      foo 1 4 0
      bar 2 d 5
      ho  3 d 5
      
      Special Dependencies:
      b ~ bar == ((a ~ foo) + -(c ~ bar))
      b ~ ho == -(a ~ foo)
      
      Custom Parameters:
          lorem
      foo   3.0
      bar  -3.2
      ho   -2.0
      

