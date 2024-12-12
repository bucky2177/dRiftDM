# modify_flex_prms -> all instructions work as expected

    Code
      a_flex_prms_obj
    Output
      Current Parameter Matrix:
             a    b   c
      foo -0.2 -0.2 0.3
      bar -0.2 -0.2 0.3
      uff  0.3  0.4 0.3
      
      Unique Parameters:
          a b c
      foo d d 0
      bar d d 0
      uff 1 2 0
      
      Special Dependencies:
      a ~ foo == -(c ~ bar) * 2/3
      a ~ bar == -(c ~ bar) * 2/3
      b ~ foo == -(c ~ bar) * 2/3
      b ~ bar == -(c ~ bar) * 2/3
      

