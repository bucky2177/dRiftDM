# Copy Class Attributes from One Object to Another

This function transfers class attributes from an `old` object to a `new`
object, ensuring that `new` inherits the class structure and missing
attributes of `old`. The primary purpose is to enforce class consistency
and restore any lost attributes when modifying or combining objects. It
is used in the internals of the package and it is not exported.

## Usage

``` r
copy_class_attributes(old, new)

# S3 method for class 'stats_dm'
copy_class_attributes(old, new)
```

## Arguments

- old:

  The source object from which class attributes will be copied.

- new:

  The target object to which class attributes will be assigned.

## Value

The modified `new` object with attributes and class from `old`.

## Details

The function assumes that all class attributes of `new` can be found in
`old`. Note also, that the order of attributes is not ensured.
